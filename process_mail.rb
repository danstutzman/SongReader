require 'rubygems'
require 'maildir'
require 'mail'
require 'time'
require './smtp_tls_patch.rb'
require 'open3'
require 'rest-client'
require './net_http_digest_auth_patch.rb'
require './rest_client_digest_auth_patch.rb'
require 'erb'

#UPLOAD_TO='http://www.songreader.net'
UPLOAD_TO='http://0.0.0.0:3000'

#RUN_ONCE_FOR_TEST=false
RUN_ONCE_FOR_TEST=true

Mail.defaults do
  delivery_method :smtp, {
    :address              => 'smtp.gmail.com',
    :port                 => 587,
    :domain               => 'localhost',
    :user_name            => 'beta@songreader.net',
    :password             => `cat ./email_password_for_smtp`,
    :authentication       => 'plain',
    :enable_starttls_auto => true,
    :openssl_verify_mode  => nil
  }
end

class GotNonZeroExitCode < StandardError
end

def run_command(command, outputs)
  puts command
  stdout, stderr, status = Open3.capture3(command)
  outputs[:stdout] += "#{stdout}\n"
  outputs[:stderr] += "#{stderr}\n"
  puts stdout
  puts stderr
  if status.exitstatus != 0
    raise GotNonZeroExitCode.new(status.exitstatus)
  end
end

create_dirs = false
maildir = Maildir.new('getmail', create_dirs)
maildir.list(RUN_ONCE_FOR_TEST ? :cur : :new).each { |message|
  photo_email = Mail.new(message.data)
  photo_email.attachments.each_with_index { |attachment, i|
    if attachment.mime_type == 'image/jpeg'
      File.open("input/#{message.unique_name}.#{i}.jpeg", 'w') { |file|
        file.write(attachment.decoded)
        puts file.path
      }
      File.open("input/#{message.unique_name}.#{i}.json", 'w') { |file|
        file.write("[]\n")
      }
      outputs = { :stdout => '', :stderr => '' }

      begin
        case_name = "#{message.unique_name}.#{i}"

        # avoid "Unable to establish connection to compilation daemon" error
        while true
          begin
            run_command("/Applications/scala-2.8.1/bin/fsc >/dev/null", outputs)
          rescue GotNonZeroExitCode => e
            raise unless (outputs[:stderr] || '').chomp ==
               'Could not connect to compilation daemon.'
          end
          break # only loop if got the "could not connect message"
        end
        
        run_command("bin/run Ocr4Music #{case_name}", outputs)
        run_command("bin/midi2wav output/midi/#{case_name}.mid " +
                    "output/wav/#{case_name}.wav", outputs)
        run_command("bin/wav2mp3 #{case_name}", outputs)
        run_command("rm output/wav/#{case_name}.wav", outputs)

        puts "Uploading to #{UPLOAD_TO}..."
        denied_response =
          Net::HTTP.get_response(URI.parse("#{UPLOAD_TO}/admin/photos"))
        RestClient::Request.execute(
          :method => :post,
          :url => "#{UPLOAD_TO}/admin/photos",
          :user => 'admin',
          :password => File.read('admin_digest_password').chomp,
          :denied_response_for_server_nonce => denied_response,
          :payload => {
            'photo[uploaded_picture]'.intern =>
            File.new("input/#{case_name}.jpeg", 'rb'),
            'photo[uploaded_mp3]'.intern =>
            File.new("output/mp3/#{case_name}.mp3", 'rb')
          }
        )

        puts 'Sending reply email...'
        mp3_filename = attachment.filename.gsub(/\.jpe?g$/i, '') + '.mp3'
        reply_mail = Mail.new do
          from 'Song Reader <beta@songreader.net>'
          to photo_email.from
          subject "Re: #{photo_email.subject}"
          body ERB.new(File.read('reply_email_good.txt.erb')).result
          add_file :filename => mp3_filename,
            :content => File.read("output/mp3/#{case_name}.mp3")
        end
        reply_mail.deliver!

      rescue Exception => e
        puts 'Sending error email to dtstutz...'
        error_mail1 = Mail.new do
          from 'Song Reader <beta@songreader.net>'
          to 'dtstutz@gmail.com'
          subject 'Error from Song Reader'
          body "___STDOUT\n#{outputs[:stdout]}" +
               "\n___STDERR\n#{outputs[:stderr]}" +
               "\n___Stack trace:\n#{e}\n#{e.backtrace.join("\n")}"
        end
        error_mail1.deliver!

        puts 'Sending error email to sender...'
        error_mail2 = Mail.new do
          from 'Song Reader <beta@songreader.net>'
          to photo_email.from
          subject "Re: #{photo_email.subject}"
          body ERB.new(File.read('reply_email_bad.txt.erb')).result
        end
        error_mail2.deliver!

        puts 'Quitting because of error'
        raise
  # EARLY EXIT
      end
    else
      puts "Skipped attachment with mime type #{attachment.mime_type}"
    end
  }
  break if RUN_ONCE_FOR_TEST
  message.process # move it from new/ to cur/ folder
}
