require 'rubygems'
require 'maildir'
require 'mail'
require 'time'
require './smtp_tls_patch.rb'
require 'open3'
require 'rest-client'
require './net_http_digest_auth_patch.rb'
require './rest_client_digest_auth_patch.rb'

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

def run_command(command)
  puts command
  output = ''
  stdout, stderr, status = Open3.capture3(command)
  output = (stdout + stderr != '') ? stdout + stderr : 'No output'
  puts stdout
  puts stderr
  if status.exitstatus != 0
    raise GotNonZeroExitCode.new(status.exitstatus)
  end
end

create_dirs = false
maildir = Maildir.new('getmail', create_dirs)
maildir.list(:new).each { |message|
  email = Mail.new(message.data)
  puts email.from
  email.attachments.each_with_index { |attachment, i|
maildir.list(RUN_ONCE_FOR_TEST ? :cur : :new).each { |message|
    if attachment.mime_type == 'image/jpeg'
      File.open("input/#{message.unique_name}.#{i}.jpeg", 'w') { |file|
        file.write(attachment.decoded)
        puts file.path
      }
      File.open("input/#{message.unique_name}.#{i}.json", 'w') { |file|
        file.write("[]\n")
      }

      begin
        case_name = "#{message.unique_name}.#{i}"

        # avoid "Unable to establish connection to compilation daemon" error
        run_command("/Applications/scala-2.8.1/bin/fsc >/dev/null")
        run_command("bin/run Ocr4Music #{case_name}")
        run_command("bin/midi2wav output/midi/#{case_name}.mid " +
                    "output/wav/#{case_name}.wav")
        run_command("bin/wav2mp3 #{case_name}")
        run_command("rm output/wav/#{case_name}.wav")

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

      rescue Exception => e
        #puts 'Sending error email...'
        #error_mail = Mail.new do
        #  from 'error@songreader.net'
        #  to 'dtstutz@gmail.com'
        #  subject 'Error from Song Reader'
        #  body output
        #end
        #error_mail.deliver!
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
