require 'rubygems'
require 'maildir'
require 'mail'
require 'time'
require './smtp_tls_patch.rb'
require 'open3'

create_dirs = false
maildir = Maildir.new('getmail', create_dirs)
maildir.list(:new).each { |message|
  email = Mail.new(message.data)
  puts email.from
  email.attachments.each_with_index { |attachment, i|
    if attachment.mime_type == 'image/jpeg'
      File.open("input/#{message.unique_name}.#{i}.jpeg", 'w') { |file|
        file.write(attachment.decoded)
        puts file.path
      }
      File.open("input/#{message.unique_name}.#{i}.json", 'w') { |file|
        file.write("[]\n")
      }

      command = "bin/run Ocr4Music #{message.unique_name}.#{i}"
      #command = "exit 2"
      puts command
      output = ''
      stdout, stderr, status = Open3.capture3(command)
      output = (stdout + stderr != '') ? stdout + stderr : 'No output'
      puts "Got error code of #{status.exitstatus}"

      if status.exitstatus != 0
        puts 'Sending error email...'
        from = 'beta@songreader.net'
        to = 'dtstutz@gmail.com'
        #content = ''
        #content += "From: #{from}\n"
        #content += "To: #{to}\n"
        #content += "Subject: Error from Song Reader\n"
        #content += "Date: #{Time.now.rfc2822}\n"
        #content += "\n"
        #content += output
        #Net::SMTP.enable_tls(OpenSSL::SSL::VERIFY_NONE)
        #Net::SMTP.start('smtp.gmail.com', 587, 'gmail.com',
        #    'beta@songreader.net', 'ddvpgiqgorvibcva', :login) do |smtp|
        #  smtp.send_message(content, from, to)
        #end
        
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
        error_mail = Mail.new do
          from 'error@songreader.net'
          to 'dtstutz@gmail.com'
          subject "Error from Song Reader"
          body output
        end
        error_mail.deliver!
        puts "Quitting..."
# EARLY EXIT
        exit status.exitstatus
      end
    else
      puts "Skipped attachment with mime type #{attachment.mime_type}"
    end
  }
  break
  email.process
}
