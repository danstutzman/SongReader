#!/usr/bin/env ruby

# adapted from http://paste.ly/5wrj

SERVER = 'imap.gmail.com'
USERNAME = ARGV[0] or raise "First arg: IMAP username"
PW = ARGV[1] or raise "Second arg: IMAP password"

require 'net/imap'
require 'rubygems'
require 'tmail'
require 'time'
require 'date'

# Extend support for idle command. See online.
# http://www.ruby-forum.com/topic/50828
# but that was wrong. see /opt/ruby-1.9.1-p243/lib/net/imap.rb.
class Net::IMAP
  def idle
    cmd = "IDLE"
    synchronize do
      @idle_tag = generate_tag
      put_string(@idle_tag + " " + cmd)
      put_string(CRLF)
    end
  end

  def say_done
    cmd = "DONE"
    synchronize do
      put_string(cmd)
      put_string(CRLF)
    end
  end

  def await_done_confirmation
    synchronize do
      get_tagged_response(@idle_tag, nil)
    end
  end
end

#Net::IMAP.debug = true
@imap = Net::IMAP.new SERVER, ssl: true

@imap.login USERNAME, PW
@imap.select 'INBOX'

# Add handler.
@imap.add_response_handler do |resp|
  if resp.kind_of?(Net::IMAP::UntaggedResponse) and resp.name == "EXISTS"
    @imap.say_done
    Thread.new do
      @imap.await_done_confirmation
      @imap.disconnect
      exit 1
    end
  end
end

@imap.idle
sleep (30 * 60) - 10 # wait 30 minutes

@imap.say_done
Thread.new do
  @imap.await_done_confirmation
  @imap.disconnect
end
