# Support for http digest auth
# Discovered here: http://johan.bingodisk.com/public/code/net_digest_auth.rb

require 'digest/md5'
require 'net/http'

module Net
  module HTTPHeader
    @@nonce_count = -1
    CNONCE = Digest::MD5.new("%x" % (Time.now.to_i + rand(65535))).hexdigest
    def digest_auth(user, password, response)
      # based on http://segment7.net/projects/ruby/snippets/digest_auth.rb
      @@nonce_count += 1

      response['www-authenticate'] =~ /^(\w+) (.*)/

      params = {}
      $2.gsub(/(\w+)="(.*?)"/) { params[$1] = $2 }

      a_1 = "#{user}:#{params['realm']}:#{password}"
      a_2 = "#{@method}:#{@path}"
      request_digest = ''
      request_digest << Digest::MD5.hexdigest(a_1)
      request_digest << ':' << params['nonce']
      request_digest << ':' << ('%08x' % @@nonce_count)
      request_digest << ':' << CNONCE
      request_digest << ':' << params['qop']
      request_digest << ':' << Digest::MD5.hexdigest(a_2)

      header = []
      header << "Digest username=\"#{user}\""
      header << "realm=\"#{params['realm']}\""
      
      header << "qop=#{params['qop']}"

      header << "algorithm=MD5"
      header << "uri=\"#{@path}\""
      header << "nonce=\"#{params['nonce']}\""
      header << "nc=#{'%08x' % @@nonce_count}"
      header << "cnonce=\"#{CNONCE}\""
      header << "response=\"#{Digest::MD5.hexdigest(request_digest)}\""
      header << "opaque=\"#{params['opaque']}\""

      @header['Authorization'] = header
    end
  end
end
