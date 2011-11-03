# use http digest instead of http basic authentication from restclient
module RestClient
  class Request
    def setup_credentials(req)
      req.digest_auth(user, password, @args[:denied_response_for_server_nonce]) if user
    end
  end
end
