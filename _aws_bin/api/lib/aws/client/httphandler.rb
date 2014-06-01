require 'uri'
require 'net/http'
require 'net/https'
require 'aws/client/handler'

module AWS
  module Client

    #
    # Executes HTTP requests via the Net::HTTP library.  Supports HTTP, HTTPS and client X509 certificates.
    #
    # Copyright:: Copyright (c) 2008, 2012 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class HttpHandler < Handler
  

      # Instantiate a new HttpHandler with the specified arguments.  Possible arguments include:
      # [:verbose]
      #   If true, the handler will output the URI it is requesting to STDOUT.
      #   This may be useful for debugging purposes.
      # [:ca_file]
      #   This parameter's value points to a valid .pem certificate file to enable the 
      #   client to validate server certificates when using SSL.
      #   If this parameter is not specified, the client operates in insecure mode and does not
      #   validate that server certificates come from a trusted source.
      # [:timeout]
      #   This value (in seconds) will be used for every socket operation during the request.
      #   Note that since a request can involve many socket operations, calls that timeout may 
      #   actually take more time than this value.  If unspecified, defaults to 5.0 seconds.  
      #   A value of zero will result in an infinite timeout.
      # [:connect_timeout]
      #   This value (in seconds) will be used as the timeout for opening a connection to the 
      #   service.  If unspecified, defaults to 5.0 seconds.  A value of zero will result in
      #   an infinite timeout.
      def initialize(args = {})
        @verbose = args[:verbose]
        @ca_file = args[:ca_file]
        @connect_timeout = args[:connect_timeout]
        @timeout = args[:timeout]

        @connect_timeout = 5.0 if @connect_timeout.nil?
        @timeout = 5.0 if @timeout.nil?

        raise ArgumentError, "connect_timeout must be non-negative" if @connect_timeout < 0
        raise ArgumentError, "timeout must be non-negative" if @timeout < 0
      end

      def before(job)
        identity = job.request[:identity]
        uri = job.request[:http_uri]
        verb = job.request[:http_verb]
        query_string = job.request[:query_string]
        headers = job.request[:headers]

        verb = 'GET' if verb.nil?

        result = http_request(uri, headers, verb, query_string, identity[:http_client_x509], identity[:http_client_x509_key])

        job.reply[:value] = result.body
        job.reply[:http_status_code] = result.code
        job.reply[:http_status_message] = result.message
        job.reply[:http_content] = nil # TODO: get content-type header
      end

      private
      
      def http_request(uri, headers, verb, query_string = nil, cert = nil, key = nil)
        puts "Requesting URL:\n#{uri}\nQuery string:\n#{query_string}\n" if @verbose

        http = Net::HTTP.new(uri.host, uri.port)
        http.read_timeout = @timeout;
        http.open_timeout = @connect_timeout;

        if(uri.scheme == 'https')
          # enable SSL
          http.use_ssl = true

          if @ca_file.nil?
            raise "Cannot load SSL certificate."
          else
            http.verify_mode = OpenSSL::SSL::VERIFY_PEER
            http.ca_file = @ca_file
          end

          store = OpenSSL::X509::Store.new
          store.set_default_paths
          http.cert_store = store

          # negotiate with the client certificate, if one is present
          unless(cert.nil? || key.nil?)
            http.cert = OpenSSL::X509::Certificate.new(cert)
            http.key = OpenSSL::PKey::RSA.new(key)
          end
        end

        if verb == 'GET'
          http.request_get("#{uri.path}?#{uri.query}", headers)
        elsif verb == 'POST'
          http.request_post("#{uri.path}?#{uri.query}", query_string, headers)
        else
          raise "Unrecognized http_verb: #{http_verb}"
        end

      end
    end

  end
end
