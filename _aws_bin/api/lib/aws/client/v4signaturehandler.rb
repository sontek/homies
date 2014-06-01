require 'openssl'
require 'base64'
require 'time'
require 'aws/client/handler'

module AWS
  module Client

    #
    # Applies an AWS version 4 signature to the outgoing request.
    #
    # Copyright:: Copyright (c) 2012 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class V4SignatureHandler < Handler

      @@algorithm = 'AWS4-HMAC-SHA256'
      @@path = '/'
      @@service_name = 'elasticbeanstalk'
      @@terminator = 'aws4_request'
      @@authorization_header = 'authorization'
      
      def before (job)
        request = job.request
        identity = request[:identity]
        @aws_access_key = identity[:aws_access_key].to_s
        @aws_secret_key = identity[:aws_secret_key].to_s
        @region = request[:region]
        @verb = request[:http_verb]
        @headers = request[:headers]
        @datetime = @headers['x-amz-date']

        @query_string = canonical_query_string(request[:query_string_map])
        request[:query_string] = @query_string
        
        @host = @headers['host']
        request[:http_host] = @host
        
        @headers['content-type'] ||= 'application/x-www-form-urlencoded'
        @headers[@@authorization_header] = authorization
      end

      def authorization ()
        auth_string = "AWS4-HMAC-SHA256 Credential=#{@aws_access_key}/#{scope}" + ","
        auth_string += "SignedHeaders=#{signed_headers(@headers)}" + ","
        auth_string += "Signature=#{hex16(signature)}"
        return auth_string
      end

      def signature ()
        k_secret = @aws_secret_key
        k_date = hmac("AWS4" + k_secret, @datetime[0,8])
        k_region = hmac(k_date, @region)
        k_service = hmac(k_region, @@service_name)
        k_credentials = hmac(k_service, 'aws4_request')
        return hmac(k_credentials, string_to_sign)
      end

      def string_to_sign ()
        string = @@algorithm + "\n"
        string += @datetime + "\n"
        string += scope + "\n"
        string += hex16(hash(canonical_request))
      end

      def scope ()
        scope_string = @datetime[0,8] + "/"
        scope_string += @region + "/"
        scope_string += @@service_name + "/"
        scope_string += @@terminator
        scope_string
      end

      def canonical_request ()
        request_string = @verb + "\n"
        request_string += @@path + "\n"
        request_string += "\n"
        request_string += canonical_headers(@headers) + "\n\n"
        request_string += signed_headers(@headers) + "\n"
        request_string += hex16(hash(@query_string))
      end

      def signed_headers (headers)
        to_sign = headers.keys.map{|k| k.to_s.downcase }
        to_sign.delete(@@authorization_header)
        to_sign.sort.join(";")
      end

      def canonical_headers (headers)
        new_headers = []
        headers.each_pair do |k,v|
          new_headers << [k.downcase,v] unless k == @@authorization_header
        end
        new_headers = new_headers.sort_by(&:first)
        new_headers.map{|k,v| "#{k}:#{canonical_header_values(v)}" }.join("\n")
      end

      def canonical_header_values (values)
        values = [values] unless values.is_a?(Array)
        values.map(&:to_s).join(',').gsub(/\s+/, ' ').strip
      end

      def canonical_query_string (query_string_map)
        query = []
        query_string_map.each_pair do |k,v|
          query << [k,v] unless k == 'Signature'
        end
        query = query.sort_by(&:first)
        query.map{|k,v| "#{UrlEncoding.encode(k)}=#{UrlEncoding.encode(v)}" }.join('&')
      end

      def hex16 (string)
        string.unpack('H*').first
      end

      def hash (string)
        Digest::SHA256.digest(string)
      end

      def hmac (key, value)
        digest = OpenSSL::Digest::Digest.new('sha256')
        OpenSSL::HMAC.digest(digest, key, value)
      end      
      
    end

  end
end
