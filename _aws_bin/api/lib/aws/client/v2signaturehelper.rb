require 'openssl'
require 'base64'
require 'time'

module AWS
  module Client

    #
    # Performs AWS V2 signatures on QueryStringMap objects.
    #
    # Copyright:: Copyright (c) 2008, 2012 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class V2SignatureHelper 
      def initialize(aws_access_key_id, aws_secret_key)
        @aws_access_key_id = aws_access_key_id.to_s
        @aws_secret_key = aws_secret_key.to_s
      end

      def sign(args)
        query_string_map = args[:query_string_map]
        add_fields(query_string_map, args[:datetime])

        request = args[:request]
        query_string = canonical_query_string(query_string_map)
        request[:query_string] = query_string + "&Signature=" + UrlEncoding.encode(compute_signature(canonicalize(args, query_string)))
      end

      def canonicalize(args, query_string)
        uri = args[:uri]
        verb = args[:verb]
        host = args[:host].downcase
          
        canonical = "#{verb}\n#{host}\n#{uri}\n" + query_string
        return canonical
      end

      def compute_signature(canonical)
        digest = OpenSSL::Digest::Digest.new('sha1')
        return Base64.encode64(OpenSSL::HMAC.digest(digest, @aws_secret_key, canonical)).strip
      end

      def add_fields(query_string_map, time)
        query_string_map['AWSAccessKeyId'] = @aws_access_key_id
        query_string_map['SignatureVersion'] = '2'
        query_string_map['SignatureMethod'] = 'HmacSHA1'
        query_string_map['Timestamp'] = time
      end

      def sort(hash)
        hash.sort
      end

      def canonical_query_string (query_string_map)
        query = []
        query_string_map.each_pair do |k,v|
          query << [k,v] unless k == 'Signature'
        end
        query = query.sort_by(&:first)
        query.map{|k,v| "#{UrlEncoding.encode(k)}=#{UrlEncoding.encode(v)}"}.join('&')
      end

    end

  end
end

