require 'openssl'
require 'base64'
require 'time'

module AWS
  module Client

    #
    # Performs AWS V1 signatures on QueryStringMap objects.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class V1SignatureHelper 
      def initialize(aws_access_key_id, aws_secret_key)
        @aws_access_key_id = aws_access_key_id.to_s
        @aws_secret_key = aws_secret_key.to_s
      end

      def sign(args)
        signT(Time.now.iso8601, args)
      end

      def signT(time, args)
        query_string_map = args[:query_string_map]
        add_fields(query_string_map, time)
        query_string_map['Signature'] = compute_signature(canonicalize(args))
      end

      def canonicalize(args)
        query_string_map = args[:query_string_map]

        # exclude any existing Signature parameter from the canonical string
        sorted = sort(query_string_map.reject { |k, v| k == 'Signature' })
    
        canonical = ''
        sorted.each do |v|
          canonical << v[0]
          canonical << v[1] unless(v[1].nil?)
        end

        return canonical
      end

      def compute_signature(canonical)
        digest = OpenSSL::Digest::Digest.new('sha1')
        return Base64.encode64(OpenSSL::HMAC.digest(digest, @aws_secret_key, canonical)).strip
      end

      def add_fields(query_string_map, time)
        query_string_map['AWSAccessKeyId'] = @aws_access_key_id
        query_string_map['SignatureVersion'] = '1'
        query_string_map['SignatureMethod'] = 'HmacSHA1'
        query_string_map['Timestamp'] = time.to_s
      end

      def sort(hash)
        hash.sort { |a,b| a[0].downcase <=> b[0].downcase }
      end
    end

  end
end
