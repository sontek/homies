require 'openssl'
require 'base64'
require 'time'

module AWS
  module Client

    #
    # Performs AWS V0 Signatures on QueryStringMap objects.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class V0SignatureHelper 
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

        service_name = query_string_map['ServiceName']
        action = query_string_map['Action']
        operation = query_string_map['Operation']
        timestamp = query_string_map['Timestamp']
        expires = query_string_map['Expires']
        credential = query_string_map['Credential']

        canonical = ''

        canonical << service_name unless service_name.nil?

        if !action.nil? then
          canonical << action
        elsif !operation.nil? then
          canonical << operation
        else
          raise 'Query string must contain Action or Operation'
        end
    
        if timestamp.nil? and expires.nil? then
          raise 'Query string must contain Timestamp or Expires'
        elsif !timestamp.nil? and !expires.nil? then
          raise 'Query string may contain only one of Timestamp or Expires'
        elsif !timestamp.nil? then
          canonical << timestamp
        else
          canonical << expires
        end

        canonical << credential unless credential.nil?

        return canonical
      end

      def compute_signature(canonical)
        digest = OpenSSL::Digest::Digest.new('sha1')
        return Base64.encode64(OpenSSL::HMAC.digest(digest, @aws_secret_key, canonical)).strip
      end

      def add_fields(query_string_map, time)
        query_string_map['AWSAccessKeyId'] = @aws_access_key_id
        query_string_map['SignatureVersion'] = '0'
        query_string_map['SignatureMethod'] = 'HmacSHA1'
        query_string_map['Timestamp'] = time.to_s
      end

      def sort(hash)
        hash.sort { |a,b| a[0].downcase <=> b[0].downcase }
      end
    end

  end
end
