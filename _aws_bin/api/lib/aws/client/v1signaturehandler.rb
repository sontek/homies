require 'openssl'
require 'base64'
require 'time'
require 'aws/client/handler'
require 'aws/client/v1signaturehelper'

module AWS
  module Client

    #
    # Applies an AWS version 1 signature to the outgoing request.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class V1SignatureHandler < Handler
      def before(job)
        request = job.request
        identity = request[:identity]
        aws_access_key = identity[:aws_access_key]
        aws_secret_key = identity[:aws_secret_key]

        query_string_map = request[:query_string_map]

        return if aws_access_key.nil? || aws_secret_key.nil? || query_string_map.nil?

        V1SignatureHelper.new(aws_access_key, aws_secret_key).sign({:query_string_map => query_string_map})
      end
    end

  end
end
