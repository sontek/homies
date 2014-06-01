require 'openssl'
require 'base64'
require 'time'
require 'aws/client/handler'
require 'aws/client/v2signaturehelper'

module AWS
  module Client

    #
    # Applies an AWS version 2 signature to the outgoing request.
    #
    # Copyright:: Copyright (c) 2008, 2012 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class V2SignatureHandler < Handler
      def before(job)
        request = job.request
        identity = request[:identity]
        aws_access_key = identity[:aws_access_key]
        aws_secret_key = identity[:aws_secret_key]

        query_string_map = request[:query_string_map]
        uri = request[:http_uri].path
        verb = request[:http_verb]
        headers = request[:headers]
        host = headers['host']
        datetime = headers['x-amz-date']
        
        headers['Authorization'] = identity[:http_authorization] unless identity[:http_authorization].nil?

        return if aws_access_key.nil? || aws_secret_key.nil? || query_string_map.nil? || uri.nil? || verb.nil? || host.nil? || datetime.nil?

        V2SignatureHelper.new(aws_access_key, aws_secret_key).sign({:query_string_map => query_string_map, :uri => uri, 
          :verb => verb, :host => host, :datetime => datetime, :request => request})
    
      end
    end

  end
end
