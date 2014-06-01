require 'uri'
require 'aws/client/handler'

module AWS
  module Client

    #
    # Compiles the request URL from AwsQueryHandler and any intervening signature handler.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class AwsQueryUriHandler < Handler
      def before(job)
        http_verb = job.request[:http_verb]

        if http_verb.nil?
          raise "http_verb must be defined"
        elsif http_verb == 'GET' || http_verb == 'HEAD'
          job.request[:http_uri].query = job.request[:query_string_map].to_s
        else
          job.request[:http_query_map] = job.request[:query_string_map]
        end
      end
    end

  end
end
