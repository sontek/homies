require 'uri'
require 'aws/client/handler'

module AWS
  module Client

    #
    # Attaches the specified endpoint URI to the outgoing request.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class HttpDestinationHandler < Handler
      # Initialize an HttpDestinationHandler with the specified endpoint URI.
      def initialize(endpoint)
        @uri = 
          case endpoint
            when URI then endpoint
            else URI.parse(endpoint)
          end
      
        @uri.path = '/' if @uri.path.nil? || @uri.path.empty?
      end

      def before(job)
        job.request[:http_verb] = 'GET'
        job.request[:http_uri] = @uri.clone
      end
    end

  end
end
