require 'uuidtools'

module AWS
  module Client

    #
    # Contains context pertaining to a specific request to a remote service.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class Call 

      # Create a new Call object tied to a specific Dispatcher.
      def initialize(dispatcher)
        @dispatcher = dispatcher
        @identity = {}
        @request_id = nil
      end

      # Specify a hash containing identity information for the outgoing request.
      # This is protocol specific but typically contains account names, certificates or other credentials.
      def identity=(i)
        @identity = i.to_hash
      end

      # Retrieve the hash of identity information for the outgoing request.
      # The returned hash is mutable such that callers may add or remove identity information from it.
      def identity
        @identity
      end

      # Specify the request ID to attach to the outgoing request.  (Internal only)
      def request_id=(r)
        @request_id = r
      end

      # Retrieve the request ID returned by the remote service.
      def request_id
        @request_id
      end

      # Invoke the remote service and return the result.
      def call(input = {})
        begin
          @request_id = UUIDTools::UUID.random_create if @request_id.nil?

          return @dispatcher.dispatch(self, input)
        rescue Timeout::Error => timeout
          return {
            "Error" => {
              "Type" => "Receiver",
              "Code" => "Timeout",
              "Details" => timeout
            }
          }
        rescue Exception => e
          return {
            "Error" => {
              "Type" => "Sender",
              "Code" => "InternalFailure",
              "Details" => e
            }
          }
        end
      end

    end

  end
end
