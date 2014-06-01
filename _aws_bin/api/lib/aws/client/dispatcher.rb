#
#

module AWS
  module Client

    #
    # Dispatches requests to a particular remote service operation.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class Dispatcher
      # Instantiates a Dispatcher with the specified Orchestrator and strings declaring the service and operation to which requests will be directed.
      def initialize(orchestrator, service_name, operation_name)
        @orchestrator = orchestrator
        @service_name = service_name
        @operation_name = operation_name
      end

      # Invoke the remote service and produce a result for the provided Call object and input hash.
      def dispatch(call, input)
        show_json = input.delete(:show_json)
        request = {:operation_name => @operation_name, :service_name => @service_name, :identity => call.identity, :id => call.request_id, :value => input}
        reply = @orchestrator.orchestrate(request, show_json)

        return reply[:value]
      end

    end

  end
end

