require 'aws/client/job'

module AWS
  module Client

    #
    # Directs a Job through a Handler chain for processing.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class Orchestrator
      # Instantiate an orchestrator with the given list of Handlers.
      def initialize(handlers)
        @handlers = handlers
      end

      # Direct the specified request down the Handler chain, invoking first each before method,
      # then in reverse order each after method.  If any exceptions are thrown along the way, orchestration
      # will stop immediately.
      def orchestrate(request, show_json)
        job = Job.new(request, show_json)

        stack = []

        @handlers.each { |handler|
          stack << handler
          handler.before(job)
        }

        stack.reverse.each { |handler|
          handler.after(job)
        }

        job.reply
      end
    end

  end
end
