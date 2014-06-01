
module AWS
  module Client
    #
    # Operates on a job before and after issuing the remote request.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class Handler

      # Operate on the specified Job on the "outbound" side of the execution
      def before(job)
      end

      # Operation on the specified Job on the "inbound" side of the execution
      def after(job)
      end

    end
    
  end
end
