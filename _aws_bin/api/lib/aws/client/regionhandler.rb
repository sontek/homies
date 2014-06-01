require 'aws/client/handler'

module AWS
  module Client

    #
    # Attempt to retrieve region from endpoint host name if not specified, and fall back to us-east-1 
    # if region extraction fail
    #
    # Copyright:: Copyright (c) 2012 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class RegionHandler < Handler
      
      # Initialize a RegionHandler with the specified region, or extract from URI otherwise.
      def initialize(region_name)
        if !region_name.nil?
          @region = region_name
        end
      end

      def before(job)
        if @region.nil?
          hostname = job.request[:http_uri].host
          if /^elasticbeanstalk.[a-zA-Z0-9\-]+.amazonaws.com$/ =~ hostname
            @region = hostname.split('.')[1]
          else
            print "###\nUsing default region us-east-1. You can override it by setting environment variable ELASTICBEANSTALK_REGION.\n###\n"
            @region = "us-east-1"
          end
        end
        
        job.request[:region] = @region
      end
    end

  end
end
