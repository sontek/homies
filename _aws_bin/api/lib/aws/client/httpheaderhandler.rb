require 'aws/client/handler'

module AWS
  module Client

    #
    # Adds necessary headers, some of which will be used for signing.
    #
    # Copyright:: Copyright (c) 2012 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class HttpHeaderHandler < Handler

      # User-Agent String for Elastic Beanstalk client
      @@user_agent = 'Elastic Beanstalk CLI v1.0.14'

      def before(job)
        request = job.request
        identity = request[:identity]
        http_uri = request[:http_uri]
        host = "#{http_uri.host}".downcase

        
        headers = {}
        headers['User-Agent'] = @@user_agent
        headers['host'] = host
        headers['x-amz-date'] = Time.now.utc.strftime("%Y%m%dT%H%M%SZ")
        headers['x-amzn-RequestId'] = "#{job.request[:id]}"
        headers['x-amzn-Delegation'] = identity[:http_delegation] unless identity[:http_delegation].nil?

        job.request[:headers] = headers
      end
    end  
  end
end
