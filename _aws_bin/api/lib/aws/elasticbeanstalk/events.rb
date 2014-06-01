###########################################################################
# AWS Elastic Beanstalk Command Line Client
# Copyright 2011 Amazon.com, Inc. or its affiliates. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the “License”). You
# may not use this file except in compliance with the License. A copy of
# the License is located at
#
#    http://aws.amazon.com/apache2.0/
#
# or in the “license” file accompanying this file. This file is
# distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF
# ANY KIND, either express or implied. See the License for the specific
# language governing permissions and limitations under the License.
#
module AWS
  module ElasticBeanstalk
    class Client
      #
      # Shorthand method to invoke the DescribeEvents operation:
      #
      # <p>Returns list of event descriptions matching criteria.</p>
      #
      # Example usage:
      #   my_output = my_client.DescribeEvents(my_input)
      #
      def describe_events(input = {})
        dispatcher = @describe_events_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'DescribeEvents')
        call_service(dispatcher, input)
      end
    end
  end
end
