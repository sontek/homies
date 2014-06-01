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
      def create_application(options = {})
        dispatcher = @create_application_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'CreateApplication')
        call_service(dispatcher, options)
      end
      
      #
      def describe_applications(options = {})
        dispatcher = @describe_applications_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'DescribeApplications')
        call_service(dispatcher, options)
      end
      
      #
      def update_application(options = {})
        dispatcher = @update_application_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'UpdateApplication')
        call_service(dispatcher, options)
      end
      
      #
      def delete_application(options = {})
        dispatcher = @delete_application_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'DeleteApplication')
        call_service(dispatcher, options)
      end
      
    end
  end
end

