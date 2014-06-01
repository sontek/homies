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
      def create_environment(options = {})
        dispatcher = @create_environment_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'CreateEnvironment')
        call_service(dispatcher, options)
      end

      #
      def describe_environment_resources(options = {})
        dispatcher = @describe_environment_resources_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'DescribeEnvironmentResources')
        call_service(dispatcher, options)
      end

      #
      def describe_environments(options = {})
        dispatcher = @describe_environments_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'DescribeEnvironments')
        call_service(dispatcher, options)
      end

      #
      def rebuild_environment(options = {})
        dispatcher = @rebuild_environment_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'RebuildEnvironment')
        call_service(dispatcher, options)
      end

      #
      def request_environment_info(options = {})
        dispatcher = @request_environment_info_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'RequestEnvironmentInfo')
        call_service(dispatcher, options)
      end

      #
      def restart_app_server(options = {})
        dispatcher = @restart_app_server_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'RestartAppServer')
        call_service(dispatcher, options)
      end
      
      #
      def retrieve_environment_info(options = {})
        dispatcher = @retrieve_environment_info_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'RetrieveEnvironmentInfo')
        call_service(dispatcher, options)
      end

      #
      def terminate_environment(options = {})
        dispatcher = @terminate_environment_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'TerminateEnvironment')
        call_service(dispatcher, options)
      end

      #
      def update_environment(options = {})
        dispatcher = @update_environment_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'UpdateEnvironment')
        call_service(dispatcher, options)
      end
      
      #
      def swap_environment_cnames(options = {})
        dispatcher = @swap_environment_cnames_dispatcher ||= AWS::Client::Dispatcher.new(@orchestrator, 'ElasticBeanstalkService', 'SwapEnvironmentCNAMEs')
        call_service(dispatcher, options)
      end

    end
  end
end
