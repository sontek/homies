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

require 'aws/client/dispatcher'
require 'aws/client/call'
require 'aws/client/awsquery'
require 'aws/elasticbeanstalk/applications'
require 'aws/elasticbeanstalk/applicationversions'
require 'aws/elasticbeanstalk/configurationsettings'
require 'aws/elasticbeanstalk/configurationtemplates'
require 'aws/elasticbeanstalk/environmentconfigurations'
require 'aws/elasticbeanstalk/environments'
require 'aws/elasticbeanstalk/events'
require 'aws/elasticbeanstalk/misc'
require 'aws/elasticbeanstalk/solutionstacks'

module AWS
  module ElasticBeanstalk
    class Client
      # Construct a new client.  Takes an orchestrator through which
      # to process requests.  See additional constructors below to use
      # pre-configured orchestrators for specific protocols.
      #
      # [orchestrator] 
      #   The Orchestrator is responsible for actually
      #   making the remote service call.  Clients construct requests,
      #   hand them off to the orchestrator, and receive responses in
      #   return.
      def initialize(config)
        @orchestrator = AWS::Client::AwsQuery.new_orchestrator(config)
      end

      protected
      
      def call_service(dispatcher, options = {})
        AWS::Client::Call.new(dispatcher).call(options)
      end
    end
  end
end
