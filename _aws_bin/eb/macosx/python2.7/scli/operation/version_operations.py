#!/usr/bin/env python
#==============================================================================
# Copyright 2012 Amazon.com, Inc. or its affiliates. All Rights Reserved.
#
# Licensed under the Amazon Software License (the "License"). You may not use
# this file except in compliance with the License. A copy of the License is
# located at
#
#       http://aws.amazon.com/asl/
#
# or in the "license" file accompanying this file. This file is distributed on
# an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, express or
# implied. See the License for the specific language governing permissions
# and limitations under the License.
#==============================================================================

import logging as _logging

from lib.utility import shell_utils
from lib.elasticbeanstalk.exception import AlreadyExistException
from scli import prompt
from scli.constants import ParameterSource, ServiceDefault
from scli.operation.base import OperationBase, OperationResult
from scli.parameter import Parameter, ParameterName
from scli.resources import CreateApplicationVersionOpMessage, \
    PushApplicationVersionOpMessage, RecordApplicationVersionOpMessage

log = _logging.getLogger('cli.op')


class CreateApplicationVersionOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.Region,
                         ParameterName.SolutionStack,
                         ParameterName.ApplicationName,
                        }
    
    _output_parameters = set()

    # Create Sample Application Version
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
        
        try:
            response = eb_client.create_application_version(app_name, 
                                                            ServiceDefault.DEFAULT_VERSION_NAME)
        except AlreadyExistException:
            log.info(u'Version "{0}" of Application "{1}" already exists.'.\
                     format(ServiceDefault.DEFAULT_VERSION_NAME, app_name))
            msg = CreateApplicationVersionOpMessage.AlreadyExist.format(ServiceDefault.DEFAULT_VERSION_NAME)
            prompt.info(msg)
   
            ret_result = OperationResult(self, None, msg, None)
        else:        
            log.info(u'Received response for CreateApplicationVersion call.')
            self._log_api_result(self.__class__.__name__, u'CreateApplicationVersion', response.result)
            msg = CreateApplicationVersionOpMessage.Succeed.format(ServiceDefault.DEFAULT_VERSION_NAME)      
            prompt.info(msg)

            ret_result = OperationResult(self, response.request_id, msg, response.result)

        return ret_result


class PushApplicationVersionOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.Region,
                         ParameterName.ApplicationName,
                         ParameterName.EnvironmentName
                        }
    
    _output_parameters = set()
   
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)

        response = eb_client.describe_environments(app_name, env_name, include_deleted = False)
        if len(response.result) > 0:
            shell_utils.git_aws_push(False)
        else:
            prompt.error(PushApplicationVersionOpMessage.EnvNotExist.format(env_name))

        ret_result = OperationResult(self,
                                     None, 
                                     None,
                                     None)
            
        return ret_result

    
class RecordApplicationVersionOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.Region,
                         ParameterName.ApplicationName,
                         ParameterName.EnvironmentName
                        }
    
    _output_parameters = {
                          ParameterName.ApplicationVersionName
                         }

   
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)

        response = eb_client.describe_environments(app_name, env_name, include_deleted = False)
        if len(response.result) > 0:    # If have result
            version_name = response.result[0].version_label
            log.info(u'Retrieved application version {0} for environment {1}'.format(version_name, env_name))
            prompt.info(RecordApplicationVersionOpMessage.Succeed.format(version_name))
            parameter_pool.put(Parameter(ParameterName.ApplicationVersionName,
                                         version_name,
                                         ParameterSource.OperationOutput),
                               True)            
            
        ret_result = OperationResult(self,
                                     response.request_id, 
                                     None,
                                     response.result)
            
        return ret_result


