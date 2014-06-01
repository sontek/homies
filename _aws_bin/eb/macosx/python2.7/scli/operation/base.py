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
import time as _time

from lib.utility import misc
from lib.elasticbeanstalk import eb_utils
from lib.elasticbeanstalk.servicecall import ElasticBeanstalkClient
from lib.iam import iam_utils
from lib.rds import rds_utils
from scli import prompt
from scli.constants import OutputLevel, ParameterName, ServiceRegionId
from scli.exception import EnvironmentNotExistError
from scli.resources import BaseOpMessage



log = _logging.getLogger('cli.op')

class OperationBase(object):
    '''
    Base class of all operation classes
    '''
    _operation_queue = None
    
    _input_parameters = set()
    
    _output_parameters = set()

    def __init__(self, operation_queue):
        ''' Constructor '''
        self._operation_queue = operation_queue
       
    def execute(self, parameter_pool):
        ''' Do nothing. '''
        pass

    @property
    def input_parameters(self):
        ''' Return list of required input parameter for this operation (for derived classes)'''
        return self._input_parameters
    
    @property
    def output_parameters(self):
        ''' Return list of output parameter for this operation (for derived classes)'''
        return self._output_parameters
    
    
    def _get_eb_client(self, parameter_pool):
        return ElasticBeanstalkClient(parameter_pool.get_value(ParameterName.AwsAccessKeyId, False), 
                                      parameter_pool.get_value(ParameterName.AwsSecretAccessKey, False),
                                      parameter_pool.get_value(ParameterName.ServiceEndpoint, False),
                                      ServiceRegionId[parameter_pool.get_value(ParameterName.Region, False)])    
    
    def _wait_for_env_operation_finish(self, 
                                       eb_client, 
                                       env_name, 
                                       original_request_id,
                                       pending_status,
                                       expected_health,
                                       operation_name, 
                                       action_name,
                                       wait_timeout, 
                                       poll_delay, 
                                       include_deleted = u'false',
                                       initial_delay = 0,
                                       quiet = False
                                       ):
        '''
        Loop polling environment status while it is in specified pending_status
        and/or health state, until status changes and/or health state meet expectation, 
        or reach wait_timeout threshold. While polling retrieve events related to 
        specified request_id or all recent events if not specified. 
        '''
        # Just return if not specify either pending status and health expectation
        if pending_status is None and expected_health is None:
            return

        if quiet:
            ori_prompt_level = prompt.get_level()
            prompt.set_level(OutputLevel.Quiet)
        
        prompt.action(BaseOpMessage.WaitForEnv.format(env_name, action_name))
        prompt.info(BaseOpMessage.UserCanInterrupt)
        _time.sleep(initial_delay) # Wait before 
        
        polling_start_time = _time.time()
        event_start_time = None if original_request_id is not None \
            else misc.unixtime_to_utc(_time.time())
        while _time.time() - polling_start_time < wait_timeout:
            
            # Retrieve related events
            log.info(u'Retrieving events for Environment "{0}" after UTC time {1}.'.\
                     format(env_name, event_start_time))
            
            event_response = eb_client.describe_events(None, 
                                                       env_name, 
                                                       request_id = original_request_id, 
                                                       start_time = event_start_time)
                
            self._log_api_result(operation_name, u'DescribeEvents', event_response.result)
            
            # Output events related to environment launch
            if len(event_response.result) > 0:
                # Having new events
                event_response.result.reverse()
                for event in event_response.result:
                    log.info(u'{0}\t{1}\t{2}'.format\
                             (event.event_date, event.severity, event.message))
                    prompt.plain(u'{0}\t{1}\t{2}'.format\
                                (event.event_date, event.severity, event.message))
                    
                    event_start_time = misc.unixtime_to_utc(event.event_date_raw + 0.001)
#            else:
#                prompt.action(BaseOpMessage.Running)            
            
            # Describe environment status
            env_response = eb_client.describe_environments(environment_names = env_name, 
                                                           include_deleted = include_deleted)
            if len(env_response.result) < 1:
                raise EnvironmentNotExistError(BaseOpMessage.EnvNotExist.format(env_name))
            
            if pending_status is None:
                # No specified pending status
                if expected_health is not None \
                    and env_response.result[0].health.lower() == expected_health.lower():
                    # Meet with expected health, stop polling
                    break;
            else:
                # Has specified pending status
                if env_response.result[0].status.lower() != pending_status.lower():
                    # Not in pending status
                    if expected_health is None:
                        # No expected health, stop polling
                        break;
                    elif env_response.result[0].health.lower() == expected_health.lower():
                        # Meet with expected health, stop polling
                        break;
                    
            log.info(u'Received response for DescribeEnvironemnts call.')
            self._log_api_result(operation_name, u'DescribeEnvironments', env_response.result)
            
            _time.sleep(poll_delay)
        else:
            log.error(u'Breach timeout threshold of waiting environment {0}.'.\
                      format(action_name))
        
        if quiet:
            prompt.set_level(ori_prompt_level)
                    
        return env_response.result
    

    def _log_api_result(self, operation_name, api_name, result):
        if log.isEnabledFor(_logging.DEBUG):
            log.debug(u'{0} response: {1}'.\
                      format(operation_name, misc.collection_to_string(result)))


    def _option_setting_handler(self, parameter_pool, template_spec, 
                                stack_name, env_name, option_settings, option_remove):
        eb_utils.apply_environment_type(parameter_pool, template_spec, stack_name, env_name, option_settings, option_remove)
        rds_utils.rds_handler(parameter_pool, template_spec, stack_name, option_settings, option_remove)
        eb_utils.trim_vpc_options(parameter_pool, option_settings, option_remove)
        iam_utils.apply_instance_profile(parameter_pool, option_settings, option_remove)


    def _extension_handler(self, parameter_pool, template_spec, 
                                stack_name, option_settings, option_remove):
        rds_utils.rds_extension_handler(parameter_pool, template_spec, stack_name, option_settings, option_remove);


class OperationResult(object):
    ''' Store execution result of one operation'''
    
    def __init__(self, operation, request_id, message, result):
        self._operation = operation
        self._request_id = request_id
        self._message = message
        self._result = result
        
    @property        
    def operation(self):
        return self._operation

    @property
    def request_id(self):
        return self._request_id

    @property
    def message(self):
        return self._message
    
    @property
    def result(self):
        return self._result
    
    