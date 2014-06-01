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

import os, logging as _logging

from lib.aws import requests
from scli import prompt
from scli import config_file
from scli import api_wrapper
from scli.constants import EbDefault, ParameterName, ParameterSource, \
    EnvironmentHealth, EnvironmentStatus, ServiceDefault, SubCommandType, ValidationSeverity
from scli.exception import EBSCliException
from scli.operation.base import OperationBase, OperationResult
from scli.parameter import Parameter
from scli.resources import CreateEnvironmentOpMessage, DescribeEnvironmentOpMessage,\
    GetEnvironmentEventsOpMessage,\
    TerminateEnvironmentOpMessage, UpdateEnvironmentOptionSettingOpMessage,\
    ValidationMessage, WaitForCreateEnvironmentFinishOpMessage, \
    WaitForTerminateEnvironmentFinishOpMessage, WaitForUpdateEnvOptionSettingFinishOpMessage,\
    EnvRetrieveInfoOpMessage
from scli.terminal.base import TerminalBase
from lib.elasticbeanstalk import eb_utils
from lib.elasticbeanstalk.exception import AlreadyExistException
from lib.aws.exception import InvalidParameterValueException
from lib.elasticbeanstalk.request import TemplateSpecification
from lib.rds import rds_utils
from lib.utility import misc, shell_utils

log = _logging.getLogger('cli.op')



class DescribeEnvironmentOperation(OperationBase):
    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.ApplicationName,
                         ParameterName.EnvironmentName,
                        }
    
    _output_parameters = {
                          ParameterName.EnvironmentName
                         }
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        prompt.action(DescribeEnvironmentOpMessage.Start.format(env_name))

        response = eb_client.describe_environments(app_name, 
                                                   env_name, 
                                                   include_deleted = u'false')
        log.info(u'Received response for DescribeEnvironemnts call.')
        self._log_api_result(self.__class__.__name__, u'DescribeEnvironments', response.result)

        # Also look up environment resources for future use
        resources = None
        try:
            resources = api_wrapper.retrieve_environment_resources(parameter_pool, env_name)
        except InvalidParameterValueException:
            pass
        
        env_present = (len(response.result) > 0) and bool(resources)
        
        if env_present:    # If have result 
            env_info = response.result[0]
            
            message = DescribeEnvironmentOpMessage.Result.format(env_info.cname, 
                                                                 env_info.status, 
                                                                 env_info.health)
            prompt.result(message)
            
            # Display sqs queue info before environment detail
            if resources.queues:
                for queue in resources.queues:
                    message = DescribeEnvironmentOpMessage.QueueInfo.format(queue.name, queue.url)
                    prompt.result(message)
            
            tier_serialized = env_info.tier.to_serialized_string() if env_info.tier else u''
            prompt.info(DescribeEnvironmentOpMessage.Detail.format(env_info.environment_name, 
                                                                   env_info.environment_id,
                                                                   tier_serialized, 
                                                                   env_info.solution_stack_name,
                                                                   env_info.version_label, 
                                                                   env_info.date_created, 
                                                                   env_info.date_updated, 
                                                                   env_info.description if env_info.description else u''))

            # If not Green, pull the most recent warning and error events
            if env_info.health in [EnvironmentHealth.Red, EnvironmentHealth.Yellow] \
                or (env_info.status == EnvironmentStatus.Ready \
                    and env_info.health == EnvironmentHealth.Grey):
                events = eb_client.describe_events(app_name, 
                                                   env_name, 
                                                   max_records = ServiceDefault.STATUS_EVENT_MAX_NUM, 
                                                   severity = ServiceDefault.STATUS_EVENT_LEVEL)
                if len(events.result) > 0:
                    # Having one error event
                    for event in events.result:
                        msg = u'{0}\t{1}\t{2}'.format(event.event_date, 
                                                      event.severity, 
                                                      event.message)
                        log.info(u'Found last error event: {0}'.format(msg))
                        prompt.plain(msg)                
                        
            
            # Display RDS instance host info
            try:
                logical_id, rds_property = rds_utils.retrieve_rds_instance_property\
                                                        (parameter_pool, resources)
                if rds_property is not None:
                    prompt.result(DescribeEnvironmentOpMessage.RdsInfo.format\
                                  (logical_id, 
                                   rds_property.endpoint.address, 
                                   rds_property.endpoint.port))
                    prompt.info(DescribeEnvironmentOpMessage.RdsDetail.format\
                                  (rds_property.engine + u' ' + rds_property.engine_version, 
                                   rds_property.allocated_storage, 
                                   rds_property.db_instance_class, 
                                   rds_property.multi_az, 
                                   rds_property.master_username, 
                                   rds_property.instance_create_time, 
                                   rds_property.db_instance_status))
                        
            except BaseException as ex:
                log.error(u'Encountered error when retrieve environment resources: {0}.'.format(ex))
                raise

            # Subcommand
            _, subcommands = parameter_pool.command
            subcommand = subcommands[0].upper() if len(subcommands) > 0 else None
            if subcommand == SubCommandType.OPEN:
                urlpath = u''
                if len(subcommands) > 1:
                    urlpath = subcommands[1] if subcommands[1].startswith(u'/') else u'/' + subcommands[1]
                shell_utils.open_url(env_info.cname + urlpath, False)
                        
        else:
            # No result. Environment not exist.
            message = DescribeEnvironmentOpMessage.NoEnvironment.format(env_name) 
            prompt.result(message)
            
        ret_result = OperationResult(self, response.request_id, message, response.result)
        return ret_result


class CreateEnvironmentOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.ApplicationName,
                         ParameterName.ApplicationVersionName,
                         ParameterName.EnvironmentName,
                         ParameterName.SolutionStack,
                         ParameterName.RdsEnabled,
                        }
    
    _output_parameters = {
                          ParameterName.EnvironmentName,
                          ParameterName.EnvironmentId,
                          ParameterName.CreateEnvironmentRequestID,
                         }
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
        version_name = eb_utils.check_app_version(parameter_pool, eb_client)         
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        stack_name = parameter_pool.get_value(ParameterName.SolutionStack, False)
        tier = parameter_pool.get_value(ParameterName.EnvironmentTier, False)

        spec = TemplateSpecification()
        
        # Try load option setting file if exist
        option_location = parameter_pool.get_value(ParameterName.OptionSettingFile, False)
        option_settings = config_file.load_env_option_setting_file(option_location,
                                                                   quiet = True)
        if option_settings is not None and len(option_settings) > 0:
            prompt.info(CreateEnvironmentOpMessage.UsingOptionSetting.format(option_location))
        else:
            option_settings = dict()

        option_remove = dict()
        
        # Process extensions first before we process options
        self._extension_handler(parameter_pool, spec, stack_name, option_settings, option_remove)
        
        # Process options
        self._option_setting_handler(parameter_pool, spec, stack_name, None, option_settings, option_remove)
                 
        prompt.action(CreateEnvironmentOpMessage.Start.format(env_name))
        try:
            response = eb_client.create_environment(application = app_name,
                                                    environment = env_name,
                                                    solution_stack = stack_name,
                                                    version_label = version_name,
                                                    option_settings = option_settings,
                                                    option_remove = option_remove,
                                                    template_specification = spec,
                                                    tier = tier)
        except AlreadyExistException:
            log.info(u'Environment "{0}" already exist.'.format(env_name))
            prompt.result(CreateEnvironmentOpMessage.AlreadyExist.format(env_name))
   
            ret_result = OperationResult(self, 
                                         None, 
                                        CreateEnvironmentOpMessage.AlreadyExist.format(env_name), 
                                         None)
        else:
            log.info(u'Received response for CreateEnvironemnt call.')
            prompt.info(CreateEnvironmentOpMessage.Succeed)
            prompt.result(CreateEnvironmentOpMessage.WaitAfterLaunch.format(env_name))
            self._log_api_result(self.__class__.__name__, u'CreateEnvironment', response.result)            
            
            parameter_pool.put(Parameter(ParameterName.CreateEnvironmentRequestID,
                                         response.request_id,
                                         ParameterSource.OperationOutput))
            
            ret_result = OperationResult(self,
                                         response.request_id, 
                                         CreateEnvironmentOpMessage.Succeed,
                                         response.result)
        return ret_result


    def _rds_creation(self):
        pass
    
    
class WaitForCreateEnvironmentFinishOperation(OperationBase):
    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.EnvironmentName,
                         ParameterName.CreateEnvironmentRequestID,
                         ParameterName.WaitForFinishTimeout,
                         ParameterName.PollDelay,
                        }
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        wait_timeout = parameter_pool.get_value(ParameterName.WaitForFinishTimeout, False)
        poll_delay = parameter_pool.get_value(ParameterName.PollDelay, False)
        create_request_id = parameter_pool.get_value(ParameterName.CreateEnvironmentRequestID)

        result = self._wait_for_env_operation_finish(
                         eb_client = eb_client, 
                         env_name = env_name, 
                         original_request_id = create_request_id,
                         pending_status = EnvironmentStatus.Launching,
                         expected_health = None,
                         operation_name = self.__class__.__name__, 
                         action_name = WaitForCreateEnvironmentFinishOpMessage.Action,
                         wait_timeout = wait_timeout, 
                         poll_delay = poll_delay, 
                         include_deleted = u'false',
                         initial_delay = 0)

        # After polling
        status = result[0].status
        health = result[0].health
        cname = result[0].cname 
        log.info(u'Stopped polling. Environment "{0}" is now {1}, health is {2}.\nURL is "{3}".'.\
                 format(env_name, status, health, cname))
        
        if status.lower() == EnvironmentStatus.Ready.lower() \
            and health.lower() == EnvironmentHealth.Green.lower():
            prompt.info(WaitForCreateEnvironmentFinishOpMessage.Succeed.format(env_name))
            prompt.result(WaitForCreateEnvironmentFinishOpMessage.Result.format(cname))
        else:
            prompt.info(WaitForCreateEnvironmentFinishOpMessage.Timeout.format(env_name))

        ret_result = OperationResult(self,
                                     None,
                                     WaitForCreateEnvironmentFinishOpMessage.Result.\
                                        format(cname, status, health),
                                     result)

        return ret_result
    
    
class TerminateEnvironmentOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.EnvironmentName,
                        }
    
    _output_parameters = {
                          ParameterName.TerminateEnvironmentRequestID,
                         }
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        prompt.action(TerminateEnvironmentOpMessage.Start.format(env_name))
        
        try:
            response = eb_client.terminate_environment(env_name)
        except:
            raise
        else:
            log.info(u'Received response for TerminateEnvironemnt call.')
            prompt.result(TerminateEnvironmentOpMessage.Succeed.format(env_name))
            
            self._log_api_result(self.__class__.__name__, u'TerminateEnvironment', response.result)            
            
            parameter_pool.put(Parameter(ParameterName.TerminateEnvironmentRequestID,
                                         response.request_id,
                                         ParameterSource.OperationOutput))
            
            ret_result = OperationResult(self,
                                         response.request_id, 
                                         TerminateEnvironmentOpMessage.Succeed,
                                         response.result)
        return ret_result
    
        
    
class WaitForTerminateEnvironmentFinishOperation(OperationBase):
    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.EnvironmentName,
                         ParameterName.TerminateEnvironmentRequestID,
                         ParameterName.WaitForFinishTimeout,
                         ParameterName.PollDelay,
                        }
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        wait_timeout = parameter_pool.get_value(ParameterName.WaitForFinishTimeout, False)
        poll_delay = parameter_pool.get_value(ParameterName.PollDelay, False)
        terminate_request_id = parameter_pool.get_value(ParameterName.TerminateEnvironmentRequestID)
        
        result = self._wait_for_env_operation_finish(
                         eb_client = eb_client, 
                         env_name = env_name, 
                         original_request_id = terminate_request_id,
                         pending_status = EnvironmentStatus.Terminating,
                         expected_health = None,
                         operation_name = self.__class__.__name__, 
                         action_name = WaitForTerminateEnvironmentFinishOpMessage.Action,
                         wait_timeout = wait_timeout, 
                         poll_delay = poll_delay, 
                         include_deleted = u'true',
                         initial_delay = ServiceDefault.TERMINATE_ENV_POLL_DELAY)                                                     

        # After polling
        status = result[0].status
        health = result[0].health
        log.info(u'Stopped polling. Environment "{0}" is now {1}, health is {2}.'.format\
                 (env_name, status, health))
        
        if status.lower() == EnvironmentStatus.Terminated.lower():
            prompt.result(WaitForTerminateEnvironmentFinishOpMessage.Succeed.format(env_name))
        else:
            prompt.result(WaitForTerminateEnvironmentFinishOpMessage.Timeout.format(env_name))
            prompt.result(WaitForTerminateEnvironmentFinishOpMessage.Status.format(status, health))

        ret_result = OperationResult(self,
                                     None,
                                     WaitForTerminateEnvironmentFinishOpMessage.Result.format(status),
                                     result)

        return ret_result
    

class UpdateEnvOptionSettingOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.EnvironmentName,
                         ParameterName.OptionSettingFile,
                         ParameterName.RdsEnabled,
                        }
    
    _output_parameters = {
                          ParameterName.TerminateEnvironmentRequestID,
                         }
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        stack_name = parameter_pool.get_value(ParameterName.SolutionStack, False)
        prompt.action(UpdateEnvironmentOptionSettingOpMessage.Start.format(env_name))
        tier = parameter_pool.get_value(ParameterName.EnvironmentTier, False)
        
        spec = TemplateSpecification() 

        # Try load option setting file if exist
        option_location = parameter_pool.get_value(ParameterName.OptionSettingFile, False)             
        option_settings = config_file.load_env_option_setting_file(option_location,
                                                                   quiet = True)
        if option_settings is not None and len(option_settings) > 0:
            prompt.info(UpdateEnvironmentOptionSettingOpMessage.UsingOptionSetting.format(option_location))
        else:
            option_settings = dict()

        option_remove = dict()
        
        # Process extensions first before we process options
        self._extension_handler(parameter_pool, spec, stack_name, option_settings, option_remove)
        
        # Process options
        self._option_setting_handler(parameter_pool, spec, stack_name, env_name, option_settings, option_remove)
        
        self._validate_change(parameter_pool, eb_client, app_name, env_name, 
                              option_settings, option_remove, spec)

        try:
            response = eb_client.update_environment(env_name, 
                                                    option_settings = option_settings,
                                                    option_remove = option_remove,
                                                    template_specification = spec,
                                                    tier = tier)
        except:
            raise
        else:
            log.info(u'Received response for UpdateEnvironemnt call.')
            prompt.result(UpdateEnvironmentOptionSettingOpMessage.Succeed.format(env_name))
            
            self._log_api_result(self.__class__.__name__, u'UpdateEnvironment', response.result)            
            
            parameter_pool.put(Parameter(ParameterName.UpdateEnvironmentRequestID,
                                         response.request_id,
                                         ParameterSource.OperationOutput))            
            
            ret_result = OperationResult(self,
                                         response.request_id, 
                                         UpdateEnvironmentOptionSettingOpMessage.Succeed.format(env_name),
                                         response.result)
        return ret_result


    def _validate_change(self, parameter_pool, eb_client, app_name, env_name, 
                         option_settings, option_remove, template_spec):
        response = eb_client.validate_configuration_settings(app_name, option_settings, 
                                                             environment_name = env_name,
                                                             option_remove = option_remove,
                                                             template_specification = template_spec)
        warning_count = 0
        error_count = 0
        for message in response.result:
            if misc.string_equal_ignore_case(message.severity, ValidationSeverity.SeverityError):
                error_count = error_count + 1
            else:
                warning_count = warning_count + 1
            prompt.error(ValidationMessage.ValidateSettingError.format\
                         (message.severity, message.namespace, message.option_name, message.message))
            
        if error_count > 0:            
            log.info(u'Validating configuration setting failed. Abort command.')
            raise EBSCliException()
        elif warning_count > 0:
            if parameter_pool.get_value(ParameterName.Force) == ServiceDefault.ENABLED:
                pass
            elif not TerminalBase.ask_confirmation(UpdateEnvironmentOptionSettingOpMessage.Continue):
                log.info(u'User cancelled command.')
                raise EBSCliException()
        else:
            log.info(u'Validating configuration setting passed.')
    
    
class WaitForUpdateEnvOptionSettingFinishOperation(OperationBase):
    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.EnvironmentName,
                         ParameterName.WaitForFinishTimeout,
                         ParameterName.PollDelay,
                        }
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        wait_timeout = parameter_pool.get_value(ParameterName.WaitForUpdateTimeout, False)
        poll_delay = parameter_pool.get_value(ParameterName.PollDelay, False)
#        update_request_id = parameter_pool.get_value(ParameterName.UpdateEnvironmentRequestID)

        result = self._wait_for_env_operation_finish(
                         eb_client = eb_client, 
                         env_name = env_name, 
                         original_request_id = None,
                         pending_status = EnvironmentStatus.Updating,
                         expected_health = EnvironmentHealth.Green,
                         operation_name = self.__class__.__name__, 
                         action_name = WaitForUpdateEnvOptionSettingFinishOpMessage.Action,
                         wait_timeout = wait_timeout, 
                         poll_delay = poll_delay, 
                         include_deleted = u'false',
                         initial_delay = ServiceDefault.UPDATE_ENV_POLL_DELAY)                                                     
                                                     
        # After polling
        status = result[0].status
        health = result[0].health
        cname = result[0].cname 
        log.info(u'Stopped polling. Environment "{0}" is now {1}, health is {2}.\nURL is "{3}".'.\
                 format(env_name, status, health, cname))
        
        if status.lower() == EnvironmentStatus.Ready.lower() \
            and health.lower() == EnvironmentHealth.Green.lower():
            prompt.result(WaitForUpdateEnvOptionSettingFinishOpMessage.Succeed.format(env_name))
        else:
            prompt.result(WaitForUpdateEnvOptionSettingFinishOpMessage.Timeout.format(env_name))

        prompt.info(WaitForUpdateEnvOptionSettingFinishOpMessage.Result.\
                    format(cname, status, health))
        ret_result = OperationResult(self,
                                     None,
                                     WaitForUpdateEnvOptionSettingFinishOpMessage.Result.\
                                        format(cname, status, health),
                                     result)

        return ret_result



class GetEnvironmentEventsOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.EnvironmentName,
                        }
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        max_records = parameter_pool.get_value(ParameterName.SubCommand)
        
        try:
            max_records = int(max_records[0]) if len(max_records) > 0 else ServiceDefault.EVENT_DEFAULT_NUM
        except ValueError:
            raise EBSCliException(GetEnvironmentEventsOpMessage.NotValidNumber.format(max_records[0]))        

        response = eb_client.describe_events(app_name, env_name, max_records=max_records)

        if len(response.result) > 0:
            for event in response.result:
                msg = u'{0}\t{1}\t{2}'.format(event.event_date, 
                                              event.severity, 
                                              event.message)
                prompt.plain(msg)       

        ret_result = OperationResult(self, response.request_id, None, response.result)
        return ret_result


class EnvRequestLogOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.EnvironmentName,
                        }
    
    _output_parameters = {
                          ParameterName.TerminateEnvironmentRequestID,
                         }
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        _, subcommands = parameter_pool.command
        info_type = subcommands[0].lower() if len(subcommands) > 0 else EbDefault.TailLog
        response = eb_client.request_environment_info(env_name, info_type=info_type)

        parameter_pool.put(Parameter(ParameterName.RequestEnvInfoRequestID,
                                     response.request_id,
                                     ParameterSource.OperationOutput))   

        ret_result = OperationResult(self, response.request_id, None, None)
        return ret_result


class EnvRetrieveLogOperation(OperationBase):
    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.EnvironmentName,
                         ParameterName.WaitForFinishTimeout,
                         ParameterName.PollDelay,
                        }
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        wait_timeout = parameter_pool.get_value(ParameterName.WaitForUpdateTimeout, False)
        poll_delay = parameter_pool.get_value(ParameterName.PollDelay, False)
        info_request_id = parameter_pool.get_value(ParameterName.RequestEnvInfoRequestID)

        self._wait_for_env_operation_finish(
                         eb_client = eb_client, 
                         env_name = env_name, 
                         original_request_id = info_request_id,
                         pending_status = EnvironmentStatus.Updating,
                         expected_health = None,
                         operation_name = self.__class__.__name__, 
                         action_name = EnvRetrieveInfoOpMessage.Action,
                         wait_timeout = wait_timeout, 
                         poll_delay = poll_delay, 
                         include_deleted = u'false',
                         initial_delay = ServiceDefault.UPDATE_ENV_POLL_DELAY,
                         quiet = False)                                                     
                                                     
        # After polling
        _, subcommands = parameter_pool.command
        info_type = subcommands[0].lower() if len(subcommands) > 0 else EbDefault.TailLog
        response = eb_client.retrieve_environment_info(env_name, info_type=info_type)
        
        # Sort and find latest log for each instance
        instance_timestamps = dict()
        instance_logs = dict()
        for env_info in response.result:
            instance_id = env_info.ec2_instance_id
            timestamp = env_info.sample_timestamp
            url = env_info.message
            
            if not instance_timestamps.has_key(instance_id)\
                or instance_timestamps[instance_id] < timestamp:
                instance_timestamps[instance_id] = timestamp
                instance_logs[instance_id] = url

        for instance_id in sorted(instance_logs.keys()):
            content = misc.to_unicode(requests.get(instance_logs[instance_id]).content)
            prompt.result(os.linesep + 
                          misc.to_terminal_codepage(EnvRetrieveInfoOpMessage.FileOuputPrefix.format(instance_id)))
            prompt.result(misc.to_terminal_codepage(content))
                                
        ret_result = OperationResult(self,
                                     None,
                                     None,
                                     None)

        return ret_result        

