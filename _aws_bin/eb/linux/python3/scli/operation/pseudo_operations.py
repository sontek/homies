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
import logging
import time as _time

from lib.rds import rds_utils
from scli import config_file, prompt
from scli.constants import CommandType, EbConfigFile, ParameterName as PName, \
    RdsDefault, ServiceDefault
from scli.exception import EBSCliException
from scli.operation.base import OperationBase, OperationResult
from scli.resources import AskConfirmationOpMessage, ConfigFileMessage
from scli.terminal.base import TerminalBase


log = logging.getLogger('cli.op')


class ValidateParameterOperation(OperationBase):
    ''' Validate all required parameters and verify all have value'''
    _input_parameters = set()
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        
        # Update parameter
        self._update_timeout_thresholds(parameter_pool)
        
        # Checking parameters
        required_params = self._operation_queue.required_parameters
        missing_params = required_params - parameter_pool.parameter_names 
        if len(missing_params) > 0:
            raise EBSCliException('Missing required parameter. "{0}"'.format(missing_params))
        
        log.debug('Finished gathering required parameter')

        ret_result = OperationResult(self, None, None, None)
        return ret_result

    
    def _update_timeout_thresholds(self, parameter_pool):
        parameter_pool.update(PName.WaitForFinishTimeout,
                              parameter_pool.get_value(PName.WaitForFinishTimeout, False)\
                                + self._rds_time_out(parameter_pool))

        parameter_pool.update(PName.WaitForUpdateTimeout,
                              parameter_pool.get_value(PName.WaitForUpdateTimeout, False)\
                                + self._rds_time_out(parameter_pool))

    
    def _rds_time_out(self, parameter_pool):
        if parameter_pool.get_value(PName.RdsEnabled):
            return ServiceDefault.RDS_ADDITION_TIMEOUT_IN_SEC
        else:
            return 0
            
    
class AskConfirmationOperation(OperationBase):
    ''' Ask for user's confirmation'''
    _input_parameters = set()
    
    _output_parameters = set()    
    

    def execute(self, parameter_pool):
        command = parameter_pool.get_value(PName.Command, False)
        self._probe_rds_change(parameter_pool, command)
        
        if parameter_pool.get_value(PName.Force) == ServiceDefault.ENABLED \
            or TerminalBase.ask_confirmation(AskConfirmationOpMessage.CommandConfirmation[command]):
            ret_result = OperationResult(self, None, None, None)
            return ret_result
        else:
            log.info('User cancelled command.')
            raise EBSCliException()
        
        
    def _probe_rds_change(self, parameter_pool, command):
        
        if parameter_pool.has(PName.ApplicationName)\
            and parameter_pool.has(PName.EnvironmentName):

            app_name = parameter_pool.get_value(PName.ApplicationName, False)
            env_name = parameter_pool.get_value(PName.EnvironmentName, False)
            
            policy = rds_utils.is_rds_delete_to_snapshot(parameter_pool, app_name, env_name)
            local_rds_switch = parameter_pool.get_value(PName.RdsEnabled, False)

            if policy is not None and not RdsDefault.del_policy_to_bool(policy):
                if command == CommandType.UPDATE:
                    if local_rds_switch:
                        pass
                    else:
                        prompt.result(AskConfirmationOpMessage.CommandWarning[command])
                else:
                    prompt.result(AskConfirmationOpMessage.CommandWarning[command])
        

        
class SleepOperation(OperationBase):
    ''' Idle sleep'''
    _input_parameters = set()
    
    _output_parameters = set()    

    def execute(self, parameter_pool):
        create_request_id = parameter_pool.get_value(PName.CreateEnvironmentRequestID)
        delay = ServiceDefault.CREATE_ENV_POLL_DELAY if create_request_id is not None else 0
        _time.sleep(delay)
                 
        ret_result = OperationResult(self, None, None, None)
        return ret_result
    

class SanitizeBranchOperation(OperationBase):
    ''' Remove branch registrations if critical parameters are changed.'''
    _input_parameters = set()
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        command = parameter_pool.get_value(PName.Command, False)
        if command == CommandType.INIT:
            sanitize = False
            for name, ori_name in EbConfigFile.BranchResetParameters.items():
                if parameter_pool.has(ori_name) and \
                    parameter_pool.get_value(name, False) != parameter_pool.get_value(ori_name, False):
                    sanitize = True
                    break

            blast = False
            if sanitize:
                if parameter_pool.has(PName.Branches):
                    parameter_pool.remove(PName.Branches)
                    blast = True
                if parameter_pool.has(PName.BranchMapping):
                    parameter_pool.remove(PName.BranchMapping)
                    blast = True
                
                if blast:
                    prompt.error(ConfigFileMessage.BranchResetWarning);

            ret_result = OperationResult(self,
                                         None,
                                         'Need clean: {0}. Removed branch: {1}'.format(sanitize, blast),
                                         None)
            return ret_result


class SanitizeRdsPasswordOperation(OperationBase):
    ''' Remove Rds master passwords from credential file'''
    _input_parameters = set()
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        command = parameter_pool.get_value(PName.Command, False)
        if command == CommandType.DELETE:
            # Remove RDS master password from crential file    
            credential_file_loc = config_file.default_aws_credential_file_location()
            # default environment
            env_name = parameter_pool.get_value(PName.EnvironmentName, False)
            param_list = [rds_utils.password_key_name(env_name)]
            # branch environment
            if parameter_pool.get_value(PName.Branches) is not None:
                branches = parameter_pool.get_value(PName.Branches, False)
                for branch in list(branches.values()):
                    env_name = branch[PName.EnvironmentName]
                    param_list.append(rds_utils.password_key_name(env_name))
            # Remove passwords
            config_file.trim_aws_credential_file(credential_file_loc, param_list, True)

        ret_result = OperationResult(self, None, None, None)
        return ret_result


class SanitizeAppVersionNameOperation(OperationBase):
    ''' Remove Application Version name from config file under certain circumstances'''
    _input_parameters = set()
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        command = parameter_pool.get_value(PName.Command, False)
        sanitize = False
        if command == CommandType.DELETE:
            sanitize = True
        elif command == CommandType.INIT:
            for name, ori_name in EbConfigFile.BranchResetParameters.items():
                if parameter_pool.has(ori_name) and \
                    parameter_pool.get_value(name, False) != parameter_pool.get_value(ori_name, False):
                    sanitize = True
                    break

        if sanitize:
            parameter_pool.remove(PName.ApplicationVersionName)

        ret_result = OperationResult(self, 
                                     None, 
                                     'Removed application version: {0}'.format(sanitize), 
                                     None)
        return ret_result

