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
import copy as _copy
import logging as _logging

from lib.rds import rds_utils
from lib.utility import shell_utils
from lib.elasticbeanstalk import eb_utils
from scli import prompt, config_file
from scli.constants import EbConfigFile, ParameterName, ParameterSource
from scli.parameter import Parameter
from scli.operation.base import OperationBase, OperationResult
from scli.resources import TerminalMessage
from scli.terminal.terminal import Terminal
from scli.terminal.beanstalk_terminal import BeanstalkTerminal

log = _logging.getLogger('cli.op')

class AskForMissiongParameterOperation(OperationBase):
    ''' Fill missing parameters using interactive interface '''
        
    def execute(self, parameter_pool):
        self._generate_service_endpoint(parameter_pool)
        self._check_rds_parameter(parameter_pool)
        
        required_params = self._operation_queue.required_parameters
        missing_params = required_params - parameter_pool.parameter_names
        if len(missing_params) > 0:
            terminal = Terminal()
            terminal.ask_parameters(parameter_pool, missing_params, True)

        ret_result = OperationResult(self, None, None, None)
        return ret_result

    def _generate_service_endpoint(self, pool):
        ''' 
        Generate EB service endpoint from region if not presents, or overwrite
        if specified region has higher priority.
        '''
        if pool.has(ParameterName.Region) and \
            (not pool.has(ParameterName.ServiceEndpoint) \
             or ParameterSource.is_ahead(pool.get_source(ParameterName.Region), 
                                         pool.get_source(ParameterName.ServiceEndpoint))\
             or not pool.has(ParameterName.DevToolsEndpoint) \
             or ParameterSource.is_ahead(pool.get_source(ParameterName.Region), 
                                         pool.get_source(ParameterName.DevToolsEndpoint))):
            
            region = pool.get(ParameterName.Region)
            eb_utils.generate_endpoint(pool, region.value, region.source)
            
        


    def _check_rds_parameter(self, pool):
        stack_name = pool.get_value(ParameterName.SolutionStack)
        rds_enable = pool.get_value(ParameterName.RdsEnabled)
            
        if rds_enable and rds_utils.is_require_rds_parameters(pool)\
            and rds_utils.is_rds_snippet_compatible(pool, stack_name):
            self._input_parameters.add(ParameterName.RdsSourceSnapshotName)
            self._input_parameters.add(ParameterName.RdsMasterPassword)
            self._input_parameters.add(ParameterName.RdsDeletionPolicy)
            
    
class AskForConfigFileParameterOperation(OperationBase):
    ''' Ask all parameters using interactive interface '''
        
    def execute(self, parameter_pool):
        
        parameters = {ParameterName.AwsAccessKeyId,
                      ParameterName.AwsSecretAccessKey,
                      ParameterName.Region,
                      ParameterName.EnvironmentTier,
                      ParameterName.SolutionStack,
                      ParameterName.ApplicationName,
                      ParameterName.EnvironmentName,
                      ParameterName.RdsEnabled,
                      ParameterName.InstanceProfileName,
                      ParameterName.EnvironmentType,
                      }
        
        terminal = Terminal()
        terminal.ask_parameters(parameter_pool, parameters, False)

        ret_result = OperationResult(self, None, None, None)
        return ret_result    
    

class RegisterBranchOperation(OperationBase):
    ''' Register current working branch '''
        
    def execute(self, parameter_pool):
        current_branch, _ = shell_utils.get_working_branch(False)
        parameter_pool.put(Parameter(ParameterName.CurrentBranch,
                                     current_branch,
                                     ParameterSource.ConfigFile))
        if current_branch:
            log.info(u'Current working branch is "{0}".'.format(current_branch))
            branch_pool = _copy.deepcopy(parameter_pool)
            
            # Fill branch environment parameter values
            branches = parameter_pool.get_value(ParameterName.Branches)
            for key in EbConfigFile.BranchSectionKeys | EbConfigFile.BranchSectionHiddenKeys:
                if branches and current_branch in branches.keys() \
                    and key in branches[current_branch].keys():
                    # Copy parameter if current branch has corresponding setting
                    branch_pool.put(Parameter(key,
                                                 branches[current_branch][key],
                                                 ParameterSource.ConfigFile))
                else:
                    # TODO: we will leave following parameter if not presents in branch, since
                    # we are not asking for them for now but they are required in terminal
                    if not key in (ParameterName.ApplicationName,
                                   ParameterName.Region,
                                   ParameterName.ServiceEndpoint,
                                   ParameterName.DevToolsEndpoint):  
                        branch_pool.remove(key)
            branch_pool.put(Parameter(ParameterName.DefaultEnvironmentName,
                                      parameter_pool.get_value(ParameterName.EnvironmentName, False),
                                      ParameterSource.ConfigFile))

            
            # Call terminal
            copy = BeanstalkTerminal.ask_branch(branch_pool)
            
            # Create mapping and branch-environment section
            if branches is None:
                parameter_pool.put(Parameter(ParameterName.Branches,
                                             dict(),
                                             ParameterSource.Terminal))
            branches = parameter_pool.get_value(ParameterName.Branches, False)
            branches[current_branch] = dict()
            source = ParameterSource.ConfigFile
            for key in EbConfigFile.BranchSectionKeys | EbConfigFile.BranchSectionHiddenKeys:
                if branch_pool.has(key):
                    branches[current_branch][key] = branch_pool.get_value(key, False)
                    if ParameterSource.is_ahead(branch_pool.get_source(key), source):
                        source = branch_pool.get_source(key)
                else:
                    # Copy parameter if not exists in branch
                    if parameter_pool.has(key): 
                        branches[current_branch][key] = parameter_pool.get_value(key, False)
            parameter_pool.update(ParameterName.Branches, source=source)
            
            # Copy over optionsetting file
            if copy:
                default_option_file = parameter_pool.get_value(ParameterName.OptionSettingFile, False)
                branch_option_file = branches[current_branch][ParameterName.OptionSettingFile]
                log.debug(u'Copying optionsettings file from {0} to {1}.'.format(default_option_file,
                                                                         branch_option_file))
                shell_utils.copy_file(default_option_file, branch_option_file, True)
                config_file.set_access_permission(branch_option_file, True)

            # Fill [branch] section
            if parameter_pool.get_value(ParameterName.BranchMapping) is None:
                parameter_pool.put(Parameter(ParameterName.BranchMapping,
                                             dict(),
                                             ParameterSource.Terminal))
            branch_mapping = parameter_pool.get_value(ParameterName.BranchMapping, False)
            branch_mapping[current_branch] = branch_pool.get_value(ParameterName.EnvironmentName, False)
            
        else:
            # local repository does not have branch committed yet.
            msg = TerminalMessage.NoBranchToRegister
            log.error(msg)
            prompt.error(msg)      

