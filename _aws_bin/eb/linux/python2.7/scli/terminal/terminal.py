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

from scli.constants import ParameterName as Name
from scli.terminal.base import TerminalBase
from scli.terminal.beanstalk_terminal import BeanstalkTerminal
from scli.terminal.service_terminal import ServiceTerminal
from scli.terminal.iam_terminal import IamTerminal
from scli.terminal.rds_terminal import RdsTerminal


log = _logging.getLogger('cli')


class Terminal(object):
    
    _handlers = dict()    # mapping for parameters need special handling 
    
    def __init__(self):
        # Register special 
        self._handlers[Name.AwsAccessKeyId] = ServiceTerminal.ask_aws_access_key_id
        self._handlers[Name.AwsSecretAccessKey] = ServiceTerminal.ask_aws_secret_access_key
        self._handlers[Name.Region] = ServiceTerminal.ask_region
        self._handlers[Name.ServiceEndpoint] = ServiceTerminal.ask_service_endpoint

        self._handlers[Name.ApplicationName] = BeanstalkTerminal.ask_application_name
        self._handlers[Name.EnvironmentName] = BeanstalkTerminal.ask_environment_name
        self._handlers[Name.EnvironmentTier] = BeanstalkTerminal.ask_environment_tier
        self._handlers[Name.SolutionStack] = BeanstalkTerminal.ask_solution_stack
        self._handlers[Name.BranchMapping] = BeanstalkTerminal.ask_branch
        
        self._handlers[Name.RdsEnabled] = RdsTerminal.ask_rds_creation
        self._handlers[Name.RdsMasterPassword] = RdsTerminal.ask_master_password
        self._handlers[Name.RdsSourceSnapshotName] = RdsTerminal.ask_snapshot_name
        self._handlers[Name.RdsDeletionPolicy] = RdsTerminal.ask_delete_to_snapshot

        self._handlers[Name.InstanceProfileName] = IamTerminal.ask_profile_creation
        self._handlers[Name.EnvironmentType] = BeanstalkTerminal.ask_environment_type 


    def ask_parameters(self, parameter_pool, parameter_names, skip_exist):
        ''' 
        Ask customer input of specified parameters via terminal interface.
        if skip_exist is set to True, then any parameters having value in pool 
        will be skipped.
        '''
        # Sort parameter list
        sorted_list = self._sort_list(parameter_names, parameter_pool) \
            if skip_exist else self._sort_list(parameter_names, None)
        
        for parameter_name in sorted_list:
            if parameter_pool.has(parameter_name) and skip_exist:
                continue
            
            if parameter_name in self._handlers:
                self._handlers[parameter_name](parameter_pool)
            else:
                TerminalBase.ask_parameter(parameter_pool, parameter_name)
                

    @classmethod
    def _sort_list(cls, parameter_names, parameter_pool = None):
        ''' 
        Return sorted list of parameter names according to their priority.
        if parameter_pool is not None, returned list will not contain parameters
        which already have value.
        '''
        sorted_list = []
        
        for parameter_name in parameter_names:
            if parameter_pool is not None and parameter_pool.has(parameter_name):    
                continue  # skip current parameter as it already present in parameter pool
            
            if len(sorted_list) < 1:
                sorted_list.append(parameter_name)
            else:
                index = cls._find_index(sorted_list, parameter_name)
                sorted_list.insert(index, parameter_name)
                
        return sorted_list


    @classmethod                
    def _find_index(cls, sorted_list, parameter_name):
        for index, name in enumerate(sorted_list):
            if not Name.is_ahead(name, parameter_name):
                return index
        else:
            return len(sorted_list)
        