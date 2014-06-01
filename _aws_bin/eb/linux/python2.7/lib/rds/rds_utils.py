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

from lib.aws.exception import InvalidParameterValueException
from lib.elasticbeanstalk import misc as eb_misc
from lib.elasticbeanstalk.request import TemplateSnippet
from lib.rds.exception import RdsDBInstanceNotFoundException
from scli import api_wrapper
from scli.constants import CommandType, ParameterName, RdsDefault, RdsEndpoint
from scli.parameter import Parameter

log = logging.getLogger('cli')


CommandRequireRdsParameter = [
    CommandType.START,
    CommandType.UPDATE,
]

#-----------------------------------
#     Utility functions
#-----------------------------------

def generate_endpoint(parameter_pool, region, source, force = False):
    parameter_pool.put(Parameter(ParameterName.RdsEndpoint, 
                                 RdsEndpoint[region], 
                                 source))     
    parameter_pool.put(Parameter(ParameterName.RdsSnippetUrl, 
                                 RdsDefault.get_snippet_url(region), 
                                 source))      


def _should_skip_rds(parameter_pool, stack_name):
    return not parameter_pool.has(ParameterName.RdsEnabled)\
        or not is_rds_snippet_compatible(parameter_pool, stack_name)


def rds_handler(parameter_pool, template_spec, stack_name, option_settings, option_to_remove):
    
    # If not specified or incompatible, skip 
    if _should_skip_rds(parameter_pool, stack_name):
        _remove_all_rds_options(option_settings, option_to_remove)
        return None


def rds_extension_handler(parameter_pool, template_spec, stack_name, option_settings, option_to_remove):
    
    # If not specified or incompatible, skip 
    if _should_skip_rds(parameter_pool, stack_name):
        return
    
    if parameter_pool.get_value(ParameterName.RdsEnabled, False):
        _add_rds_extension(parameter_pool, template_spec, option_settings, option_to_remove)
    else:
        _remove_rds_extension(parameter_pool, template_spec, option_settings, option_to_remove)


def is_rds_snippet_compatible(parameter_pool, solution_stack):
    return not eb_misc.match_legacy_stack(solution_stack)


def is_rds_delete_to_snapshot(parameter_pool, app_name, env_name):
    policy_options = {RdsDefault.Namespace: {RdsDefault.OptionNames[ParameterName.RdsDeletionPolicy]}}
    try:
        policy_setting = api_wrapper.retrieve_configuration_settings(parameter_pool, app_name,
                                                                     env_name = env_name, 
                                                                     options = policy_options)
    except InvalidParameterValueException:
        return None  # Environment not exist. No RDS instance        

    if RdsDefault.Namespace in policy_setting\
        and RdsDefault.OptionNames[ParameterName.RdsDeletionPolicy] in policy_setting[RdsDefault.Namespace]:
        return RdsDefault.del_policy_to_bool(policy_setting[RdsDefault.Namespace]\
                                             [RdsDefault.OptionNames[ParameterName.RdsDeletionPolicy]])
    else: 
        return None 


def retrieve_rds_instance_property(parameter_pool, resources):
    #TODO: handling multiple rds instances
    #Search for first RDS instance
    for resource in resources.resources:
        if resource.type == RdsDefault.ResourceType:
            physical_id = resource.physical_resource_id
            logical_id = resource.logical_resource_id
            break
    else:
        return None, None # Cannot find any RDS instance

    try:
        return logical_id, api_wrapper.retrive_rds_instance(parameter_pool, physical_id)
    except RdsDBInstanceNotFoundException:
        return logical_id, None # RDS Instance is 


def has_rds_instance(parameter_pool, env_name):
    
    #TODO: handling multiple rds instances
    #Search for first RDS instance
    try:
        resources = api_wrapper.retrieve_environment_resources(parameter_pool, env_name)
    except InvalidParameterValueException:
        return False # environment not exists
    
    #TODO: handling multiple rds instances
    _, rds_property = retrieve_rds_instance_property(parameter_pool, resources)
    if rds_property is None:
        return False
    else:
        return True


def password_key_name(env_name):
    return env_name + u'_' + ParameterName.RdsMasterPassword


def is_require_rds_parameters(parameter_pool):
    command = parameter_pool.get_value(ParameterName.Command, False)
    if command in CommandRequireRdsParameter:
        return True
    else:
        return False
   
#-----------------------------------
#     Helper functions
#-----------------------------------

def _add_rds_extension(parameter_pool, template_spec, option_settings, option_to_remove):
    region = parameter_pool.get_value(ParameterName.Region, False)
    
    #Generate snippet    
    env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
    if not has_rds_instance(parameter_pool, env_name):
        snippet = TemplateSnippet()
        snippet.snippet_name = RdsDefault.SnippetName
        snippet.source_url = RdsDefault.get_snippet_url(region)
        snippet.order = RdsDefault.SnippetAddOrder
        template_spec.template_snippets.append(snippet)
    
    #Add/update option settings
    #TODO: change option_settings to o(1) structure while retain order for output
    for pname in RdsDefault.OptionMinSet:
        if parameter_pool.get_value(pname) is not None:
            _update_option_setting(option_settings, 
                                   RdsDefault.Namespace, 
                                   RdsDefault.OptionNames[pname], 
                                   parameter_pool.get_value(pname, False))
    
    _trim_rds_options(option_settings, option_to_remove)


def _remove_rds_extension(parameter_pool, template_spec, option_settings, option_to_remove):
    region = parameter_pool.get_value(ParameterName.Region, False)
    
    #Generate snippet
    snippet = TemplateSnippet()
    snippet.snippet_name = RdsDefault.SnippetName
    snippet.source_url = RdsDefault.get_snippet_url(region)
    snippet.order = RdsDefault.SnippetRemoveOrder
    template_spec.template_snippets.append(snippet)
    
    #Remove option settings
    _remove_all_rds_options(option_settings, option_to_remove)

    
def _remove_all_rds_options(option_settings, option_to_remove):
    if RdsDefault.Namespace in option_settings:
        del option_settings[RdsDefault.Namespace]
    for pname in RdsDefault.OptionNames:
        _add_option_to_remove(option_to_remove, 
                              RdsDefault.Namespace, 
                              RdsDefault.OptionNames[pname])

def _trim_rds_options(option_settings, option_to_remove):
    if RdsDefault.Namespace in option_settings:
        for option, value in option_settings[RdsDefault.Namespace].items():
            if value is None or len(value) == 0:
                del option_settings[RdsDefault.Namespace][option]
                _add_option_to_remove(option_to_remove, RdsDefault.Namespace, option)


def _update_option_setting(option_settings, namespace, option_name, value):
    if namespace not in option_settings:
        option_settings[namespace] = dict()
    option_settings[namespace][option_name] = value

                
def _add_option_to_remove(option_to_remove, namespace, option_name):
    if namespace not in option_to_remove:
        option_to_remove[namespace] = set()
    option_to_remove[namespace].add(option_name)

