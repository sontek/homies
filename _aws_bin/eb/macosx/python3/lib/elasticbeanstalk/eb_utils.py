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

import re
import logging

from lib.elasticbeanstalk.request import TemplateSpecification
from lib.utility import misc, shell_utils
from scli import api_wrapper, config_file, prompt
from scli.constants import DevToolsEndpoint, DevToolsDefault, DefaultAppSource, \
    OptionSettingVPC, OptionSettingEnvironmentType, OptionSettingApplicationEnvironment, \
    OptionSettingContainerPrefix, OptionSettingTemplatePrefix, \
    ParameterName as PName, ParameterSource as PSource, ServiceDefault, ServiceEndpoint
from scli.parameter import Parameter
from scli.resources import DevToolsMessage, ValidationMessage
from scli.terminal.base import TerminalBase


log = logging.getLogger('eb')


def generate_endpoint(parameter_pool, region, source, force = False):
    parameter_pool.put(Parameter(PName.ServiceEndpoint, 
                                 ServiceEndpoint[region], 
                                 source))     
    parameter_pool.put(Parameter(PName.DevToolsEndpoint, 
                                 DevToolsEndpoint[region], 
                                 source))      


def has_default_app(parameter_pool, solution_stack, eb_client = None):
    appsource_options = {DefaultAppSource.Namespace : {DefaultAppSource.OptionName}}
    
    if not eb_client:
        eb_client = api_wrapper.create_eb_client(parameter_pool)
    
    spec = TemplateSpecification()
    spec.template_source.solution_stack_name = solution_stack,     
    
    options = api_wrapper.retrieve_configuration_options(eb_client = eb_client,
                                            solution_stack = solution_stack,
                                            options = appsource_options,
                                            template_specification = spec)
    for option in options:
        if misc.string_equal_ignore_case(DefaultAppSource.Namespace, option.namespace) \
            and misc.string_equal_ignore_case(DefaultAppSource.OptionName, option.name):
            return True
        
    return False


def trim_vpc_options(parameter_pool, option_settings, option_to_remove):
    if OptionSettingVPC.Namespace in option_settings\
        and OptionSettingVPC.MagicOptionName in option_settings[OptionSettingVPC.Namespace]\
        and not misc.is_blank_string(option_settings[OptionSettingVPC.Namespace]\
                                     [OptionSettingVPC.MagicOptionName]):
        # VPC enabled
        for namespace in OptionSettingVPC.TrimOption:
            for option in OptionSettingVPC.TrimOption[namespace]:
                remove_option_setting(option_settings, option_to_remove, namespace, option)

        # Reapply DBSubnets if RDS is enabled
        if parameter_pool.get_value(PName.RdsEnabled):
            option_location = parameter_pool.get_value(PName.OptionSettingFile, False)             
            ori_option_settings = config_file.load_env_option_setting_file(option_location,
                                                                   quiet = True)
            
            if OptionSettingVPC.Namespace in ori_option_settings\
                and OptionSettingVPC.DBSubnets in ori_option_settings[OptionSettingVPC.Namespace]:
                dbsubnets = ori_option_settings[OptionSettingVPC.Namespace][OptionSettingVPC.DBSubnets]
                if not misc.is_blank_string(dbsubnets):
                    add_option_setting(option_settings, option_to_remove, 
                                       OptionSettingVPC.Namespace, 
                                       OptionSettingVPC.DBSubnets, 
                                       dbsubnets)
                
    else:
        # VPC disabled
        remove_option_namespace(option_settings, option_to_remove, OptionSettingVPC.Namespace)


def apply_environment_type(parameter_pool, template_spec, stack_name, env_name, option_settings, option_to_remove):
    # If not specified, skip 
    envtype = parameter_pool.get_value(PName.EnvironmentType)
    if envtype:
        # Describe applicable option settings
        eb_client = api_wrapper.create_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(PName.ApplicationName, False)
        
        if env_name:
            raw_option_defs = api_wrapper.retrieve_configuration_options(eb_client=eb_client,
                                                              app_name=app_name,
                                                              env_name=env_name,
                                                              template_specification=template_spec,
                                                              options=None)
        else:
            raw_option_defs = api_wrapper.retrieve_configuration_options(eb_client=eb_client,
                                                              app_name=app_name,
                                                              solution_stack=stack_name,
                                                              template_specification=template_spec,
                                                              options=None)
        
        option_defs = set()
        for raw_option_def in raw_option_defs:
            option_defs.add(raw_option_def.namespace + '-' + raw_option_def.name)

        # Return if environment type option is not available
        if OptionSettingEnvironmentType.Namespace + '-' + \
            OptionSettingEnvironmentType.OptionName not in option_defs:
            prompt.result(ValidationMessage.EnvTypeInapplicable.format(envtype))
            return

        # remove inapplicable option settings
        removed = False
        for namespace in list(option_settings.keys()):
            
            # TODO Fix this temporary hack to let environment tier options pass through
            if namespace == 'aws:elasticbeanstalk:sqsd':
                continue
            
            for option_name in list(option_settings[namespace].keys()):
                if not is_customizable_namespace(namespace)\
                    and namespace + '-' + option_name not in option_defs:
                    remove_option_setting(option_settings, option_to_remove, 
                                          namespace, option_name, False)
                    removed = True
        if removed:
            prompt.result(ValidationMessage.EnvTypeBlowAwayOptionSettings)

        # Set environment type
        add_option_setting(option_settings, option_to_remove, 
                           OptionSettingEnvironmentType.Namespace, 
                           OptionSettingEnvironmentType.OptionName, 
                           envtype)


def check_app_version(parameter_pool, eb_client = None):
    #TODO: Do we need to blast version info away if this part is strong enough?
    if not parameter_pool.has(PName.ApplicationVersionName) \
        or parameter_pool.get_source(PName.ApplicationVersionName) == PSource.Default:

        version_name = get_head_version(parameter_pool, eb_client=eb_client, quiet=True)        
        
        if version_name is not None:
            log.info('Found a version from local repository: {0}. Using this version.'.\
                     format(version_name))
            return version_name
        else:
            # Otherwise try push a new one
            if not parameter_pool.get_value(PName.Force) == ServiceDefault.ENABLED\
                and not TerminalBase.ask_confirmation(DevToolsMessage.PushLocalHead):
                return ServiceDefault.DEFAULT_VERSION_NAME
            else:
                if shell_utils.git_aws_push(push_only=True, quiet=False):
                    version_name = get_head_version(parameter_pool, 
                                                    eb_client=eb_client, 
                                                    quiet=False)
                    if version_name:
                        return version_name   
                return ServiceDefault.DEFAULT_VERSION_NAME
    else:
        # Verify existence of version
        app_name = parameter_pool.get_value(PName.ApplicationName, False)
        version_names = api_wrapper.get_all_versions(parameter_pool, app_name, eb_client)
        version_name = parameter_pool.get_value(PName.ApplicationVersionName)    
        if version_name in version_names:
            # Assume version is still valid and compatible with current solution stack
            return version_name
        else:
            # 
            return ServiceDefault.DEFAULT_VERSION_NAME


def get_head_version(parameter_pool, eb_client = None, quiet = True):
    # Get all versions
    app_name = parameter_pool.get_value(PName.ApplicationName, False)
    version_names = api_wrapper.get_all_versions(parameter_pool, app_name, eb_client)

    # Try get local commit HEAD hash
    head_hash = shell_utils.get_repo_head_hash(quiet)
    if head_hash is None:
        return ServiceDefault.DEFAULT_VERSION_NAME
    
    # Try find a version corresponding to local HEAD
    version_re = re.compile(DevToolsDefault.VersionNameRe.format(head_hash),re.UNICODE)
    timestamp = 0
    for version in version_names:
        if version_re.match(version):
            cur_timestamp = int(version.split(DevToolsDefault.NameDelimiter)[2])
            timestamp = cur_timestamp if cur_timestamp > timestamp else timestamp
    
    if timestamp > 0:
        # Found a version generated from local repos HEAD
        log.info('Found a version generated from local HEAD {0}. Using this version.'.\
                 format(head_hash))
        return DevToolsDefault.VersionNameMask.format(head_hash, timestamp)
    else:
        return None


# Add/update an option setting of specified value to option setting dict        
def add_option_setting(option_settings, option_remove, namespace, option, value):
    if namespace not in option_settings:
        option_settings[namespace] = dict()
    option_settings[namespace][option] = value
    
    if namespace in option_remove and option in option_remove[namespace]:
        option_remove[namespace].remove(option)


# Remove an option setting from option setting dict        
def remove_option_setting(option_settings, option_remove, 
                          namespace, option, add_to_remove = True):
    if namespace in option_settings and option in option_settings[namespace]:
        del option_settings[namespace][option]
        if len(option_settings[namespace]) < 1:
            del option_settings[namespace]

    if add_to_remove:
        if namespace not in option_remove:
            option_remove[namespace] = set()
        option_remove[namespace].add(option)


# Remove an entire option namespace from option setting dict     
def remove_option_namespace(option_settings, option_remove, 
                            namespace, add_to_remove = True):
    if namespace in option_settings:
        if add_to_remove:
            for option in list(option_settings[namespace].keys()):
                remove_option_setting(option_settings, option_remove, 
                                      namespace, option, True)
        else:
            del option_settings[namespace]


# Get definition for a particular option setting
def get_option_def(eb_client, app_name, namespace, option_name, 
                   solution_stack = None, env_name = None):

    options = dict()
    options[namespace] = set()
    options[namespace].add(option_name)
    optionDef = api_wrapper.retrieve_configuration_options(eb_client = eb_client, 
                                                          app_name = app_name, 
                                                          solution_stack =solution_stack, 
                                                          options = options)
    
    if len(optionDef) > 0:
        return optionDef[0]
    else:
        return None
    

def is_customizable_namespace(namespace):
    if namespace.startswith(OptionSettingApplicationEnvironment.Namespace)\
        or namespace.startswith(OptionSettingContainerPrefix) \
        or namespace.startswith(OptionSettingTemplatePrefix):
        return True
    else:
        return False

