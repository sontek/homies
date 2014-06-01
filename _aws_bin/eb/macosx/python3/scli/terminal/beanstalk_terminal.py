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
import os
import re

from lib.utility import shell_utils, misc
from scli import api_wrapper, config_file, prompt
from scli.constants import EbLocalDir, OptionSettingFile, OptionSettingEnvironmentType, \
    ParameterSource, ParameterName, ServiceDefault
from scli.terminal.iam_terminal import IamTerminal 
from scli.terminal.rds_terminal import RdsTerminal 
from scli.terminal.base import TerminalBase
from scli.resources import TerminalMessage, TerminalPromptAskingMessage, \
    TerminalPromptSettingParameterMessage
from scli.parameter import Parameter
from lib.elasticbeanstalk import eb_utils
from lib.elasticbeanstalk.model import EnvironmentTier

class BeanstalkTerminal(TerminalBase):

    @classmethod
    def ask_application_name(cls, parameter_pool):
        if not parameter_pool.has(ParameterName.ApplicationName):
            app_name = shell_utils.get_current_dir_name()
            cls.ask_parameter(parameter_pool, 
                               ParameterName.ApplicationName,
                               autogen_value = app_name)
        else:
            cls.ask_parameter(parameter_pool, ParameterName.ApplicationName)            
        

    @classmethod
    def ask_environment_name(cls, parameter_pool):
        # Auto generate environment name if not specified by user
        if not parameter_pool.has(ParameterName.EnvironmentName):
            old_env_name = None
            app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
            env_name = cls.generate_env_name(app_name)
            cls.ask_parameter(parameter_pool, 
                               ParameterName.EnvironmentName,
                               autogen_value = env_name)
        else:
            old_env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
            cls.ask_parameter(parameter_pool, ParameterName.EnvironmentName)            
    
        # Post processing
        new_env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        new_env_source = parameter_pool.get_source(ParameterName.EnvironmentName)
        
        # Reload RDS password if environment name changes
        if old_env_name != new_env_name:
            location = parameter_pool.get_value(ParameterName.AwsCredentialFile)
            rds_password = config_file.read_rds_master_password(new_env_name, location)
            if rds_password:
                parameter_pool.put(Parameter(ParameterName.RdsMasterPassword,
                                             rds_password,
                                             new_env_source))
            else:
                parameter_pool.remove(ParameterName.RdsMasterPassword)
    
        # Generate option setting file pathname
        if parameter_pool.get_source(ParameterName.OptionSettingFile) == ParameterSource.Default\
            or not misc.string_equal_ignore_case(old_env_name, new_env_name):
            new_opsetting_path = os.path.join(os.getcwd(), EbLocalDir.Path, 
                                           OptionSettingFile.Name + '.' + new_env_name)
            old_opsetting_path = parameter_pool.get_value(ParameterName.OptionSettingFile, False)
            
            # Rename old style optionsetting file 
            if parameter_pool.get_source(ParameterName.OptionSettingFile) == ParameterSource.Default\
                and parameter_pool.has(ParameterName.OriginalSolutionStack)\
                and old_opsetting_path and os.path.exists(old_opsetting_path):
                os.rename(old_opsetting_path, new_opsetting_path)

            # Update optionsetting file name in parameter pool                
            parameter_pool.put(Parameter(ParameterName.OptionSettingFile,
                                         new_opsetting_path,
                                         new_env_source))


    @classmethod
    def ask_branch_environment_name(cls, parameter_pool):
        # Auto generate environment name if not specified by user
        if not parameter_pool.has(ParameterName.EnvironmentName):
            old_env_name = None
            app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
            current_branch = parameter_pool.get_value(ParameterName.CurrentBranch, False)
            env_name = cls.generate_env_name(app_name \
                                             + ServiceDefault.Environment.BRANCH_NAME_SEPERATOR\
                                             + current_branch)
            cls.ask_parameter(parameter_pool, 
                               ParameterName.EnvironmentName,
                               autogen_value = env_name)
        else:
            old_env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
            cls.ask_parameter(parameter_pool, ParameterName.EnvironmentName)            
    
        # Generate option setting file pathname
        new_env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
        if not misc.string_equal_ignore_case(old_env_name, new_env_name):
            new_opsetting_path = os.path.join(os.getcwd(), EbLocalDir.Path, 
                                           OptionSettingFile.Name + '.' + new_env_name)
            parameter_pool.put(Parameter(ParameterName.OptionSettingFile,
                                         new_opsetting_path,
                                         parameter_pool.get_source(ParameterName.EnvironmentName)))

    # Hardcoded list of supported tiers
    ENVIRONMENT_TIERS = [EnvironmentTier.from_values("WebServer","Standard","1.0"),
                         EnvironmentTier.from_values("Worker","SQS/HTTP","1.0"),]
    @classmethod
    def ask_environment_tier(cls, parameter_pool):
        # Skip if user supplies environment tier string as CLI arguments, or already by terminal
        if parameter_pool.has(ParameterName.EnvironmentTier) \
            and ParameterSource.is_ahead(parameter_pool.get_source(ParameterName.EnvironmentTier),
                                         ParameterSource.ConfigFile):
            serialized_tier = parameter_pool.get_value(ParameterName.EnvironmentTier, False).to_serialized_string()
            print((TerminalPromptSettingParameterMessage[ParameterName.EnvironmentTier].\
                  format(serialized_tier)))
            return
        
        original_value = parameter_pool.get_value(ParameterName.EnvironmentTier)
        append_message = '' if original_value is None \
            else TerminalMessage.CurrentValue.format(original_value.to_serialized_string())
        print((TerminalPromptAskingMessage[ParameterName.EnvironmentTier].\
              format(append_message)))
        
        # TODO replace with call to list tiers API once it is created
        tiers = BeanstalkTerminal.ENVIRONMENT_TIERS
        serialized_tiers = [tier.to_serialized_string() for tier in tiers]
        tier_index = cls.single_choice(serialized_tiers, 
                                        TerminalMessage.AvailableEnvironmentTier, None,
                                        original_value is not None)
        
        value = tiers[tier_index] if tier_index is not None else original_value
        tier = Parameter(ParameterName.EnvironmentTier, value, ParameterSource.Terminal)
        parameter_pool.put(tier, True)
    
    
    @classmethod
    def ask_solution_stack(cls, parameter_pool):
        # Skip if user supplies solution stack string as CLI arguments, or already by terminal
        if parameter_pool.has(ParameterName.SolutionStack) \
            and ParameterSource.is_ahead(parameter_pool.get_source(ParameterName.SolutionStack),
                                         ParameterSource.ConfigFile):
            print((TerminalPromptSettingParameterMessage[ParameterName.SolutionStack].\
                  format(parameter_pool.get_value(ParameterName.SolutionStack, False))))
            return            
        
        original_value = parameter_pool.get_value(ParameterName.SolutionStack)
        append_message = '' if original_value is None \
            else TerminalMessage.CurrentValue.format(original_value)        
        print((TerminalPromptAskingMessage[ParameterName.SolutionStack].\
              format(append_message)))
        
        stacks = api_wrapper.retrieve_solution_stacks(parameter_pool)
        stack_index = cls.single_choice(stacks, 
                                        TerminalMessage.AvailableSolutionStack, None,
                                        original_value is not None)
        
        value = stacks[stack_index] if stack_index is not None else original_value
        stack = Parameter(ParameterName.SolutionStack, value, ParameterSource.Terminal)
        parameter_pool.put(stack, True)



    @classmethod
    def ask_branch(cls, parameter_pool):
        current_branch = parameter_pool.get_value(ParameterName.CurrentBranch, False)
        prompt.result(TerminalPromptSettingParameterMessage[ParameterName.CurrentBranch]\
                      .format(current_branch))
        
        previous_env_name = parameter_pool.get_value(ParameterName.EnvironmentName)
        cls.ask_branch_environment_name(parameter_pool)
        
        # Ask whether copy from default 
        def_env_name = parameter_pool.get_value(ParameterName.DefaultEnvironmentName, False)
        if previous_env_name is None:
            if cls.ask_confirmation(TerminalMessage.CopyDefaultToBranch.format(def_env_name)):
                return True #Copy from default
        
        # Ask for branch environment settings
        cls.ask_environment_type(parameter_pool)
        RdsTerminal.ask_rds_creation(parameter_pool)
        IamTerminal.ask_profile_creation(parameter_pool)
        
        return False # Use user input
    
    
    @classmethod
    def ask_environment_type(cls, parameter_pool):
        # Ensure application existence        
        eb_client = api_wrapper.create_eb_client(parameter_pool)
        app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)    
        api_wrapper.create_application(parameter_pool, app_name, eb_client = eb_client)

        # Get available environment types
        solution_stack = parameter_pool.get_value(ParameterName.SolutionStack, False)
        optionDef = eb_utils.get_option_def(eb_client = eb_client, 
                                            app_name = app_name, 
                                            namespace = OptionSettingEnvironmentType.Namespace, 
                                            option_name = OptionSettingEnvironmentType.OptionName, 
                                            solution_stack = solution_stack)
        
        if optionDef and len(optionDef.value_options) > 0:
            # Ask for environment type is option is available
            original_value = parameter_pool.get_value(ParameterName.EnvironmentType)
            append_message = '' if original_value is None \
                else TerminalMessage.CurrentValue.format(original_value)        
            print((TerminalPromptAskingMessage[ParameterName.EnvironmentType].\
                  format(append_message)))
            
            availableTypes = optionDef.value_options
            type_index = cls.single_choice(choice_list = availableTypes, 
                                       title = TerminalMessage.AvailableEnvironmentType, 
                                       message = None, 
                                       can_return_none = original_value is not None)
            value = availableTypes[type_index] if type_index is not None else original_value
            envtype = Parameter(ParameterName.EnvironmentType, value, ParameterSource.Terminal)
            parameter_pool.put(envtype, True)
        else:
            # Remove environment type
            parameter_pool.remove(ParameterName.EnvironmentType)

    
    #--------------------------------
    # Helper methods
    @classmethod
    def generate_env_name(cls, prefix):
        env_name = re.sub(ServiceDefault.Environment.REGEX_NAME_FILTER, 
                           '', prefix, flags = re.UNICODE)
        if len(env_name) > 0:
            env_name = env_name + ServiceDefault.Environment.NAME_POSTFIX
            if len(env_name) > ServiceDefault.Environment.MAX_NAME_LEN:
                env_name = env_name[:ServiceDefault.Environment.MAX_NAME_LEN]
        else:
            env_name = None    
    
        return env_name
   
