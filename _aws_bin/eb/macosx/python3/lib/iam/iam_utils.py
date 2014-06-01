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

from lib.elasticbeanstalk import  eb_utils
from scli.constants import OptionSettingIAMProfile, ParameterName
from scli import api_wrapper

log = logging.getLogger('cli')


def apply_instance_profile(parameter_pool, option_settings, option_to_remove):
    app_name = parameter_pool.get_value(ParameterName.ApplicationName, False)
    solution_stack = parameter_pool.get_value(ParameterName.SolutionStack, False)
    eb_client = api_wrapper.create_eb_client(parameter_pool)
    optionDef = eb_utils.get_option_def(eb_client = eb_client, 
                                        app_name = app_name, 
                                        namespace = OptionSettingIAMProfile.Namespace, 
                                        option_name = OptionSettingIAMProfile.OptionName, 
                                        solution_stack = solution_stack)    
    if optionDef:
        # Solution stack supports instance profile
        profile_name = parameter_pool.get_value(ParameterName.InstanceProfileName)
    
        if OptionSettingIAMProfile.Namespace in option_settings\
            and OptionSettingIAMProfile.OptionName in option_settings[OptionSettingIAMProfile.Namespace]:
                # skip reset IAM profile name if option settings already have it
                return
        else:
            eb_utils.add_option_setting(option_settings = option_settings, 
                                        option_remove = option_to_remove, 
                                        namespace = OptionSettingIAMProfile.Namespace, 
                                        option = OptionSettingIAMProfile.OptionName, 
                                        value = profile_name)
    else:
        eb_utils.remove_option_setting(option_settings = option_settings,
                                       option_remove = option_to_remove, 
                                       namespace = OptionSettingIAMProfile.Namespace,
                                       option = OptionSettingIAMProfile.OptionName,
                                       add_to_remove = False)
