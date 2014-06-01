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
from collections import deque
import logging

from lib.aws import requests
from lib.aws.exception import AccessDeniedException
from lib.iam.exception import IamEntityAlreadyExistsException, IamLimitExceededException
from lib.utility import misc
from scli import api_wrapper, prompt
from scli.constants import EbDefault, ParameterSource as PSource, ParameterName as PName,\
    PolicyBucket, TerminalConstant
from scli.terminal.base import TerminalBase
from scli.resources import IamTerminalMessage, TerminalMessage
from scli.parameter import Parameter
from scli.exception import EBSCliException

log = logging.getLogger('cli')

class IamTerminal(TerminalBase):

    @classmethod
    def ask_profile_creation(cls, parameter_pool):
        try:
            iam_client = api_wrapper.create_iam_client(parameter_pool)
            
            original_value = parameter_pool.get_value(PName.InstanceProfileName)
    
            if original_value is None or len(original_value) < 1:
                append_message = TerminalMessage.CurrentValue.format(IamTerminalMessage.CreateProfile)
            else:
                append_message = TerminalMessage.CurrentValue.format(original_value)
            print(IamTerminalMessage.ProfileNameSelect.format(append_message))
            
            profiles = iam_client.list_instance_profiles().result
            sorted_profiles = cls._sort_instance_profile_by_time(profiles)
            
            profile_list = [IamTerminalMessage.CreateProfile];
            for i in range(0, min(len(sorted_profiles), TerminalConstant.IamProfileListNumber)):
                profile_list.append(sorted_profiles[i].name)
            profile_list.append(IamTerminalMessage.OtherProfile)
    
            profile_index = cls.single_choice(choice_list = profile_list,
                                              can_return_none = True)
    
            if profile_index == 0:
                # Create profile instance from scratch
                value = None
            elif profile_index == len(profile_list) - 1:
                # Name not in list
                value = cls._ask_for_specific_profile(parameter_pool, sorted_profiles)
            else:
                value = profile_list[profile_index] if profile_index is not None else original_value
            
            if value is None or len(value) < 1:
                value = cls._create_default_profile(iam_client, parameter_pool)
        except AccessDeniedException as ex:
            prompt.error(IamTerminalMessage.AccessDeniedMessage.format(ex.message))
            if cls.ask_confirmation(IamTerminalMessage.ContinueWithoutRole):
                value = u''
            else:
                raise EBSCliException()
                
        profile = Parameter(PName.InstanceProfileName, value, PSource.Terminal)
        parameter_pool.put(profile, True)


    @classmethod
    def _sort_instance_profile_by_time(cls, profiles):
        sorted_profiles = deque()
        for item in profiles:
            if len(sorted_profiles) < 1:
                sorted_profiles.append(item)
            elif item._create_date_raw <  sorted_profiles[-1]._create_date_raw:
                sorted_profiles.append(item)
            else:
                shift = 0
                while item._create_date_raw <  sorted_profiles[0]._create_date_raw:
                    sorted_profiles.rotate(-1)
                    shift = shift + 1
                sorted_profiles.appendleft(item)
                sorted_profiles.rotate(shift)
        
        return sorted_profiles

    
    @classmethod
    def _create_default_profile(cls, iam_client, parameter_pool):
        try:
            region = parameter_pool.get_value(PName.Region, False)
            log.info(u'Creating IAM role {0}'.format(EbDefault.DefaultRoleName))
            assume_policy_url = EbDefault.RoleAssumePolicyUrlMask.format(PolicyBucket[region])
            assume_policy = misc.to_unicode(requests.get(assume_policy_url).content)
            iam_client.create_role(EbDefault.DefaultRoleName, assume_policy)
        except IamEntityAlreadyExistsException:
            log.info(u'Role {0} already exists.'.format(EbDefault.DefaultRoleName))
            pass

        try:
            log.info(u'Creating IAM instance profile {0}'.format(EbDefault.DefaultInstanceProfileName))            
            iam_client.create_instance_profile(EbDefault.DefaultInstanceProfileName)
        except IamEntityAlreadyExistsException:
            log.info(u'Profile {0} already exists.'.format(EbDefault.DefaultInstanceProfileName))
            pass

        try:
            log.info(u'Adding IAM role {0} to instance profile {1}'.format
                     (EbDefault.DefaultRoleName, EbDefault.DefaultInstanceProfileName))            
            iam_client.add_role_to_instance_profile(EbDefault.DefaultRoleName, 
                                                    EbDefault.DefaultInstanceProfileName)
        except IamLimitExceededException:
            log.info(u'Profile {0} already has one role.'.format(EbDefault.DefaultInstanceProfileName))
            pass
        
        return EbDefault.DefaultInstanceProfileName


    @classmethod
    def _ask_for_specific_profile(cls, parameter_pool, profile_list):
        nameset = set()
        arnset = set()
        for profile in profile_list:
            nameset.add(profile.name)
            arnset.add(profile.arn) 
        
        value = None
        while value is None:
            value = cls.ask_value(parameter_pool, PName.InstanceProfileName)
            if not value in nameset and not value in arnset:
                prompt.error(IamTerminalMessage.ProfileNotExist.format(value))
                value = None  
                
        return value

