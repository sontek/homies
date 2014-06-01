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

from lib.utility import misc
from lib.elasticbeanstalk import eb_utils
from lib.rds import rds_utils
from scli.constants import AvailableServiceRegion, ParameterSource, ParameterName, \
    ServiceEndpoint, ServiceRegionName
from scli.terminal.base import TerminalBase
from scli.resources import TerminalMessage, TerminalPromptAskingMessage, \
    TerminalPromptSettingParameterMessage
from scli.parameter import Parameter

log = logging.getLogger('cli')

class ServiceTerminal(TerminalBase):


    @classmethod
    def ask_aws_access_key_id(cls, parameter_pool):
        print(TerminalMessage.AWSKeyLocateHelp)
        msg_value = parameter_pool.get_value(ParameterName.AwsAccessKeyId)
        cls.ask_parameter(parameter_pool, 
                           ParameterName.AwsAccessKeyId,
                           misc.mask_string(msg_value))
    

    @classmethod
    def ask_aws_secret_access_key(cls, parameter_pool):
        msg_value = parameter_pool.get_value(ParameterName.AwsSecretAccessKey)
        cls.ask_parameter(parameter_pool, 
                           ParameterName.AwsSecretAccessKey,
                           misc.mask_string(msg_value))
        

    @classmethod
    def ask_region(cls, parameter_pool):
        original_value = parameter_pool.get_value(ParameterName.Region)
        
        original_source = parameter_pool.get_source(ParameterName.Region) \
            if parameter_pool.has(ParameterName.Region) else None
        
        if original_value is not None and \
            ParameterSource.is_ahead(original_source, ParameterSource.Terminal):
            print(TerminalPromptSettingParameterMessage[ParameterName.Region].\
                      format(ServiceRegionName[original_value]))            
            region_value = original_value
        else:
            append_message = u'' if original_value is None \
                else TerminalMessage.CurrentValue.format(ServiceRegionName[original_value])        
            print TerminalPromptAskingMessage[ParameterName.Region].format(append_message)
            
            region_name_list = list()
            for region in AvailableServiceRegion:
                region_name_list.append(ServiceRegionName[region])
            region_index = cls.single_choice(choice_list = region_name_list, 
                                             title = TerminalMessage.AvailableRegion, 
                                             can_return_none = original_value is not None)
            
            region_value = AvailableServiceRegion[region_index] \
                if region_index is not None else original_value
            region = Parameter(ParameterName.Region, 
                               misc.to_unicode(region_value), 
                               ParameterSource.Terminal)
            parameter_pool.put(region, True)

        # Set RDS service endpoints if not specified as CLI arguments
        log.info(u'Generate RDS endpoint from region "{0}".'.format(region_value))
        eb_utils.generate_endpoint(parameter_pool, region_value, ParameterSource.Terminal)
        # Set RDS endpont and snippet if not specified as CLI arguments
        log.info(u'Generate RDS snippet URL from region "{0}".'.format(region_value))
        rds_utils.generate_endpoint(parameter_pool, region_value, ParameterSource.Terminal)


    @classmethod
    def ask_service_endpoint(cls, parameter_pool):
        endpoint_source = parameter_pool.get_source(ParameterName.ServiceEndpoint)\
            if parameter_pool.has(ParameterName.ServiceEndpoint) else None
        region_source = parameter_pool.get_source(ParameterName.Region)\
            if parameter_pool.has(ParameterName.Region) else None
        
        if region_source is None:
            cls.ask_region(parameter_pool)
        
        if endpoint_source is not None \
            and not ParameterSource.is_ahead(region_source, endpoint_source):
            return

        endpoint = Parameter(ParameterName.ServiceEndpoint, 
                             ServiceEndpoint[parameter_pool.get_value(ParameterName.Region, False)], 
                             region_source)
        parameter_pool.put(endpoint, False)
            