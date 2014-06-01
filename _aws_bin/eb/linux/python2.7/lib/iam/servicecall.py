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

from lib.utility import misc
from lib.aws.webservice import AWSQueryClient, AWSSignature
from lib.aws.exception import AccessDeniedException, AwsErrorCode, AwsServiceException, \
    MissingParameterException, InsufficientPrivilegesException, \
    InvalidParameterValueException, OptInRequiredException
from lib.iam.exception import IamErrorCode, IamEntityAlreadyExistsException,\
    IamNoSuchEntityException, IamMalformedPolicyDocumentException, IamLimitExceededException
from lib.iam.request import Request, Response
from lib.iam.model import InstanceProfile, Role
from scli.constants import IamEndpoint, IamRegion

log = _logging.getLogger('aws')

class IamClient(object):
    '''
    Web service client for IAM
    '''
    _signature_version = AWSSignature.SigV4
    _api_version = u'2010-05-08'
    _service_name = u'iam'

    def __init__(self, accessKey, secretKey, result_format = 'json'):
        '''
        Constructor
        '''
        self._accessKey = accessKey
        self._secretKey = secretKey
        self._endpoint = IamEndpoint
        self._format = result_format
        self._region = IamRegion
        
        self._client = AWSQueryClient(self._accessKey, self._secretKey, 
                                      self._endpoint, self._region, 
                                      self._service_name, self._format, 
                                      self._signature_version, self._api_version)

        
    def call(self, request):
        '''Make API call and translate AWSServiceException to more specific exception'''
        try:
            log.debug(request)
            return_msg = self._client.call(request, self._format)
            log.debug(u'Request ID: {0}'.format(return_msg.json().values()[0]\
                                                [u'ResponseMetadata'][u'RequestId'])) 
                      
            return return_msg.json()
            
        except AwsServiceException as ex:
            log.debug(misc.to_unicode(ex))

            # Translate general IAM exception
            if misc.string_equal_ignore_case(ex.code, AwsErrorCode.AccessDenied):
                raise AccessDeniedException(ex)

            elif misc.string_equal_ignore_case(ex.code, AwsErrorCode.OptInRequired):
                raise OptInRequiredException(ex)

            elif misc.string_equal_ignore_case(ex.code, AwsErrorCode.InsufficientPrivileges):
                raise InsufficientPrivilegesException(ex)

            elif misc.string_equal_ignore_case(ex.code, AwsErrorCode.InvalidParameterValue):
                raise InvalidParameterValueException(ex)
            
            elif misc.string_equal_ignore_case(ex.code, AwsErrorCode.MissingParameter):
                raise MissingParameterException(ex)

            elif misc.string_equal_ignore_case(ex.code, IamErrorCode.EntityAlreadyExists):
                raise IamEntityAlreadyExistsException(ex)

            elif misc.string_equal_ignore_case(ex.code, IamErrorCode.NoSuchEntity):
                raise IamNoSuchEntityException(ex)

            elif misc.string_equal_ignore_case(ex.code, IamErrorCode.MalformedPolicyDocument):
                raise IamMalformedPolicyDocumentException(ex)

            elif misc.string_equal_ignore_case(ex.code, IamErrorCode.LimitExceeded):
                raise IamLimitExceededException(ex)
            
            raise
            
 
    #---------------------------------------
    # service calls
    def create_role(self, role_name, assume_role_policy_document, path = None):
        request = Request()
        request.set_action(u'CreateRole')
        request.set_role_name(role_name)
        request.set_assume_role_policy_document(assume_role_policy_document)
        if path is not None: 
            request.set_path(path)
        
        try:    
            response = self.call(request)
        except AwsServiceException:       
            raise 
        
        role = Role.from_json(response[u'CreateRoleResponse'][u'CreateRoleResult'][u'Role'])
        request_id = response[u'CreateRoleResponse'][u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id, role)
    
    
    def create_instance_profile(self, instance_profile_name, path = None):
        request = Request()
        request.set_action(u'CreateInstanceProfile')
        request.set_instance_profile_name(instance_profile_name)
        if path is not None: 
            request.set_path(path)
        
        try:    
            response = self.call(request)
        except AwsServiceException:
            raise 
        
        profile = InstanceProfile.from_json(response[u'CreateInstanceProfileResponse']\
                                            [u'CreateInstanceProfileResult'][u'InstanceProfile'])
        request_id = response[u'CreateInstanceProfileResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id, profile)
    

    def add_role_to_instance_profile(self, role_name, instance_profile_name):
        request = Request()
        request.set_action(u'AddRoleToInstanceProfile')
        request.set_role_name(role_name)
        request.set_instance_profile_name(instance_profile_name)
        
        try:    
            response = self.call(request)
        except AwsServiceException:
            raise 
        
        request_id = response[u'AddRoleToInstanceProfileResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id)


    def put_role_policy(self, role_name, policy_name, policy_document):
        request = Request()
        request.set_action(u'PutRolePolicy')
        request.set_role_name(role_name)
        request.set_policy_name(policy_name)
        request.set_policy_document(policy_document)
        
        try:    
            response = self.call(request)
        except AwsServiceException:
            raise 
        
        request_id = response[u'PutRolePolicyResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id)

        
    def list_instance_profiles(self, max_items = None, path_prefix = None,  marker = None):
        request = Request()
        request.set_action(u'ListInstanceProfiles')
        if max_items is not None: 
            request.set_max_items(max_items)
        if path_prefix is not None: 
            request.set_path_prefix(path_prefix)
        if marker is not None: 
            request.set_marker(marker)
        
        try:    
            response = self.call(request)
        except AwsServiceException:
            raise 
        
        results = response[u'ListInstanceProfilesResponse']\
            [u'ListInstanceProfilesResult'][u'InstanceProfiles']
        request_id = response[u'ListInstanceProfilesResponse']\
            [u'ResponseMetadata'][u'RequestId']

        profiles = []
        for result in results:
            profiles.append(InstanceProfile.from_json(result))                
        return Response(request_id, profiles)        
    
    
    