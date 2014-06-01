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

from lib.aws.exception import AwsServiceException

class IamErrorCode(object):
    '''IAM error code'''
    EntityAlreadyExists = u'EntityAlreadyExists'
    NoSuchEntity = u'NoSuchEntity'
    LimitExceeded = u'LimitExceeded'    
    MalformedPolicyDocument = u'MalformedPolicyDocument'    
    

class IamEntityAlreadyExistsException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(IamEntityAlreadyExistsException, self).__init__(ex.message, ex.code, ex.http_code)
        
class IamLimitExceededException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(IamLimitExceededException, self).__init__(ex.message, ex.code, ex.http_code)

class IamMalformedPolicyDocumentException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(IamMalformedPolicyDocumentException, self).__init__(ex.message, ex.code, ex.http_code)

class IamNoSuchEntityException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(IamNoSuchEntityException, self).__init__(ex.message, ex.code, ex.http_code)





