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

class AwsErrorCode(object):
    '''AWS common error code'''
    AccessDenied = u'AccessDenied'
    InsufficientPrivileges = u'InsufficientPrivileges'
    InvalidClientTokenId = u'InvalidClientTokenId'
    InvalidParameterCombination = u'InvalidParameterCombination'
    InvalidParameterValue = u'InvalidParameterValue'
    InvalidQueryParameter = u'InvalidQueryParameter'
    MalformedQueryString = u'MalformedQueryString'
    MissingParameter = u'MissingParameter'
    OptInRequired = u'OptInRequired'
    RequestExpired = u'RequestExpired'
    Throttling = u'Throttling'        
    

class AwsServiceException(Exception):
    
    def __init__(self, msg, code, http_code):
        self._msg = msg
        self._code = code
        self._http_code = http_code
        
    @property
    def message(self):
        return self._msg
    
    @property
    def code(self):
        return self._code
    
    @property
    def http_code(self):
        return self._http_code    

    def __str__(self):
        return u'{0}. {1}'.format(self._code, self._msg)
    
    def __repr__(self):
        return u'HTTP {0}:{1}. {2}'.format(self._http_code, self._code, self._msg)


class UnknownHttpCodeException(AwsServiceException):
    ''' Exception of receiving http code other than 200'''    
    def __init__(self, message, code, http_code):
        super(UnknownHttpCodeException, self).__init__(message, code, http_code)

        
class MissingParameterException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(MissingParameterException, self).__init__(ex.message, ex.code, ex.http_code)
        
class InsufficientPrivilegesException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(InsufficientPrivilegesException, self).__init__(ex.message, ex.code, ex.http_code)
        
class InvalidParameterValueException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(InvalidParameterValueException, self).__init__(ex.message, ex.code, ex.http_code)

class OptInRequiredException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(OptInRequiredException, self).__init__(ex.message, ex.code, ex.http_code)
        
class AccessDeniedException(AwsServiceException):
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(AccessDeniedException, self).__init__(ex.message, ex.code, ex.http_code)        
        