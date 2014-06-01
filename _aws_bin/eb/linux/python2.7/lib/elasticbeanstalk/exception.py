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


class EBErrorCode(object):
    '''Elastic Beanstalk error code'''
    
    TooManyApplicationsException = u'TooManyApplicationsException'
    TooManyConfigurationTemplates = u'TooManyConfigurationTemplates'
    OperationInProgress = u'OperationInProgress'    
    
    SourceBundleDeletion = u'SourceBundleDeletion'
    S3LocationNotInServiceRegion = u'S3LocationNotInServiceRegion'


class ElasticBeanstalkException(AwsServiceException):
    
    def __init__(self, ex):
        if not issubclass(ex.__class__, AwsServiceException):
            raise AttributeError(u'Must initialize from instance of AwsServiceException subclass.')
        super(ElasticBeanstalkException, self).__init__(ex.message, ex.code, ex.http_code)


class TooManyApplicationsException(ElasticBeanstalkException):
    pass        

class TooManyApplicationVersionsException(ElasticBeanstalkException):
    pass

class TooManyEnvironmentsException(ElasticBeanstalkException):
    pass        


class AlreadyExistException(ElasticBeanstalkException):
    pass        


class OperationInProgressException(ElasticBeanstalkException):
    pass

class ApplicationHasRunningEnvException(ElasticBeanstalkException):
    pass

class SourceBundleDeletionException(ElasticBeanstalkException):
    pass

class S3LocationNotInServiceRegionException(ElasticBeanstalkException):
    pass
