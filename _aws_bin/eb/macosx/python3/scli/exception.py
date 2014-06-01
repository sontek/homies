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

class EBSCliException(Exception):
    '''Base exception class for all exceptions generated in EB Cli'''
    pass


class ArgumentError(EBSCliException):
    '''Command line argument error'''
    pass


class ValidationError(EBSCliException):
    '''Exception raised when validation fails'''
    pass


class ApplicationNotExistError(EBSCliException):
    '''Exception raised when expected application not exists'''
    pass

class ApplicationVersionNotExistError(EBSCliException):
    '''Exception raised when expected application version not exists'''
    pass

class EnvironmentNotExistError(EBSCliException):
    '''Exception raised when expected environment not exists'''
    pass
    
class EBConfigFileNotExistError(EBSCliException):
    '''Exception raised when Elastic Beanstalk configuration file not exists.'''
    pass
