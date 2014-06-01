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
import os

from scli.constants import EbLocalDir, OptionSettingFile, ParameterName, \
    ParameterSource, ServiceRegion, ServiceDefault
from scli.resources import ValidationMessage
from scli.exception import ValidationError

log =  logging.getLogger("cli")

class Parameter(object):
    '''
    Parameter store parameter value used by operations
    '''
    def __init__(self, name, value, source):
        self._name = name
        self._source = source
        self._value = value
        
    @property    
    def name(self):
        return self._name
        
    @property    
    def value(self):
        return self._value
    
    @property    
    def source(self):
        return self._source

    @name.setter    
    def name(self, name):
        self._name = name
        
    @value.setter    
    def value(self, value):
        self._value = value
    
    @source.setter    
    def source(self, source):
        self._source = source


class ParameterPool(object):
    '''
    A collection of runtime parameters.
    '''
    def __init__(self):
        self._pool = dict()

    @property    
    def command(self):  # one pool can have at most one command
        return (self._pool[ParameterName.Command].value, self._pool[ParameterName.SubCommand].value)

    @property
    def parameter_names(self):
        params = set()
        for param_name in list(self._pool.keys()):
            params.add(param_name)
        return params

    @property
    def parameters(self):
        return self._pool

    def __getitem__(self, name):
        return self._pool[name]

    def get(self, name):
        return self._pool[name]

    def get_value(self, name, none_if_not_exist = True):
        try:
            return self._pool[name].value
        except KeyError:
            if none_if_not_exist:
                return None
            else:
                raise
    
    def get_source(self, name):
        return self._pool[name].source
    
    def put(self, param, force = False):
        ''' 
        Add new parameter to pool.
        When new parameter is not presented in pool or force is set to True, new
        parameter will be always added/updated to pool. Otherwise, it will be 
        only updated to pool when source of new parameter has higher or equal priority 
        than the one in pool.          
        '''
        if not isinstance(param, Parameter):
            raise AttributeError("Cannot add item that's not instance of Parameter.")
        if param.name not in self._pool \
            or force\
            or param.source == self._pool[param.name].source \
            or ParameterSource.is_ahead(param.source, self._pool[param.name].source):
            self._pool[param.name] = param
            
    def update(self, name, value = None, source = None):
        if name in self._pool:
            if value is not None:
                self._pool[name].value = value
            if source is not None:
                self._pool[name].source = source
        else:
            self.put(Parameter(name, value, source)) 
    
    def has(self, name):
        return name in self._pool
    
    def remove(self, name):
        if self.has(name):
            del self._pool[name]
    
    def validate(self, source = None):
        validator = ParameterValidator()
        validator.validate(self)


class ParameterValidator(object):
    
    _validators = dict()
    
    def __init__(self):
        self._validators[ParameterName.ApplicationName] = \
            self.validate_application_name
        self._validators[ParameterName.ApplicationVersionName] = \
            self.validate_application_version_name
        self._validators[ParameterName.EnvironmentName] = \
            self.validate_environment_name
        self._validators[ParameterName.SolutionStack] = self.validate_solution_stack
        self._validators[ParameterName.ServiceEndpoint] = self.validate_endpoint
        self._validators[ParameterName.Region] = self.validate_region
    
    #-------------------------------
    # Helper method
    #-------------------------------

    @classmethod
    def validate_alphanumeric(cls, value, min_size = None, max_size = None):
        if value is not None:
            size = len(value)
            if min_size is not None and size < min_size:
                return False
            elif max_size is not None and size > max_size:
                return False
            else:
                return value.isalnum()
        else:
            return False

    @classmethod
    def validate_RDS_password(cls, value, min_size = None, max_size = None):
        if value is not None:
            size = len(value)
            if min_size is not None and size < min_size:
                return False
            elif max_size is not None and size > max_size:
                return False
            else:
                return not("/" in value or "\\" in value or "@" in value)
        else:
            return False
    
    @classmethod
    def _validate_string(cls, value, name):
        if len(value) < 1:
            raise ValidationError(ValidationMessage.EmptyString.format(name))
        
    @classmethod
    def _validate_integer(cls, param, max_value = None, min_value = None):
        try:
            value = int(param)
        except ValueError:
            raise ValidationError(ValidationMessage.InvalidNumber.format(param))
        if max_value is not None and max_value < value:
            raise ValidationError(ValidationMessage.NumberTooBig.format(value))
        if min_value is not None and min_value > value:
            raise ValidationError(ValidationMessage.NumberTooSmall.format(value))
    
    #-------------------------------
    # Validation method
    #-------------------------------
    
    @classmethod
    def validate(self, parameter_pool, source = None):
        ''' Validate parameters in pool when their sources equal to specified source.
            Where source is None, validate all. '''
        for name, parameter in list(parameter_pool.parameters.items()):
            if source is None or parameter.source == source:
                try:
                    self._validators[name](parameter_pool, source)
                except KeyError:
                    continue    # skip if don't have validator

    @classmethod
    def validate_application_name(cls, parameter_pool, source):
        if parameter_pool.has(ParameterName.ApplicationName):
            name = parameter_pool.get_value(ParameterName.ApplicationName)
            cls._validate_string(name, ParameterName.ApplicationName)

    @classmethod
    def validate_application_version_name(cls, parameter_pool, source):
        if parameter_pool.has(ParameterName.ApplicationVersionName):
            name = parameter_pool.get_value(ParameterName.ApplicationVersionName)
            cls._validate_string(name, ParameterName.ApplicationVersionName)

    @classmethod
    def validate_environment_name(cls, parameter_pool, source):
        if parameter_pool.has(ParameterName.EnvironmentName):
            name = parameter_pool.get_value(ParameterName.EnvironmentName)
            cls._validate_string(name, ParameterName.EnvironmentName)
                
    @classmethod
    def validate_solution_stack(cls, parameter_pool, source):
        if parameter_pool.has(ParameterName.SolutionStack):
            name = parameter_pool.get_value(ParameterName.SolutionStack)
            cls._validate_string(name, ParameterName.SolutionStack)

    @classmethod
    def validate_region(cls, parameter_pool, source):
        if (parameter_pool.has(ParameterName.Region)):
            region = parameter_pool.get_value(ParameterName.Region)
            if region not in ServiceRegion:
                raise ValidationError(ValidationMessage.InvalidRegion.\
                                      format(region))
    
    @classmethod
    def validate_endpoint(cls, parameter_pool, source):
        if parameter_pool.has(ParameterName.ServiceEndpoint):
            name = parameter_pool.get_value(ParameterName.ServiceEndpoint)
            cls._validate_string(name, ParameterName.ServiceEndpoint)


class DefaultParameterValue(object):

    @classmethod
    def fill_default(cls, parameter_pool):
        cls.fill_version_name(parameter_pool)
        cls.fill_option_setting_file_name(parameter_pool)
        cls.fill_connection_timeout(parameter_pool)
        cls.fill_wait_timeout(parameter_pool)
        cls.fill_update_timeout(parameter_pool)
        cls.fill_poll_delay(parameter_pool)

    @classmethod
    def fill_version_name(cls, parameter_pool):
        parameter_pool.put(Parameter(ParameterName.ApplicationVersionName,
                                     ServiceDefault.DEFAULT_VERSION_NAME,
                                     ParameterSource.Default
                                     ))

    @classmethod
    def fill_option_setting_file_name(cls, parameter_pool):
        path = os.path.join(EbLocalDir.Path, OptionSettingFile.Name)
        parameter_pool.put(Parameter(ParameterName.OptionSettingFile,
                                     path,
                                     ParameterSource.Default
                                     ))

    @classmethod
    def fill_connection_timeout(cls, parameter_pool):
        parameter_pool.put(Parameter(ParameterName.ServiceConnectionTimeout,
                                     ServiceDefault.CONNECTION_TIMEOUT_IN_SEC,
                                     ParameterSource.Default
                                     ))
            
    @classmethod
    def fill_wait_timeout(cls, parameter_pool):
        parameter_pool.put(Parameter(ParameterName.WaitForFinishTimeout,
                                     ServiceDefault.WAIT_TIMEOUT_IN_SEC,
                                     ParameterSource.Default
                                     ))

    @classmethod
    def fill_update_timeout(cls, parameter_pool):
        parameter_pool.put(Parameter(ParameterName.WaitForUpdateTimeout,
                                     ServiceDefault.UPDATE_TIMEOUT_IN_SEC,
                                     ParameterSource.Default
                                     ))
            
    @classmethod
    def fill_poll_delay(cls, parameter_pool):
        parameter_pool.put(Parameter(ParameterName.PollDelay,
                                     ServiceDefault.POLL_DELAY_IN_SEC,
                                     ParameterSource.Default
                                     ))            
                        

        