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
import operator
import re
import stat
from ConfigParser import MissingSectionHeaderError
from collections import OrderedDict

from lib.elasticbeanstalk.model import EnvironmentTier
from lib.rds import rds_utils
from lib.utility import misc
from lib.utility.configfile_parser import NoSectionConfigParser, SectionedConfigParser

from scli import prompt
from scli.constants import (AwsCredentialFileDefault, EbConfigFile, FileDefaultParameter,
    FileErrorConstant, GitIgnoreFile,  LocalOptionSettings,
    OptionSettingContainerPrefix, OptionSettingApplicationEnvironment, 
    ParameterName, ParameterSource, ServiceRegionId)
from scli.exception import (EBSCliException, EBConfigFileNotExistError)
from scli.parameter import Parameter, ParameterPool
from scli.resources import (CredentialFileErrorMessage, ConfigFileErrorMessage,
    GeneralFileMessage, OptionSettingFileErrorMessage)

log = logging.getLogger('cli')

#------------------------------
# Helper
#------------------------------

    
def rotate_file(location, max_retry = FileDefaultParameter.RotationMaxRetry):
    ''' Rotate a file by adding a incremental postfix to filename'''
    if not os.path.exists(location):
        return
     
    filename = os.path.basename(location)
    path = os.path.dirname(location)
    for i in range(1, max_retry):
        new_location = os.path.join(path, (filename + u'_{0}'.format(i)))
        if not os.path.exists(new_location):
            log.info(u'Renamed file "{0}" to "{1}".'.format(location, new_location))
            prompt.info(GeneralFileMessage.RenameFile.format(location, new_location))
            os.rename(location, new_location)
            return
    else:
        log.error(u'Cannot rotate file {0} because all available names are used.'.\
                  format(location))
        prompt.error(GeneralFileMessage.RotationNameNotAvailable.format(location))
        return
    

#------------------------------
# File access permission
#------------------------------
def check_access_permission(location, quiet = False):
    log.info(u'Checking file access permission at "{0}".'.format(location))
    if misc.is_os_windows(): 
        log.debug(u'Skipped checking file access permission for Windows platform.')
        return None

    try:
        file_mode = os.stat(location).st_mode
        if 0 != stat.S_IMODE(file_mode) & stat.S_IRWXG or\
            0 != stat.S_IMODE(file_mode) & stat.S_IRWXO :
            return False
        return True
    except BaseException as ex:
        log.error(u'Encountered error when checking access permission for file "{0}", because "{1}".'.\
                  format(location, ex))
        if quiet:
            return None
        else:
            raise


def set_access_permission(location, quiet = False):
    log.info(u'Setting file access permission at "{0}".'.format(location))
    if misc.is_os_windows():
        log.debug(u'Skipped setting file access permission for Windows platform.')
        return False

    try:
        os.chmod(location, stat.S_IRUSR | stat.S_IWUSR)
        return True
    except BaseException as ex:
        log.error(u'Encountered error when setting access permission for file "{0}", because "{1}".'.\
                  format(location, ex))
        if quiet:
            return False
        else:
            raise


#------------------------------
# Git ignore file
#------------------------------
def add_ignore_file(location):
    '''
    Add EB config files and log files to git ignore file
    '''
    log.info(u'Adding ignore files to "{0}".'.format(location))
    # Compile ignore file dict and regular expressions
    namelist = dict()
    relist = dict()
    for item in GitIgnoreFile.Files:
        namelist[item.Name] = True
        relist[item.Name] = re.compile(item.NameRe, re.UNICODE)
    log.debug(u'Files needs to present in git ignore list: {0}'.\
              format(misc.collection_to_string(namelist.keys())))

    with open(location, 'a+') as f:
        # Search for filenames
        f.seek(0, os.SEEK_SET)
        lines = f.readlines()
        for line in lines:
            for name, regex in relist.items():
                if regex.match(line):
                    namelist[name] = False
        
        # Add filenames if not present in ignore file
        f.seek(0, os.SEEK_END)
        if len(lines) > 0 and not os.linesep in lines[-1]: # Add EOL if last line doesn't have it
            f.write(os.linesep)
        for name, add in namelist.items():
            if add:
                log.debug(u'Adding file "{0}" to git ignore list.'.format(name))
                f.write(u'{0}{1}'.format(name, os.linesep))
  

#------------------------------
# Credential File
#------------------------------

def default_aws_credential_file_path():
    # Get home folder of current user
    return os.path.join(os.path.expanduser('~'), AwsCredentialFileDefault.FilePath)

def default_aws_credential_file_location():
    return os.path.join(default_aws_credential_file_path(), AwsCredentialFileDefault.FileName)        


def read_aws_credential_file(location, parameter_pool, func_matrix, source, quiet = False):
    try:
        
        log.info(u'Reading AWS credential from file: "{0}"'.format(location))
        parser = NoSectionConfigParser()
        parser.read(location)

        for branch, name, from_file_func in func_matrix:
            try:
                if branch:
                    env_name = parameter_pool.get_value(ParameterName.Branches, False)\
                        [branch][ParameterName.EnvironmentName]
                else:
                    env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)
            except BaseException:
                # No environment name. Set it to invalid value. 
                env_name = u''
            
            if name == ParameterName.RdsMasterPassword:
                key_name = rds_utils.password_key_name(env_name)
            else:
                key_name = AwsCredentialFileDefault.KeyName[name]
                
            if parser.has_option(key_name):
                value = parser.get(key_name)
                value = from_file_func(value) if from_file_func is not None else value
                if branch:
                    branch_setting = parameter_pool.get_value(ParameterName.Branches, False)[branch]
                    branch_setting[name] = value
                else:
                    parameter_pool.put(Parameter(name, value, source))
        log.info(u'Finished reading AWS credential from file.')
                
    except BaseException as ex:
        log.error(u'Failed to retrieve AWS credential from file "{0}", because: "{1}"'.\
                  format(location, ex))
        if not quiet:
            msg = CredentialFileErrorMessage.ReadError.format(location)
            prompt.error(msg)
            raise EBSCliException(msg)
        else:          
            return False # if failed, just skip 
    

def write_aws_credential_file(location, parameter_pool, 
                              func_matrix,
                              quiet = False):
    try:
        log.info(u'Writing AWS credential to file: "{0}"'.format(location))
        parser = NoSectionConfigParser()
        try:
            parser.read(location)
        except IOError as ex:
            pass # No existing file
        
        for branch, name, to_file_func in func_matrix:
            if branch:
                value = parameter_pool.get_value(ParameterName.Branches, False)[branch][name]
                env_name = parameter_pool.get_value(ParameterName.Branches, False)[branch]\
                    [ParameterName.EnvironmentName]       
            else:
                value = parameter_pool.get_value(name, False)
                env_name = parameter_pool.get_value(ParameterName.EnvironmentName, False)       
            
            if to_file_func:
                value = to_file_func(value) 

            if name == ParameterName.RdsMasterPassword:
                key_name = rds_utils.password_key_name(env_name)
            else:
                key_name = AwsCredentialFileDefault.KeyName[name]
            parser.set(key_name, value)
        
        parser.write(location)
        log.info(u'Finished writing AWS credential to file.')
                
        # Set access permission
        set_access_permission(location, False)
        log.info(u'Set AWS credential file access permission.')
        
    except BaseException as ex:
        log.error(u'Failed to update AWS credential file at "{0}", because: "{1}"'.\
                  format(location, ex))
        msg = CredentialFileErrorMessage.WriteError.format(location)
        prompt.error(msg)
        if not quiet:
            raise EBSCliException(msg)
        else:          
            return False # if failed, just skip 


def trim_aws_credential_file(location, param_list, quiet = False):
    try:
        log.info(u'Trimming AWS credential file: "{0}"'.format(location))
        parser = NoSectionConfigParser()
        try:
            parser.read(location)
        except IOError as ex:
            return # File not exists
        
        for name in param_list:
            parser.remove_option(name)
        
        parser.write(location)
        log.info(u'Finished trimming AWS credential file.')
                
        # Set access permission
        set_access_permission(location, False)
        log.info(u'Set AWS credential file access permission.')
        
    except BaseException as ex:
        log.error(u'Failed to trim AWS credential file at "{0}", because: "{1}"'.\
                  format(location, ex))
        msg = CredentialFileErrorMessage.WriteError.format(location)
        prompt.error(msg)
        if not quiet:
            raise EBSCliException(msg)
        else:          
            return False # if failed, just skip 
        

def read_rds_master_password(env_name, location):
    empty_pool = ParameterPool()
    empty_pool.put(Parameter(ParameterName.EnvironmentName,
                             env_name,
                             ParameterSource.Default))
    func_matrix = [(None, ParameterName.RdsMasterPassword, None)]
    if location is None:
        location = default_aws_credential_file_location()        
    
    read_aws_credential_file(location, empty_pool, func_matrix, empty_pool, True)
    
    return empty_pool.get_value(ParameterName.RdsMasterPassword)
        
#------------------------------
# Config File
#------------------------------

def _region_id_to_region(region_id):
    return ServiceRegionId.keys()[ServiceRegionId.values().index(region_id)]

def _region_to_region_id(region):
    return ServiceRegionId[region]

def _none_to_empty_string(value):
    if value is None:
        return u''
    else:
        return value

def _empty_string_to_none(value):
    if value == u'' or len(value) < 1:
        return None
    else:
        return value

def serialize_environment_tier(value):
    if value is None:
        return u'';
    else:
        return value.to_serialized_string()

def deserialize_environment_tier(value):
    return EnvironmentTier.from_serialized_string(value)
    
# Format: ParameterName => (from_file function, to_file function)
ConfigFileParameters = OrderedDict([
    (ParameterName.AwsCredentialFile, (None, None)), 
    (ParameterName.ApplicationName, (None, None)), 
    (ParameterName.ApplicationVersionName, (None, None)),
    (ParameterName.DevToolsEndpoint, (None, None)), 
    (ParameterName.EnvironmentName, (None, None)), 
    (ParameterName.OptionSettingFile,(None, None)),
    (ParameterName.SolutionStack, (None, None)), 
    (ParameterName.EnvironmentTier, (deserialize_environment_tier, serialize_environment_tier)), 
    (ParameterName.Region, (_region_id_to_region, _region_to_region_id)), 
    (ParameterName.ServiceEndpoint, (None, None)), 
    (ParameterName.RdsEndpoint,  (None, None)), 
    (ParameterName.RdsEnabled, (misc.string_to_boolean, misc.bool_to_yesno)), 
    (ParameterName.RdsSourceSnapshotName, (_empty_string_to_none, _none_to_empty_string)), 
    (ParameterName.RdsDeletionPolicy, (None, None)), 
    (ParameterName.InstanceProfileName, (None, None)), 
    (ParameterName.EnvironmentType, (None, None)),
])


def load_eb_config_file(location, parameter_pool, quiet = False):
    log.info(u'Reading EB configuration from file: "{0}"'.format(location))
    try:
        try: 
            parser = SectionedConfigParser()
            parser.read(location)

            log.debug(u'Found a sectioned config file.')
            extra_config = dict()
            extra_config[EbConfigFile.RootSectionName] = dict()
            branches = dict()
            
            for section in parser.sections():
                log.debug(u'Reading section "{0}" from config file.'.format(section))                
                # Known sections
                if section in EbConfigFile.KnownSections.keys(): 
                    for key, value in parser.items(section):
                        if ConfigFileParameters.has_key(key):
                            from_file, _ = ConfigFileParameters[key]
                            if from_file:
                                value = from_file(value)
                            parameter_pool.put(Parameter(key, value, ParameterSource.ConfigFile))
                        else:
                            extra_config[EbConfigFile.RootSectionName][key] = value
                            
                elif section == EbConfigFile.BranchSectionName:
                    #branch section
                    branch_mapping = dict()
                    for key, value in parser.items(section):
                        branch_mapping[key] = value
                    parameter_pool.put(Parameter(ParameterName.BranchMapping, 
                                                 branch_mapping, 
                                                 ParameterSource.ConfigFile))
                    
                elif section.startswith(EbConfigFile.BranchSectionPrefix):
                    #branch environment session
                    parsed = section.split(EbConfigFile.SectionNameDelimiter)
                    if len(parsed) != 2 or misc.is_blank_string(parsed[1]):
                        continue    # skip if no environment name
                    branch_name = parsed[1]
                    branches[branch_name] = dict()
                    for key, value in parser.items(section):
                        if ConfigFileParameters.has_key(key):
                            from_file, _ = ConfigFileParameters[key]
                            if from_file:
                                value = from_file(value)
                            branches[branch_name][key] = value
                        else:
                            if not extra_config.has_key(section):
                                extra_config[section] = dict()
                            extra_config[section][key] = value
                
                else:
                    # unknown sections
                    new_section = dict()
                    for key, value in parser.items(section):
                        new_section[key] = value 
                    extra_config[section] = new_section

            parameter_pool.put(Parameter(ParameterName.ConfigFileExtra, 
                                         extra_config, 
                                         ParameterSource.ConfigFile))
            parameter_pool.put(Parameter(ParameterName.Branches, 
                                         branches, 
                                         ParameterSource.ConfigFile))     
            
        except MissingSectionHeaderError as ex:
            # old format: sectionless config file
            log.debug(u'Found a section-less config file.')
            nosect_parser = NoSectionConfigParser()
            nosect_parser.read(location)

            for name, (from_file, _) in ConfigFileParameters.iteritems():
                if nosect_parser.has_option(name):
                    value = nosect_parser.get(name)
                    if from_file is not None:
                        value = from_file(value)
                    parameter_pool.put(Parameter(name, value, ParameterSource.ConfigFile))
        
        # Add original parameter infos
        for name, ori_name in EbConfigFile.BranchResetParameters.iteritems():
            if parameter_pool.has(name):
                parameter_pool.put(Parameter(ori_name, 
                                             parameter_pool.get_value(name, False), 
                                             ParameterSource.ConfigFile))

        log.info(u'Finished reading from EB configuration file.')
   
    except BaseException as ex:
        log.error(u'Failed to parse EB configuration from file, because: "{0}"'.format(ex))
        if not quiet:
            if (isinstance(ex, OSError) or isinstance(ex, IOError)) and\
                ex.errno == FileErrorConstant.FileNotFoundErrorCode:
                raise EBConfigFileNotExistError(ex)
            else:
                msg = ConfigFileErrorMessage.ReadError.format(location)
                prompt.error(msg)
                raise EBConfigFileNotExistError(msg)
        else:    
            pass # if failed, just skip     

        
def save_eb_config_file(location, parameter_pool, quiet = False):
    log.info(u'Writing EB configuration to file: "{0}".'.format(location))
    try:
        parser = SectionedConfigParser()
        parser.add_section(EbConfigFile.RootSectionName)    # Make root section the first

        # add known session
        for section_name, (condition, keys) in EbConfigFile.KnownSections.iteritems():
            if parameter_pool.get_value(condition):
                log.debug(u'Create section "{0}" in config file.'.format(section_name))
                parser.add_section(section_name)
                for key in sorted(keys):
                    if parameter_pool.has(key):
                        
                        #TODO: Generalize this special case
                        if key == ParameterName.ApplicationVersionName\
                            and parameter_pool.get_source(key) == ParameterSource.Default:
                            continue
                        
                        value = parameter_pool.get_value(key, False)
                        if key in ConfigFileParameters:
                            _, to_file = ConfigFileParameters[key]
                            if to_file is not None:
                                value = to_file(value)
                        parser.set(section_name, key, value)

        # add branch mapping sections
        if parameter_pool.has(ParameterName.BranchMapping)\
            and len(parameter_pool.get_value(ParameterName.BranchMapping, False)) > 0:
            log.debug(u'Create section "{0}" in config file.'.format(EbConfigFile.BranchSectionName))
            parser.add_section(EbConfigFile.BranchSectionName)
            branch_map = parameter_pool.get_value(ParameterName.BranchMapping, False)
            for key in sorted(branch_map.keys()):
                parser.set(EbConfigFile.BranchSectionName, key, branch_map[key])

        # add branch environment sections
        if parameter_pool.has(ParameterName.Branches)\
            and len(parameter_pool.get_value(ParameterName.Branches, False)) > 0:
            branches = parameter_pool.get_value(ParameterName.Branches, False)
            for branch_name in sorted(branches.keys()):
                section_name = EbConfigFile.BranchSectionPrefix + branch_name
                log.debug(u'Create section "{0}" in config file.'.format(section_name))
                parser.add_section(section_name)
                branch = branches[branch_name]
                for key in sorted(EbConfigFile.BranchSectionKeys):
                    if key in branch:
                        value = branch[key]
                        if key in ConfigFileParameters:
                            _, to_file = ConfigFileParameters[key]
                            if to_file is not None:
                                value = to_file(value)
                        parser.set(section_name, key, value)
            
        # add else
        if parameter_pool.has(ParameterName.ConfigFileExtra): 
            extra_config = parameter_pool.get_value(ParameterName.ConfigFileExtra, False) 
            for section, pairs in sorted(extra_config.iteritems(), key=operator.itemgetter(0)):
                if not parser.has_section(section):
                    log.debug(u'Create section "{0}" in config file.'.format(section))
                    parser.add_section(section)
                for key, value in pairs.iteritems():
                    parser.set(section, key, value)

        parser.write(location)
        log.info(u'Finished writing EB configuration file.')
        
    except BaseException as ex:
        log.exception(u'Failed to save EB configuration file, because: "{0}"'.format(ex))
        prompt.error(ConfigFileErrorMessage.WriteError.format(location))        
        raise
          
            
#------------------------------
# Option Setting File
#------------------------------

def load_env_option_setting_file(location, option_settings = None, quiet = False):
    log.info(u'Reading environment option settings from file at "{0}".'.format(location))
    
    if option_settings is None:
        option_settings = dict()
    
    try:
        parser = SectionedConfigParser()
        parser.read(location)
        
        for section in parser.sections():
            for option, value in parser.items(section):
                section = misc.to_unicode(section)
                option = misc.to_unicode(option)
                value = misc.to_unicode(value) 
                if section not in option_settings:
                    option_settings[section] = dict()
                option_settings[section][option] = value 
        
        log.debug(u'Option settings read from file include: "{0}".'.\
                  format(misc.collection_to_string(option_settings)))
        
        check_access_permission(location, True)
        return option_settings
    
    except BaseException as ex:
        log.error(u'Failed to load environment option setting file, because: "{0}"'.format(ex))
        if quiet:
            return option_settings
        else:
            prompt.error(OptionSettingFileErrorMessage.ReadError.format(location))        
            raise

            
def save_env_option_setting_file(location, option_settings):
    log.info(u'Try loading option settings file from {0}'.format(location))
#    old_option_settings = load_env_option_setting_file(location, quiet = True)
    
    log.info(u'Writing environment option settings to file at "{0}".'.format(location))
    try:
        parser = SectionedConfigParser()
        
        for namespace, options in sorted(option_settings.iteritems()):
            if len(options) < 1:
                continue
            
            for option, value in sorted(options.iteritems()):
                if not _is_whitelisted_option(namespace, option):
#                    and (namespace not in old_option_settings \
#                         or option not in old_option_settings[namespace]):
                    continue
                
                if not parser.has_section(namespace):
                    parser.add_section(namespace)
                
                # Add option setting to new file
                if value is None:
                    value = u''
                parser.set(namespace, option, value)
        
        parser.write(location)
        log.debug(u'Option settings written to file include: "{0}".'.\
                  format(misc.collection_to_string(option_settings)))
       
        set_access_permission(location, True)
    except BaseException as ex:
        log.error(u'Failed to save environment option setting file, because: "{0}"'.format(ex))
        prompt.error(OptionSettingFileErrorMessage.WriteError.format(location))        
        raise    


def _is_whitelisted_option(namespace, option_name):
    if namespace.startswith(OptionSettingContainerPrefix):
        return True
    elif namespace.startswith(OptionSettingApplicationEnvironment.Namespace):
        if option_name in OptionSettingApplicationEnvironment.IgnoreOptionNames:
            return False
        else:
            return True
    elif namespace not in LocalOptionSettings \
        or option_name not in LocalOptionSettings[namespace]:
        return False

    return True
