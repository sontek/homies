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
import os
import logging
import subprocess

from scli import api_wrapper, config_file, prompt
from scli.constants import AwsCredentialFileDefault, CommandType, DevToolsConfigFile, EbConfigFile, \
                        EbLocalDir, FileErrorConstant, GitIgnoreFile, OSSpecific, \
                        ParameterName, ParameterSource, RdsDefault
from scli.exception import EBSCliException, EBConfigFileNotExistError
from scli.operation.base import OperationBase, OperationResult
from scli.parameter import Parameter
from scli.resources import ConfigFileMessage, ConfigFileErrorMessage, \
                        DevToolsMessage, SaveConfigurationSettingOpMessage, \
                        TerminalMessage, WriteAwsCredentialFileOpMessage
from lib.utility import misc, shell_utils


log = logging.getLogger('cli.op')


def create_eb_local_dir():
    shell_utils.create_directory(os.getcwd() + os.path.sep + EbLocalDir.Path)    


# Format: ParameterName, from_file function, to_file function
CredentialFileParameters = [
    (ParameterName.AwsAccessKeyId, None, None), 
    (ParameterName.AwsSecretAccessKey, None, None), 
    (ParameterName.RdsMasterPassword, None, None), 
]


class ReadAwsCredentialFileOperation(OperationBase):
    '''
    Try to read AWS credential ID and key from AWS_CREDENTIAL_FILE specified by user 
    or stored in local OS environment variables
    '''  
    _input_parameters = set()
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        # Try to retrieve all credential info from specified file        
        file_param = parameter_pool.get(ParameterName.AwsCredentialFile) \
            if parameter_pool.has(ParameterName.AwsCredentialFile) else None
        if file_param is not None:
            self._try_read_credential_file(parameter_pool, 
                                           file_param.value, 
                                           file_param.source)
        else:
            location = config_file.default_aws_credential_file_location()        
            self._try_read_credential_file(parameter_pool, 
                                           location, 
                                           ParameterSource.ConfigFile)
        
        osenv_location =  os.getenv(AwsCredentialFileDefault.OSVariableName)
        if osenv_location is not None:
            self._try_read_credential_file(parameter_pool, 
                                           osenv_location, 
                                           ParameterSource.OsEnvironment)

        ret_result = OperationResult(self, None, None, None)
        return ret_result


    def _try_read_credential_file(self, parameter_pool, location, source):
        func_matrix = []
        # Loop over default settings
        for name, from_file, _ in CredentialFileParameters:
            if not parameter_pool.has(name):
                func_matrix.append((None, name, from_file))
                
        # Loop over branch environment settings
        branches = parameter_pool.get_value(ParameterName.Branches)
        if branches:
            for branch_name, branch_setting in branches.items():
                if not name in branch_setting:
                    func_matrix.append((branch_name, name, from_file))
                        
        config_file.read_aws_credential_file(location, parameter_pool, 
                                             func_matrix, source, True)


class UpdateAwsCredentialFileOperation(OperationBase):
    '''
    Generate AWS credential file if it is not retrieved from local OS environment variables
    '''  
    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         }
    
    _output_parameters = set()
        
    def execute(self, parameter_pool):
        
        func_matrix = []
        # Loop over default settings
        for name, _, to_file in CredentialFileParameters:
            if not parameter_pool.has(name):
                continue
            elif ParameterSource.is_ahead(ParameterSource.Terminal,\
                                        parameter_pool.get_source(name)):
                continue
            else:
                func_matrix.append((None, name, to_file))

        # Loop over current branch settings
        cur_branch = parameter_pool.get_value(ParameterName.CurrentBranch)
        if cur_branch and ParameterSource.is_ahead(parameter_pool.get_source(ParameterName.Branches),
                                                   ParameterSource.ConfigFile):
            branch_setting = parameter_pool.get_value(ParameterName.Branches, False)[cur_branch]
            for name, _, to_file in CredentialFileParameters:
                if not name in branch_setting:
                    continue
                else:
                    func_matrix.append((cur_branch, name, to_file))
        
        if len(func_matrix) < 1:
            log.info('Skipped updating credential file as credentials are not changed.')
            return 

        location = config_file.default_aws_credential_file_location()        
        # Create directory if needed
        try:
            shell_utils.create_directory(config_file.default_aws_credential_file_path())
            config_file.write_aws_credential_file(location, parameter_pool, func_matrix)
        except BaseException as ex:
            log.error('Encountered error when creating AWS Credential file at "{0}", because {1}.'.\
                      format(location, ex))
            return
        
        else:
            log.info(WriteAwsCredentialFileOpMessage.Succeed.format(location))
            prompt.result(WriteAwsCredentialFileOpMessage.Succeed.format(location))
            
            parameter_pool.put(Parameter(ParameterName.AwsCredentialFile,
                                         location,
                                         ParameterSource.OperationOutput),
                               True)
            
            ret_result = OperationResult(self, None, None, None)
            return ret_result


class LoadEbConfigFileOperation(OperationBase):
    _input_parameters = set()
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        location = EbLocalDir.Path + os.path.sep + EbConfigFile.Name
        
        try:
            config_file.load_eb_config_file(location, parameter_pool, False)
            if config_file.check_access_permission(location) is False:
                message = ConfigFileErrorMessage.PermissionError.format(EbConfigFile.Name)
                log.info(message)
                prompt.error(message)
           
            #Post processing
            if not parameter_pool.has(ParameterName.RdsSnippetUrl)\
                and parameter_pool.has(ParameterName.Region):
                region = parameter_pool.get_value(ParameterName.Region, False)
                parameter_pool.put(Parameter(ParameterName.RdsSnippetUrl,
                                             RdsDefault.get_snippet_url(region),
                                             ParameterSource.ConfigFile))
            
        except EBConfigFileNotExistError:
            log.error('Configuration file "{0}" not exist.'.format(EbConfigFile.Name))
            prompt.error(ConfigFileMessage.CannotFind.format\
                        (EbConfigFile.Name, CommandType.INIT.lower()))
            raise EBSCliException()
            
        except BaseException as ex:
            log.error('Encountered error when load configuration file "{0}", becuase "{1}".'.\
                      format(EbConfigFile.Name, ex))
            prompt.error(ConfigFileMessage.CorrectionSuggestion.
                         format(location,CommandType.INIT.lower()))
            raise

        ret_result = OperationResult(self, None, None, None)
        return ret_result

    
class TryLoadEbConfigFileOperation(OperationBase):
    _input_parameters = set()
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        location = EbLocalDir.Path + os.path.sep + EbConfigFile.Name
        
        config_file.load_eb_config_file(location, parameter_pool, True)

        ret_result = OperationResult(self, None, None, None)
        return ret_result



class SaveEbConfigFileOperation(OperationBase):
    _input_parameters = set()
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        create_eb_local_dir()
        
        location = EbLocalDir.Path + os.path.sep + EbConfigFile.Name
        
        config_file.save_eb_config_file(location, parameter_pool, False)
        config_file.set_access_permission(location)
        
        ret_result = OperationResult(self, None, None, None)
        return ret_result
    

class UpdateDevToolsConfigOperation(OperationBase):

    _input_parameters = set()
    _output_parameters = set()
    
    def execute(self, pool):
        
        # Test if git local repo exists
        if not os.path.isdir(os.path.join(os.getcwd(), DevToolsConfigFile.Path)):
            prompt.error(DevToolsMessage.GitRepoNotExist.format(''))
            return

        error = False
        current_path = os.getcwd()        
        try:
            log.info('Running Dev Tools initialization script.')
            if misc.is_os_windows():
                path = shell_utils.climb_dir_tree(shell_utils.ori_path(), OSSpecific.WindowsClimbUpDepth)
                #TODO: replace current workaround for WindowsModuleScript
                current_path = os.getcwd()
                script_path = os.path.join(path, OSSpecific.WindowsModuleScriptPath)
                log.debug('Changing path to {0}.'.format(script_path))
                os.chdir(script_path)

                log.info('Running script "{0}".'.format(OSSpecific.WindowsModuleScriptName))
                shell_utils.call([OSSpecific.WindowsModuleScriptName])
                
                log.debug('Changing path to {0}.'.format(current_path))
                os.chdir(current_path)
                
                log.info('Running script "{0}".'.format(OSSpecific.WindowsRepoScript))
                fullpath = os.path.join(path, OSSpecific.WindowsRepoScript)
                prompt.error(shell_utils.call([fullpath]))
            else:
                path = shell_utils.climb_dir_tree(shell_utils.ori_path(), OSSpecific.LinuxClimbUpDepth)
                log.info('Running script "{0}" at {1}.'.format(OSSpecific.LinuxRepoScript,
                                                                path))
                fullpath = os.path.join(path, OSSpecific.LinuxRepoScript)
                prompt.error(shell_utils.call([fullpath]))
            
            location = DevToolsConfigFile.Path + os.path.sep + DevToolsConfigFile.Name        
            config_file.set_access_permission(location, True)

        except subprocess.CalledProcessError as ex:
            # Git returned with an error code
            log.error('Dev Tools initialization script report an error, because "{0}".'.format(ex))
            error = True
            prompt.error(DevToolsMessage.InitError.format(ex.message))
        
        except (OSError, IOError) as ex:
            log.error('Failed to call Dev Tools initialization script, because "{0}".'.format(ex))
            # Cannot find or run script
            error = True
            if ex.errno == FileErrorConstant.FileNotFoundErrorCode:
                if fullpath:
                    prompt.error(DevToolsMessage.FileMissingError.format(fullpath))
                else:
                    prompt.error(ex)

        finally:
            if error:            
                prompt.error(DevToolsMessage.ExecutionError.format(DevToolsConfigFile.InitHelpUrl))
        
        ret_result = OperationResult(self, None, None, None)
        return ret_result



class CheckGitIgnoreFileOperation(OperationBase):

    _input_parameters = set()
    
    _output_parameters = set()

    def execute(self, pool):
        location = GitIgnoreFile.Path + os.path.sep + GitIgnoreFile.Name        
        config_file.add_ignore_file(location)
        
        ret_result = OperationResult(self, None, None, None)
        return ret_result        



class SaveConfigurationSettingOperation(OperationBase):
    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                         ParameterName.ApplicationName,
                         ParameterName.EnvironmentName,
                         ParameterName.OptionSettingFile,
                        }
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        create_eb_local_dir()

        app_name = parameter_pool.get_value(ParameterName.ApplicationName)
        env_name = parameter_pool.get_value(ParameterName.EnvironmentName)
        location = parameter_pool.get_value(ParameterName.OptionSettingFile)            

        prompt.action(SaveConfigurationSettingOpMessage.Start.format(env_name))
        
        try:
            option_settings = api_wrapper.retrieve_configuration_settings(parameter_pool,
                                                                          app_name,
                                                                          env_name)
            config_file.save_env_option_setting_file(location, option_settings)
        except Exception as ex:
            # Never fail. Just log event if any exception
            log.info('Cannot dump environment option settings before termination, because '.\
                     format(misc.to_unicode(ex)))
            option_settings = None
        else:
            log.info(SaveConfigurationSettingOpMessage.Succeed.format(location))
            prompt.info(SaveConfigurationSettingOpMessage.Succeed.format(location))
                   
        ret_result = OperationResult(self,
                                     None, 
                                     None,
                                     option_settings)
        return ret_result



class RotateOptionsettingFileOperation(OperationBase):
    _input_parameters = {
                         ParameterName.OptionSettingFile,
                        }
    
    _output_parameters = set()
    
    def execute(self, parameter_pool):
        ori_stack = parameter_pool.get_value(ParameterName.OriginalSolutionStack)\
            if parameter_pool.has(ParameterName.OriginalSolutionStack) else None
        stack = parameter_pool.get_value(ParameterName.SolutionStack)\
            if parameter_pool.has(ParameterName.SolutionStack) else None
        
        if ori_stack is None or ori_stack == stack:
            log.info('Solution stack is not changed. Keeping current optionsettings file.')
        else:
            log.info('Rotate optionsettings file because solution stack is changed.')
            
            # Rotate default option setting file            
            location = parameter_pool.get_value(ParameterName.OptionSettingFile)            
            config_file.rotate_file(location)

            # Rotate branch environment option setting file if any
            branches = parameter_pool.get_value(ParameterName.Branches)\
                if parameter_pool.has(ParameterName.Branches) else None
            if branches:
                for _, branch in branches.items():
                    location = branch[ParameterName.OptionSettingFile]            
                    config_file.rotate_file(location)
        
        ret_result = OperationResult(self, None, None, None)
        return ret_result
       
        
class TryGetCurrentBranchOperation(OperationBase):
    _input_parameters = set()
    
    _output_parameters = {
                          ParameterName.CurrentBranch,
                          }
    
    def execute(self, parameter_pool):
        current_branch, branch_count = shell_utils.get_working_branch(True)
        parameter_pool.put(Parameter(ParameterName.CurrentBranch,
                                     current_branch,
                                     ParameterSource.ConfigFile))
        if current_branch:
            log.info('Current working branch is "{0}".'.format(current_branch))
            branches = parameter_pool.get_value(ParameterName.Branches)
            if branches and current_branch in list(branches.keys()):
                log.info('Found registered environment for branch "{0}".'.format(current_branch))
                for key, value in branches[current_branch].items():
                    parameter_pool.put(Parameter(key, value, ParameterSource.ConfigFile))
            else:
                if branch_count == 1:
                    log.info('Only one unregistered branch found. Using default settings.')
                    pass
                else:
                    msg = TerminalMessage.FallBackToDefaultBranch.format(current_branch)
                    log.error(msg)
                    prompt.error(msg)
        else:
            # local repository does not have more than one branch, using default
            pass

