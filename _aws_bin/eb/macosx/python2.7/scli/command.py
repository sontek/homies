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
from lib.utility import misc
from scli.constants import CommandCombination, CommandType, Key
from scli.operation_queue import OperationQueue
from scli.operation.application_operations import CreateApplicationOperation, \
    DeleteApplicationOperation 
from scli.operation.environment_operations import CreateEnvironmentOperation, \
    DescribeEnvironmentOperation, EnvRequestLogOperation, GetEnvironmentEventsOperation, \
    TerminateEnvironmentOperation, UpdateEnvOptionSettingOperation, \
    WaitForCreateEnvironmentFinishOperation, EnvRetrieveLogOperation, \
    WaitForTerminateEnvironmentFinishOperation, WaitForUpdateEnvOptionSettingFinishOperation
from scli.operation.file_operations import CheckGitIgnoreFileOperation, \
    ReadAwsCredentialFileOperation, LoadEbConfigFileOperation, TryGetCurrentBranchOperation, \
    TryLoadEbConfigFileOperation, SaveConfigurationSettingOperation, SaveEbConfigFileOperation, \
    RotateOptionsettingFileOperation, UpdateDevToolsConfigOperation, UpdateAwsCredentialFileOperation
from scli.operation.pseudo_operations import AskConfirmationOperation, SleepOperation, \
    SanitizeAppVersionNameOperation, SanitizeBranchOperation, SanitizeRdsPasswordOperation, \
    ValidateParameterOperation
from scli.operation.terminal_operations import AskForMissiongParameterOperation, \
    AskForConfigFileParameterOperation, RegisterBranchOperation
from scli.operation.version_operations import CreateApplicationVersionOperation, \
    PushApplicationVersionOperation, RecordApplicationVersionOperation
from scli.resources import EBSCliAttr



def compile_operation_queue(commandlist):
    command = commandlist[0]
    subcommand = commandlist[1]
    subcommand = _fill_default_subcommand(command, subcommand)
    
    queue = OperationQueue()    
    
    if command == CommandType.INIT:
        queue.add(TryLoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(AskForConfigFileParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(RotateOptionsettingFileOperation(queue))
        queue.add(SanitizeBranchOperation(queue))
        queue.add(UpdateAwsCredentialFileOperation(queue))
        queue.add(SanitizeAppVersionNameOperation(queue))
        queue.add(SaveEbConfigFileOperation(queue))
        queue.add(UpdateDevToolsConfigOperation(queue))
        queue.add(CheckGitIgnoreFileOperation(queue))
    
    elif command == CommandType.START:
        queue.add(CheckGitIgnoreFileOperation(queue))
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(TryGetCurrentBranchOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(CreateApplicationOperation(queue))
        queue.add(CreateApplicationVersionOperation(queue))
        queue.add(CreateEnvironmentOperation(queue))
        queue.add(SleepOperation(queue))
        queue.add(SaveConfigurationSettingOperation(queue))
        queue.add(WaitForCreateEnvironmentFinishOperation(queue))
        
    elif command == CommandType.UPDATE:
        queue.add(CheckGitIgnoreFileOperation(queue))
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(TryGetCurrentBranchOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(AskConfirmationOperation(queue))
        queue.add(UpdateEnvOptionSettingOperation(queue))
        queue.add(WaitForUpdateEnvOptionSettingFinishOperation(queue))
            
    elif command == CommandType.STATUS:
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(TryGetCurrentBranchOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(DescribeEnvironmentOperation(queue))
        
    elif command == CommandType.STOP:
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(TryGetCurrentBranchOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(AskConfirmationOperation(queue))
        queue.add(SaveConfigurationSettingOperation(queue))
        queue.add(RecordApplicationVersionOperation(queue))
        queue.add(TerminateEnvironmentOperation(queue))
        queue.add(SaveEbConfigFileOperation(queue))
        queue.add(WaitForTerminateEnvironmentFinishOperation(queue))
        
    elif command == CommandType.DELETE:
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(AskConfirmationOperation(queue))
        queue.add(DeleteApplicationOperation(queue))
        queue.add(SanitizeRdsPasswordOperation(queue))
        queue.add(SanitizeAppVersionNameOperation(queue))
        queue.add(SaveEbConfigFileOperation(queue))
            
    elif command == CommandType.BRANCH:
        queue.add(CheckGitIgnoreFileOperation(queue))
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(RegisterBranchOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(UpdateAwsCredentialFileOperation(queue))
        queue.add(SaveEbConfigFileOperation(queue))

    elif command == CommandType.LOGS:
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(TryGetCurrentBranchOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(EnvRequestLogOperation(queue))
        queue.add(EnvRetrieveLogOperation(queue))
        
    elif command == CommandType.EVENTS:
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(TryGetCurrentBranchOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(GetEnvironmentEventsOperation(queue))        

    elif command == CommandType.PUSH:
        queue.add(CheckGitIgnoreFileOperation(queue))
        queue.add(LoadEbConfigFileOperation(queue))
        queue.add(ReadAwsCredentialFileOperation(queue))
        queue.add(TryGetCurrentBranchOperation(queue))
        queue.add(AskForMissiongParameterOperation(queue))
        queue.add(ValidateParameterOperation(queue))
        queue.add(PushApplicationVersionOperation(queue))
        queue.add(WaitForUpdateEnvOptionSettingFinishOperation(queue))   
    
    else:
        _error_out(command, subcommand)
        
    return queue
    
def _fill_default_subcommand(command, subcommand):
    if command not in CommandCombination:
        return subcommand
    
    if subcommand is None or len(subcommand) < 1:
        return [CommandCombination[command][Key.Default]]
    
    return subcommand
    
    
def _error_out(command, subcommand):
    raise AttributeError(EBSCliAttr.NotSupportedCommand.format
        (misc.to_unicode(command).lower(), misc.to_unicode(*subcommand).lower()))
