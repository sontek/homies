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
from collections import deque

from lib.rds import rds_utils
from lib.rds.model import DBSnapshot
from lib.utility import misc
from scli import api_wrapper
from scli import prompt
from scli.constants import ParameterSource as PSource
from scli.constants import ParameterName as PName
from scli.constants import RdsDefault
from scli.constants import TerminalConstant
from scli.exception import EBSCliException
from scli.terminal.base import TerminalBase
from scli.resources import RdsTerminalMessage
from scli.resources import TerminalMessage
from scli.resources import TerminalPromptAskingMessage as AskMsg
from scli.parameter import Parameter
from scli.parameter import ParameterValidator

log = logging.getLogger('cli')

class RdsTerminal(TerminalBase):

    @classmethod
    def ask_rds_creation(cls, parameter_pool):
        
        stack = parameter_pool.get_value(PName.SolutionStack, False)
        if not rds_utils.is_rds_snippet_compatible(parameter_pool, stack):
            parameter_pool.put(Parameter(PName.RdsEnabled, 
                                         False, 
                                         PSource.Terminal), 
                               True)        
            
            return  # solution stack not compatible with snippet
        
        if parameter_pool.has(PName.RdsEnabled):
            ori_rds_creation = parameter_pool.get_value(PName.RdsEnabled, False)
            msg = AskMsg[PName.RdsEnabled].format(TerminalMessage.CurrentValue.\
                                                  format(misc.bool_to_yesno(ori_rds_creation)))
            raw_answer = cls.line_input(msg, True)
            if raw_answer is None:
                rds_creation = ori_rds_creation
            else:
                rds_creation = misc.string_to_boolean(raw_answer)
        else:
            raw_answer = cls.line_input(AskMsg[PName.RdsEnabled].format(u''))
            rds_creation = misc.string_to_boolean(raw_answer)

        parameter_pool.put(Parameter(PName.RdsEnabled, rds_creation, PSource.Terminal), 
                           True)        
        
        if rds_creation:
            cls.ask_snapshot_name(parameter_pool)
            cls.ask_master_password(parameter_pool)
            cls.ask_delete_to_snapshot(parameter_pool)


    @classmethod
    def ask_snapshot_name(cls, parameter_pool):
        original_value = parameter_pool.get_value(PName.RdsSourceSnapshotName)
        append_message = TerminalMessage.CurrentValue.format(RdsTerminalMessage.NoSnapshot)\
             if original_value is None else TerminalMessage.CurrentValue.format(original_value)        
        print(RdsTerminalMessage.RdsSnapshotNameSelect.format(append_message))
        
        snapshots = api_wrapper.retrive_rds_snapshots(parameter_pool)
        sorted_snapshots = cls._sort_snapshots_by_create_time(snapshots)
        
        snapshot_list = [RdsTerminalMessage.NoSnapshot,];
        for i in range(0, min(len(sorted_snapshots), TerminalConstant.RdsSnapshotListNumber)):
            snapshot_list.append(sorted_snapshots[i].db_snapshot_identifier)
        snapshot_list.append(RdsTerminalMessage.OtherSnapshot)
        
        snapshot_index = cls.single_choice(choice_list = snapshot_list, can_return_none = True)
        if snapshot_index == 0:
            # Create RDS instance from scratch
            value = None
        elif snapshot_index == len(snapshot_list) - 1:
            # Name not in list
            value = cls.ask_value(parameter_pool, PName.RdsSourceSnapshotName)
        else:
            value = snapshot_list[snapshot_index] if snapshot_index is not None else original_value
            
        snapshot = Parameter(PName.RdsSourceSnapshotName, value, PSource.Terminal)
        parameter_pool.put(snapshot, True)


    @classmethod
    def ask_master_password(cls, parameter_pool):
        for _ in range(0, RdsDefault.PasswordMismatchThreshold):
            password = cls.ask_value(parameter_pool, PName.RdsMasterPassword, no_echo = True)
            if password is None:
                return 
            else:
                if not ParameterValidator.validate_RDS_password(password,
                                                                RdsDefault.PasswordMinSize,
                                                                RdsDefault.PasswordMaxSize):
                    prompt.error(RdsTerminalMessage.PasswordWrongFormat)
                    continue
                
                confirm = cls.line_input(RdsTerminalMessage.PasswordConfirmation, 
                                         can_return_none = False, no_echo = True)
                if confirm != password:
                    prompt.error(RdsTerminalMessage.PasswordNotMatch)
                else:
                    parameter_pool.put(Parameter(PName.RdsMasterPassword, 
                                                 password, 
                                                 PSource.Terminal), 
                                       True)
                    return                    
        else:
            prompt.error(RdsTerminalMessage.PasswordMatchFailure)
            raise EBSCliException()
        
    @classmethod
    def ask_delete_to_snapshot(cls, parameter_pool):
        if parameter_pool.has(PName.RdsDeletionPolicy):
            original_value = parameter_pool.get_value(PName.RdsDeletionPolicy, False)
            display_value = RdsDefault.del_policy_to_bool(original_value)
            display_value = misc.bool_to_yesno(display_value)
        else:
            display_value = None
                    
        value = cls.ask_parameter(parameter_pool, PName.RdsDeletionPolicy, display_value)

        if value is None:
            return
        elif not isinstance(value, bool):
            value = RdsDefault.bool_to_del_policy(misc.string_to_boolean(value))
            
        parameter_pool.put(Parameter(PName.RdsDeletionPolicy, 
                                     value, 
                                     PSource.Terminal), 
                           True)        


    @classmethod
    def _sort_snapshots_by_create_time(cls, snapshots):
        sorted_snapshots = deque()
        for item in snapshots:
            if misc.string_equal_ignore_case(item.snapshot_type, DBSnapshot.TypeAutomated):
                continue    #skip if snapshot is created automated
            
            if len(sorted_snapshots) < 1:
                sorted_snapshots.append(item)
            elif item._snapshot_create_time_raw <  sorted_snapshots[-1]._snapshot_create_time_raw:
                sorted_snapshots.append(item)
            else:
                shift = 0
                while item._snapshot_create_time_raw <  sorted_snapshots[0]._snapshot_create_time_raw:
                    sorted_snapshots.rotate(-1)
                    shift = shift + 1
                sorted_snapshots.appendleft(item)
                sorted_snapshots.rotate(shift)
        
        return sorted_snapshots
    
