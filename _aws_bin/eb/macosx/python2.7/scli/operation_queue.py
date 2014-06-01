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

from scli.operation.base import OperationBase 
from scli.operation.base import OperationResult

log = _logging.getLogger('cli')

class OperationQueue(object):
    
    def __init__(self):
        self._queue = []
        self._index = -1;
    
    def add(self, operation):
        if not isinstance(operation, OperationBase):
            raise AttributeError(u'{0} is not an operation instance'.format\
                                 (operation.__class__.__name__))
        self._queue.append(operation)
        log.debug(u'Add operation "{0}" to queue'.format(operation.__class__.__name__))
        
        # set index to 0 if adding first operation
        if self._index < 0:
            self._index = 0
            
    @property            
    def index(self):
        return self._index
    
    @property
    def size(self):
        return len(self._queue)
    
    @property
    def required_parameters(self):
        if self.size < 1:
            return None
        
        input_params = set()
        output_params = set()
        
        for op in self._queue:
            input_params |= op.input_parameters - output_params 
            output_params |= op.output_parameters
        
        return input_params
    
    def run(self, parameter_pool, result):
        if self._index < 0:
            return # has no operation to run
        
        if self._index > 0:
            pass # we are resuming in the middle of execution
        else:
            for index, op in enumerate(self._queue):
                try:
                    self._index = index
                    log.info(u'Run {0}th operation "{1}" now...'.format\
                             (index + 1, op.__class__.__name__))
                    operation_result = op.execute(parameter_pool)
                    if operation_result is None:
                        operation_result = OperationResult(op, None, None, None)
                    result.append(operation_result)
                    log.info(u'Operation "{0}" completed'.format(op.__class__.__name__))
                except AttributeError:
                    raise
                except Exception:
                    raise
                
    