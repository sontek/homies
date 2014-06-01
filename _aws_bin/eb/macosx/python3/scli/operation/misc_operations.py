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

from scli import prompt  
from scli.constants import ParameterName
from scli.constants import ParameterSource
from scli.operation.base import OperationBase
from scli.operation.base import OperationResult
from scli.parameter import Parameter
from scli.resources import ListSolutionStackOpMessage

log = _logging.getLogger('cli.op')


class ListSolutionStackOperation(OperationBase):

    _input_parameters = {
                         ParameterName.AwsAccessKeyId, 
                         ParameterName.AwsSecretAccessKey,
                         ParameterName.ServiceEndpoint, 
                        }
    
    _output_parameters = {
                          ParameterName.AvailableSolutionStacks,
                         }

    
    def execute(self, parameter_pool):
        eb_client = self._get_eb_client(parameter_pool)
        
        prompt.action(ListSolutionStackOpMessage.Start)

        response = eb_client.list_available_solutionstacks()
        
        name_set = set()
        name_string = '\n\t' 
        for stack in response.result:
            name_set.add(stack.solutionstack_name)
            name_string +=  stack.solutionstack_name + '\n\t'
        
        log.info('Available solution stacks: \n{0}'.format(name_string))
        prompt.result(ListSolutionStackOpMessage.Result.format(name_string))

        parameter_pool.put(Parameter(ParameterName.AvailableSolutionStacks,
                                     name_set,
                                     ParameterSource.OperationOutput))        
        
        ret_result = OperationResult(self,
                                      response.request_id, 
                                      ListSolutionStackOpMessage.Result.format(name_string),
                                      response.result)
            
        return ret_result
    
    