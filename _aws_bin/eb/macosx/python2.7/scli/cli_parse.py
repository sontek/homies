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

import argparse
from contextlib import closing
import logging
from StringIO import StringIO

from lib.utility import misc
from lib.elasticbeanstalk.model import EnvironmentTier 
from scli.resources import CLISwitch, CLISwitchMsg, EBSCliAttr 
from scli.constants import CommandType, ServiceDefault, ServiceRegionId, \
    ParameterName, ParameterSource
from scli.parameter import Parameter
from scli.exception import ArgumentError


log = logging.getLogger('cli')


def _word_join(word_list, separator = u''):
    x =  separator.join(map(misc.to_unicode, word_list))
    return x


def command(string):
    command = misc.to_unicode(string)
    for item in CommandType:
        if item.lower() == command.lower().strip():
            return item
    raise AttributeError(EBSCliAttr.InvalidCommand.format(command))


def subcommand(string):
    subcommand = misc.to_unicode(string)
    return subcommand

def _init_parser(parser):
    
    commands = u', '.join(map(unicode.lower, CommandType))
    parser.add_argument(CLISwitch[ParameterName.Command], 
                        type = command,
                        metavar = u'COMMAND', help = commands)

    parser.add_argument(CLISwitch[ParameterName.SubCommand], 
                        type = subcommand, nargs = '*',
                        default = None,
                        metavar = u'SUBCOMMAND',)
    
    # AWS credential
    parser.add_argument(u'-I', u'--' + CLISwitch[ParameterName.AwsAccessKeyId], 
                        dest = ParameterName.AwsAccessKeyId,
                        metavar = u'ACCESS_KEY_ID',
                        help = CLISwitchMsg[ParameterName.AwsAccessKeyId])
    
    parser.add_argument(u'-S', '--' + CLISwitch[ParameterName.AwsSecretAccessKey], 
                        dest = ParameterName.AwsSecretAccessKey,
                        metavar = u'SECRET_ACCESS_KEY',
                        help = CLISwitchMsg[ParameterName.AwsSecretAccessKey])
    
    parser.add_argument(u'--' + CLISwitch[ParameterName.AwsCredentialFile],
                        dest = ParameterName.AwsCredentialFile,
                        metavar = u'FILE_PATH_NAME',
                        help = CLISwitchMsg[ParameterName.AwsCredentialFile]) 
    
    # Application/environment
    parser.add_argument(u'-t', u'--' + CLISwitch[ParameterName.EnvironmentTier],
                        dest = ParameterName.EnvironmentTier,
                        metavar = u'ENVIRONMENT_TIER',
                        help = CLISwitchMsg[ParameterName.EnvironmentTier])
    
    parser.add_argument(u'-s', u'--' + CLISwitch[ParameterName.SolutionStack],
                        dest = ParameterName.SolutionStack, nargs = '+',
                        metavar = u'',
                        help = CLISwitchMsg[ParameterName.SolutionStack])
    
    parser.add_argument(u'-a', u'--' + CLISwitch[ParameterName.ApplicationName], 
                        dest = ParameterName.ApplicationName,
                        metavar = u'APPLICATION_NAME',
                        help = CLISwitchMsg[ParameterName.ApplicationName]) 
    
    parser.add_argument(u'-l', u'--' + CLISwitch[ParameterName.ApplicationVersionName], 
                        dest = ParameterName.ApplicationVersionName,
                        metavar = u'VERSION_LABEL',
                        help = CLISwitchMsg[ParameterName.ApplicationVersionName]) 
    
    parser.add_argument(u'-e', u'--' + CLISwitch[ParameterName.EnvironmentName], 
                        dest = ParameterName.EnvironmentName,
                        metavar = u'ENVIRONMENT_NAME',
                        help = CLISwitchMsg[ParameterName.EnvironmentName]) 
    
    # Output
    parser.add_argument(u'--' + CLISwitch[ParameterName.Verbose],
                        action = 'store_const', const = ServiceDefault.ENABLED,
                        dest = ParameterName.Verbose,
                        metavar = u'',
                        help = CLISwitchMsg[ParameterName.Verbose])    

    parser.add_argument(u'-f', u'--' + CLISwitch[ParameterName.Force],
                        action = 'store_const', const = ServiceDefault.ENABLED,
                        dest = ParameterName.Force,
                        metavar = u'',
                        help = CLISwitchMsg[ParameterName.Force])    
        
    # Service
    parser.add_argument(u'--' + CLISwitch[ParameterName.WaitForFinishTimeout], type = int,
                        dest = ParameterName.WaitForFinishTimeout, 
                        metavar = u'TIMEOUT_IN_SEC',
                        help = unicode.format(CLISwitchMsg[ParameterName.WaitForFinishTimeout], 
                                              ServiceDefault.WAIT_TIMEOUT_IN_SEC))
    
    parser.add_argument(u'--' + CLISwitch[ParameterName.Region],
                        dest = ParameterName.Region, 
                        metavar = u'REGION',
                        help = CLISwitchMsg[ParameterName.Region])
        

    parser.add_argument(u'--' + CLISwitch[ParameterName.ServiceEndpoint],
                        dest = ParameterName.ServiceEndpoint, 
                        metavar = u'ENDPOINT',
                        help = CLISwitchMsg[ParameterName.ServiceEndpoint])
    
    # SCli Helper switch
    parser.add_argument(u'--version', action=u'version', version=EBSCliAttr.Version)    

# List of non string parameters
NON_STRING_PARAMETERS = [ParameterName.EnvironmentTier]

def parse(parameter_pool, line = None):
    ''' Parse command arguments'''
    parser = ArgumentParser(description = EBSCliAttr.Name, 
                            usage = EBSCliAttr.Usage)
    _init_parser(parser)

    if line is not None:
        args = vars(parser.parse_args(line.split()))
    else:
        args = vars(parser.parse_args())
   
    # Post prcessing
    if args[ParameterName.EnvironmentTier] is not None:
        tier_serialized = args[ParameterName.EnvironmentTier]
        args[ParameterName.EnvironmentTier] = EnvironmentTier.from_serialized_string(tier_serialized)
    
    if args[ParameterName.SolutionStack] is not None:
        solution_stack = _word_join(args[ParameterName.SolutionStack], u' ')
        args[ParameterName.SolutionStack] = solution_stack

    if args[ParameterName.Region] is not None:
        region_id = args[ParameterName.Region]
        region = ServiceRegionId.keys()[ServiceRegionId.values().index(region_id)]
        args[ParameterName.Region] = region


    # Store command line arguments into parameter pool     
    for arg, value in args.iteritems():
        arg = misc.to_unicode(arg, convert_none=False)
        
        # Try to convert string/list-of-string parameters to unicode
        if arg not in NON_STRING_PARAMETERS:
            if isinstance(value, list):
                value = [misc.to_unicode(item) for item in value]
            else:
                value = misc.to_unicode(value, convert_none=False)
        
        if arg == CLISwitch[ParameterName.Command]:
            parameter_pool.put(Parameter(ParameterName.Command, 
                                         value, 
                                         ParameterSource.CliArgument))
        elif arg == CLISwitch[ParameterName.SubCommand]:
            parameter_pool.put(Parameter(ParameterName.SubCommand, 
                                         value, 
                                         ParameterSource.CliArgument))
        elif value is not None:
            parameter_pool.put(Parameter(arg, 
                                         value, 
                                         ParameterSource.CliArgument))
    
    log.info(u'Finished parsing command line arguments')
    if log.isEnabledFor(logging.DEBUG): 
        log.debug(u'Received arguments: {0}'.\
                  format(misc.collection_to_string(parameter_pool.parameter_names)))
 
    return args


class ArgumentParser(argparse.ArgumentParser):
    '''Subclass of argparse.ArgumentParser to override behavior of error()'''
    def error(self, error_message):
        with closing(StringIO()) as usage:
            self.print_usage(usage)
            message = EBSCliAttr.ErrorMsg.format(error_message, usage.getvalue(), self.prog)
        raise ArgumentError(message)
    
    
