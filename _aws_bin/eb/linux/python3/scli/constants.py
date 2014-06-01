#!/usr/bin/env python
# -*- coding: utf-8 -*-
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

from lib.utility.basetype import ValuedEnum
from lib.utility.basetype import OrderedEnum

EbCliVersion = 'v2.6.2'

class Key(object):
    Default = 'default'
    Options = 'options'

#----------------------------------------------
# Parameters
#----------------------------------------------
# Standard name of parameters used in Elastic Beanstalk Command Line Interface
ParameterName = ValuedEnum({
    'Command' : 0,
    'SubCommand' : 1,
    
    'AwsAccessKeyId' : 11,
    'AwsSecretAccessKey' : 12,
    'AwsCredentialFile' : 13,
    
    'Region' : 21,
    'OriginalRegion' : 22,
    'ServiceEndpoint' : 31,
    'DevToolsEndpoint' : 41,
    
    'ApplicationName': 101,
    'OriginalApplicationName': 102,
    'ApplicationVersionName':111,
    'EnvironmentName':121,
    'EnvironmentId':122,
    
    'EnvironmentTier':150,
    
    'SolutionStack' : 201,
    'OriginalSolutionStack' : 202,

    'EnvironmentType' : 211,

    'Branches': 301,
    'CurrentBranch': 302,
    'BranchMapping': 303,
    'DefaultEnvironmentName': 351,
    
    'OptionSettingFile' : 501,
    'ConfigFileExtra' : 511,
    
    'RdsEnabled': 601,
    'RdsEndpoint': 602,
    'RdsSnippetUrl': 603,
    'RdsSourceSnapshotName': 606,
    'RdsEngine': 611,
    'RdsEngineVersion': 612,
    'RdsInstanceClass': 613,
    'RdsMultiAZ': 614,
    'RdsLicenseModel': 615,
    'RdsAllocatedStorage': 616,
    'RdsInstanceName': 621,
    'RdsMasterUsername': 622,
    'RdsMasterPassword': 623,
    'RdsDbName' : 631,    
    'RdsDeletionPolicy': 651,

    'InstanceProfileName': 701,
    
    'ServiceConnectionTimeout' : 1001,
    'ServiceRetryThreshold' : 1011,
    'Force' : 1021,
    
    'Verbose' : 1051,
    
    'WaitForFinishTimeout': 1101,
    'WaitForUpdateTimeout': 1102,
    'PollDelay' : 1201,
    
    'CreateEnvironmentRequestID' : 2001,
    'TerminateEnvironmentRequestID' : 2002,
    'UpdateEnvironmentRequestID' : 2003,
    'RequestEnvInfoRequestID' : 2004,
    
    'AvailableSolutionStacks': 2101,
})


# Source of parameter value
ParameterSource = ValuedEnum({ 
    'CliArgument' : 0,
    'Terminal' : 1,
    'ConfigFile' : 2,
    'OsEnvironment' : 3,
    'OperationOutput' : 4,
    'Default' : 10,
})

#----------------------------------------------
# Command
#----------------------------------------------
CommandType = OrderedEnum([
                          'INIT',
                          'BRANCH',
                          'START', 
                          'STATUS', 
                          'UPDATE',
                          'STOP',
                          'DELETE',
                          'LOGS',
                          'EVENTS',
                          'PUSH',
                          ])

SubCommandType = OrderedEnum([
                          # LOGS command
                          'TAIL',
                          'OPEN'
                          ])

CommandCombination = {
    CommandType.LOGS : {
        Key.Default : SubCommandType.TAIL,
        Key.Options : [
            SubCommandType.TAIL,
        ]
    },
}


#----------------------------------------------
# Terminal
#----------------------------------------------
class TerminalConstant(object):
    Y = 'Y'
    Yes = 'Yes'
    N = 'N'
    No = 'No'
    TRUE = 'True'
    FALSE = 'False'

    RdsSnapshotListNumber = 5
    IamProfileListNumber = 6    

#----------------------------------------------
# Services
#----------------------------------------------
ServiceRegion = OrderedEnum([
    'UsEast1',
    'UsWest1',
    'UsWest2',
    'EuWest1',
    'ApNortheast1',
    'ApSoutheast1',
    'ApSoutheast2',
    'SaEast1',
])

AvailableServiceRegion = [
   ServiceRegion.UsEast1,
   ServiceRegion.UsWest2,
   ServiceRegion.UsWest1,
   ServiceRegion.EuWest1,
   ServiceRegion.ApSoutheast1,
   ServiceRegion.ApNortheast1,
   ServiceRegion.ApSoutheast2,
   ServiceRegion.SaEast1,
]


ServiceRegionName = {
    ServiceRegion.ApNortheast1 : 'Asia Pacific (Tokyo)',
    ServiceRegion.ApSoutheast1 : 'Asia Pacific (Singapore)',
    ServiceRegion.ApSoutheast2 : 'Asia Pacific (Sydney)',
    ServiceRegion.EuWest1: 'EU West (Ireland)',
    ServiceRegion.SaEast1: 'South America (Sao Paulo)',
    ServiceRegion.UsEast1 : 'US East (Virginia)',
    ServiceRegion.UsWest1 : 'US West (North California)',
    ServiceRegion.UsWest2 : 'US West (Oregon)',
}

ServiceRegionId = {
    ServiceRegion.ApNortheast1 : 'ap-northeast-1',
    ServiceRegion.ApSoutheast1 : 'ap-southeast-1',
    ServiceRegion.ApSoutheast2 : 'ap-southeast-2',
    ServiceRegion.EuWest1: 'eu-west-1',    
    ServiceRegion.SaEast1: 'sa-east-1',
    ServiceRegion.UsEast1 : 'us-east-1',
    ServiceRegion.UsWest1 : 'us-west-1',
    ServiceRegion.UsWest2 : 'us-west-2',
}

ServiceEndpoint = {
    ServiceRegion.ApNortheast1 : 'https://elasticbeanstalk.ap-northeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast1 : 'https://elasticbeanstalk.ap-southeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast2 : 'https://elasticbeanstalk.ap-southeast-2.amazonaws.com',
    ServiceRegion.EuWest1: 'https://elasticbeanstalk.eu-west-1.amazonaws.com',
    ServiceRegion.SaEast1: 'https://elasticbeanstalk.sa-east-1.amazonaws.com',
    ServiceRegion.UsEast1 : 'https://elasticbeanstalk.us-east-1.amazonaws.com',
    ServiceRegion.UsWest1 : 'https://elasticbeanstalk.us-west-1.amazonaws.com',
    ServiceRegion.UsWest2 : 'https://elasticbeanstalk.us-west-2.amazonaws.com',
}

SnippetBucket = {
    ServiceRegion.ApNortheast1 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-ap-northeast-1/eb_snippets',
    ServiceRegion.ApSoutheast1 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-ap-southeast-1/eb_snippets',
    ServiceRegion.ApSoutheast2 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-ap-southeast-2/eb_snippets',
    ServiceRegion.EuWest1 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-eu-west-1/eb_snippets',
    ServiceRegion.SaEast1 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-sa-east-1/eb_snippets',
    ServiceRegion.UsEast1 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-us-east-1/eb_snippets',
    ServiceRegion.UsWest1 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-us-west-1/eb_snippets',
    ServiceRegion.UsWest2 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-us-west-2/eb_snippets',
}

PolicyBucket = {
    ServiceRegion.ApNortheast1 : 'https://elasticbeanstalk-env-resources-ap-northeast-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.ApSoutheast1 : 'https://elasticbeanstalk-env-resources-ap-southeast-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.ApSoutheast2 : 'https://elasticbeanstalk-env-resources-ap-southeast-2.s3.amazonaws.com/eb_policies',
    ServiceRegion.EuWest1 : 'https://elasticbeanstalk-env-resources-eu-west-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.SaEast1 : 'https://elasticbeanstalk-env-resources-sa-east-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.UsEast1 : 'https://s3.amazonaws.com/elasticbeanstalk-env-resources-us-east-1/eb_policies',
    ServiceRegion.UsWest1 : 'https://elasticbeanstalk-env-resources-us-west-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.UsWest2 : 'https://elasticbeanstalk-env-resources-us-west-2.s3.amazonaws.com/eb_policies',
}

DevToolsEndpoint = {
    ServiceRegion.ApNortheast1 : 'git.elasticbeanstalk.ap-northeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast1 : 'git.elasticbeanstalk.ap-southeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast2 : 'git.elasticbeanstalk.ap-southeast-2.amazonaws.com',
    ServiceRegion.EuWest1: 'git.elasticbeanstalk.eu-west-1.amazonaws.com',
    ServiceRegion.SaEast1: 'git.elasticbeanstalk.sa-east-1.amazonaws.com',
    ServiceRegion.UsEast1 : 'git.elasticbeanstalk.us-east-1.amazonaws.com',
    ServiceRegion.UsWest1 : 'git.elasticbeanstalk.us-west-1.amazonaws.com',
    ServiceRegion.UsWest2 : 'git.elasticbeanstalk.us-west-2.amazonaws.com',
}


class EbDefault(object):

    TailLog = 'tail'
    
    RoleAssumePolicyUrlMask = '{0}/role-assume-policy'
    DefaultRoleName = 'aws-elasticbeanstalk-ec2-role'
    DefaultInstanceProfileName = 'aws-elasticbeanstalk-ec2-role'
    

class DevToolsDefault(object):
    NameDelimiter = '-'
    VersionNameRe = '^git-{0}-\d+$'
    VersionNameMask = 'git-{0}-{1}'
    
    AwsPush = ['git', 'aws.push']
    AwsCreateAppVersion = ['git', 'aws.createapplicationversion']
    
    
#----------------------------------------------
# Solution stacks and sample app
#----------------------------------------------

class DefaultAppSource(object):
    Namespace = 'aws:cloudformation:template:parameter'
    OptionName = 'AppSource' 

class LegacyContainer(object):
    Regex = '\(legacy\) *$'

class TomcatAppContainer(object):
    Name = 'Tomcat'
    Regex = '^(32|64)bit Amazon Linux running Tomcat (6|7)(( (L|l)egacy)|( \((L|l)egacy\)))?$'
    
class PhpAppContainer(object):
    Name = 'PHP'
    Regex = '^(32|64)bit Amazon Linux running PHP 5.3(( (L|l)egacy)|( \((L|l)egacy\)))?$'

class IisAppContainer(object):
    Name = 'IIS'
    Regex = '^64bit Windows Server 2008 R2 running IIS 7.5(( (L|l)egacy)|( \((L|l)egacy\)))?$'

class PythonAppContainer(object):
    Name = 'Python'
    Regex = '^(32|64)bit Amazon Linux running Python.*'

class RubyAppContainer(object):
    Name = 'Ruby'
    Regex = '^(32|64)bit Amazon Linux running Ruby .*'


#----------------------------------------------
# RDS
#----------------------------------------------

RdsEndpoint = {
    ServiceRegion.ApNortheast1 : 'https://rds.ap-northeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast1 : 'https://rds.ap-southeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast2 : 'https://rds.ap-southeast-2.amazonaws.com',
    ServiceRegion.EuWest1: 'https://rds.eu-west-1.amazonaws.com',
    ServiceRegion.SaEast1: 'https://rds.sa-east-1.amazonaws.com',
    ServiceRegion.UsEast1 : 'https://rds.amazonaws.com',
    ServiceRegion.UsWest1 : 'https://rds.us-west-1.amazonaws.com',
    ServiceRegion.UsWest2 : 'https://rds.us-west-2.amazonaws.com',
}


class RdsDefault(object):
    PasswordMismatchThreshold = 3
    
    SnippetUrlMask = '{0}/rds/rds.json'
    SnippetName = 'RdsExtensionEB'
    SnippetAddOrder = 10000
    SnippetRemoveOrder = -1

    DbIdLengthLimit = {
                       'mysql' : 63,
                       'sqlserver-ex' : 15,
                       'sqlserver-se' : 15,
                       'sqlserver-web' : 15,
                       }
    
    DeletionPolicySnapshot = 'Snapshot'
    DeletionPolicyDelete = 'Delete'
    ResourceType = 'AWS::RDS::DBInstance'
    HostnameType = 'Endpoint'
    PortType = 'Port'
    
    @classmethod
    def get_snippet_url(cls, region):
        return cls.SnippetUrlMask.format(SnippetBucket[region])    

    @classmethod
    def bool_to_del_policy(cls, switch):
        if switch:
            return cls.DeletionPolicySnapshot
        else:
            return cls.DeletionPolicyDelete

    @classmethod
    def del_policy_to_bool(cls, policy):
        if policy == cls.DeletionPolicySnapshot:
            return True
        else:
            return False

    Namespace = 'aws:rds:dbinstance'

    OptionNames = {
        ParameterName.RdsEngine : 'DBEngine',
        ParameterName.RdsEngineVersion : 'DBEngineVersion',
        ParameterName.RdsInstanceClass : 'DBInstanceClass',
        ParameterName.RdsAllocatedStorage : 'DBAllocatedStorage',
        ParameterName.RdsMultiAZ : 'MultiAZDatabase',
        ParameterName.RdsLicenseModel : 'DBLicenseModel',
        
        ParameterName.RdsSourceSnapshotName : 'DBSnapshotIdentifier',
        ParameterName.RdsDbName : 'DBName',
        ParameterName.RdsMasterUsername : 'DBUser',
        ParameterName.RdsMasterPassword : 'DBPassword',
        ParameterName.RdsDeletionPolicy : 'DBDeletionPolicy',
    }
         
    OptionMinSet = {
        ParameterName.RdsEngine,
        ParameterName.RdsSourceSnapshotName,                     
        ParameterName.RdsMasterPassword,
        ParameterName.RdsDeletionPolicy,
    }
    
    PasswordMinSize = 8
    PasswordMaxSize = 41

    
#----------------------------------------------
# IAM
#----------------------------------------------
IamEndpoint = 'https://iam.amazonaws.com'
IamRegion = 'us-east-1'
    
        
#----------------------------------------------
# Application and environment default
#----------------------------------------------

class EnvironmentStatus(object):
    Launching = 'Launching'
    Ready = 'Ready'
    Updating = 'Updating'
    Terminating = 'Terminating'
    Terminated = 'Terminated'
    
class EnvironmentHealth(object):
    Green = 'Green'
    Yellow = 'Yellow'
    Red = 'Red'
    Grey = 'Grey'
    
class EventSeverity(object):
    Trace = 'TRACE'
    Debug = 'Debug'
    Info = 'INFO'
    Warn = 'WARN'
    Error = 'ERROR'
    Fatal = 'FATAL'      

class ValidationSeverity(object):
    SeverityError = 'error'
    SeverityWarning = 'warning'

class ServiceDefault(object):
    """ Defines CLI related constant values. """
    DEFAULT_VERSION_NAME = 'Sample Application'
    
    SERVICE_CALL_MAX_RETRY = 5
    
    CONNECTION_TIMEOUT_IN_SEC = 30   
    WAIT_TIMEOUT_IN_SEC = 600
    UPDATE_TIMEOUT_IN_SEC = 300
    RDS_ADDITION_TIMEOUT_IN_SEC = 300
    
    POLL_DELAY_IN_SEC = 5
    CREATE_ENV_POLL_DELAY = 3
    TERMINATE_ENV_POLL_DELAY = 0
    UPDATE_ENV_POLL_DELAY = 0

    CHAR_CODEC = 'utf-8'
    ENABLED = 'Enabled'
    USER_AGENT = 'eb ' + EbCliVersion

    STATUS_EVENT_LEVEL = EventSeverity.Warn
    STATUS_EVENT_MAX_NUM = 3
    
    EVENT_DEFAULT_NUM = 10

    
    class Environment(object):
        REGEX_NAME_FILTER = '[^A-Za-z0-9\-]+'
        NAME_POSTFIX = '-env' 
        MAX_NAME_LEN = 23
        BRANCH_NAME_SEPERATOR = '-'

OutputLevel = OrderedEnum([
                    'Info',
                    'ResultOnly',
                    'Quiet',
                    'Silence',
                    ])
    
#----------------------------------------------
# Configuration file and log file
#----------------------------------------------

class FileDefaultParameter(object):
    RotationMaxRetry = 1000


class OSSpecific(object):

    '''Windows specific constants'''
    WindowsName = 'Windows'
    WindowsClimbUpDepth = 2
    WindowsModuleScriptPath = 'AWSDevTools\\Windows'
    WindowsModuleScriptName = 'AWSDevTools-OneTimeSetup.bat'
    WindowsRepoScript = 'AWSDevTools\\Windows\\AWSDevTools-RepositorySetup.bat'
    
    
    '''Nix specific constants'''
    LinuxName = 'Linux'     
    LinuxClimbUpDepth = 3
    LinuxRepoScript = 'AWSDevTools/Linux/AWSDevTools-RepositorySetup.sh'


class AwsCredentialFileDefault(object):
    FilePath = '.elasticbeanstalk'
    FileName = 'aws_credential_file'
    OSVariableName = 'AWS_CREDENTIAL_FILE'
    KeyName = {
        ParameterName.AwsAccessKeyId : 'AWSAccessKeyId',
        ParameterName.AwsSecretAccessKey : 'AWSSecretKey',
        ParameterName.RdsMasterPassword : 'RDSMasterPassword', 
    }


class EbLocalDir(object):
    Path = '.elasticbeanstalk'
    Name = Path + '/'
    NameRe = Path + '/'
    LogDir = 'log'

    
class EbLogFile(object):
    Name = 'eb-cli.log'
    NameRe = '.*eb-cli\.log.*'


class EbConfigFile(object):
    Name = 'config'
    NameRe = '.*\config.*'

    SectionNameDelimiter = ':'

    RootSectionName = 'global'
    RootSectionKeys = {
        ParameterName.AwsCredentialFile, 
        ParameterName.ApplicationName, 
        ParameterName.ApplicationVersionName, 
        ParameterName.DevToolsEndpoint,
        ParameterName.EnvironmentName, 
        ParameterName.OptionSettingFile,
        ParameterName.EnvironmentTier, 
        ParameterName.SolutionStack, 
        ParameterName.Region, 
        ParameterName.ServiceEndpoint, 
        ParameterName.RdsEnabled,
        ParameterName.RdsSourceSnapshotName,
        ParameterName.RdsDeletionPolicy,
        ParameterName.InstanceProfileName,
        ParameterName.EnvironmentType,
    }

    BranchResetParameters = {
        ParameterName.ApplicationName : ParameterName.OriginalApplicationName, 
        ParameterName.Region : ParameterName.OriginalRegion, 
        ParameterName.SolutionStack : ParameterName.OriginalSolutionStack, 
    }
    
    BranchSectionName = 'branches'
    BranchSectionPrefix = 'branch' + SectionNameDelimiter
    BranchSectionKeys = {
        ParameterName.ApplicationVersionName, 
        ParameterName.EnvironmentName,
        ParameterName.EnvironmentTier,
        ParameterName.OptionSettingFile, 
        ParameterName.RdsEnabled,
        ParameterName.RdsSourceSnapshotName,
        ParameterName.RdsDeletionPolicy,
        ParameterName.InstanceProfileName,
        ParameterName.EnvironmentType,
    }
    BranchSectionHiddenKeys = {
        ParameterName.RdsMasterPassword,         
    }
    
    # Map from section name to (section existence condition, list of member keys) 
    KnownSections = {
        RootSectionName : (ParameterName.ApplicationName, RootSectionKeys),
    }
    

class OptionSettingFile(object):
    Name = 'optionsettings'


class CABundle(object):
    Path = '.'
    Name = 'ca-bundle.crt'
    

class FileErrorConstant(object):
    FileNotFoundErrorCode = 2
    FileNotFoundErrorMsg = 'No such file or directory'    


#----------------------------------------------
# Git and DevTools file
#----------------------------------------------

class GitDefault(object):
    HeadRe = '\* .+'
    GetBranch = ['git', 'branch']
    GetHeadHash = ['git', 'rev-parse', 'HEAD']
        
class GitIgnoreFile(object):
    Name = '.gitignore'
    Path = '.'
    Files = {
             EbLocalDir,
             }

class DevToolsConfigFile(object):
    Name = 'config'
    Path = '.git'
    InitHelpUrl = 'http://docs.amazonwebservices.com/elasticbeanstalk'\
        '/latest/dg/command-reference-get-started.html'
    
#----------------------------------------------
# OptionSettingList
#----------------------------------------------

LocalOptionSettings = {
    'aws:autoscaling:launchconfiguration' : {
        'EC2KeyName',
        'InstanceType', 
    },
    'aws:elasticbeanstalk:sns:topics' : {
        'Notification Endpoint', 
        'Notification Protocol',
    },
    'aws:elasticbeanstalk:monitoring' : {
        'Automatically Terminate Unhealthy Instances',
    },
    'aws:elasticbeanstalk:hostmanager' : {
        'LogPublicationControl',
    },
    'aws:elasticbeanstalk:application' : {
        'Application Healthcheck URL',
    },
    'aws:autoscaling:asg' : {
        'MaxSize',
        'MinSize',
        'Custom Availability Zones',
    },
    'aws:autoscaling:updatepolicy:rollingupdate' : {
        'RollingUpdateEnabled',
    },
    'aws:rds:dbinstance' : {
        'DBDeletionPolicy',
        'DBEngine',
        'DBInstanceClass',
        'DBSnapshotIdentifier',
        'DBUser',
    },
    'aws:ec2:vpc' : {
        'VPCId',
        'Subnets',
        'ELBSubnets',
        'DBSubnets',
        'ELBScheme',
        'AutoScalingGroupScheme',
    },
    'aws:elasticbeanstalk:sqsd' : {
        'WorkerQueueURL',
        'HttpPath',
        'MimeType',
        'MaxRetries',
        'HttpConnections',
        'ConnectTimeout',
        'InactivityTimeout',
        'VisibilityTimeout',
        'RetentionPeriod',
    },
}

OptionSettingContainerPrefix = 'aws:elasticbeanstalk:container'
OptionSettingTemplatePrefix = 'aws:cloudformation:template'

class OptionSettingApplicationEnvironment(object): 
    Namespace = 'aws:elasticbeanstalk:application:environment'
    IgnoreOptionNames = {
        'AWS_ACCESS_KEY_ID',
        'AWS_SECRET_KEY',
    }
    
class OptionSettingVPC(object): 
    Namespace = 'aws:ec2:vpc'
    MagicOptionName = 'Subnets'
    DBSubnets = 'DBSubnets'
    TrimOption = {
        'aws:autoscaling:asg' : {
            'Custom Availability Zones',
        },                  
    }

class OptionSettingIAMProfile(object): 
    Namespace = 'aws:autoscaling:launchconfiguration'
    OptionName = 'IamInstanceProfile'


class OptionSettingEnvironmentType(object): 
    Namespace = 'aws:elasticbeanstalk:environment'
    OptionName = 'EnvironmentType'
    
