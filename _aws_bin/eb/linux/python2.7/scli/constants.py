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
    u'Command' : 0,
    u'SubCommand' : 1,
    
    u'AwsAccessKeyId' : 11,
    u'AwsSecretAccessKey' : 12,
    u'AwsCredentialFile' : 13,
    
    u'Region' : 21,
    u'OriginalRegion' : 22,
    u'ServiceEndpoint' : 31,
    u'DevToolsEndpoint' : 41,
    
    u'ApplicationName': 101,
    u'OriginalApplicationName': 102,
    u'ApplicationVersionName':111,
    u'EnvironmentName':121,
    u'EnvironmentId':122,
    
    u'EnvironmentTier':150,
    
    u'SolutionStack' : 201,
    u'OriginalSolutionStack' : 202,

    u'EnvironmentType' : 211,

    u'Branches': 301,
    u'CurrentBranch': 302,
    u'BranchMapping': 303,
    u'DefaultEnvironmentName': 351,
    
    u'OptionSettingFile' : 501,
    u'ConfigFileExtra' : 511,
    
    u'RdsEnabled': 601,
    u'RdsEndpoint': 602,
    u'RdsSnippetUrl': 603,
    u'RdsSourceSnapshotName': 606,
    u'RdsEngine': 611,
    u'RdsEngineVersion': 612,
    u'RdsInstanceClass': 613,
    u'RdsMultiAZ': 614,
    u'RdsLicenseModel': 615,
    u'RdsAllocatedStorage': 616,
    u'RdsInstanceName': 621,
    u'RdsMasterUsername': 622,
    u'RdsMasterPassword': 623,
    u'RdsDbName' : 631,    
    u'RdsDeletionPolicy': 651,

    u'InstanceProfileName': 701,
    
    u'ServiceConnectionTimeout' : 1001,
    u'ServiceRetryThreshold' : 1011,
    u'Force' : 1021,
    
    u'Verbose' : 1051,
    
    u'WaitForFinishTimeout': 1101,
    u'WaitForUpdateTimeout': 1102,
    u'PollDelay' : 1201,
    
    u'CreateEnvironmentRequestID' : 2001,
    u'TerminateEnvironmentRequestID' : 2002,
    u'UpdateEnvironmentRequestID' : 2003,
    u'RequestEnvInfoRequestID' : 2004,
    
    u'AvailableSolutionStacks': 2101,
})


# Source of parameter value
ParameterSource = ValuedEnum({ 
    u'CliArgument' : 0,
    u'Terminal' : 1,
    u'ConfigFile' : 2,
    u'OsEnvironment' : 3,
    u'OperationOutput' : 4,
    u'Default' : 10,
})

#----------------------------------------------
# Command
#----------------------------------------------
CommandType = OrderedEnum([
                          u'INIT',
                          u'BRANCH',
                          u'START', 
                          u'STATUS', 
                          u'UPDATE',
                          u'STOP',
                          u'DELETE',
                          u'LOGS',
                          u'EVENTS',
                          u'PUSH',
                          ])

SubCommandType = OrderedEnum([
                          # LOGS command
                          u'TAIL',
                          u'OPEN'
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
    Y = u'Y'
    Yes = u'Yes'
    N = u'N'
    No = u'No'
    TRUE = u'True'
    FALSE = u'False'

    RdsSnapshotListNumber = 5
    IamProfileListNumber = 6    

#----------------------------------------------
# Services
#----------------------------------------------
ServiceRegion = OrderedEnum([
    u'UsEast1',
    u'UsWest1',
    u'UsWest2',
    u'EuWest1',
    u'ApNortheast1',
    u'ApSoutheast1',
    u'ApSoutheast2',
    u'SaEast1',
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
    ServiceRegion.ApNortheast1 : u'Asia Pacific (Tokyo)',
    ServiceRegion.ApSoutheast1 : u'Asia Pacific (Singapore)',
    ServiceRegion.ApSoutheast2 : u'Asia Pacific (Sydney)',
    ServiceRegion.EuWest1: u'EU West (Ireland)',
    ServiceRegion.SaEast1: u'South America (Sao Paulo)',
    ServiceRegion.UsEast1 : u'US East (Virginia)',
    ServiceRegion.UsWest1 : u'US West (North California)',
    ServiceRegion.UsWest2 : u'US West (Oregon)',
}

ServiceRegionId = {
    ServiceRegion.ApNortheast1 : u'ap-northeast-1',
    ServiceRegion.ApSoutheast1 : u'ap-southeast-1',
    ServiceRegion.ApSoutheast2 : u'ap-southeast-2',
    ServiceRegion.EuWest1: u'eu-west-1',    
    ServiceRegion.SaEast1: u'sa-east-1',
    ServiceRegion.UsEast1 : u'us-east-1',
    ServiceRegion.UsWest1 : u'us-west-1',
    ServiceRegion.UsWest2 : u'us-west-2',
}

ServiceEndpoint = {
    ServiceRegion.ApNortheast1 : u'https://elasticbeanstalk.ap-northeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast1 : u'https://elasticbeanstalk.ap-southeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast2 : u'https://elasticbeanstalk.ap-southeast-2.amazonaws.com',
    ServiceRegion.EuWest1: u'https://elasticbeanstalk.eu-west-1.amazonaws.com',
    ServiceRegion.SaEast1: u'https://elasticbeanstalk.sa-east-1.amazonaws.com',
    ServiceRegion.UsEast1 : u'https://elasticbeanstalk.us-east-1.amazonaws.com',
    ServiceRegion.UsWest1 : u'https://elasticbeanstalk.us-west-1.amazonaws.com',
    ServiceRegion.UsWest2 : u'https://elasticbeanstalk.us-west-2.amazonaws.com',
}

SnippetBucket = {
    ServiceRegion.ApNortheast1 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-ap-northeast-1/eb_snippets',
    ServiceRegion.ApSoutheast1 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-ap-southeast-1/eb_snippets',
    ServiceRegion.ApSoutheast2 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-ap-southeast-2/eb_snippets',
    ServiceRegion.EuWest1 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-eu-west-1/eb_snippets',
    ServiceRegion.SaEast1 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-sa-east-1/eb_snippets',
    ServiceRegion.UsEast1 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-us-east-1/eb_snippets',
    ServiceRegion.UsWest1 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-us-west-1/eb_snippets',
    ServiceRegion.UsWest2 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-us-west-2/eb_snippets',
}

PolicyBucket = {
    ServiceRegion.ApNortheast1 : u'https://elasticbeanstalk-env-resources-ap-northeast-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.ApSoutheast1 : u'https://elasticbeanstalk-env-resources-ap-southeast-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.ApSoutheast2 : u'https://elasticbeanstalk-env-resources-ap-southeast-2.s3.amazonaws.com/eb_policies',
    ServiceRegion.EuWest1 : u'https://elasticbeanstalk-env-resources-eu-west-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.SaEast1 : u'https://elasticbeanstalk-env-resources-sa-east-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.UsEast1 : u'https://s3.amazonaws.com/elasticbeanstalk-env-resources-us-east-1/eb_policies',
    ServiceRegion.UsWest1 : u'https://elasticbeanstalk-env-resources-us-west-1.s3.amazonaws.com/eb_policies',
    ServiceRegion.UsWest2 : u'https://elasticbeanstalk-env-resources-us-west-2.s3.amazonaws.com/eb_policies',
}

DevToolsEndpoint = {
    ServiceRegion.ApNortheast1 : u'git.elasticbeanstalk.ap-northeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast1 : u'git.elasticbeanstalk.ap-southeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast2 : u'git.elasticbeanstalk.ap-southeast-2.amazonaws.com',
    ServiceRegion.EuWest1: u'git.elasticbeanstalk.eu-west-1.amazonaws.com',
    ServiceRegion.SaEast1: u'git.elasticbeanstalk.sa-east-1.amazonaws.com',
    ServiceRegion.UsEast1 : u'git.elasticbeanstalk.us-east-1.amazonaws.com',
    ServiceRegion.UsWest1 : u'git.elasticbeanstalk.us-west-1.amazonaws.com',
    ServiceRegion.UsWest2 : u'git.elasticbeanstalk.us-west-2.amazonaws.com',
}


class EbDefault(object):

    TailLog = u'tail'
    
    RoleAssumePolicyUrlMask = u'{0}/role-assume-policy'
    DefaultRoleName = u'aws-elasticbeanstalk-ec2-role'
    DefaultInstanceProfileName = u'aws-elasticbeanstalk-ec2-role'
    

class DevToolsDefault(object):
    NameDelimiter = u'-'
    VersionNameRe = u'^git-{0}-\d+$'
    VersionNameMask = u'git-{0}-{1}'
    
    AwsPush = [u'git', u'aws.push']
    AwsCreateAppVersion = [u'git', u'aws.createapplicationversion']
    
    
#----------------------------------------------
# Solution stacks and sample app
#----------------------------------------------

class DefaultAppSource(object):
    Namespace = u'aws:cloudformation:template:parameter'
    OptionName = u'AppSource' 

class LegacyContainer(object):
    Regex = u'\(legacy\) *$'

class TomcatAppContainer(object):
    Name = u'Tomcat'
    Regex = u'^(32|64)bit Amazon Linux running Tomcat (6|7)(( (L|l)egacy)|( \((L|l)egacy\)))?$'
    
class PhpAppContainer(object):
    Name = u'PHP'
    Regex = u'^(32|64)bit Amazon Linux running PHP 5.3(( (L|l)egacy)|( \((L|l)egacy\)))?$'

class IisAppContainer(object):
    Name = u'IIS'
    Regex = u'^64bit Windows Server 2008 R2 running IIS 7.5(( (L|l)egacy)|( \((L|l)egacy\)))?$'

class PythonAppContainer(object):
    Name = u'Python'
    Regex = u'^(32|64)bit Amazon Linux running Python.*'

class RubyAppContainer(object):
    Name = u'Ruby'
    Regex = u'^(32|64)bit Amazon Linux running Ruby .*'


#----------------------------------------------
# RDS
#----------------------------------------------

RdsEndpoint = {
    ServiceRegion.ApNortheast1 : u'https://rds.ap-northeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast1 : u'https://rds.ap-southeast-1.amazonaws.com',
    ServiceRegion.ApSoutheast2 : u'https://rds.ap-southeast-2.amazonaws.com',
    ServiceRegion.EuWest1: u'https://rds.eu-west-1.amazonaws.com',
    ServiceRegion.SaEast1: u'https://rds.sa-east-1.amazonaws.com',
    ServiceRegion.UsEast1 : u'https://rds.amazonaws.com',
    ServiceRegion.UsWest1 : u'https://rds.us-west-1.amazonaws.com',
    ServiceRegion.UsWest2 : u'https://rds.us-west-2.amazonaws.com',
}


class RdsDefault(object):
    PasswordMismatchThreshold = 3
    
    SnippetUrlMask = u'{0}/rds/rds.json'
    SnippetName = u'RdsExtensionEB'
    SnippetAddOrder = 10000
    SnippetRemoveOrder = -1

    DbIdLengthLimit = {
                       u'mysql' : 63,
                       u'sqlserver-ex' : 15,
                       u'sqlserver-se' : 15,
                       u'sqlserver-web' : 15,
                       }
    
    DeletionPolicySnapshot = u'Snapshot'
    DeletionPolicyDelete = u'Delete'
    ResourceType = u'AWS::RDS::DBInstance'
    HostnameType = u'Endpoint'
    PortType = u'Port'
    
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

    Namespace = u'aws:rds:dbinstance'

    OptionNames = {
        ParameterName.RdsEngine : u'DBEngine',
        ParameterName.RdsEngineVersion : u'DBEngineVersion',
        ParameterName.RdsInstanceClass : u'DBInstanceClass',
        ParameterName.RdsAllocatedStorage : u'DBAllocatedStorage',
        ParameterName.RdsMultiAZ : u'MultiAZDatabase',
        ParameterName.RdsLicenseModel : u'DBLicenseModel',
        
        ParameterName.RdsSourceSnapshotName : u'DBSnapshotIdentifier',
        ParameterName.RdsDbName : u'DBName',
        ParameterName.RdsMasterUsername : u'DBUser',
        ParameterName.RdsMasterPassword : u'DBPassword',
        ParameterName.RdsDeletionPolicy : u'DBDeletionPolicy',
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
IamEndpoint = u'https://iam.amazonaws.com'
IamRegion = u'us-east-1'
    
        
#----------------------------------------------
# Application and environment default
#----------------------------------------------

class EnvironmentStatus(object):
    Launching = u'Launching'
    Ready = u'Ready'
    Updating = u'Updating'
    Terminating = u'Terminating'
    Terminated = u'Terminated'
    
class EnvironmentHealth(object):
    Green = u'Green'
    Yellow = u'Yellow'
    Red = u'Red'
    Grey = u'Grey'
    
class EventSeverity(object):
    Trace = u'TRACE'
    Debug = u'Debug'
    Info = u'INFO'
    Warn = u'WARN'
    Error = u'ERROR'
    Fatal = u'FATAL'      

class ValidationSeverity(object):
    SeverityError = u'error'
    SeverityWarning = u'warning'

class ServiceDefault(object):
    """ Defines CLI related constant values. """
    DEFAULT_VERSION_NAME = u'Sample Application'
    
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
    ENABLED = u'Enabled'
    USER_AGENT = 'eb ' + EbCliVersion

    STATUS_EVENT_LEVEL = EventSeverity.Warn
    STATUS_EVENT_MAX_NUM = 3
    
    EVENT_DEFAULT_NUM = 10

    
    class Environment(object):
        REGEX_NAME_FILTER = u'[^A-Za-z0-9\-]+'
        NAME_POSTFIX = u'-env' 
        MAX_NAME_LEN = 23
        BRANCH_NAME_SEPERATOR = u'-'

OutputLevel = OrderedEnum([
                    u'Info',
                    u'ResultOnly',
                    u'Quiet',
                    u'Silence',
                    ])
    
#----------------------------------------------
# Configuration file and log file
#----------------------------------------------

class FileDefaultParameter(object):
    RotationMaxRetry = 1000


class OSSpecific(object):

    '''Windows specific constants'''
    WindowsName = u'Windows'
    WindowsClimbUpDepth = 2
    WindowsModuleScriptPath = u'AWSDevTools\\Windows'
    WindowsModuleScriptName = u'AWSDevTools-OneTimeSetup.bat'
    WindowsRepoScript = u'AWSDevTools\\Windows\\AWSDevTools-RepositorySetup.bat'
    
    
    '''Nix specific constants'''
    LinuxName = u'Linux'     
    LinuxClimbUpDepth = 3
    LinuxRepoScript = u'AWSDevTools/Linux/AWSDevTools-RepositorySetup.sh'


class AwsCredentialFileDefault(object):
    FilePath = u'.elasticbeanstalk'
    FileName = u'aws_credential_file'
    OSVariableName = u'AWS_CREDENTIAL_FILE'
    KeyName = {
        ParameterName.AwsAccessKeyId : u'AWSAccessKeyId',
        ParameterName.AwsSecretAccessKey : u'AWSSecretKey',
        ParameterName.RdsMasterPassword : u'RDSMasterPassword', 
    }


class EbLocalDir(object):
    Path = u'.elasticbeanstalk'
    Name = Path + u'/'
    NameRe = Path + u'/'
    LogDir = u'log'

    
class EbLogFile(object):
    Name = u'eb-cli.log'
    NameRe = u'.*eb-cli\.log.*'


class EbConfigFile(object):
    Name = u'config'
    NameRe = u'.*\config.*'

    SectionNameDelimiter = u':'

    RootSectionName = u'global'
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
    
    BranchSectionName = u'branches'
    BranchSectionPrefix = u'branch' + SectionNameDelimiter
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
    Name = u'optionsettings'


class CABundle(object):
    Path = u'.'
    Name = u'ca-bundle.crt'
    

class FileErrorConstant(object):
    FileNotFoundErrorCode = 2
    FileNotFoundErrorMsg = u'No such file or directory'    


#----------------------------------------------
# Git and DevTools file
#----------------------------------------------

class GitDefault(object):
    HeadRe = u'\* .+'
    GetBranch = [u'git', u'branch']
    GetHeadHash = [u'git', u'rev-parse', u'HEAD']
        
class GitIgnoreFile(object):
    Name = u'.gitignore'
    Path = u'.'
    Files = {
             EbLocalDir,
             }

class DevToolsConfigFile(object):
    Name = u'config'
    Path = u'.git'
    InitHelpUrl = u'http://docs.amazonwebservices.com/elasticbeanstalk'\
        '/latest/dg/command-reference-get-started.html'
    
#----------------------------------------------
# OptionSettingList
#----------------------------------------------

LocalOptionSettings = {
    u'aws:autoscaling:launchconfiguration' : {
        u'EC2KeyName',
        u'InstanceType', 
    },
    u'aws:elasticbeanstalk:sns:topics' : {
        u'Notification Endpoint', 
        u'Notification Protocol',
    },
    u'aws:elasticbeanstalk:monitoring' : {
        u'Automatically Terminate Unhealthy Instances',
    },
    u'aws:elasticbeanstalk:hostmanager' : {
        u'LogPublicationControl',
    },
    u'aws:elasticbeanstalk:application' : {
        u'Application Healthcheck URL',
    },
    u'aws:autoscaling:asg' : {
        u'MaxSize',
        u'MinSize',
        u'Custom Availability Zones',
    },
    u'aws:autoscaling:updatepolicy:rollingupdate' : {
        u'RollingUpdateEnabled',
    },
    u'aws:rds:dbinstance' : {
        u'DBDeletionPolicy',
        u'DBEngine',
        u'DBInstanceClass',
        u'DBSnapshotIdentifier',
        u'DBUser',
    },
    u'aws:ec2:vpc' : {
        u'VPCId',
        u'Subnets',
        u'ELBSubnets',
        u'DBSubnets',
        u'ELBScheme',
        u'AutoScalingGroupScheme',
    },
    u'aws:elasticbeanstalk:sqsd' : {
        u'WorkerQueueURL',
        u'HttpPath',
        u'MimeType',
        u'MaxRetries',
        u'HttpConnections',
        u'ConnectTimeout',
        u'InactivityTimeout',
        u'VisibilityTimeout',
        u'RetentionPeriod',
    },
}

OptionSettingContainerPrefix = u'aws:elasticbeanstalk:container'
OptionSettingTemplatePrefix = u'aws:cloudformation:template'

class OptionSettingApplicationEnvironment(object): 
    Namespace = u'aws:elasticbeanstalk:application:environment'
    IgnoreOptionNames = {
        u'AWS_ACCESS_KEY_ID',
        u'AWS_SECRET_KEY',
    }
    
class OptionSettingVPC(object): 
    Namespace = u'aws:ec2:vpc'
    MagicOptionName = u'Subnets'
    DBSubnets = u'DBSubnets'
    TrimOption = {
        u'aws:autoscaling:asg' : {
            u'Custom Availability Zones',
        },                  
    }

class OptionSettingIAMProfile(object): 
    Namespace = u'aws:autoscaling:launchconfiguration'
    OptionName = u'IamInstanceProfile'


class OptionSettingEnvironmentType(object): 
    Namespace = u'aws:elasticbeanstalk:environment'
    OptionName = u'EnvironmentType'
    
