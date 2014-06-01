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

from lib.elasticbeanstalk.servicecall import ElasticBeanstalkClient
from lib.elasticbeanstalk.exception import AlreadyExistException
from lib.iam.servicecall import IamClient
from lib.rds.servicecall import RdsClient
from lib.utility import misc
from scli.constants import ParameterName, RdsEndpoint, ServiceRegionId

log = logging.getLogger('cli')


def log_response(api_name, result):
    if log.isEnabledFor(logging.DEBUG):
        log.debug(u'{0} response: {1}'.\
                  format(api_name, misc.collection_to_string(result)))


#---------------------------------------------
# Elastic Beanstalk API wrappers
#---------------------------------------------

def create_eb_client(parameter_pool):
    endpoint = parameter_pool.get_value(ParameterName.ServiceEndpoint, False)
    eb_client = ElasticBeanstalkClient(parameter_pool.get_value(ParameterName.AwsAccessKeyId, False), 
                                       parameter_pool.get_value(ParameterName.AwsSecretAccessKey, False),
                                       endpoint,
                                       ServiceRegionId[parameter_pool.get_value(ParameterName.Region, False)])

    log.info(u'Create EB client to talk to {0}.'.format(endpoint))

    return eb_client


def retrieve_solution_stacks(parameter_pool, eb_client = None):
    if eb_client is None:
        eb_client = create_eb_client(parameter_pool)

    log.info(u'Send request for ListAvailableSolutionStack call.')
    response = eb_client.list_available_solutionstacks()
    log.info(u'Received response for ListAvailableSolutionStack call.')
    log_response(u'ListAvailableSolutionStack', response.result)            
    
    stack_list = list()
    for stack in response.result:
        stack_list.append(stack.solutionstack_name)
    
    return stack_list


def retrieve_environment_resources(parameter_pool, env_name = None, eb_client = None):
    if eb_client is None:
        eb_client = create_eb_client(parameter_pool)

    log.info(u'Send request for DescribeEnvironmentResources call.')
    response = eb_client.describe_environment_resources(env_name)
    log.info(u'Received response for DescribeEnvironments call.')
    log_response(u'DescribeEnvironmentResources', response.result)            

    return response.result


def retrieve_configuration_settings(parameter_pool, app_name, 
                                    env_name = None, template = None, 
                                    options = None, eb_client = None):
    if eb_client is None:
        eb_client = create_eb_client(parameter_pool)
        
    log.info(u'Send request for DescribeConfigurationSettings call.')
    response = eb_client.describe_configuration_settings(app_name,
                                                         environment_name = env_name,
                                                         template = template,
                                                         options = options)
    log.info(u'Received response for DescribeConfigurationSettings call.')
    log_response(u'DescribeConfigurationSettings', response.result)            
    
    option_settings = dict()
    for option in response.result.option_settings:
        if option.namespace not in option_settings:
            option_settings[option.namespace] = dict()
        option_settings[option.namespace][option.option_name] = option.value
        
    return option_settings


def retrieve_configuration_options(eb_client,
                                   app_name = None, solution_stack = None,
                                   env_name = None, template = None, 
                                   options = None, template_specification = None):

    log.info(u'Send request for DescribeConfigurationOptions call.')
    response = eb_client.describe_configuration_options(application_name = app_name, 
                                                        solution_stack = solution_stack, 
                                                        environment_name = env_name, 
                                                        template = template, 
                                                        options = options,
                                                        template_specification = template_specification)
    log.info(u'Received response for DescribeConfigurationOptions call.')
    log_response(u'DescribeConfigurationOptions', response.result)            
    
    return response.result


def create_application(parameter_pool, app_name, eb_client = None):
    if eb_client is None:
        eb_client = create_eb_client(parameter_pool)
    try:
        log.info(u'Send request for CreateApplication call.')
        eb_client.create_application(app_name)
        log.info(u'Received response for CreateApplication call.')
    except AlreadyExistException:
        log.info(u'Application "{0}" already exists.'.format(app_name))
    else:
        log.info(u'Created Application "{0}".'.format(app_name))
    

def get_all_versions(parameter_pool, app_name, eb_client = None):
    if eb_client is None:
        eb_client = create_eb_client(parameter_pool)
    log.info(u'Send request for DescribeApplications call.')
    apps = eb_client.describe_applications(app_name).result
    log.info(u'Received response for DescribeApplications call.')
    if len(apps) > 0:
        return set(apps[0].versions)
    else:
        return set()


def get_all_running_version(parameter_pool, app_name, eb_client = None):
    if eb_client is None:
        eb_client = create_eb_client(parameter_pool)
    log.info(u'Send request for DescribeEnvironments call.')
    environments = eb_client.describe_environments(app_name, include_deleted = 'false').result
    log.info(u'Received response for DescribeEnvironments call.')
    
    versions = set()
    for environment in environments:
        versions.add(environment.version_label)
    return versions

#---------------------------------------------
# RDS API wrappers
#---------------------------------------------

def create_rds_client(parameter_pool):
    if parameter_pool.has(ParameterName.RdsEndpoint):
        rds_endpoint = parameter_pool.get_value(ParameterName.RdsEndpoint, False)
    else:
        rds_endpoint = RdsEndpoint[parameter_pool.get_value(ParameterName.Region, False)]

    rds_client = RdsClient(parameter_pool.get_value(ParameterName.AwsAccessKeyId, False), 
                           parameter_pool.get_value(ParameterName.AwsSecretAccessKey, False),
                           rds_endpoint,
                           ServiceRegionId[parameter_pool.get_value(ParameterName.Region, False)])

    log.info(u'Create RDS client to talk to {0}.'.format(rds_client))

    return rds_client

def retrive_rds_instance(parameter_pool, instance_id):
    rds_client = create_rds_client(parameter_pool)
    
    response = rds_client.describe_db_instances(instance_id)
    log.info(u'Received response for DescribeDBInstances call.')
    log_response(u'DescribeDBInstances', response.result)            
    
    if not isinstance(response.result, list):
        return list(response.result)
    else:
        return response.result[0]


def retrive_rds_snapshots(parameter_pool):
    rds_client = create_rds_client(parameter_pool)
    
    response = rds_client.describe_db_snapshots()
    log.info(u'Received response for DescribeDBSnapshots call.')
    log_response(u'DescribeDBSnapshots', response.result)            
    
    if not isinstance(response.result, list):
        return list(response.result)
    else:
        return response.result


def retrive_rds_engine_versions(parameter_pool):
    rds_client = create_rds_client(parameter_pool)
    
    response = rds_client.describe_db_engine_versions()
    log.info(u'Received response for DescribeDBEngineVersions call.')
    log_response(u'DescribeDBEngineVersions', response.result)            
    
    if not isinstance(response.result, list):
        return list(response.result)
    else:
        return response.result


def retrive_rds_engines(parameter_pool):
    engine_versions = retrive_rds_engine_versions(parameter_pool)
    
    db_engines = deque()
    for engine_version in engine_versions:
        if engine_version.engine not in db_engines:
            db_engines.append(engine_version.engine)

    return list(db_engines)


def retrive_rds_default_engine_versions(parameter_pool):
    
    db_engines = retrive_rds_engines(parameter_pool)
    rds_client = create_rds_client(parameter_pool)

    db_default_versions = deque()
    for engine in db_engines:
        response = rds_client.describe_db_engine_versions(engine = engine,
                                                          default_only = u'true')
        log.info(u'Received response for DescribeDBEngineVersions call.')
        log_response(u'DescribeDBEngineVersions', response.result)            
        db_default_versions.append(response.result[0])
    
    return list(db_default_versions)


#---------------------------------------------
# IAM API wrappers
#---------------------------------------------

def create_iam_client(parameter_pool):
    iam_client = IamClient(parameter_pool.get_value(ParameterName.AwsAccessKeyId, False), 
                           parameter_pool.get_value(ParameterName.AwsSecretAccessKey, False))

    log.info(u'Create IAM client.')

    return iam_client