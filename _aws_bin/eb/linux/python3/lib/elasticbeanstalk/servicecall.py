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
import re as _re 

from lib.utility import misc
from lib.aws.exception import AwsErrorCode, AwsServiceException, MissingParameterException, \
    InsufficientPrivilegesException, InvalidParameterValueException, OptInRequiredException
from lib.aws.webservice import AWSQueryClient, AWSSignature
from lib.elasticbeanstalk.request import Request, Response
from lib.elasticbeanstalk import strings as Strings 
from lib.elasticbeanstalk.model import ApplicationDescription, ApplicationVersionDescription,\
    ConfigurationSettingsDescription, ConfigurationOptionDescription, EnvironmentDescription,\
    EnvironmentInfoDescription, EnvironmentResourceDescription, EventDescription, \
    SolutionStackDescription, ValidationMessage
from lib.elasticbeanstalk.exception import EBErrorCode, AlreadyExistException, \
    ApplicationHasRunningEnvException, OperationInProgressException, \
    SourceBundleDeletionException, S3LocationNotInServiceRegionException
from scli.constants import EbDefault


log = _logging.getLogger('eb')

class ElasticBeanstalkClient(object):
    '''
    Web service client for Elastic Beanstalk
    '''
    _signature_version = AWSSignature.SigV4
    _api_version = '2010-12-01'
    _service_name = 'elasticbeanstalk'

    def __init__(self, accessKey, secretKey, endpoint, region, result_format = 'json'):
        '''
        Constructor
        '''
        self._accessKey = accessKey
        self._secretKey = secretKey
        self._endpoint = endpoint
        self._format = result_format
        self._region = region
        
        self._client = AWSQueryClient(self._accessKey, self._secretKey, 
                                      self._endpoint, self._region, 
                                      self._service_name, self._format,
                                      self._signature_version, self._api_version)

        
    def call(self, request):
        '''Make API call and translate AWSServiceException to more specific exception'''
        try:
            log.debug(request)
            return_msg = self._client.call(request, self._format)
            log.debug('Request ID: {0}'.format(list(return_msg.json().values())[0]\
                                                ['ResponseMetadata']['RequestId'])) 

            #TODO: set more specific charset code  
            return return_msg.json()

        except AwsServiceException as ex:
            log.debug(misc.to_unicode(ex))

            # Translate general Elastic Beanstalk exception
            if misc.string_equal_ignore_case(ex.code, AwsErrorCode.OptInRequired):
                raise OptInRequiredException(ex)

            if misc.string_equal_ignore_case(ex.code, AwsErrorCode.InsufficientPrivileges):
                raise InsufficientPrivilegesException(ex)

            if misc.string_equal_ignore_case(ex.code, AwsErrorCode.InvalidParameterValue):
                raise InvalidParameterValueException(ex)
            
            if misc.string_equal_ignore_case(ex.code, AwsErrorCode.MissingParameter):
                raise MissingParameterException(ex)

            raise
            
 
    #---------------------------------------
    # service calls
    def create_application(self, name, description = None):
        request = Request()
        request.set_operation('CreateApplication')
        request.set_app_name(name)
        if description is not None: 
            request.set_description(description)
        
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code.lower() == AwsErrorCode.InvalidParameterValue.lower()\
                and _re.search(Strings.APP_EXIST_RE, ex.message):
                raise AlreadyExistException(ex)
            raise 
        
        # TODO: take care of too many application exception?
        result = response['CreateApplicationResponse']\
            ['CreateApplicationResult']['Application']
        request_id = response['CreateApplicationResponse']\
            ['ResponseMetadata']['RequestId']
                
        return Response(request_id, ApplicationDescription.from_json(result))


    def delete_application(self, name, terminate_env = 'false'):
        request = Request()
        request.set_operation('DeleteApplication')
        request.set_app_name(name)
        request.set_terminate_env(terminate_env)
        
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code.lower() == AwsErrorCode.InvalidParameterValue.lower()\
                and _re.search(Strings.APP_HAS_RUNNING_ENV, ex.message):
                raise ApplicationHasRunningEnvException(ex)
            if ex.code.lower() == EBErrorCode.OperationInProgress.lower():
                raise OperationInProgressException(ex)
            raise 
                
        request_id = response['DeleteApplicationResponse']\
            ['ResponseMetadata']['RequestId']
                
        return Response(request_id)


    def create_application_version(self, application, version_label, 
                                   s3bucket = None, s3key = None, 
                                   auto_create_app = 'false', description = None):
        if (s3bucket is None and s3key is not None) \
            or (s3bucket is not None and s3key is None):
            raise AttributeError('Must specify both s3 bucket and key')
        
        request = Request()
        request.set_operation('CreateApplicationVersion')
        request.set_app_name(application)
        request.set_version_label(version_label)
        if s3bucket is not None:
            request.set_s3bucket(s3bucket) 
            request.set_s3key(s3key) 
        request.set_auto_create_app(auto_create_app)
        if description is not None: 
            request.set_description(description)
            
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code.lower() == AwsErrorCode.InvalidParameterValue.lower()\
                and _re.search(Strings.VER_EXIST_RE, ex.message):
                raise AlreadyExistException(ex)
            raise    
        # TODO: take care of too many application and/or version exception

        result = response['CreateApplicationVersionResponse']\
            ['CreateApplicationVersionResult']['ApplicationVersion']
        request_id = response['CreateApplicationVersionResponse']\
            ['ResponseMetadata']['RequestId']
                
        return Response(request_id, ApplicationVersionDescription.from_json(result))

        
    def delete_application_version(self, application, 
                                   version_label, delete_bundle = 'false'):
        
        request = Request()
        request.set_operation('DeleteApplicationVersion')
        request.set_app_name(application)
        request.set_version_label(version_label)
        request.set_delete_source_bundle(delete_bundle)
            
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code.lower() == EBErrorCode.SourceBundleDeletion:
                raise SourceBundleDeletionException(ex)
            if ex.code.lower() == EBErrorCode.S3LocationNotInServiceRegion:
                raise S3LocationNotInServiceRegionException(ex)
            if ex.code.lower() == EBErrorCode.OperationInProgress:
                raise OperationInProgressException(ex)
            raise    

        request_id = response['DeleteApplicationVersionResponse']\
            ['ResponseMetadata']['RequestId']
        return Response(request_id)


    def create_environment(self, application, environment, cname_prefix = None, 
                           template = None, solution_stack = None, version_label = None,
                           option_settings = None, option_remove = None, 
                           template_specification = None,
                           description = None, tier = None):
        request = Request()
        request.set_operation('CreateEnvironment')
        request.set_app_name(application)
        request.set_env_name(environment)

        if cname_prefix is not None:
            request.set_cname(cname_prefix)
        if template is not None:
            request.set_template(template)
        if solution_stack is not None:
            request.set_solution_stack(solution_stack)
        if version_label is not None:
            request.set_version_label(version_label)
        if option_settings is not None: 
            request.set_option_settings(option_settings)
        if option_remove is not None: 
            request.set_options_to_remove(option_remove)
        if template_specification is not None: 
            request.set_template_specification(template_specification)
        if description is not None: 
            request.set_description(description)
        if tier is not None:
            request.set_tier(tier)
        
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code.lower() == AwsErrorCode.InvalidParameterValue.lower()\
                and _re.search(Strings.ENV_EXIST_RE, ex.message):
                raise AlreadyExistException(ex)
            raise       

        result = response['CreateEnvironmentResponse']['CreateEnvironmentResult']
        request_id = response['CreateEnvironmentResponse']\
            ['ResponseMetadata']['RequestId']
                
        return Response(request_id, EnvironmentDescription.from_json(result))


    def update_environment(self, env_name = None, env_id = None, 
                           template = None, version_label = None,
                           option_settings = None, option_remove = None,
                           template_specification = None,
                           description = None, tier = None):
        request = Request()
        request.set_operation('UpdateEnvironment')
        if env_name is not None:
            request.set_env_name(env_name)
        if env_id is not None:
            request.set_env_id(env_id)
        if template is not None:
            request.set_template(template)
        if version_label is not None:
            request.set_version_label(version_label)
        if option_settings is not None: 
            request.set_option_settings(option_settings)
        if option_remove is not None: 
            request.set_options_to_remove(option_remove)
        if template_specification is not None: 
            request.set_template_specification(template_specification)            
        if description is not None: 
            request.set_description(description)
        if tier is not None:
            request.set_tier(tier)
        
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code.lower() == AwsErrorCode.InvalidParameterValue.lower():
                raise InvalidParameterValueException(ex)
            raise       

        result = response['UpdateEnvironmentResponse']['UpdateEnvironmentResult']
        request_id = response['UpdateEnvironmentResponse']\
            ['ResponseMetadata']['RequestId']
                
        return Response(request_id, EnvironmentDescription.from_json(result))


    def terminate_environment(self, environment_name, 
                              environment_id = None, delete_resource = 'true'):
        request = Request()
        request.set_operation('TerminateEnvironment')
        if environment_name is not None:        
            request.set_env_name(environment_name)
        if environment_id is not None:        
            request.set_env_id(environment_id)
        request.set_terminate_resources(delete_resource)
            
        try:    
            response = self.call(request)
        except:
            raise        

        result = response['TerminateEnvironmentResponse']\
            ['TerminateEnvironmentResult']
        request_id = response['TerminateEnvironmentResponse']\
        ['ResponseMetadata']['RequestId']
                
        return Response(request_id, EnvironmentDescription.from_json(result))

        
    def describe_applications(self, names):
        request = Request()
        request.set_operation('DescribeApplications')
        request.set_app_names(names)
        
        try:    
            response = self.call(request)
        except:
            raise

        results = response['DescribeApplicationsResponse']\
            ['DescribeApplicationsResult']['Applications']
        request_id = response['DescribeApplicationsResponse']\
            ['ResponseMetadata']['RequestId']

        applications = []
        for result in results:
            applications.append(ApplicationDescription.from_json(result))
        return Response(request_id, applications)


    def describe_configuration_options(self, 
                                       application_name = None, 
                                       environment_name = None, 
                                       template = None,
                                       solution_stack = None,
                                       version_label = None,
                                       options = None, 
                                       option_remove = None,
                                       template_specification = None):
        request = Request()
        request.set_operation('DescribeConfigurationOptions')
        if application_name is not None:
            request.set_app_name(application_name)
        if solution_stack is not None:
            request.set_solution_stack(solution_stack)
        if environment_name is not None:
            request.set_env_name(environment_name)
        if template is not None:
            request.set_template(template)
        if version_label is not None:
            request.set_version_label(version_label)
        if options is not None:
            request.set_options(options)
        if option_remove is not None: 
            request.set_options_to_remove(option_remove)
        if template_specification is not None: 
            request.set_template_specification(template_specification)     
        
        try:    
            response = self.call(request)
        except:
            raise

        options = response['DescribeConfigurationOptionsResponse']\
            ['DescribeConfigurationOptionsResult']['Options']
        request_id = response['DescribeConfigurationOptionsResponse']\
            ['ResponseMetadata']['RequestId']

        option_descriptions = list()
        for option in options:
            option_descriptions.append(ConfigurationOptionDescription.from_json(option))
        return Response(request_id, option_descriptions)


    def describe_configuration_settings(self, application_name, 
                                        environment_name = None, template = None,
                                        options = None):
        request = Request()
        request.set_operation('DescribeConfigurationSettings')
        request.set_app_name(application_name)
        if environment_name is not None:
            request.set_env_name(environment_name)
        if template is not None:
            request.set_template(template)
        if options is not None:
            request.set_options(options)
        
        try:    
            response = self.call(request)
        except:
            raise

        result = response['DescribeConfigurationSettingsResponse']\
            ['DescribeConfigurationSettingsResult']['ConfigurationSettings'][0]
        request_id = response['DescribeConfigurationSettingsResponse']\
            ['ResponseMetadata']['RequestId']

        configuration = ConfigurationSettingsDescription.from_json(result)
        return Response(request_id, configuration)

    
    def describe_environments (self, application = None, 
                               environment_names = None, environment_ids = None, 
                               include_deleted = None, included_deleted_backto = None, 
                               version_label = None):
        request = Request()
        request.set_operation('DescribeEnvironments')

        if application is not None:        
            request.set_app_name(application)
        if environment_names is not None:
            request.set_env_names(environment_names)
        if environment_ids is not None:
            request.set_env_ids(environment_ids)
        if include_deleted is not None:        
            request.set_include_deleted(include_deleted)
        if included_deleted_backto is not None:        
            request.set_included_deleted_backto(included_deleted_backto)
        if version_label is not None:        
            request.set_version_label(version_label)            
                    
        try:    
            response = self.call(request)
        except:
            raise
        
        # parse message
        results = response['DescribeEnvironmentsResponse']\
            ['DescribeEnvironmentsResult']['Environments']
        request_id = response['DescribeEnvironmentsResponse']\
            ['ResponseMetadata']['RequestId']
                
        environments = []
        for result in results:
            environments.append(EnvironmentDescription.from_json(result))
        return Response(request_id, environments)
    
    
    def describe_environment_resources (self, environment_name, environment_id = None):
        request = Request()
        request.set_operation('DescribeEnvironmentResources')
        if environment_name is not None:        
            request.set_env_name(environment_name)
        if environment_id is not None:        
            request.set_env_id(environment_id)
        
        try:    
            response = self.call(request)
        except:
            raise

        # parse message
        result = response['DescribeEnvironmentResourcesResponse']\
            ['DescribeEnvironmentResourcesResult']['EnvironmentResources']
        request_id = response['DescribeEnvironmentResourcesResponse']\
            ['ResponseMetadata']['RequestId']
                
        resources = EnvironmentResourceDescription.from_json(result)
        return Response(request_id, resources)
    

    def request_environment_info (self, environment_name = None, 
                                  environment_id = None, info_type = EbDefault.TailLog):
        request = Request()
        request.set_operation('RequestEnvironmentInfo')
        if environment_name is not None:        
            request.set_env_name(environment_name)
        if environment_id is not None:        
            request.set_env_id(environment_id)
        if info_type is not None:        
            request.set_info_type(info_type)
        
        try:    
            response = self.call(request)
        except:
            raise

        # parse message
        request_id = response['RequestEnvironmentInfoResponse']\
            ['ResponseMetadata']['RequestId']
            
        return Response(request_id)
    
        
    def retrieve_environment_info (self, environment_name = None, 
                                  environment_id = None, info_type = EbDefault.TailLog):
        request = Request()
        request.set_operation('RetrieveEnvironmentInfo')
        if environment_name is not None:        
            request.set_env_name(environment_name)
        if environment_id is not None:        
            request.set_env_id(environment_id)
        if info_type is not None:        
            request.set_info_type(info_type)
                    
        try:    
            response = self.call(request)
        except:
            raise

        # parse message
        results = response['RetrieveEnvironmentInfoResponse']\
            ['RetrieveEnvironmentInfoResult']['EnvironmentInfo']
        request_id = response['RetrieveEnvironmentInfoResponse']\
            ['ResponseMetadata']['RequestId']
                
        env_infos = []
        for result in results:
            env_infos.append(EnvironmentInfoDescription.from_json(result))
        return Response(request_id, env_infos)                

    
    def describe_events (self, application = None, 
                         environment_name = None, environment_id = None,
                         start_time = None, end_time = None, 
                         severity = None, request_id = None, 
                         template = None, version_label = None,
                         max_records = None, next_token = None
                         ):

        request = Request()
        request.set_operation('DescribeEvents')
        if application is not None:        
            request.set_app_name(application)
        if environment_name is not None:        
            request.set_env_name(environment_name)
        if environment_id is not None:        
            request.set_env_id(environment_id)
        if start_time is not None:        
            request.set_start_time(start_time)
        if end_time is not None:        
            request.set_end_time(end_time)
        if severity is not None:        
            request.set_severity(severity)
        if request_id is not None:        
            request.set_requst_id(request_id)
        if template is not None:        
            request.set_template(template)
        if version_label is not None:        
            request.set_version_label(version_label)
        if max_records is not None:        
            request.set_max_records(max_records)
        if next_token is not None:        
            request.set_next_token(next_token)
        
        try:    
            response = self.call(request)
        except:
            raise

        # parse message
        combined_result = response['DescribeEventsResponse']\
            ['DescribeEventsResult'] 
        results = combined_result['Events']
        next_token = combined_result['NextToken'] \
            if 'NextToken' in combined_result else None
        request_id = response['DescribeEventsResponse']\
            ['ResponseMetadata']['RequestId']

        events = []
        for result in results:
            events.append(EventDescription.from_json(result))
        return Response(request_id, events, next_token)    

    
    def list_available_solutionstacks(self):
        request = Request()
        request.set_operation('ListAvailableSolutionStacks')
        
        try:    
            response = self.call(request)
        except:
            raise

        results = response['ListAvailableSolutionStacksResponse']\
            ['ListAvailableSolutionStacksResult']['SolutionStackDetails']
        request_id = response['ListAvailableSolutionStacksResponse']\
            ['ResponseMetadata']['RequestId']

        solutionstacks = []
        for result in results:
            solutionstacks.append(SolutionStackDescription.from_json(result))
        return Response(request_id, solutionstacks)    
    
    
    def create_configuration_template(self, application, template, 
                                      environment_id = None, solution_stack = None, 
                                      source_configuration = None,
                                      option_settings = None,
                                      option_remove = None,
                                      template_specification = None,
                                      description = None):
        request = Request()
        request.set_operation('CreateConfigurationTemplate')
        request.set_app_name(application)
        request.set_template(template)

        if environment_id is not None:
            request.set_env_id(environment_id)
        if solution_stack is not None:
            request.set_solution_stack(solution_stack)
        if source_configuration is not None:
            request.set_source_configuration(source_configuration)
        if option_settings is not None: 
            request.set_option_settings(option_settings)
        if option_remove is not None: 
            request.set_options_to_remove(option_remove)
        if template_specification is not None: 
            request.set_template_specification(template_specification)
        if description is not None: 
            request.set_description(description)
        
        response = self.call(request)

        result = response['CreateConfigurationTemplateResponse']['CreateConfigurationTemplateResult']
        request_id = response['CreateConfigurationTemplateResponse']\
            ['ResponseMetadata']['RequestId']
                
        configuration = ConfigurationSettingsDescription.from_json(result)
        return Response(request_id, configuration)


    def update_configuration_template(self, application, template, 
                                      environment_id = None, solution_stack = None, 
                                      source_configuration = None,
                                      option_settings = None,
                                      option_remove = None, 
                                      template_specification = None,
                                      description = None):
                
        request = Request()
        request.set_operation('UpdateConfigurationTemplate')
        request.set_app_name(application)
        request.set_template(template)

        if environment_id is not None:
            request.set_env_id(environment_id)
        if solution_stack is not None:
            request.set_solution_stack(solution_stack)
        if source_configuration is not None:
            request.set_source_configuration(source_configuration)
        if option_settings is not None: 
            request.set_option_settings(option_settings)
        if option_remove is not None: 
            request.set_options_to_remove(option_remove)
        if template_specification is not None: 
            request.set_template_specification(template_specification)
        if description is not None: 
            request.set_description(description)
        
        response = self.call(request)

        result = response['UpdateConfigurationTemplateResponse']['UpdateConfigurationTemplateResult']
        request_id = response['UpdateConfigurationTemplateResponse']\
            ['ResponseMetadata']['RequestId']
                
        configuration = ConfigurationSettingsDescription.from_json(result)
        return Response(request_id, configuration)

    
    def delete_configuration_template(self, application, template):
        
        request = Request()
        request.set_operation('DeleteConfigurationTemplate')
        request.set_app_name(application)
        request.set_template(template)
        
        response = self.call(request)

        request_id = response['DeleteConfigurationTemplateResponse']\
            ['ResponseMetadata']['RequestId']
                
        return Response(request_id, None)
    

    def validate_configuration_settings(self, 
                                        application_name = None, 
                                        option_settings = None, 
                                        environment_name = None,                                         
                                        template = None, 
                                        version_label = None,
                                        option_remove = None,
                                        template_specification = None):
        request = Request()
        request.set_operation('ValidateConfigurationSettings')
        if application_name is not None:
            request.set_app_name(application_name)
        if option_settings is not None:
            request.set_option_settings(option_settings)
        if environment_name is not None:
            request.set_env_name(environment_name)
        if template is not None:
            request.set_template(template)
        if version_label is not None:
            request.set_version_label(version_label)
        if option_remove is not None: 
            request.set_options_to_remove(option_remove)
        if template_specification is not None: 
            request.set_template_specification(template_specification)            
            
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code.lower() == AwsErrorCode.InsufficientPrivileges.lower():
                raise InsufficientPrivilegesException(ex)
            if ex.code.lower() == AwsErrorCode.MissingParameter.lower():
                raise MissingParameterException(ex)
            if ex.code.lower() == AwsErrorCode.InvalidParameterValue.lower():
                raise InvalidParameterValueException(ex)
            raise       

        results = response['ValidateConfigurationSettingsResponse']\
            ['ValidateConfigurationSettingsResult']['Messages']
        request_id = response['ValidateConfigurationSettingsResponse']\
            ['ResponseMetadata']['RequestId']
            
        messages = []
        if results is not None:
            for result in results:
                messages.append(ValidationMessage.from_json(result))
        return Response(request_id, messages)                
    