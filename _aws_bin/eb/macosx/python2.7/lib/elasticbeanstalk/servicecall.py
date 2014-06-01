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
    _api_version = u'2010-12-01'
    _service_name = u'elasticbeanstalk'

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
            log.debug(u'Request ID: {0}'.format(return_msg.json().values()[0]\
                                                [u'ResponseMetadata'][u'RequestId'])) 

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
        request.set_operation(u'CreateApplication')
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
        result = response[u'CreateApplicationResponse']\
            [u'CreateApplicationResult'][u'Application']
        request_id = response[u'CreateApplicationResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id, ApplicationDescription.from_json(result))


    def delete_application(self, name, terminate_env = u'false'):
        request = Request()
        request.set_operation(u'DeleteApplication')
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
                
        request_id = response[u'DeleteApplicationResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id)


    def create_application_version(self, application, version_label, 
                                   s3bucket = None, s3key = None, 
                                   auto_create_app = u'false', description = None):
        if (s3bucket is None and s3key is not None) \
            or (s3bucket is not None and s3key is None):
            raise AttributeError(u'Must specify both s3 bucket and key')
        
        request = Request()
        request.set_operation(u'CreateApplicationVersion')
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

        result = response[u'CreateApplicationVersionResponse']\
            [u'CreateApplicationVersionResult'][u'ApplicationVersion']
        request_id = response[u'CreateApplicationVersionResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id, ApplicationVersionDescription.from_json(result))

        
    def delete_application_version(self, application, 
                                   version_label, delete_bundle = u'false'):
        
        request = Request()
        request.set_operation(u'DeleteApplicationVersion')
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

        request_id = response[u'DeleteApplicationVersionResponse']\
            [u'ResponseMetadata'][u'RequestId']
        return Response(request_id)


    def create_environment(self, application, environment, cname_prefix = None, 
                           template = None, solution_stack = None, version_label = None,
                           option_settings = None, option_remove = None, 
                           template_specification = None,
                           description = None, tier = None):
        request = Request()
        request.set_operation(u'CreateEnvironment')
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

        result = response[u'CreateEnvironmentResponse'][u'CreateEnvironmentResult']
        request_id = response[u'CreateEnvironmentResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id, EnvironmentDescription.from_json(result))


    def update_environment(self, env_name = None, env_id = None, 
                           template = None, version_label = None,
                           option_settings = None, option_remove = None,
                           template_specification = None,
                           description = None, tier = None):
        request = Request()
        request.set_operation(u'UpdateEnvironment')
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

        result = response[u'UpdateEnvironmentResponse'][u'UpdateEnvironmentResult']
        request_id = response[u'UpdateEnvironmentResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id, EnvironmentDescription.from_json(result))


    def terminate_environment(self, environment_name, 
                              environment_id = None, delete_resource = u'true'):
        request = Request()
        request.set_operation(u'TerminateEnvironment')
        if environment_name is not None:        
            request.set_env_name(environment_name)
        if environment_id is not None:        
            request.set_env_id(environment_id)
        request.set_terminate_resources(delete_resource)
            
        try:    
            response = self.call(request)
        except:
            raise        

        result = response[u'TerminateEnvironmentResponse']\
            [u'TerminateEnvironmentResult']
        request_id = response[u'TerminateEnvironmentResponse']\
        [u'ResponseMetadata'][u'RequestId']
                
        return Response(request_id, EnvironmentDescription.from_json(result))

        
    def describe_applications(self, names):
        request = Request()
        request.set_operation(u'DescribeApplications')
        request.set_app_names(names)
        
        try:    
            response = self.call(request)
        except:
            raise

        results = response[u'DescribeApplicationsResponse']\
            [u'DescribeApplicationsResult'][u'Applications']
        request_id = response[u'DescribeApplicationsResponse']\
            [u'ResponseMetadata'][u'RequestId']

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
        request.set_operation(u'DescribeConfigurationOptions')
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

        options = response[u'DescribeConfigurationOptionsResponse']\
            [u'DescribeConfigurationOptionsResult'][u'Options']
        request_id = response[u'DescribeConfigurationOptionsResponse']\
            [u'ResponseMetadata'][u'RequestId']

        option_descriptions = list()
        for option in options:
            option_descriptions.append(ConfigurationOptionDescription.from_json(option))
        return Response(request_id, option_descriptions)


    def describe_configuration_settings(self, application_name, 
                                        environment_name = None, template = None,
                                        options = None):
        request = Request()
        request.set_operation(u'DescribeConfigurationSettings')
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

        result = response[u'DescribeConfigurationSettingsResponse']\
            [u'DescribeConfigurationSettingsResult'][u'ConfigurationSettings'][0]
        request_id = response[u'DescribeConfigurationSettingsResponse']\
            [u'ResponseMetadata'][u'RequestId']

        configuration = ConfigurationSettingsDescription.from_json(result)
        return Response(request_id, configuration)

    
    def describe_environments (self, application = None, 
                               environment_names = None, environment_ids = None, 
                               include_deleted = None, included_deleted_backto = None, 
                               version_label = None):
        request = Request()
        request.set_operation(u'DescribeEnvironments')

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
        results = response[u'DescribeEnvironmentsResponse']\
            [u'DescribeEnvironmentsResult'][u'Environments']
        request_id = response[u'DescribeEnvironmentsResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        environments = []
        for result in results:
            environments.append(EnvironmentDescription.from_json(result))
        return Response(request_id, environments)
    
    
    def describe_environment_resources (self, environment_name, environment_id = None):
        request = Request()
        request.set_operation(u'DescribeEnvironmentResources')
        if environment_name is not None:        
            request.set_env_name(environment_name)
        if environment_id is not None:        
            request.set_env_id(environment_id)
        
        try:    
            response = self.call(request)
        except:
            raise

        # parse message
        result = response[u'DescribeEnvironmentResourcesResponse']\
            [u'DescribeEnvironmentResourcesResult'][u'EnvironmentResources']
        request_id = response[u'DescribeEnvironmentResourcesResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        resources = EnvironmentResourceDescription.from_json(result)
        return Response(request_id, resources)
    

    def request_environment_info (self, environment_name = None, 
                                  environment_id = None, info_type = EbDefault.TailLog):
        request = Request()
        request.set_operation(u'RequestEnvironmentInfo')
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
        request_id = response[u'RequestEnvironmentInfoResponse']\
            [u'ResponseMetadata'][u'RequestId']
            
        return Response(request_id)
    
        
    def retrieve_environment_info (self, environment_name = None, 
                                  environment_id = None, info_type = EbDefault.TailLog):
        request = Request()
        request.set_operation(u'RetrieveEnvironmentInfo')
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
        results = response[u'RetrieveEnvironmentInfoResponse']\
            [u'RetrieveEnvironmentInfoResult'][u'EnvironmentInfo']
        request_id = response[u'RetrieveEnvironmentInfoResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
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
        request.set_operation(u'DescribeEvents')
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
        combined_result = response[u'DescribeEventsResponse']\
            [u'DescribeEventsResult'] 
        results = combined_result[u'Events']
        next_token = combined_result[u'NextToken'] \
            if u'NextToken' in combined_result else None
        request_id = response[u'DescribeEventsResponse']\
            [u'ResponseMetadata'][u'RequestId']

        events = []
        for result in results:
            events.append(EventDescription.from_json(result))
        return Response(request_id, events, next_token)    

    
    def list_available_solutionstacks(self):
        request = Request()
        request.set_operation(u'ListAvailableSolutionStacks')
        
        try:    
            response = self.call(request)
        except:
            raise

        results = response[u'ListAvailableSolutionStacksResponse']\
            [u'ListAvailableSolutionStacksResult'][u'SolutionStackDetails']
        request_id = response[u'ListAvailableSolutionStacksResponse']\
            [u'ResponseMetadata'][u'RequestId']

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
        request.set_operation(u'CreateConfigurationTemplate')
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

        result = response[u'CreateConfigurationTemplateResponse'][u'CreateConfigurationTemplateResult']
        request_id = response[u'CreateConfigurationTemplateResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
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
        request.set_operation(u'UpdateConfigurationTemplate')
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

        result = response[u'UpdateConfigurationTemplateResponse'][u'UpdateConfigurationTemplateResult']
        request_id = response[u'UpdateConfigurationTemplateResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        configuration = ConfigurationSettingsDescription.from_json(result)
        return Response(request_id, configuration)

    
    def delete_configuration_template(self, application, template):
        
        request = Request()
        request.set_operation(u'DeleteConfigurationTemplate')
        request.set_app_name(application)
        request.set_template(template)
        
        response = self.call(request)

        request_id = response[u'DeleteConfigurationTemplateResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
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
        request.set_operation(u'ValidateConfigurationSettings')
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

        results = response[u'ValidateConfigurationSettingsResponse']\
            [u'ValidateConfigurationSettingsResult'][u'Messages']
        request_id = response[u'ValidateConfigurationSettingsResponse']\
            [u'ResponseMetadata'][u'RequestId']
            
        messages = []
        if results is not None:
            for result in results:
                messages.append(ValidationMessage.from_json(result))
        return Response(request_id, messages)                
    