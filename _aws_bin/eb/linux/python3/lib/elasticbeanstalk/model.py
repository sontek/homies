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

import datetime


class BaseModel(object):
    
    def __str__(self):
        return self.__repr__()
   
    def __repr__(self):
        ret = "\n"
        for attr, value in self.__dict__.items():
            ret += "(" + str(attr) + ":" + str(value) + ")\n"
        return ret    

def extract_list_elements(json_data, property_name, factory_method):
    elements = []
    if property_name in json_data:
        list_property = json_data[property_name]
        if list_property:
            for element in list_property:
                elements.append(factory_method(element))
    return elements

def extract_element(json_data, property_name, factory_method):
    if property_name in json_data:
        element = json_data[property_name]
        if element:
            return factory_method(element)
    return None

class ApplicationDescription(BaseModel):
    """ <p>Describes the properties of an application.</p>\n"""

    def __init__(self):
        self._application_name = None
        self._configuration_templates = None
        self._date_created = None
        self._date_created_raw = None
        self._date_updated = None
        self._date_updated_raw = None
        self._description = None
        self._versions = None 
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of ApplicationDescription from structured json data"""
        ad = cls()
        ad._application_name = json_data['ApplicationName']
        ad._configuration_templates = json_data['ConfigurationTemplates']
        ad._date_created_raw = json_data['DateCreated']
        ad._date_created = datetime.datetime.fromtimestamp(ad._date_created_raw).\
            replace(microsecond=0)
        ad._date_updated_raw = json_data['DateUpdated']
        ad._date_updated = datetime.datetime.fromtimestamp(ad._date_updated_raw).\
            replace(microsecond=0)
        ad._description = json_data['Description']
        ad._versions = json_data['Versions'] 
        return ad
            
    @property
    def application_name(self):
        """ <p>The name of the application.</p>\n"""
        return self._application_name
            
    @property
    def configuration_templates(self):
        """ <p> The names of the configuration templates associated with this 
            application. </p>\n"""
        return self._configuration_templates

    @property
    def date_created(self):
        """ <p>The date when the application was created.</p>\n"""
        return self._date_created

    @property
    def date_updated(self):
        """ <p>The date when the application was last modified.</p>\n"""
        return self._date_updated
    
    @property
    def description(self):
        """ <p>User-defined description of the application.</p>\n"""
        return self._description

    @property
    def versions(self):
        """ <p>The names of the versions for this application.</p>\n"""
        return self._versions

    @property
    def date_created_raw(self):
        return self._date_created_raw

    @property
    def date_updated_raw(self):
        return self._date_updated_raw


class S3Location(BaseModel):
    """ <p>A specification of a location in Amazon S3.</p>\n"""
    def __init__(self):
        self._s3_bucket = None
        self._s3_key = None
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of S3Location from structured json data"""
        s3l = cls()
        s3l._s3_bucket = json_data['S3Bucket']
        s3l._s3_key = json_data['S3Key']
        return s3l
            
    @property
    def s3_bucket(self):
        """ <p>The Amazon S3 bucket where the data is located.</p>\n"""
        return self._s3_bucket

    @property
    def s3_key(self):
        """ <p>The Amazon S3 key where the data is located.</p>\n"""
        return self._s3_key


class ApplicationVersionDescription(BaseModel):
    """ <p> Describes the properties of an application version. </p>\n"""
    def __init__(self):
        self._application_name = None
        self._date_created = None
        self._date_created_raw = None
        self._date_updated = None
        self._date_updated_raw = None
        self._description = None
        self._source_bundle = None
        self._version_label = None 
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of ApplicationVersionDescription from structured json data"""
        avd = cls()
        avd._application_name = json_data['ApplicationName']
        avd._date_created_raw = json_data['DateCreated']
        avd._date_created = datetime.datetime.fromtimestamp(avd._date_created_raw).\
            replace(microsecond=0)
        avd._date_updated_raw = json_data['DateUpdated']
        avd._date_updated = datetime.datetime.fromtimestamp(avd._date_updated_raw).\
            replace(microsecond=0)
        avd._description = json_data['Description']
        avd._source_bundle = S3Location.from_json(json_data['SourceBundle'])
        avd._version_label = json_data['VersionLabel']
        return avd 
            
    @property
    def application_name(self):
        """ <p>The name of the application associated with this release.</p>\n"""
        return self._application_name

    @property
    def date_created(self):
        """ <p>The creation date of the application version.</p>\n"""
        return self._date_created

    @property
    def date_updated(self):
        """ <p>The last modified date of the application version.</p>\n"""
        return self._date_updated
    
    @property
    def description(self):
        """ <p>The description of this application version.</p>\n"""
        return self._description

    @property
    def source_bundle(self):
        """ <p>The location where the source bundle is located for this version.</p>\n"""
        return self._version_label

    @property
    def version_label(self):
        """ <p>A label uniquely identifying the version for the associated application.</p>\n"""
        return self._version_label

    @property
    def date_created_raw(self):
        return self._date_created_raw

    @property
    def date_updated_raw(self):
        return self._date_updated_raw
    

class EnvironmentDescription(BaseModel):
    """ <p>Describes the properties of an environment.</p>\n"""

    def __init__(self):
        self._application_name = None
        self._cname = None
        self._date_created = None
        self._date_created_raw = None
        self._date_updated = None
        self._date_updated_raw = None
        self._description = None
        self._endpoint_url = None
        self._environment_name = None
        self._environment_id = None
        self._health = None
        self._resources = None
        self._status = None
        self._solution_stack_name = None
        self._template_name = None
        self._version_label = None 

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of EnvironmentDescription from structured json data"""
        en = cls()
        en._application_name = json_data['ApplicationName']
        en._cname = json_data['CNAME']
        en._date_created_raw = json_data['DateCreated']
        en._date_created = datetime.datetime.fromtimestamp(en._date_created_raw).\
            replace(microsecond=0)
        en._date_updated_raw = json_data['DateUpdated']
        en._date_updated = datetime.datetime.fromtimestamp(en._date_updated_raw).\
            replace(microsecond=0)
        en._description = json_data['Description']
        en._endpoint_url = json_data['EndpointURL']
        en._environment_id = json_data['EnvironmentId']
        en._environment_name = json_data['EnvironmentName']
        en._health = json_data['Health']
        en._resources = json_data['Resources']
        en._solution_stack_name = json_data['SolutionStackName']
        en._status = json_data['Status']
        en._template_name = json_data['TemplateName']
        en._version_label = json_data['VersionLabel']
        en._tier = extract_element(json_data, 'Tier', EnvironmentTier.from_json)
        
        return en

    @property
    def environment_name(self):
        """ <p>The name of this environment.</p>\n"""
        return self._environment_name

    @property
    def environment_id(self):
        """ <p>The ID of this environment.</p>\n"""
        return self._environment_id

    @property
    def application_name(self):
        """ <p>The name of the application associated with this environment.</p>\n"""
        return self._application_name

    @property
    def version_label(self):
        """ <p>The application version deployed in this environment.</p>\n"""
        return self._version_label

    @property
    def tier(self):
        """ <p>The environment tier for this environment.</p>\n"""
        return self._tier
    
    @property
    def solution_stack_name(self):
        """ <p>The name of the <code>SolutionStack</code> deployed with this environment.</p>\n"""
        return self._solution_stack_name

    @property
    def template_name(self):
        """ <p>The name of the configuration template used to originally launch this environment.</p>\n"""
        return self._template_name

    @property
    def description(self):
        """ <p>Describes this environment.</p>\n"""
        return self._description

    @property
    def endpoint_url(self):
        """ <p>The URL to the LoadBalancer for this environment.</p>\n"""
        return self._endpoint_url

    @property
    def cname(self):
        """ <p>The URL to the CNAME for this environment.</p>\n"""
        return self._cname

    @property
    def date_created(self):
        """ <p>The creation date for this environment.</p>\n"""
        return self._date_created

    @property
    def date_updated(self):
        """ <p>The last modified date for this environment.</p>\n"""
        return self._date_updated

    @property
    def status(self):
        """ <p>The current operational status of the environment:</p>
    
          <ul>
             <li>
                <code>Launching</code>: Environment is in the process of
                initial deployment.
             </li>
             <li>
                <code>Updating</code>: Environment is in the process of
                updating its configuration settings or
                application version.
             </li>
             <li>
                <code>Ready</code>: Environment is available to have an action
                performed on it, such as update or terminate.
             </li>
             <li>
                <code>Terminating</code>: Environment is in the shut-down process.
             </li>
             <li>
                <code>Terminated</code>: Environment is not running.
             </li>
          </ul>\n"""
        return self._status

    @property
    def health(self):
        """ <p> Describes the health status of the environment. 
                AWS Elastic Beanstalk indicates the failure levels for a running environment:
          </p>
          <enumValues>
             <value name="Red">
                <p>
                   <code>Red</code>
                   : Indicates the environment is not working.
                </p>
             </value>
             <value name="Yellow">
                <p>
                   <code>Yellow</code>: Indicates that something is wrong, the application
                   might not be available, but the instances appear running.
                </p>
             </value>
             <value name="Green">
                <p>
                   <code>Green</code>: Indicates the environment is
                   healthy and fully functional.
                </p>
             </value>
          </enumValues>"""        
        return self._health

    @property
    def resources(self):
        """ <p>The description of the AWS resources used by this environment.</p>\n"""        
        return self._resources

    @property
    def date_created_raw(self):
        return self._date_created_raw

    @property
    def date_updated_raw(self):
        return self._date_updated_raw


class EnvironmentTier(BaseModel):
    
    def __init__(self):
        self._name = None
        self._type = None
        self._version = None
    
    @classmethod
    def from_values(cls, tier_name, tier_type, tier_version):
        et = cls()
        et._name = tier_name
        et._type = tier_type
        et._version = tier_version
        return et
    
    SEPARATOR = "::"
    @classmethod
    def from_serialized_string(cls, serialized_string):
        
        elements = serialized_string.split(EnvironmentTier.SEPARATOR)
        if elements and (len(elements)==3):
            return  cls.from_values(elements[0], elements[1], elements[2])
        
        return None
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of EnvironmentTier from structured json data"""
        et = cls()
        et._name = json_data['Name']
        et._type = json_data['Type']
        et._version = json_data['Version']
        return et
    
    @property
    def name(self):
        return self._name
    
    @property
    def type(self):
        return self._type
    
    @property
    def version(self):
        return self._version
    
    def to_serialized_string(self):
        return EnvironmentTier.SEPARATOR.join((self.name, self.type, self.version))


class EnvironmentResourceDescription(BaseModel):
    """ <p>Describes the AWS resources in use by this environment. This data is live.</p>\n"""

    def __init__(self):
        self._auto_scaling_groups = None
        self._environment_name = None
        self._instances = None
        self._launch_configurations = None
        self._load_balancers = None 
        self._triggers = None
        self._resources = None
        self._queues = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of EnvironmentResourceDescription from structured json data"""
        erd = cls()
        erd._auto_scaling_groups = json_data['AutoScalingGroups']
        erd._environment_name = json_data['EnvironmentName']
        erd._instances = json_data['Instances']
        erd._launch_configurations = json_data['LaunchConfigurations']
        erd._load_balancers = json_data['LoadBalancers'] 
        erd._triggers = json_data['Triggers']
        erd._resources = extract_list_elements(json_data, 'Resources', ResourceDescription.from_json)
        erd._queues = extract_list_elements(json_data, 'Queues', Queue.from_json)
        return erd
    
    @property
    def auto_scaling_groups(self):
        """<p> The <code>AutoScalingGroups</code> used by this environment. </p>\n"""
        return self._auto_scaling_groups

    @property
    def environment_name(self):
        """<p> Environment name. </p>\n"""
        return self._environment_name
     
    @property
    def instances(self):
        """<p>The Amazon EC2 instances used by this environment.</p>\n"""
        return self._instances
     
    @property
    def launch_configurations(self):
        """<p>The Auto Scaling launch configurations in use by this environment.</p>\n"""
        return self._launch_configurations
    
    @property
    def load_balancers(self):
        """ <p>The LoadBalancers in use by this environment.</p>\n"""
        return self._load_balancers
    
    @property
    def triggers(self):
        """The <code>AutoScaling</code> triggers in use by this environment. </p>\n"""
        return self._triggers

    @property
    def resources(self):
        return self._resources
    
    @property
    def queues(self):
        return self._queues
    
class ResourceProperty(BaseModel):

    def __init__(self):
        self._name = None
        self._value = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of ResourceProperty from structured json data"""
        rp = cls()
        rp._name = json_data['Name']
        rp._value = json_data['Value']
        return rp
    
    @property
    def name(self):
        return self._name

    @property
    def value(self):
        return self._value


class ResourceDescription(BaseModel):

    def __init__(self):
        self._description = None
        self._logical_resource_id = None
        self._physical_resource_id = None
        self._properties = None
        self._type = None 

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of ResourceDescription from structured json data"""
        rd = cls()
        rd._description = json_data['Description']
        rd._logical_resource_id = json_data['LogicalResourceId']
        rd._physical_resource_id = json_data['PhysicalResourceId']
        rd._properties = extract_list_elements(json_data, 'Properties', ResourceProperty.from_json)
        rd._type = json_data['Type']
        return rd

    @property
    def description(self):
        return self._description

    @property
    def logical_resource_id(self):
        return self._logical_resource_id
     
    @property
    def physical_resource_id(self):
        return self._physical_resource_id
     
    @property
    def properties(self):
        return self._properties
    
    @property
    def type(self):
        return self._type


class Queue(BaseModel):
    
    def __init__(self):
        self._name = None
        self._url = None
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of Queue from structured json data"""
        q = cls()
        q._name = json_data['Name']
        q._url = json_data['URL']
        return q
    
    @property
    def name(self):
        return self._name
    
    @property
    def url(self):
        return self._url


class EventDescription(BaseModel):
    """ <p>Describes an event.</p>\n"""

    def __init__(self):
        self._application_name = None
        self._environment_name = None
        self._event_date = None
        self._event_date_raw = None
        self._message = None
        self._request_id = None
        self._severity = None
        self._template_name = None
        self._version_label = None 

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of EventDescription from structured json data"""
        ev = cls()
        ev._application_name = json_data['ApplicationName']
        ev._environment_name = json_data['EnvironmentName']
        ev._event_date_raw = json_data['EventDate']
        ev._event_date = datetime.datetime.fromtimestamp(ev._event_date_raw).\
            replace(microsecond=0)
        ev._message = json_data['Message']
        ev._request_id = json_data['RequestId']
        ev._severity = json_data['Severity']
        ev._template_name = json_data['TemplateName']
        ev._version_label = json_data['VersionLabel'] 
        return ev


    @property
    def application_name(self):
        """ <p>The application associated with the event.</p>\n"""
        return self._application_name

    @property
    def environment_name(self):
        """ <p>The name of the environment associated with this event.</p>\n"""
        return self._environment_name

    @property
    def event_date(self):
        """ <p>The date when the event occurred.</p>\n"""
        return self._event_date

    @property
    def message(self):
        """ <p>The event message.</p>\n"""
        return self._message


    @property
    def request_id(self):
        """ <p>The web service request ID for the activity of this event.</p>\n"""
        return self._request_id


    @property
    def severity(self):
        """ <p>The severity level of this event. </p>\n"""
        return self._severity


    @property
    def template_name(self):
        """ <p>The name of the configuration associated with this event.</p>\n"""
        return self._template_name
    
    @property
    def version_label(self):
        """ <p>The release label for the application version associated with this event.</p>\n"""
        return self._version_label

    @property
    def event_date_raw(self):
        return self._event_date_raw
    

class SolutionStackDescription(BaseModel):
    """ <p>Describes the solution stack.</p>\n"""

    def __init__(self):
        self._solutionstack_name = None
        self._permitted_file_types = None
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of SolutionStackDescription from structured json data"""
        ssd = cls()
        ssd._solutionstack_name = json_data['SolutionStackName']
        ssd._permitted_file_types = json_data['PermittedFileTypes']
        return ssd
            
    @property
    def solutionstack_name(self):
        """ <p>The name of the solution stack.</p>\n"""
        return self._solutionstack_name
    
    @property
    def permitted_file_types(self):
        """ <p> The permitted file types allowed for a solution stack. </p>\n"""
        return self._permitted_file_types 


class ConfigurationOptionDescription(BaseModel):
    """ <p> Describes the possible values for a configuration option. </p>\n"""

    def __init__(self):
        self._namespace = None
        self._name = None
        self._default_value = None
        self._change_severity = None
        self._user_defined = None
        self._value_type = None
        self._value_options = None
        self._min_value = None
        self._max_value = None
        self._max_length = None
        self._regex = None
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of ConfigurationOptionDescription from structured json data"""
        co = cls()
        co._namespace = json_data['Namespace']
        co._name = json_data['Name']
        co._default_value = json_data['DefaultValue']
        co._change_severity = json_data['ChangeSeverity']
        co._user_defined = json_data['UserDefined']
        co._value_type = json_data['ValueType']
        co._value_options = json_data['ValueOptions']
        co._min_value = json_data['MinValue']
        co._max_value = json_data['MaxValue']
        co._max_length = json_data['MaxLength']
        co._regex = json_data['Regex']
        return co
            
    @property
    def namespace(self):
        """ <p> A unique namespace identifying the option's associated AWS resource. </p>\n"""
        return self._namespace
    
    @property
    def name(self):
        """ <p> The name of the configuration option. </p>\n"""
        return self._name 

    @property
    def default_value(self):
        """ <p> The default value for this configuration option. </p>\n"""
        return self._default_value

    @property
    def change_severity(self):
        """ <p>
         An indication of which action is required if the value for this
         configuration option changes:
        </p>
      <enumValues>
         <value name="NoInterruption">
            <p>
               NoInterruption - There is no interruption to the
               environment or application availability.
                </p>
         </value>
         <value name="RestartEnvironment">
            <p>
               RestartEnvironment - The environment is
               restarted, all AWS resources are deleted and recreated, and
               the environment is unavailable during the process.
                </p>
         </value>
         <value name="RestartApplicationServer">
            <p>
               RestartApplicationServer - The environment is available
               the entire time. However, a short application
               outage occurs when the application servers on the running Amazon EC2 instances
               are restarted.
                </p>
         </value>
      </enumValues>
        """
        return self._change_severity

    @property
    def user_defined(self):
        """ <p>
         An indication of whether the user defined this configuration option:
        </p>
      <enumValues>
         <value name="true">
            <p>
               <code>true</code>
               : This configuration option was defined by the user. It is a
               valid choice for specifying this as an Option to Remove when
               updating configuration settings.

            </p>

         </value>
         <value name="false">
            <p>
               <code>false</code>
               : This configuration was not defined by the user.
            </p>
         </value>
      </enumValues>
        """
        return self._user_defined

    @property
    def value_type(self):
        """ <p>
         An indication of which type of values this option has and whether
         it is allowable to select one or more than one of the possible values:
        </p>
      <enumValues>
         <value name="Scalar">
            <p>
               <code>Scalar</code>
               : Values for this option are a single selection from the
               possible values, or a unformatted string or numeric value governed
               by the MIN/MAX/Regex constraints:
            </p>
         </value>
         <value name="List">
            <p>
               <code>List</code>
               : Values for this option are multiple selections of the
               possible values.
            </p>
         </value>
         <value name="Boolean">
            <p>
               <code>Boolean</code>
               : Values for this option are either
               <code>true</code>
               or
               <code>false</code>
               .
            </p>
         </value>
      </enumValues>
        """
        return self._value_type

    @property
    def value_options(self):
        """ <p> If specified, values for the configuration option are selected
         from this list. </p>\n"""
        return self._value_options

    @property
    def min_value(self):
        """ <p> If specified, the configuration option must be a numeric value
         greater than this value. </p>\n"""
        return self._min_value

    @property
    def max_value(self):
        """ <p> If specified, the configuration option must be a numeric value less than this
         value. </p>\n"""
        return self._max_value

    @property
    def max_length(self):
        """ <p> If specified, the configuration option must be a string value no longer than
         this value. </p>\n"""
        return self._max_length

    @property
    def regex(self):
        """ <p> If specified, the configuration option must be a string value that satisfies
         this regular expression. </p>\n"""
        return self._regex


class OptionSepcification(BaseModel):
    """ <p> A specification identifying an individual configuration option. </p>\n"""
    def __init__(self, namespace = None, option_name = None):
        self._namespace = namespace
        self._option_name = option_name
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of OptionSepcification from structured json data"""
        os = cls()
        os._namespace = json_data['Namespace']
        os._option_name = json_data['OptionName']
        return os
            
    @property
    def namespace(self):
        """ <p> A unique namespace identifying the option's associated AWS resource. </p>\n"""
        return self._namespace
    
    @property
    def option_name(self):
        """ <p> The name of the configuration option. </p>\n"""
        return self._option_name 


class ConfigurationOptionSetting(BaseModel):
    """ <p> A specification identifying an individual configuration option along with its
         current value. </p>\n
    """

    def __init__(self, namespace = None, option_name = None, value = None):
        self._namespace = namespace
        self._option_name = option_name
        self._value = value
    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of ConfigurationOptionSetting from structured json data"""
        cos = cls()
        cos._namespace = json_data['Namespace']
        cos._option_name = json_data['OptionName']
        cos._value = json_data['Value']
        return cos
            
    @property
    def namespace(self):
        """ <p> A unique namespace identifying the option's associated AWS resource. </p>\n"""
        return self._namespace
    
    @property
    def option_name(self):
        """ <p> The name of the configuration option. </p>\n"""
        return self._option_name 

    @property
    def value(self):
        """ <p> The current value for the configuration option. </p>\n"""
        return self._value 

    @namespace.setter
    def namespace(self, namespace):
        self._namespace = namespace
    
    @option_name.setter
    def option_name(self, option_name):
        self._option_name = option_name

    @value.setter
    def value(self, value):
        self._value = value


class ConfigurationSettingsDescription(BaseModel):
    """ <p> Describes the settings for a configuration set. </p>\n"""

    def __init__(self):
        self._application_name = None
        self._date_created = None
        self._date_created_raw = None
        self._date_updated = None
        self._date_updated_raw = None
        self._deployment_status = None
        self._description = None
        self._environment_name = None
        self._option_settings = None
        self._solution_stack_name = None
        self._template_name = None

    
    @classmethod
    def from_json(cls, json_data):
        """ Create instance of ConfigurationSettingsDescription from structured json data"""
        csd = cls()
        csd._application_name = json_data['ApplicationName']
        csd._date_created_raw = json_data['DateCreated']
        csd._date_created = datetime.datetime.fromtimestamp(csd._date_created_raw).\
            replace(microsecond=0)
        csd._date_updated_raw = json_data['DateUpdated']
        csd._date_updated = datetime.datetime.fromtimestamp(csd._date_updated_raw).\
            replace(microsecond=0)
        csd._deployment_status = json_data['DeploymentStatus']
        csd._description = json_data['Description']
        csd._environment_name = json_data['EnvironmentName']
        csd._option_settings = extract_list_elements(json_data, 'OptionSettings', ConfigurationOptionSetting.from_json)
        csd._solution_stack_name = json_data['SolutionStackName']
        csd._template_name = json_data['TemplateName']
        return csd

    @property
    def application_name(self):
        """ <p> The name of the application associated with this configuration set. </p>\n"""
        return self._solution_stack_name

    @property
    def date_created(self):
        """ <p> The date (in UTC time) when this configuration set was created. </p>\n"""
        return self._date_created

    @property
    def date_updated(self):
        """ <p> The date (in UTC time) when this configuration set was last modified. </p>\n"""
        return self._date_updated

    @property
    def deployment_status(self):
        """ <p>
           If this configuration set is associated with an environment, the 
           <code>DeploymentStatus</code> parameter indicates
           the deployment status of this configuration set:
          </p>
        <enumValues>
           <value name="null">
              <p>
                 <code>null</code>: This configuration is not associated with a running
                 environment.
              </p>
           </value>
           <value name="pending">
              <p>
                 <code>pending</code>: This is a draft configuration that is not deployed
                 to the
                 associated environment but is in the process of deploying.
              </p>
           </value>
           <value name="deployed">
              <p>
                 <code>deployed</code>: This is the configuration that is currently deployed
                 to the associated running environment.
              </p>
           </value>
           <value name="failed">
              <p>
                 <code>failed</code>: This is a draft configuration, that
                 failed to successfully deploy.
              </p>
           </value>
        </enumValues>"""
        return self._deployment_status

    @property
    def description(self):
        """ <p> Describes this configuration set. </p>\n"""
        return self._description

    @property
    def environment_name(self):
        """ <p> If not <code>null</code>, the name of the environment for this configuration set.
      </p>\n"""
        return self._environment_name

    @property
    def option_settings(self):
        """ <p> A list of the configuration options and their values in this configuration
         set. </p>\n"""
        return self._option_settings

    @property
    def template_name(self):
        """ <p> The name of the solution stack this configuration set uses. </p>\n"""
        return self._template_name

    @property
    def solution_stack_name(self):
        """ <p> The name of the solution stack this configuration set uses. </p>\n"""
        return self._solution_stack_name
            

class ValidationMessage(BaseModel):
    """ <p> An error or warning for a desired configuration option value. </p>\n"""

    def __init__(self):
        self._message = None
        self._namespace = None
        self._option_name = None
        self._severity = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of ValidationMessage from structured json data"""
        vm = cls()

        vm._message = json_data['Message']
        vm._namespace = json_data['Namespace']
        vm._option_name = json_data['OptionName']
        vm._severity = json_data['Severity']
        
        return vm
    
    @property
    def message(self):
        """ <p> A message describing the error or warning. </p>\n"""        
        return self._message

    @property
    def severity(self):
        """ <p> An indication of the severity of this message: </p>
      <enumValues>
         <value name="error">
            <p>
               error: This message indicates that this is not a valid setting for an option.
            </p>
         </value>
         <value name="warning">
            <p>
               warning: This message is providing information you should take into
               account.
                </p>
         </value>
      </enumValues>"""
        return self._severity

    @property
    def namespace(self):
        return self._namespace

    @property
    def option_name(self):
        return self._option_name


class EnvironmentInfoDescription(BaseModel):
    """ <p>The information retrieved from the Amazon EC2 instances.</p>\n"""

    def __init__(self):
        self._info_type = None
        self._ec2_instance_id = None
        self._message = None
        self._sample_timestamp = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of EnvironmentInfoDescription from structured json data"""
        eid = cls()
        eid._info_type = json_data['InfoType']
        eid._ec2_instance_id = json_data['Ec2InstanceId']
        eid._message = json_data['Message']
        eid._sample_timestamp = json_data['SampleTimestamp']

        return eid

    @property
    def info_type(self):
        """ <p>The type of information retrieved.</p>\n"""
        return self._info_type

    @property
    def ec2_instance_id(self):
        """ <p>The Amazon EC2 Instance ID for this information.</p>\n"""
        return self._ec2_instance_id

    @property
    def sample_timestamp(self):
        """ <p>The time stamp when this information was retrieved.</p>\n"""
        return self._sample_timestamp

    @property
    def message(self):
        """ <p>The retrieved information.</p>\n"""
        return self._message
