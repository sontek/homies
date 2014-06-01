from aws.ini import * 
from subprocess import check_output
from subprocess import CalledProcessError
import os

#-*-python-*-

# Copyright 2014 Amazon.com, Inc. or its affiliates. All Rights
# Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"). You
# may not use this file except in compliance with the License. A copy
# of the License is located at
#
#   http://aws.amazon.com/apache2.0/
#
# or in the "license" file accompanying this file. This file is
# distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF
# ANY KIND, either express or implied. See the License for the
# specific language governing permissions and limitations under the
# License.

class ElasticBeanstalkConfig:

    def __init__(self, root_directory):
	self.root_directory = root_directory

	self.EB_CONFIG_KEYS = { 
		"region": "Region",
		"application_name": "ApplicationName",
		"environment_name": "EnvironmentName",
		"dev_tools_endpoint": "DevToolsEndpoint"
	}

	self.CREDENTIAL_KEYS = { 
		"access_key_id" : "AWSAccessKeyId",
		"secret_access_key" : "AWSSecretKey"
	}

	self.GIT_CONFIG_KEYS = { 
		"region" : "aws.region",
		"application_name" : "aws.elasticbeanstalk.application",
		"environment_name" : "aws.elasticbeanstalk.environment",
		"dev_tools_endpoint" : "aws.elasticbeanstalk.host",
		"access_key_id" : "aws.accesskey",
		"secret_access_key" : "aws.secretkey"
	}

	self.KNOWN_REGIONS = [
		"us-east-1",
		"us-west-1",
		"us-west-2",
		"eu-west-1",
		"ap-northeast-1",
		"ap-southeast-1",
		"ap-southeast-2",
		"sa-east-1"
	]

	self.eb_config_file = None
	self.eb_config_settings = None
	self.credential_file_path = None 
	self.credential_file = None
	self.credential_settings = None

	self.set_eb_config_file()
	self.set_eb_config_settings()
	self.set_credential_file_path()
	self.set_credential_file()
	self.set_credential_settings()

    def set_eb_config_file(self):
	if not self.eb_config_file:
	    self.eb_config_file = INI(os.path.join(self.root_directory, ".elasticbeanstalk", "config"))

    def set_eb_config_settings(self):
	if self.eb_config_file:
	    self.eb_config_settings = self.eb_config_file["global"]

    def should_write_credential_file(self):
	return (not self.credential_file_configured() and os.getenv('HOME') and not os.path.isfile(self.default_credential_file_path()))

    def default_credential_file_path(self):
	if os.getenv('HOME'):
	    return os.path.join(os.getenv('HOME'), ".elasticbeanstalk", "aws_credential_file")
	return None

    def set_credential_file_path(self):
	self.credential_file_path = os.getenv('AWS_CREDENTIAL_FILE') or (self.eb_config_settings and self.eb_config_settings.get("AwsCredentialFile")) or self.default_credential_file_path()

    def set_credential_file(self):
	if not self.credential_file and self.credential_file_exists():
	    self.credential_file = INI(self.credential_file_path, False)

    def credential_file_exists(self):
	return self.credential_file_path and os.path.isfile(self.credential_file_path)

    def credential_file_configured(self):
	return (os.getenv('AWS_CREDENTIAL_FILE') or (self.eb_config_file and self.eb_config_settings.get("AwsCredentialFile")))

    def credential_file_readable(self): 
	if self.credential_file_path:
	    return os.access(self.credential_file_path, os.R_OK)
	return False

    def set_credential_settings(self): 
	if self.credential_file:
	    self.credential_settings = self.credential_file["global"]

    def git_setting(self, key):
	value = None
	try:
	    value = check_output(["git config", "--get {0}".format(key)])
	    if value:
		value = value.strip()
	except CalledProcessError:
	    return None
	except OSError:
	    return None

	return value

    def dev_tools_endpoint(self):
	endpoint = self.eb_endpoint_settings["DevToolsEndpoint"]
	if not endpoint:
	    endpoint = dev_tools_endpoint_default()
	return endpoint

    def dev_tools_endpoint_default(self, region):
	if region in self.KNOWN_REGIONS:
	    return "git.elasticbeanstalk.{0}.amazonaws.com".format(region)
	return None

    def branch_mappings(self):
	return self.eb_config_file["branches"]

    def environment_name(self):
	return self.eb_default("environment_name")

    def access_key(self):
	return self.credential_default("access_key_id")

    def secret_key(self):
	return self.credential_default("secret_access_key")

    def application_name(self):
	return self.eb_default("application_name")

    def region(self):
	return self.eb_default("region")

    def eb_default(self, attr):
	return self.default_value(attr, self.eb_config_settings, self.EB_CONFIG_KEYS)

    def credential_default(self, attr):
	return self.default_value(attr, self.credential_settings, self.CREDENTIAL_KEYS)

    def default_value(self, attr, settings, dict_to_check):
        if settings and (attr in dict_to_check):
	    if dict_to_check[attr] in settings:
		val = settings[dict_to_check[attr]]
		if val:
		    return val
	
	if attr in self.GIT_CONFIG_KEYS:
	    return self.git_setting(self.GIT_CONFIG_KEYS[attr])

	return None

    def write_settings(self, settings):
	eb_settings_to_write = {}
	credential_settings_to_write = {}

	if settings:
	    for key, value in settings.items():
		if key in self.EB_CONFIG_KEYS:
		    eb_settings_to_write[self.EB_CONFIG_KEYS[key]] = value

	eb_dir = os.path.dirname(self.eb_config_file.filename)
	if not os.path.exists(eb_dir):
	    os.makedirs(eb_dir)

	if eb_settings_to_write:
	    self.eb_config_file.write_settings("global", eb_settings_to_write)
	    self.set_eb_config_settings()

	if self.should_write_credential_file():
	    cred_dir = os.path.dirname(self.default_credential_file_path())
	    if not os.path.exists(cred_dir):
		os.makedirs(cred_dir)

	    self.credential_file = INI(self.default_credential_file_path(), False)
	    self.credential_file.write_settings("global", {
		    self.CREDENTIAL_KEYS["access_key_id"] :  settings.get("access_key_id",""),
		    self.CREDENTIAL_KEYS["secret_access_key"] : settings.get("secret_access_key", "")
		    })

	    self.eb_config_file.write_settings("global", { "AwsCredentialFile" : self.credential_file_path})
	    self.set_credential_settings()
