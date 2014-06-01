###########################################################################
# AWS Elastic Beanstalk Command Line Client
# Copyright 2011 Amazon.com, Inc. or its affiliates. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the “License”). You
# may not use this file except in compliance with the License. A copy of
# the License is located at
#
#    http://aws.amazon.com/apache2.0/
#
# or in the “license” file accompanying this file. This file is
# distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF
# ANY KIND, either express or implied. See the License for the specific
# language governing permissions and limitations under the License.
#
$:.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))

require 'aws/elasticbeanstalk'

ca_bundle_file = File.join(File.dirname(__FILE__), '..', 'ca-bundle.crt')
abort "CA Bundle required" unless File.exists?(ca_bundle_file)

config = {
  :endpoint => ENV['ELASTICBEANSTALK_URL'] || 'https://elasticbeanstalk.us-east-1.amazonaws.com',
  :region => ENV['ELASTICBEANSTALK_REGION'] || nil,
  :timeout  => 30,
  :ca_file => ca_bundle_file
}

if ENV['AWS_CREDENTIAL_FILE'] && File.exists?(ENV['AWS_CREDENTIAL_FILE'])
  cred_file = File.open(ENV['AWS_CREDENTIAL_FILE'], 'r')

  cred_file.each do |line|
    config[:aws_access_key] = $1 if line[/AWSAccessKeyId=(.*)/]
    config[:aws_secret_key] = $1 if line[/AWSSecretKey=(.*)/]
  end
elsif ENV['AMAZON_ACCESS_KEY_ID'] && ENV['AMAZON_SECRET_ACCESS_KEY']
  config[:aws_access_key] = ENV['AMAZON_ACCESS_KEY_ID']
  config[:aws_secret_key] = ENV['AMAZON_SECRET_ACCESS_KEY']
end

config[:aws_access_key].strip! if config[:aws_access_key] 
config[:aws_secret_key].strip! if config[:aws_secret_key]

abort "AWS credentials required" unless config[:aws_access_key] && config[:aws_secret_key]

@elasticbeanstalk = AWS::ElasticBeanstalk::Client.new(config)
