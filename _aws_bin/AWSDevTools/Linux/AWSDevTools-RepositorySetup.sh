#!/bin/sh

# Copyright 2012 Amazon.com, Inc. or its affiliates. All Rights
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

SCRIPTDIR="$( cd "$( dirname "$0" )" && pwd )"
GIT_DIRECTORY=$GIT_DIR
if [ -z "$GIT_DIRECTORY" ]; then GIT_DIRECTORY=.git; fi

rm -rf $GIT_DIRECTORY/AWSDevTools
cp -r "$SCRIPTDIR"/scripts $GIT_DIRECTORY/AWSDevTools

git config alias.aws.elasticbeanstalk.remote "!$GIT_DIRECTORY/AWSDevTools/aws.elasticbeanstalk.push --remote-url"
git config aws.endpoint.us-east-1 git.elasticbeanstalk.us-east-1.amazonaws.com
git config aws.endpoint.ap-northeast-1 git.elasticbeanstalk.ap-northeast-1.amazonaws.com
git config aws.endpoint.eu-west-1 git.elasticbeanstalk.eu-west-1.amazonaws.com
git config aws.endpoint.us-west-1 git.elasticbeanstalk.us-west-1.amazonaws.com
git config aws.endpoint.us-west-2 git.elasticbeanstalk.us-west-2.amazonaws.com
git config aws.endpoint.ap-southeast-1 git.elasticbeanstalk.ap-southeast-1.amazonaws.com
git config aws.endpoint.ap-southeast-2 git.elasticbeanstalk.ap-southeast-2.amazonaws.com
git config aws.endpoint.sa-east-1 git.elasticbeanstalk.sa-east-1.amazonaws.com
git config alias.aws.elasticbeanstalk.push "!$GIT_DIRECTORY/AWSDevTools/aws.elasticbeanstalk.push"
git config alias.aws.push '!git aws.elasticbeanstalk.push'
git config alias.aws.elasticbeanstalk.config "!$GIT_DIRECTORY/AWSDevTools/aws.elasticbeanstalk.config"
git config alias.aws.config '!git aws.elasticbeanstalk.config'
git config alias.aws.elasticbeanstalk.createapplicationversion "!$GIT_DIRECTORY/AWSDevTools/aws.elasticbeanstalk.createapplicationversion"
git config alias.aws.createapplicationversion '!git aws.elasticbeanstalk.createapplicationversion'
