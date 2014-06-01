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



APP_EXIST_RE = u'Application .+ already exist'
VER_EXIST_RE = u'Version .+ already exist'
ENV_EXIST_RE = u'Environment (\w|-)+ already exist'


APP_IN_ACTION_RE = u'Cannot create .+ is currently .+'


APP_HAS_RUNNING_ENV = u'Unable to delete application .+ because it is deployed to running environments.'