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


from lib.utility import misc

class Request(object):
    '''
    Convert and store IAM request parameters
    '''
    
    def __init__(self):
        self._request = dict()

    def _set_to_list(self, name_set):
        name_list = []
        if isinstance(name_set, set):
            for name in name_set:
                name_list.append(unicode(name))
        else:
            name_list.append(unicode(name_set))
        return name_list         
    
    def __repr__(self):
        try:
            text = u'Request API: {0}. \nParameters: [\n'.format(self._request[u'Operation'])
        except:
            text = u'Parameters:[\n'
        
        for key,value in self._request.iteritems():
            text = text + u' {0} : {1}\n'.format(key, value)
        text = text + u']'
        
        return text
    
    def get_dict(self):
        return self._request
    
    def set_action(self, name):
        self._request[u'Action'] = misc.to_unicode(name)

    def set_path(self, string):
        self._request[u'Path'] = misc.to_unicode(string)

    def set_role_name(self, string):
        self._request[u'RoleName'] = misc.to_unicode(string)

    def set_assume_role_policy_document(self, string):
        self._request[u'AssumeRolePolicyDocument'] = misc.to_unicode(string)

    def set_instance_profile_name(self, string):
        self._request[u'InstanceProfileName'] = misc.to_unicode(string)

    def set_marker(self, string):
        self._request[u'Marker'] = misc.to_unicode(string)

    def set_max_items(self, string):
        self._request[u'MaxItems'] = misc.to_unicode(string)

    def set_path_prefix(self, string):
        self._request[u'PathPrefix'] = misc.to_unicode(string)

    def set_policy_name(self, string):
        self._request[u'PolicyName'] = misc.to_unicode(string)

    def set_policy_document(self, string):
        self._request[u'PolicyDocument'] = misc.to_unicode(string)
    
        
class Response(object):
    
    def __init__(self, request_id, result = None, marker = None):
        self._request_id = request_id
        self._result = result
        self._marker = marker
        
    def __repr__(self):
        return u'API Response.\n Request ID: {0}\n Results: {1}'.\
            format(self.request_id, misc.collection_to_string(self._result))
        
    @property
    def request_id(self):
        return self._request_id
    
    @property
    def result(self):
        return self._result
    
    @property
    def marker(self):
        return self._marker
   
