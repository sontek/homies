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

import random
import time
try:
    import simplejson as json
except ImportError:
    import json
import logging


from . import requests
from scli import prompt    
from scli.constants import ServiceDefault
from lib.aws.exception import AwsErrorCode
from lib.aws.exception import AwsServiceException
from lib.aws.http_client import HTTP_GET, HTTP_POST
from lib.aws.signature import AWSSignature    
from lib.utility import misc


log = logging.getLogger('aws') 




#----------------------------------------------------
# Helper methods
def _exponential_backoff(max_tries):
    """
    Returns a series of floating point numbers between 0 and 2^i-1 for i in 0 to max_tries
    """
    return [random.random() * (2**i - 1) for i in range(0, max_tries)]


def _extend_backoff(durations):
    """
    Adds another exponential delay time to a list of delay times
    """
    durations.append(random.random() * (2**len(durations) - 1))

def _extractAwsErrorMessage(e):
    try :
        eDoc = json.loads(e.read().decode('utf-8'))['Error']
        return (misc.to_unicode(eDoc['Code']), misc.to_unicode(eDoc['Message']))
    except :
        return ('Error', '{0}:{1}'.format(e.code, e.msg))

    
    
class AWSQueryClient(object):
    '''
    Client implementing AWS/Query protocol
    '''
    def __init__(self, accessKey, secretKey, endpoint, region, service_name,
                 result_format, signature_version, api_version):
        '''
        Constructor
        '''
        self._accessKey = accessKey
        self._secretKey = secretKey
        self._endpoint = endpoint
        self._result_format = result_format
        self._signature_version = signature_version
        self._api_version = api_version
        
        self._signer = AWSSignature(accessKey, secretKey, endpoint, region, 
                                    service_name, api_version, signature_version)


    def call(self, params, format_ = None,  method = HTTP_POST):
        
        if format_ is None: 
            format_ = self._result_format

        if method == HTTP_GET:        
            raw_headers={'Accept' : 'application/' + format_,
                     'Accept-Charset' : ServiceDefault.CHAR_CODEC,
                     'User-Agent' : ServiceDefault.USER_AGENT}
            url, headers = self._signer.construct_get_url(params.get_dict(), raw_headers)
                        
            result = self._request_with_retry(HTTP_GET, url, headers)
            
        elif method == HTTP_POST:
            url = self._endpoint if self._endpoint.endswith('/') else self._endpoint + '/'
            raw_headers = {'Accept' : 'application/' + format_,
#                       'Accept-Charset' : ServiceDefault.CHAR_CODEC,
                       'User-Agent' : ServiceDefault.USER_AGENT,
                       'Content-Type' : 'application/x-www-form-urlencoded',
                       'Expect' : '100-continue'}
            post_data, headers = self._signer.construct_post_data(params.get_dict(), raw_headers)

            result = self._request_with_retry(HTTP_POST, url, headers, post_data)

        return result


    def _request_with_retry(self, verb, url, headers, data, max_tries = 5):
        aws_code = None
        http_code = None    
        durations = _exponential_backoff(max_tries)
        
        for index, length in enumerate(durations):
            if length > 0:
                log.debug('Sleeping for %f seconds before retrying', length)
                time.sleep(length)
    
            try:
                if verb == HTTP_GET:
                    response = requests.get(url, headers = headers, verify=True)
                elif verb == HTTP_POST:
                    response = requests.post(url, data = data, headers = headers, verify=True)
                else:
                    raise AttributeError('Not supported HTTP action "{0}".'.format(verb))

                # check status code
                if response.status_code != 200:
                    http_code = response.status_code
                    try:
                        aws_code = response.json()['Error']['Code']
                        message = response.json()['Error']['Message']
                    except TypeError as ex:
                        raise AttributeError('HTTP {0}: {1}'.format(http_code, response.text))

                    if http_code < 500 and aws_code != AwsErrorCode.Throttling:
                        raise AwsServiceException(message, aws_code, http_code)
                    
                    elif AwsErrorCode.Throttling == aws_code or http_code == 503:
                        prompt.info('Request is throttled.')
                        _extend_backoff(durations)
                        
                    if index + 1 < len(durations):
                        prompt.info('Error {0}:{1}. Wait {2} seconds before retry.'.\
                                     format(aws_code, message, durations[index + 1]))
                    log.error(message + ' ' + aws_code)
                    last_message = message
                    
                else:
                    if response.json() is None:
                        raise ValueError('Cannot parse response form {0}.'.format(response.url))
                    return response
            
            except requests.exceptions.SSLError as ex:
                log.error('SSL Error: %s', ex)
                raise
            
            except (requests.exceptions.HTTPError, 
                    requests.exceptions.ConnectionError,
                    requests.exceptions.Timeout) as ex:
                log.error(ex)
                last_message = ex
                
        else:
            if aws_code is None:
                aws_code = ''
            if http_code is None:
                http_code = ''
            raise AwsServiceException(last_message, aws_code, http_code)
            
