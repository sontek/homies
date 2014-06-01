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
from datetime import datetime
import urlparse
import urllib
import base64
import hashlib
import hmac
import logging
import operator
import re

from http_client import HTTP_GET, HTTP_POST
from ..utility import misc
 

log = logging.getLogger('aws') 

class AWSSignature:
    
    SigV2 = u'2'
    SigV4 = u'4'
    
    def __init__(self, accesskey, secretkey, endpoint, 
                 region, service_name, api_version, 
                 signature_version = SigV2, 
                 terminator = 'aws4_request'):
        '''
        Constructor
        '''
        self._accesskey = accesskey
        self._secretkey = secretkey
        self._endpoint = endpoint
        self._region = region
        self._service_name = service_name
        self._api_version = api_version        
        self._signature_version = signature_version
        self._terminator = terminator


    def v2_sign(self, verb, request_string):
        # This assumes path is always '/'.
        stringToSign = verb + '\n' + urlparse.urlsplit(self._endpoint)[1] + '\n/\n' + request_string
    
        return base64.b64encode(hmac.new(misc.to_bytes(self._secretkey), 
                                         misc.to_bytes(stringToSign), 
                                         hashlib.sha256).digest())


    def v4_sign(self, verb, query_string, headers, region, service_name, timestamp):
        #TODO: Now this assumes path is always '/'.
        formatted_timestamp = timestamp.strftime('%Y%m%dT%H%M%SZ')
        date = timestamp.strftime('%Y%m%d')
        scope =  date + '/' + self._region + '/' + self._service_name + '/' + self._terminator

        # Process headers        
        headers['Host'] = urlparse.urlsplit(self._endpoint).netloc
        if 'Date' in headers:
            del headers['Date']
        headers['X-Amz-Date'] = formatted_timestamp
        (canonical_headers, signed_headers) = self._canonicalize_headers(headers)

        # Generate canonical query string for signature                        
        canonical_request = verb + '\n/\n'
        canonical_request += (query_string if verb == HTTP_GET else '') + '\n'
        canonical_request += canonical_headers + '\n' + signed_headers + '\n'
        canonical_request += hashlib.sha256(query_string.encode('utf-8') \
                                            if verb == HTTP_POST else '').hexdigest()
                                            
        # Generate string to sign                                            
        string_to_sign = 'AWS4-HMAC-SHA256\n' + formatted_timestamp + '\n' + scope + '\n' \
            + hashlib.sha256(canonical_request.encode('utf-8')).hexdigest()

        # Generate signing key
        derived_key = hmac.new((u'AWS4' + self._secretkey).encode('utf-8'),
                               date.encode('utf-8'), hashlib.sha256).digest()
        derived_key = hmac.new(derived_key, 
                               self._region.encode('utf-8'), hashlib.sha256).digest()
        derived_key = hmac.new(derived_key, 
                               self._service_name.encode('utf-8'), hashlib.sha256).digest()
        derived_key = hmac.new(derived_key, 
                               u'aws4_request'.encode('utf-8'), hashlib.sha256).digest()

        # Sign
        signature = hmac.new(derived_key, 
                             string_to_sign.encode('utf-8'), hashlib.sha256).hexdigest()
        
        # Fill signature into header (recommended way)
        credential = self._accesskey + '/' + scope
        headers['Authorization'] = 'AWS4-HMAC-SHA256 Credential=%s,SignedHeaders=%s,Signature=%s' \
            % (credential, signed_headers, signature)

        return headers
        
            
    def construct_get_url(self, params, headers):
        host = self._endpoint if self._endpoint.endswith(u'/') else self._endpoint + u'/'

        if self._signature_version == self.SigV2:
            query_string = self._generate_v2_query_string(params)
            
            return (misc.to_bytes(host + '?' + query_string + '&Signature=' \
                                 + urllib.quote(self.v2_sign(HTTP_GET, query_string))),
                    headers) 

        elif self._signature_version == self.SigV4:
            timestamp = datetime.utcnow().isoformat()
            query_string = self._generate_v4_query_string(params, timestamp)
            
            return (misc.to_bytes(host + '?' + query_string),
                    self.v4_sign(HTTP_POST, query_string, headers, self._region, 
                                 self._service_name, timestamp))
        
        else:
            raise AttributeError(u'Not supported signature version: "{0}"'.\
                                 format(self._signature_version))
        
    
    
    def construct_post_data(self, params, headers):

        if self._signature_version == self.SigV2:
            query_string = self._generate_v2_query_string(params)
            
            return (misc.to_bytes(query_string + '&Signature=' \
                                 + urllib.quote(self.v2_sign(HTTP_POST, query_string))),
                    headers)
            
        elif self._signature_version == self.SigV4:
            timestamp = datetime.utcnow()
            query_string = self._generate_v4_query_string(params, timestamp.isoformat())
            return (query_string,
                    self.v4_sign(HTTP_POST, query_string, headers, self._region, 
                                 self._service_name, timestamp))
        else:
            raise AttributeError(u'Not supported signature version: "{0}"'.\
                                 format(self._signature_version))
    

    def _generate_v2_query_string(self, params):
        data = dict(params)
        data[u'SignatureVersion'] = self._signature_version
        data[u'Version'] = self._api_version
        data[u'AWSAccessKeyId'] = self._accesskey
        data[u'Timestamp'] = datetime.utcnow().isoformat()
        data[u'SignatureMethod'] = u'HmacSHA256'
        data[u'ContentType'] = u'JSON'
        return self._construct_query(data)        


    def _generate_v4_query_string(self, params, timestamp):
        data = dict(params)
        data[u'Version'] = self._api_version
        data[u'Timestamp'] = timestamp
        data[u'ContentType'] = u'JSON'
        return self._construct_query(data)        
        
       
    
    def _canonicalize_uri(self, uri):
        split = urlparse.urlsplit(uri)
        if not split.path:
            return '/'
        path = urlparse.urlsplit(urlparse.urljoin('http://foo.com', \
                                                  split.path.lstrip('/'))).path.rstrip('/')
        return urllib.quote(misc.to_bytes(path), '/~') if path else '/'


    def _canonicalize_headers(self, headers):
        canon_headers = {}
        for key, value in ((key.lower(), re.sub(r'(?su)[\s]+', ' ', value).strip()) \
                           for key, value in headers.iteritems()):
            if key in canon_headers:
                canon_headers[key] = canon_headers[key] + ',' + value
            else:
                canon_headers[key] = value

        sorted_entries = sorted(canon_headers.iteritems(), key=operator.itemgetter(0))

        return ('\n'.join((':'.join(entry) for entry in sorted_entries)) \
                + '\n', ';'.join((entry[0] for entry in sorted_entries)))


    def _construct_query(self, params):
        if not params:
            return ''        
        
        ret_str = ''
        for k, vs in sorted(params.iteritems(), key=operator.itemgetter(0)):
            if isinstance(vs, list):
                for v in sorted(vs):
                    ret_str += '&'.join(urllib.quote(misc.to_bytes(k), safe='~') \
                                        + '=' + urllib.quote(misc.to_bytes(v), safe='~'))
            else:
                if ret_str:
                    ret_str += '&'
                ret_str += urllib.quote(misc.to_bytes(k), safe='~') \
                            + '=' + urllib.quote(misc.to_bytes(vs), safe='~')

        return ret_str        
    
    