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

from http.client import HTTPSConnection
import os
import socket
import ssl
from urllib.request import HTTPSHandler

from scli.constants import CABundle
from lib.utility import shell_utils


HTTP_GET = 'GET'
HTTP_POST = 'POST'   

class CaValidationHttpsConnection(HTTPSConnection):
    '''Override HTTPSConnection to verify server certification'''
    
    def connect(self):
        sock = socket.create_connection((self.host, self.port),
                                        self.timeout, self.source_address)
        if self._tunnel_host:
            self.sock = sock
            self._tunnel()

        self.sock = ssl.wrap_socket(sock, 
                                    ssl_version = ssl.PROTOCOL_TLSv1,
                                    cert_reqs = ssl.CERT_REQUIRED, 
                                    ca_certs = os.path.join(shell_utils.ori_path(),
                                                            CABundle.Path,
                                                            CABundle.Name))
        

class CaValidationHttpsHandler(HTTPSHandler):
    '''Override HTTPSHandler to use CaValidationHttpsConnection for connection'''
            
    def https_open(self, req):
        return self.do_open(CaValidationHttpsConnection, req)    
    