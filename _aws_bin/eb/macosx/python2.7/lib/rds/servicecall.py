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

from lib.utility import misc
from lib.aws.webservice import AWSQueryClient
from lib.aws.webservice import AWSSignature
from lib.aws.exception import AwsErrorCode
from lib.aws.exception import AwsServiceException
from lib.aws.exception import MissingParameterException
from lib.aws.exception import InsufficientPrivilegesException
from lib.aws.exception import InvalidParameterValueException
from lib.aws.exception import OptInRequiredException
from lib.rds.exception import RdsDBInstanceNotFoundException
from lib.rds.exception import RdsDBSnapshotNotFoundException
from lib.rds.model import DBEngineVersion
from lib.rds.model import DBInstance
from lib.rds.model import DBSnapshot
from lib.rds.request import Request
from lib.rds.request import Response



log = _logging.getLogger('aws')

class RdsClient(object):
    '''
    Web service client for RDS
    '''
    _signature_version = AWSSignature.SigV2
    _api_version = u'2012-04-23'
    _service_name = u'rds'

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
                      
            return return_msg.json()
            
        except AwsServiceException as ex:
            log.debug(misc.to_unicode(ex))

            # Translate general RDS exception
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
    def describe_db_engine_versions(self, db_parameter_group_family = None, default_only = None, 
                                    engine = None, engine_version = None, 
                                    list_supported_character_sets = None,
                                    marker = None, max_records = None):
        request = Request()
        request.set_action(u'DescribeDBEngineVersions')
        if db_parameter_group_family is not None: 
            request.set_db_parameter_group_family(db_parameter_group_family)
        if default_only is not None: 
            request.set_default_only(default_only)
        if engine is not None: 
            request.set_engine(engine)
        if engine_version is not None: 
            request.set_engine_version(engine_version)
        if list_supported_character_sets is not None: 
            request.set_list_supported_character_sets(list_supported_character_sets)
        if marker is not None: 
            request.set_marker(marker)
        if max_records is not None: 
            request.set_max_records(max_records)
        
        try:    
            response = self.call(request)
        except AwsServiceException:
            raise 
        
        results = response[u'DescribeDBEngineVersionsResponse']\
            [u'DescribeDBEngineVersionsResult'][u'DBEngineVersions']
        marker = response[u'DescribeDBEngineVersionsResponse']\
            [u'DescribeDBEngineVersionsResult'][u'Marker']
        request_id = response[u'DescribeDBEngineVersionsResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        engine_versions = []
        for result in results:
            engine_versions.append(DBEngineVersion.from_json(result))
        return Response(request_id, engine_versions, marker)


    def describe_db_instances(self, db_instance_identifier = None, 
                              marker = None, max_records = None):
        request = Request()
        request.set_action(u'DescribeDBInstances')
        if db_instance_identifier is not None: 
            request.set_db_instance_identifier(db_instance_identifier)
        if marker is not None: 
            request.set_marker(marker)
        if max_records is not None: 
            request.set_max_records(max_records)
        
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code == 404:
                raise RdsDBInstanceNotFoundException(ex)
            raise
        
        results = response[u'DescribeDBInstancesResponse']\
            [u'DescribeDBInstancesResult'][u'DBInstances']
        marker = response[u'DescribeDBInstancesResponse']\
            [u'DescribeDBInstancesResult'][u'Marker']
        request_id = response[u'DescribeDBInstancesResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        instances = []
        for result in results:
            instances.append(DBInstance.from_json(result))
        return Response(request_id, instances, marker)



    def describe_db_snapshots(self, db_instance_identifier = None, 
                              db_snapshot_identifier = None, 
                              snapshot_type = None, marker = None, max_records = None):
        request = Request()
        request.set_action(u'DescribeDBSnapshots')
        if db_instance_identifier is not None: 
            request.set_db_instance_identifier(db_instance_identifier)
        if db_snapshot_identifier is not None: 
            request.set_db_snapshot_identifier(db_snapshot_identifier)
        if snapshot_type is not None: 
            request.set_snapshot_type(snapshot_type)
        if marker is not None: 
            request.set_marker(marker)
        if max_records is not None: 
            request.set_max_records(max_records)
        
        try:    
            response = self.call(request)
        except AwsServiceException as ex:
            if ex.code == 404:
                raise RdsDBSnapshotNotFoundException(ex)
            raise
        
        results = response[u'DescribeDBSnapshotsResponse']\
            [u'DescribeDBSnapshotsResult'][u'DBSnapshots']
        marker = response[u'DescribeDBSnapshotsResponse']\
            [u'DescribeDBSnapshotsResult'][u'Marker']
        request_id = response[u'DescribeDBSnapshotsResponse']\
            [u'ResponseMetadata'][u'RequestId']
                
        snapshots = []
        for result in results:
            snapshots.append(DBSnapshot.from_json(result))
        return Response(request_id, snapshots, marker)
    
    
