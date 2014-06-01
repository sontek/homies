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
        ret = u"\n"
        for attr, value in self.__dict__.iteritems():
            ret += "(" + unicode(attr) + ":" + unicode(value) + ")\n"
        return ret    


class Endpoint(BaseModel):

    def __init__(self):
        self._address = None
        self._port = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of Endpoint from structured json data"""
        dbpgs = cls()        
        if json_data is not None:
            dbpgs._address = json_data[u'Address']
            dbpgs._port = json_data[u'Port']

        return dbpgs

    @property
    def address(self):
        return self._address

    @property
    def port(self):
        return self._port


class DBParameterGroupStatus(BaseModel):

    def __init__(self):
        self._db_parameter_group_name = None
        self._parameter_apply_status = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of DBParameterGroupStatus from structured json data"""
        dbpgs = cls()        
        
        dbpgs._db_parameter_group_name = json_data[u'DBParameterGroupName']
        dbpgs._parameter_apply_status = json_data[u'ParameterApplyStatus']
        
        return dbpgs

    @property
    def db_parameter_group_name(self):
        return self._db_parameter_group_name

    @property
    def parameter_apply_status(self):
        return self._parameter_apply_status


class DBSecurityGroupMembership(BaseModel):

    def __init__(self):
        self._db_security_group_name = None
        self._status = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of DBSecurityGroupMembership from structured json data"""
        dbpgs = cls()        
        
        dbpgs._db_security_group_name = json_data[u'DBSecurityGroupName']
        dbpgs._status = json_data[u'Status']
        
        return dbpgs

    @property
    def db_security_group_name(self):
        return self._db_security_group_name

    @property
    def status(self):
        return self._status


class DBInstance(BaseModel):
    #TODO: not including all attributes yet
    
    def __init__(self):
        self._allocated_storage = None
        self._auto_minor_version_upgrade = None
        self._availability_zone = None
        self._backup_retention_period = None
        self._character_set_name = None
        self._db_instance_class = None
        self._db_instance_identifier = None     
        self._dbi_nstance_status = None
        self._db_name = None
        self._db_parameter_groups = None
        self._db_security_groups = None
        self._endpoint = None
        self._engine = None     
        self._engine_version = None     
        self._instance_create_time = None     
        self._latest_restorable_time = None
        self._license_model = None     
        self._master_username = None     
        self._multi_az = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of DBSnapshot from structured json data"""
        dbi = cls()        
        
        dbi._allocated_storage = json_data[u'AllocatedStorage']
        dbi._auto_minor_version_upgrade = json_data[u'AutoMinorVersionUpgrade']
        dbi._availability_zone = json_data[u'AvailabilityZone']
        dbi._backup_retention_period = json_data[u'BackupRetentionPeriod']
        dbi._character_set_name = json_data[u'CharacterSetName']
        dbi._db_instance_class = json_data[u'DBInstanceClass']
        dbi._db_instance_identifier = json_data[u'DBInstanceIdentifier']     
        dbi._db_instance_status = json_data[u'DBInstanceStatus']     
        dbi._db_name = json_data[u'DBName']     
        dbi._db_parameter_groups = list() 
        for param_group in json_data[u'DBParameterGroups']:
            dbi._db_parameter_groups.append(DBParameterGroupStatus.from_json(param_group))        
        dbi._db_security_groups = list() 
        for sec_group in json_data[u'DBSecurityGroups']:
            dbi._db_security_groups.append(DBSecurityGroupMembership.from_json(sec_group))        
        dbi._endpoint = Endpoint.from_json(json_data[u'Endpoint'])     
        dbi._engine = json_data[u'Engine']     
        dbi._engine_version = json_data[u'EngineVersion']     
        dbi._instance_create_time_raw = json_data[u'InstanceCreateTime']     
        dbi._instance_create_time = datetime.datetime.fromtimestamp\
            (dbi._instance_create_time_raw).replace(microsecond=0)     
        dbi._latest_restorable_time_raw = json_data[u'LatestRestorableTime']     
        dbi._latest_restorable_time = datetime.datetime.fromtimestamp \
            (dbi._latest_restorable_time_raw).replace(microsecond=0)\
            if dbi._latest_restorable_time_raw is not None else None     
        dbi._license_model = json_data[u'LicenseModel']     
        dbi._master_username = json_data[u'MasterUsername'] 
        dbi._multi_az = json_data[u'MultiAZ'] 

        return dbi


    @property
    def allocated_storage(self):
        return self._allocated_storage

    @property
    def auto_minor_version_upgrade(self):
        return self._auto_minor_version_upgrade

    @property
    def availability_zone(self):
        return self._availability_zone

    @property
    def backup_retention_period(self):
        return self._backup_retention_period

    @property
    def character_set_name(self):
        return self._character_set_name

    @property
    def db_instance_class(self):
        return self._db_instance_class

    @property
    def db_instance_identifier(self):
        return self._db_instance_identifier

    @property
    def db_instance_status(self):
        return self._db_instance_status

    @property
    def db_name(self):
        return self._db_name

    @property
    def db_parameter_groups(self):
        return self._db_parameter_groups

    @property
    def db_security_groups(self):
        return self._db_security_groups

    @property
    def endpoint(self):
        return self._endpoint

    @property
    def engine(self):
        return self._engine

    @property
    def engine_version(self):
        return self._engine_version

    @property
    def instance_create_time(self):
        return self._instance_create_time

    @property
    def latest_restorable_time(self):
        return self._latest_restorable_time

    @property
    def license_model(self):
        return self._license_model

    @property
    def master_username(self):
        return self._master_username

    @property
    def multi_az(self):
        return self._multi_az


class DBSnapshot(BaseModel):
    
    TypeAutomated = u'automated'
    TypeManual = u'manual'
    
    def __init__(self):
        self._allocated_storage = None
        self._availability_zone = None
        self._db_instance_identifier = None     
        self._db_snapshot_identifier = None     
        self._engine = None     
        self._engine_version = None     
        self._instance_create_time = None     
        self._license_model = None     
        self._master_username = None     
        self._port = None     
        self._snapshot_create_time = None     
        self._snapshot_type = None     
        self._status = None     
        self._vpc_id = None     

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of DBSnapshot from structured json data"""
        dbs = cls()        
        
        dbs._allocated_storage = json_data[u'AllocatedStorage']
        dbs._availability_zone = json_data[u'AvailabilityZone']
        dbs._db_instance_identifier = json_data[u'DBInstanceIdentifier']     
        dbs._db_snapshot_identifier = json_data[u'DBSnapshotIdentifier']     
        dbs._engine = json_data[u'Engine']     
        dbs._engine_version = json_data[u'EngineVersion']     
        dbs._instance_create_time_raw = json_data[u'InstanceCreateTime']     
        dbs._instance_create_time = datetime.datetime.fromtimestamp\
            (dbs._instance_create_time_raw).replace(microsecond=0)     
        dbs._license_model = json_data[u'LicenseModel']     
        dbs._master_username = json_data[u'MasterUsername']     
        dbs._port = json_data[u'Port']     
        dbs._snapshot_create_time_raw = json_data[u'SnapshotCreateTime']     
        dbs._snapshot_create_time = datetime.datetime.fromtimestamp\
            (dbs._snapshot_create_time_raw).replace(microsecond=0)
        dbs._snapshot_type = json_data[u'SnapshotType']     
        dbs._status = json_data[u'Status']     
        dbs._vpc_id = json_data[u'VpcId']             
        
        return dbs

    @property
    def allocated_storage(self):
        return self._allocated_storage

    @property
    def availability_zone(self):
        return self._availability_zone

    @property
    def db_instance_identifier(self):
        return self._db_instance_identifier

    @property
    def db_snapshot_identifier(self):
        return self._db_snapshot_identifier

    @property
    def engine(self):
        return self._engine

    @property
    def engine_version(self):
        return self._engine_version

    @property
    def instance_create_time(self):
        return self._instance_create_time

    @property
    def license_model(self):
        return self._license_model

    @property
    def master_username(self):
        return self._master_username

    @property
    def port(self):
        return self._port

    @property
    def snapshot_create_time(self):
        return self._snapshot_create_time

    @property
    def snapshot_type(self):
        return self._snapshot_type

    @property
    def status(self):
        return self._status

    @property
    def vpc_id(self):
        return self._vpc_id



class DBEngineVersion(BaseModel):
    
    def __init__(self):
        self._db_engineDescription = None
        self._db_engineVersionDescription = None
        self._db_parameterGroupFamily = None
        self._engine = None
        self._engine_version = None
        self._supported_character_sets = None

    @classmethod
    def from_json(cls, json_data):
        """ Create instance of DBEngineVersion from structured json data"""
        dbe = cls()        
        
        dbe._db_engineDescription = json_data[u'DBEngineDescription']
        dbe._db_engineVersionDescription = json_data[u'DBEngineVersionDescription']
        dbe._db_parameterGroupFamily = json_data[u'DBParameterGroupFamily']
        dbe._engine = json_data[u'Engine']
        dbe._engine_version = json_data[u'EngineVersion']
        dbe._supported_character_sets = json_data[u'SupportedCharacterSets']

        return dbe

    @property
    def db_engine_description(self):
        return self._db_engineDescription

    @property
    def db_engine_version_description(self):
        return self._db_engineVersionDescription

    @property
    def db_parameter_group_family(self):
        return self._db_parameterGroupFamily

    @property
    def engine(self):
        return self._engine

    @property
    def engine_version(self):
        return self._engine_version

    @property
    def supported_character_sets(self):
        return self._supported_character_sets


