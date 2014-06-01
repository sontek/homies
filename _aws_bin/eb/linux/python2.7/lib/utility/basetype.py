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

class ValuedEnum(dict):
    ''' 
    Implementation of Enum type using dict as base class.
    Each item can have an associated value, and random access time is O(1)
    However, order of items (dict keys) does not follow either value of their
    appearance order.
    '''

    def __getattr__(self, name):
        if name in self.keys():
            return name
        raise AttributeError(u"Item not exists: {0}".format(name))
    
    
    def __setattr__(self, name, value):
        raise AttributeError(u'Enum instance is read only.')
        

    def __setitem__(self, key, value):
        raise AttributeError(u'Enum instance is read only.')
    
    def __contains__(self, item):
        return item in self.keys()
    
    def items(self):
        return self.keys()
    
    def order(self, name):
        if self.has_key(name):
            return self[name]
        raise AttributeError(u"Item not exists: {0}".format(name))


    def is_ahead(self, name1, name2):
        ''' 
        Return true is name1 in Enum has smaller order than name2.
        Otherwise return false
        '''
        if name1 is None:
            return False
        if name2 is None:
            return True 
        
        if self.order(name1) < self.order(name2):
            return True
        else:
            return False
    
    
            
class OrderedEnum(list):
    '''
    Implementation of Enum type using list as base class.
    Order of items are guaranteed to follow their appearance order when initialing.
    However, random access time is O(n).
    '''

    def __getattr__(self, name):
        if name in self:
            return name
        raise AttributeError(u"Item not exists: {0}".format(name))
    
    
    def __setattr__(self, name, value):
        raise AttributeError(u'Enum instance is read only.')
        

    def __setitem__(self, key, value):
        raise AttributeError(u'Enum instance is read only.')

    def items(self):
        return self
    
    def order(self, name):
        if name in self:
            return self.index(name)
        raise AttributeError(u"Item not exists: {0}".format(name))


    def is_ahead(self, name1, name2):
        ''' 
        Return true is name1 in Enum has smaller order than name2.
        Otherwise return false
        '''
        if self.order(name1) < self.order(name2):
            return True
        else:
            return False                
            