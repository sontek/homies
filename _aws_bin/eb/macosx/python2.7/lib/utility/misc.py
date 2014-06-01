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
from contextlib import closing
from datetime import datetime
import locale, platform
from StringIO import StringIO
import sys

from scli.constants import OSSpecific, ServiceDefault, TerminalConstant

def unixtime_to_utc(unixtime):
    ''' Convert unix timestamp to ISO8601 UTC time'''
    utc_time = datetime.utcfromtimestamp(unixtime)
    return utc_time.isoformat()


def get_os_name():
    return to_unicode(platform.system())

        
def is_os_windows():
    if 'nt' in sys.builtin_module_names \
        or get_os_name() == OSSpecific.WindowsName:
        return True
    else:
        return False

    
def to_unicode(string, convert_none = True, codec = ServiceDefault.CHAR_CODEC):
    """ Do our best to convert strings to Unicode if we can."""
    if string is None:
        if convert_none:
            return u''
        else:
            return None    
    
    if sys.version_info > (3, 0):
        if isinstance(string, bytes):
            return string.decode(codec, 'replace')
        elif isinstance(string, str):
            return string
        else:
            return str(string)
    else:
        if isinstance(string, unicode):
            return string
        elif isinstance(string, str):        
            return unicode(string, codec, 'replace')
        else:
            return unicode(string)

        
def to_bytes(string, convert_none = True, codec = ServiceDefault.CHAR_CODEC):
    """ Do you best to convert string to bytes if we can.
        Note: in Python 2.7 "bytes" is alias of "str". 
    """
    if string is None:
        if convert_none:
            return ''
        else:
            return None
    
    if sys.version_info > (3, 0):
        if isinstance(string, str):
            return string.encode(codec, 'replace')
        elif isinstance(string, bytes):
            return string
        else:
            return str(string).encode(codec, 'replace')
    else:        
        if isinstance(string, unicode):
            return string.encode(codec, 'replace')
        elif isinstance(string, str):
            return string
        else:
            return str(string)


def to_terminal_codepage(string, convert_none = True):
    if sys.version_info > (3, 0):
        return string
    else:
        return to_bytes(string, convert_none, locale.getpreferredencoding())


def _is_container(collection):
    if isinstance(collection, dict) \
        or isinstance(collection, dict) \
        or isinstance(collection, list) \
        or isinstance(collection, set) \
        or isinstance(collection, frozenset)\
        or isinstance(collection, tuple):
        return True
    else:
        return False

        
def _itr_printer(collection, depth, stream):        
    if depth == 1 or not _is_container(collection):
        stream.write(to_unicode(collection))    
    else:
        stream.write(u'{')
        if isinstance(collection, dict):
            for key, item in enumerate(collection):
                stream.write(u'{0} : '.format(key))
                _itr_printer(item, depth - 1, stream)
                stream.write(u',')
        else:
            for item in collection:
                _itr_printer(item, depth - 1, stream)
                stream.write(u',')
        stream.write(u'}')

    
def collection_to_string(collection, depth=3):
    with closing(StringIO()) as item_list:
        _itr_printer(collection, depth = depth, stream = item_list)
        return item_list.getvalue()
    
    
def string_equal_ignore_case(string1, string2):
    if not string1 or not string2:
        return False 
    if string1.lower() == string2.lower():
        return True
    else:
        return False

    
def mask_string(string, head = 5, tail = 5, mask_number = 5, mask_char = u'*'):
    if string is None or len(string) < 1:
        return string
    
    if len(string) < head + tail + mask_number:
        head = tail = len(string) / 3
        mask_number = len(string) - head - tail
    
    new_string = string[:head] + mask_char * mask_number + string[-tail:]
    return new_string 


def string_to_boolean(string):
    if string is None:
        raise AttributeError(u'Cannot convert None to boolean.')
    if string_equal_ignore_case(string, TerminalConstant.Y)\
        or string_equal_ignore_case(string, TerminalConstant.Yes)\
        or string_equal_ignore_case(string, TerminalConstant.TRUE):
        return True
    elif string_equal_ignore_case(string, TerminalConstant.N)\
        or string_equal_ignore_case(string, TerminalConstant.No)\
        or string_equal_ignore_case(string, TerminalConstant.FALSE):
        return False
    else:
        raise AttributeError(u'Not recognized boolean value: "{0}".'.format(string))
        
    
def bool_to_yesno(value):
    if not isinstance(value, bool):
        raise AttributeError(u'"{0}" is not instance of boolean.'.format(value))
    if value:
        return TerminalConstant.Yes
    else:
        return TerminalConstant.No


def is_blank_string(string):
    if string is None:
        return True
    if len(string.strip()) < 1:
        return True
    return False


