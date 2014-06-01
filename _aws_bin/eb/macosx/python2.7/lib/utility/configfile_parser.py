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
import codecs
import os
import re

from ConfigParser import RawConfigParser as _RawConfigParser
from StringIO import StringIO as _StringIO
from contextlib import closing as _closing

from scli.constants import ServiceDefault

class SectionedConfigParser(_RawConfigParser):
    ''' 
    Derived ConfigParser.RawConfigParser to support Unicode write and case sensitive key name.
    '''
    
    OPTCRE_NV = re.compile(
        r'(?P<option>[^=\s][^=]*)'          # override _RawConfigParser re
        r'\s*(?:'                           # to only use "=" as separator 
        r'(?P<vi>[=])\s*'                    
        r'(?P<value>.*))?$'                  
        )
    
    def __init__(self):
        return _RawConfigParser.__init__(self, allow_no_value = True)    
    
    def read(self, pathfilename):
        with codecs.open(pathfilename, 'r', encoding=ServiceDefault.CHAR_CODEC) as input_file:
            _RawConfigParser.readfp(self, input_file)

    def write(self, pathfilename):
        with codecs.open(pathfilename, 'w', encoding=ServiceDefault.CHAR_CODEC) as output_file:
            for section in self._sections:
                output_file.write(u'[{0}]{1}'.format(section.strip(), os.linesep))
                for (option, value) in self._sections[section].items():
                    if option == "__name__":
                        continue
                    output_file.write(u'{0}={1}{2}'.format(option.strip(), value.strip(), os.linesep))
                output_file.write(os.linesep)

    def optionxform(self, optionstr):
        return optionstr                

    def add_section(self, section):
        if not self.has_section(section):
            self._sections[section] = self._dict()


class NoSectionConfigParser(_RawConfigParser):
    ''' 
    Derived ConfigParser.RawConfigParser to access no section config file.
    Also support Unicode write 
    '''
    _default_section = u'defaultsection'
    
    def read(self, pathfilename):
        with codecs.open(pathfilename, 'r', encoding=ServiceDefault.CHAR_CODEC) as input_file:
            config_pairs = input_file.read()
        with _closing(_StringIO(u"[{0}]{1}{2}".format(self._default_section, 
                                                      os.linesep, 
                                                      config_pairs))) \
                as default_section: 
            _RawConfigParser.readfp(self, default_section)

    def write(self, pathfilename):
        with codecs.open(pathfilename, 'w', encoding=ServiceDefault.CHAR_CODEC) as output_file:
            for option in self.options(self._default_section):
                output_file.write(u'{0}={1}{2}'.format(option, self.get(option), os.linesep))

    def get(self, option):
        return _RawConfigParser.get(self, self._default_section, option)
    
    def set(self, option, value = None):
        if len(self.sections()) < 1:
            self.add_section(self._default_section)
        _RawConfigParser.set(self, self._default_section, option, value)
 
    def has_option(self, option):
        return _RawConfigParser.has_option(self, self._default_section, option)

    def remove_option(self, option):
        return _RawConfigParser.remove_option(self, self._default_section, option)

    def optionxform(self, optionstr):
        return optionstr                
