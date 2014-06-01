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

import sys as _sys
from lib.utility import misc
from scli.constants import OutputLevel

_EMPTY_PROMPT = ''
_STAR_PROMPT = '* '
_DASH_PROMPT = '--'
_EXCLA_PROMPT = '! '
_ARROW_PROMPT = '> '

class _OutputStream(object):

    def __init__(self, stream = _sys.stdout):
        self._out_stream = stream

    def write(self, msg):
        if self._out_stream is not None:
            self._out_stream.write('{0}\n'.format(msg))
            self._out_stream.flush()

    def set_stream(self, stream = _sys.stdout):
        self._out_stream = stream


_std_out = _OutputStream(_sys.stdout)
_err_out = _OutputStream(_sys.stderr)
_null_out = _OutputStream(None)

_info = _std_out
_result = _std_out
_err = _err_out

_current_level = OutputLevel.Info

def _output(stream, prompt_string, message):
    stream.write(prompt_string + message)

def get_level():
    return _current_level

def set_level(level):
    global _current_level 
    global _std_out, _err_out, _null_out
    global _info, _result, _err
    
    _current_level = level
    
    if level == OutputLevel.Info:
        _info = _result = _std_out
        _err = _err_out
    elif level == OutputLevel.ResultOnly:
        _info = _null_out 
        _result = _std_out
        _err = _err_out
    elif level == OutputLevel.Quiet:
        _info = _result = _null_out
        _err = _err_out
    elif level == OutputLevel.Silence:
        _info = _result = _err = _null_out

def plain(message):
    global _result
    _output(_result, _EMPTY_PROMPT, message)

def action(message):
    global _info
    _output(_info, _EMPTY_PROMPT, message)

def info(message):
    global _info
    _output(_info, _EMPTY_PROMPT, message)
    
def result(message):
    global _result
    _output(_result, _EMPTY_PROMPT, message)

def error(message):
    global _err
    _output(_err, _EMPTY_PROMPT, message)
    
    