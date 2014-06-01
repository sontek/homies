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
import locale, logging, os, sys, re, shlex, shutil, subprocess, webbrowser

from scli import prompt
from scli.constants import DevToolsDefault, FileErrorConstant, GitDefault
from scli.exception import EBSCliException
from scli.resources import DevToolsMessage
from lib.utility import misc

log = logging.getLogger('scli')


def get_current_dir_name():
    ''' Return current directory name '''
    return os.path.basename(os.getcwd())


def ori_path():
    ''' Return absolute path where the executable is hosted. '''
    if sys.version_info > (3, 0):
        return os.path.dirname(os.path.realpath(sys.argv[0]))
    else:
        return os.path.dirname((os.path.realpath(sys.argv[0])).\
                                decode(locale.getpreferredencoding()))


def create_directory(directory):
    ''' Create a directory at location. Return if exist. '''                    
    if not os.path.exists(directory):
        os.makedirs(directory)
    
    
def call(command, quiet = True):
    '''
    Call external process. command is a list of command line arguments including name
    of external process and arguments.
    '''
    if isinstance(command, str):
        command_line = shlex.split(command)
    elif isinstance(command, list):
        command_line = command
    else:
        raise EBSCliException('Parameter must be instance of list or string.')
    
    log.debug('Running external commands "{0}".'.\
              format(misc.collection_to_string(command_line)))
    # Using OS native code page 
    command_line = [x.encode(locale.getpreferredencoding()) for x in command_line]
    args = {'args':command_line}
    if misc.is_os_windows():
        # TODO: set shell to True will allow Windows translate "git" to "git.cmd", 
        # but might introduce other issues.
        args['shell'] = True
    if quiet:
        args['stderr'] = subprocess.STDOUT        
        
    return misc.to_unicode(subprocess.check_output(**args), False, locale.getpreferredencoding())


def climb_dir_tree(path, level):
    target_path = path
    for _ in range(level):
        target_path = os.path.dirname(target_path)
    return target_path


def get_working_branch(quiet = False):
    try:
        result = misc.to_unicode(call(GitDefault.GetBranch), locale.getpreferredencoding()) 
        branches = result.splitlines()
        if len(branches) == 0:
            return None, 0
        else:
            head_re = re.compile(GitDefault.HeadRe, re.UNICODE)        
            for branch in branches:
                if head_re.match(branch):
                    return branch.split(' ')[1], len(branches)
                
    except subprocess.CalledProcessError as ex:
        # Git returned with an error code
        log.error('Git local repository is not set up:  "{0}".'.format(ex))
        if quiet:
            return None, 0
        else:
            raise EBSCliException(DevToolsMessage.GitRepoNotExist.format(ex.message))
    
    except (OSError, IOError) as ex:
        log.error('Failed to call git, because "{0}".'.format(ex))
        if quiet:
            return None, 0
        else:
            # Cannot find or run script
            if ex.errno == FileErrorConstant.FileNotFoundErrorCode:
                log.error('Cannot find Git executable "git".')
            raise EBSCliException(DevToolsMessage.GitCommandError.format(ex.message))
        

def copy_file(src, dst, quiet = True):
    try:
        if src == dst:
            return
        shutil.copy(src, dst)
    except BaseException as ex:
        log.error('Encountered an error when copying file from {0} to {1}, because: {2}'.\
                  format(src, dst, ex))
        if not quiet:
            raise


def get_repo_head_hash(quiet = False):
    try:
        headhash = misc.to_unicode(call(GitDefault.GetHeadHash), locale.getpreferredencoding())
        return headhash.replace(os.linesep, '')
    except subprocess.CalledProcessError as ex:
        # Git returned with an error code
        log.error('Git local repository does not have HEAD info:  "{0}".'.format(ex))
        if quiet:
            return None
        else:
            raise EBSCliException(DevToolsMessage.GitHeadNotExist.format(ex.message))
    
    except (OSError, IOError) as ex:
        log.error('Failed to call git, because "{0}".'.format(ex))
        if quiet:
            return None
        else:
            # Cannot find or run script
            if ex.errno == FileErrorConstant.FileNotFoundErrorCode:
                log.error('Cannot find Git executable "git".')
            raise EBSCliException(DevToolsMessage.GitCommandError.format(ex.message))


def git_aws_push(push_only = False, quiet = False):
    output = prompt.info if quiet else prompt.result
    cmd = DevToolsDefault.AwsCreateAppVersion if push_only else DevToolsDefault.AwsPush
    
    try:
        output(misc.to_unicode(call(cmd, quiet=quiet), locale.getpreferredencoding())) 
        return True
    except subprocess.CalledProcessError as ex:
        # Git returned with an error code
        log.error('Failed to push local HEAD to EB:  "{0}".'.format(ex))
        if quiet:
            return False
        else:
            raise EBSCliException(DevToolsMessage.PushFail.format(ex.message))
    
    except (OSError, IOError) as ex:
        log.error('Failed to call git, because "{0}".'.format(ex))
        if quiet:
            return False
        else:
            # Cannot find or run script
            if ex.errno == FileErrorConstant.FileNotFoundErrorCode:
                log.error('Cannot find Git executable "git".')
            raise EBSCliException(DevToolsMessage.GitCommandError.format(ex.message))
        

def open_url(url, quiet = True):
    try:
        webbrowser.open(url)
    except webbrowser.Error as ex: 
        log.error('Failed to open URL "{0}" in default browser, because "{1}".'.format(url, ex))
        if not quiet:
            raise
