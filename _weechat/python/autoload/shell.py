
# =============================================================================
#  shell.py (c) March 2006, 2009 by Kolter <kolter@openics.org>
#
#  Licence     : GPL v2
#  Description : running shell commands in WeeChat
#  Syntax      : try /help shell to get some help on this script
#  Precond     : needs weechat >= 0.3.0 to run
#
#
# ### changelog ###
#
#  * version 0.4, 2009-05-02, FlashCode <flashcode@flashtux.org>:
#      - sync with last API changes
#  * version 0.3, 2009-03-06, FlashCode <flashcode@flashtux.org>:
#      - use of hook_process to run background process
#      - add option -t <timeout> to kill process after <timeout> seconds
#      - show process running, kill it with -kill
#  * version 0.2, 2009-01-31, FlashCode <flashcode@flashtux.org>:
#      - conversion to WeeChat 0.3.0+
#  * version 0.1, 2006-03-13, Kolter <kolter@openics.org> :
#      - first release
#
# =============================================================================

import weechat, os, datetime

SCRIPT_NAME    = "shell"
SCRIPT_AUTHOR  = "Kolter"
SCRIPT_VERSION = "0.4"
SCRIPT_LICENSE = "GPL2"
SCRIPT_DESC    = "Run shell commands in WeeChat"

SHELL_CMD      = "shell"
SHELL_PREFIX   = "[shell] "

cmd_hook_process   = ""
cmd_command        = ""
cmd_start_time     = None
cmd_buffer         = ""
cmd_stdout         = ""
cmd_stderr         = ""
cmd_send_to_buffer = False
cmd_timeout        = 0

if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                    SCRIPT_DESC, "", ""):
    weechat.hook_command(
        SHELL_CMD,
        "Running shell commands in WeeChat",
        "[-kill | [-o] [-t seconds] <command line>]",
        "         -kill: kill running process\n"
        "            -o: send output to current buffer (simulate user entry "
        "with command output - dangerous, be careful when using this option)\n"
        "    -t seconds: auto-kill process after timeout (seconds) if process "
        "is still running\n"
        "<command line>: shell command or builtin like cd, getenv, setenv, "
        "unsetenv",
        "-kill|-o|-t|cd|getenv|setenv|unsetenv -o|-t|cd|getenv|setenv|unsetenv",
        "shell_cmd", ""
        )

def shell_init():
    global cmd_hook_process, cmd_command, cmd_start_time, cmd_buffer, cmd_stdout, cmd_stderr
    cmd_hook_process = ""
    cmd_command      = ""
    cmd_start_time   = None
    cmd_buffer       = ""
    cmd_stdout       = ""
    cmd_stderr       = ""

def shell_process_cb(data, command, rc, stdout, stderr):
    global cmd_hook_process, cmd_buffer, cmd_stdout, cmd_stderr, cmd_send_to_buffer
    cmd_stdout += stdout
    cmd_stderr += stderr
    if int(rc) >= 0:
        if cmd_stdout != "":
            lines = cmd_stdout.split("\n")
            if cmd_send_to_buffer:
                for line in lines:
                    if line != "":
                        weechat.command(cmd_buffer, "%s" % line)
            else:
                weechat.prnt(cmd_buffer, "%sCommand '%s' (rc %s), stdout:"
                             % (SHELL_PREFIX, command, rc))
                for line in lines:
                    if line != "":
                        weechat.prnt(cmd_buffer, " \t%s" % line)
        if cmd_stderr != "":
            lines = cmd_stderr.split("\n")
            if cmd_send_to_buffer:
                for line in lines:
                    if line != "":
                        weechat.command(cmd_buffer, "%s" % line)
            else:
                weechat.prnt(cmd_buffer, "%s\t%sCommand '%s' (rc %s), stderr:"
                             % (weechat.prefix("error"), SHELL_PREFIX, command, rc))
                for line in lines:
                    if line != "":
                        weechat.prnt(cmd_buffer, " \t%s" % line)
        cmd_hook_process = ""
    return weechat.WEECHAT_RC_OK

def shell_exec(buffer, command):
    global cmd_hook_process, cmd_command, cmd_start_time, cmd_buffer
    global cmd_stdout, cmd_stderr, cmd_timeout
    if cmd_hook_process != "":
        weechat.prnt(buffer,
                     "%sanother process is running! (use '/%s -kill' to kill it)"
                     % (SHELL_PREFIX, SHELL_CMD))
        return
    shell_init()
    cmd_command = command
    cmd_start_time = datetime.datetime.now()
    cmd_buffer = buffer
    cmd_hook_process = weechat.hook_process(command, cmd_timeout * 1000, "shell_process_cb", "")

def shell_show_process(buffer):
    global cmd_command, cmd_start_time
    if cmd_hook_process == "":
        weechat.prnt(buffer, "%sno process running" % SHELL_PREFIX)
    else:
        weechat.prnt(buffer, "%sprocess running: '%s' (started on %s)"
                     % (SHELL_PREFIX, cmd_command, cmd_start_time.ctime()))

def shell_kill_process(buffer):
    global cmd_hook_process, cmd_command
    if cmd_hook_process == "":
        weechat.prnt(buffer, "%sno process running" % SHELL_PREFIX)
    else:
        weechat.unhook(cmd_hook_process)
        weechat.prnt(buffer, "%sprocess killed (command '%s')" % (SHELL_PREFIX, cmd_command))
        shell_init()

def shell_chdir(buffer, directory):
    if directory == "":
        if os.environ.has_key('HOME'):
            directory = os.environ['HOME']
    try:
        os.chdir(directory)
    except:
        weechat.prnt(buffer, "%san error occured while running command 'cd %s'" % (SHELL_PREFIX, directory))
    else:
        weechat.prnt(buffer, "%schdir to '%s' ok" % (SHELL_PREFIX, directory))

def shell_getenv(buffer, var):
    global cmd_send_to_buffer
    var = var.strip()
    if var == "":
        weechat.prnt(buffer, "%swrong syntax, try 'getenv VAR'" % (SHELL_PREFIX))
        return
        
    value = os.getenv(var)
    if value == None:
        weechat.prnt(buffer, "%s$%s is not set" % (SHELL_PREFIX, var))
    else:
        if cmd_send_to_buffer:
            weechat.command(buffer, "$%s=%s" % (var, os.getenv(var)))
        else:
            weechat.prnt(buffer, "%s$%s=%s" % (SHELL_PREFIX, var, os.getenv(var)))
        
def shell_setenv(buffer, expr):
    global cmd_send_to_buffer
    expr = expr.strip()
    lexpr = expr.split('=')
    
    if (len(lexpr) < 2):
        weechat.prnt(buffer, "%swrong syntax, try 'setenv VAR=VALUE'" % (SHELL_PREFIX))
        return

    os.environ[lexpr[0].strip()] = "=".join(lexpr[1:])
    if not cmd_send_to_buffer:
        weechat.prnt(buffer, "%s$%s is now set to '%s'" % (SHELL_PREFIX, lexpr[0], "=".join(lexpr[1:])))

def shell_unsetenv(buffer, var):
    global cmd_send_to_buffer
    var = var.strip()
    if var == "":
        weechat.prnt(buffer, "%swrong syntax, try 'unsetenv VAR'" % (SHELL_PREFIX))
        return
    
    if os.environ.has_key(var):
        del os.environ[var]
        weechat.prnt(buffer, "%s$%s is now unset" % (SHELL_PREFIX, var))
    else:
        weechat.prnt(buffer, "%s$%s is not set" % (SHELL_PREFIX, var))        
    
def shell_cmd(data, buffer, args):
    global cmd_send_to_buffer, cmd_timeout
    largs = args.split(" ")
    
    # strip spaces
    while '' in largs:
        largs.remove('')
    while ' ' in largs:
        largs.remove(' ')
    
    if len(largs) ==  0:
        shell_show_process(buffer)
    else:
        if largs[0] == '-kill':
            shell_kill_process(buffer)
        else:
            cmd_send_to_buffer = False
            cmd_timeout = 0
            while True:
                if largs[0] == '-o':
                    cmd_send_to_buffer = True
                    largs = largs[1:]
                    continue
                if largs[0] == '-t' and len(largs) > 2:
                    cmd_timeout = int(largs[1])
                    largs = largs[2:]
                    continue
                break;
            if len(largs) > 0:
                if largs[0] == 'cd':
                    shell_chdir(buffer, " ".join(largs[1:]))
                elif largs[0] == 'getenv':
                    shell_getenv (buffer, " ".join(largs[1:]))
                elif largs[0] == 'setenv':
                    shell_setenv (buffer, " ".join(largs[1:]))
                elif largs[0] == 'unsetenv':
                    shell_unsetenv (buffer, " ".join(largs[1:]))
                else:
                    shell_exec(buffer, " ".join(largs))
    
    return weechat.WEECHAT_RC_OK
