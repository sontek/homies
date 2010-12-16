# -*- coding: utf-8 -*-
#
# Copyright (C) 2009-2010 Sebastien Helleu <flashcode@flashtux.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#
# WeeChat scripts manager.
# (this script requires WeeChat 0.3.0 (or newer) and python 2.5)
#
# History:
#
# 2010-11-08, Sebastien Helleu <flashcode@flashtux.org>:
#     version 1.1: get python 2.x binary for hook_process (fix problem
#                  when python 3.x is default python version, requires
#                  WeeChat >= 0.3.4)
# 2010-02-22, Blake Winton <bwinton@latte.ca>:
#     version 1.0: add option "listinstalled" for command /weeget
# 2010-01-25, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.9: fix "running" status of scripts with /weeget check
# 2009-09-30, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.8: fix bugs and add missing info in "/weeget show",
#                  display warning if url for plugins.xml.gz is old site
# 2009-09-07, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.7: update weechat site with new URL
# 2009-05-02, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.6: sync with last API changes
# 2009-04-15, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.5: display missing module(s) when import failed
# 2009-04-11, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.4: use new completion for command arguments
# 2009-04-07, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.3: fix bug with install/upgrade when weeget is updated with
#                  other scripts: ensure that weeget is always the last
#                  installed script
# 2009-04-07, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.2: add author's mail in script description
# 2009-04-05, Sebastien Helleu <flashcode@flashtux.org>:
#     version 0.1: initial release
#

SCRIPT_NAME    = "weeget"
SCRIPT_AUTHOR  = "Sebastien Helleu <flashcode@flashtux.org>"
SCRIPT_VERSION = "1.1"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC    = "WeeChat scripts manager"

SCRIPT_COMMAND = "weeget"

import_ok = True

try:
    import weechat
except ImportError:
    print "This script must be run under WeeChat."
    print "Get WeeChat now at: http://www.weechat.org/"
    import_ok = False

try:
    import os, stat, time, gzip, hashlib, xml.dom.minidom
except ImportError, message:
    print "Missing package(s) for %s: %s" % (SCRIPT_NAME, message)
    import_ok = False

CONFIG_FILE_NAME = "wg"

SCRIPT_EXTENSION = {
    "perl"  : "pl",
    "python": "py",
    "ruby"  : "rb",
    "lua"   : "lua",
    "tcl"   : "tcl",
}

# timeout for download of plugins.xml.gz
TIMEOUT_UPDATE = 60 * 1000

# timeout for download of a script
TIMEOUT_SCRIPT = 60 * 1000

# config file and options
wg_config_file            = ""
wg_config_option          = {}

# action (install, remove, ..) and arguments
wg_action                 = ""
wg_action_args            = ""

# loaded scripts
wg_loaded_scripts         = {}

# hook process and stdout
wg_hook_process           = { "update": "", "script": "" }
wg_stdout                 = { "update": "", "script": "" }

# scripts read from plugins.xml.gz
wg_scripts                = {}

# list of script to install, and script currently installing
wg_scripts_to_install     = []
wg_current_script_install = {}

# =================================[ config ]=================================

def wg_config_init():
    """
    Initialization of configuration file.
    Sections: color, scripts.
    """
    global wg_config_file, wg_config_option
    wg_config_file = weechat.config_new(CONFIG_FILE_NAME,
                                        "wg_config_reload_cb", "")
    if wg_config_file == "":
        return
    
    # section "color"
    section_color = weechat.config_new_section(
        wg_config_file, "color", 0, 0, "", "", "", "", "", "", "", "", "", "")
    if section_color == "":
        weechat.config_free(wg_config_file)
        return
    wg_config_option["color_script"] = weechat.config_new_option(
        wg_config_file, section_color,
        "script", "color", "Color for script names", "", 0, 0,
        "cyan", "cyan", 0, "", "", "", "", "", "")
    wg_config_option["color_installed"] = weechat.config_new_option(
        wg_config_file, section_color,
        "installed", "color", "Color for \"installed\" indicator", "", 0, 0,
        "yellow", "yellow", 0, "", "", "", "", "", "")
    wg_config_option["color_running"] = weechat.config_new_option(
        wg_config_file, section_color,
        "running", "color", "Color for \"running\" indicator", "", 0, 0,
        "lightgreen", "lightgreen", 0, "", "", "", "", "", "")
    wg_config_option["color_obsolete"] = weechat.config_new_option(
        wg_config_file, section_color,
        "obsolete", "color", "Color for \"obsolete\" indicator", "", 0, 0,
        "lightmagenta", "lightmagenta", 0, "", "", "", "", "", "")
    wg_config_option["color_unknown"] = weechat.config_new_option(
        wg_config_file, section_color,
        "unknown", "color", "Color for \"unknown status\" indicator", "", 0, 0,
        "lightred", "lightred", 0, "", "", "", "", "", "")
    wg_config_option["color_language"] = weechat.config_new_option(
        wg_config_file, section_color,
        "language", "color", "Color for language names", "", 0, 0,
        "lightblue", "lightblue", 0, "", "", "", "", "", "")
    
    # section "scripts"
    section_scripts = weechat.config_new_section(
        wg_config_file, "scripts", 0, 0, "", "", "", "", "", "", "", "", "", "")
    if section_scripts == "":
        weechat.config_free(wg_config_file)
        return
    wg_config_option["scripts_url"] = weechat.config_new_option(
        wg_config_file, section_scripts,
        "url", "string", "URL for file with list of plugins", "", 0, 0,
        "http://www.weechat.org/files/plugins.xml.gz",
        "http://www.weechat.org/files/plugins.xml.gz", 0, "", "", "", "", "", "")
    wg_config_option["scripts_dir"] = weechat.config_new_option(
        wg_config_file, section_scripts,
        "dir", "string", "Local cache directory for" + SCRIPT_NAME, "", 0, 0,
        "%h/" + SCRIPT_NAME, "%h/" + SCRIPT_NAME, 0, "", "", "", "", "", "")
    wg_config_option["scripts_cache_expire"] = weechat.config_new_option(
        wg_config_file, section_scripts,
        "cache_expire", "integer", "Local cache expiration time, in minutes "
        "(-1 = never expires, 0 = always expires)", "",
        -1, 60*24*365, "60", "60", 0, "", "", "", "", "", "")

def wg_config_reload_cb(data, config_file):
    """ Reload configuration file. """
    return weechat.config_read(config_file)

def wg_config_read():
    """ Read configuration file. """
    global wg_config_file
    return weechat.config_read(wg_config_file)

def wg_config_write():
    """ Write configuration file. """
    global wg_config_file
    return weechat.config_write(wg_config_file)

def wg_config_color(color):
    """ Get a color from configuration. """
    global wg_config_option
    option = wg_config_option.get("color_" + color, "")
    if option == "":
        return ""
    return weechat.color(weechat.config_string(option))

def wg_config_get_dir():
    """ Return weeget directory, with expanded WeeChat home dir. """
    global wg_config_option
    return weechat.config_string(
        wg_config_option["scripts_dir"]).replace("%h",
                                                 weechat.info_get("weechat_dir", ""))

def wg_config_create_dir():
    """ Create weeget directory. """
    dir = wg_config_get_dir()
    if not os.path.isdir(dir):
        os.makedirs(dir, mode=0700)

def wg_config_get_cache_filename():
    """ Get local cache filename, based on URL. """
    global wg_config_option
    return wg_config_get_dir() + os.sep + \
           os.path.basename(weechat.config_string(wg_config_option["scripts_url"]))

# ================================[ scripts ]=================================

def wg_search_script_by_name(name):
    """
    Search a script in list by name.
    Name can be short name ('weeget') or full name ('weeget.py').
    """
    global wg_scripts
    for id, script in wg_scripts.iteritems():
        if script["name"] == name or script["full_name"] == name:
            return script
    return None

def wg_get_loaded_scripts():
    """
    Get python dictionary with loaded scripts.
    Keys are filenames and values are path to script, for example:
      'weeget.py': '/home/xxx/.weechat/python/weeget.py'
    """
    global wg_loaded_scripts
    wg_loaded_scripts = {}
    for language in SCRIPT_EXTENSION.keys():
        infolist = weechat.infolist_get(language + "_script", "", "")
        while weechat.infolist_next(infolist):
            filename = weechat.infolist_string(infolist, "filename")
            if filename != "":
                wg_loaded_scripts[os.path.basename(filename)] = filename
        weechat.infolist_free(infolist)

def wg_is_local_script_loaded(filename):
    """ Check if a script filename (like 'python/weeget.py') is loaded. """
    global wg_loaded_scripts
    filename2 = filename
    if filename2.startswith("autoload/"):
        filename2 = filename2[9:]
    for name, path in wg_loaded_scripts.iteritems():
        if path.endswith(filename) or path.endswith(filename2):
            return True
    return False

def wg_get_local_script_status(script):
    """
    Check if a script is installed.
    'script' is a dictionary retrieved from scripts xml list.
    """
    global wg_loaded_scripts
    status = { "installed": "", "obsolete": "", "running": "" }
    local_dir = weechat.info_get("weechat_dir", "") + os.sep + script["language"]
    local_name = local_dir + os.sep + "autoload" + os.sep + script["full_name"]
    if not os.path.isfile(local_name):
        local_name = local_dir + os.sep + script["full_name"]
    if os.path.isfile(local_name):
        status["installed"] = "1"
        f = file(local_name, "rb")
        md5 = hashlib.md5()
        md5.update(f.read())
        local_md5 = md5.hexdigest()
        if local_md5 != script["md5sum"]:
            status["obsolete"] = "1"
    if script["full_name"] in wg_loaded_scripts.keys():
        status["running"] = "1"
    return status

def wg_get_local_scripts():
    """
    Get list of all local scripts (in languages and autoload dirs).
    Return a dictionary with language as key and list of paths as value,
    with autoloaded scripts at beginning of list, for example:
      { 'perl':   [ 'autoload/buffers.pl',
                    'autoload/weetris.pl',
                    'beep.pl',
                    'launcher.pl' ],
        'python': [ 'autoload/weeget.py',
                    'go.py',
                    'vdm.py' ]
      }
    """
    files = {}
    for language in SCRIPT_EXTENSION.keys():
        files[language] = []
        autoloaded_files = []
        rootdir = weechat.info_get("weechat_dir", "") + os.sep + language
        for root, dirs, listfiles in os.walk(rootdir):
            if root == rootdir:
                files[language] = listfiles
            elif root == rootdir + os.sep + "autoload":
                autoloaded_files = listfiles
        for file in autoloaded_files:
            if file in files[language]:
                files[language].remove(file)
            files[language].insert(0, "autoload" + os.sep + file)
    return files

def wg_get_local_scripts_status():
    """
    Return list of all local scripts with status (unknown/obsolete/running).
    For example:
      [ 'perl/weetris.pl':  { 'unknown': '', 'obsolete': '1', 'running': ''  },
        'python/weeget.py': { 'unknown': '', 'obsolete': '',  'running': '1' }
      ]
    """
    local_scripts_status = []
    local_scripts = wg_get_local_scripts()
    if len(local_scripts) > 0:
        for language, files in local_scripts.iteritems():
            for file in files:
                script_status = { "unknown": "", "obsolete": "", "running": "" }
                name_with_ext = os.path.basename(file)
                script = wg_search_script_by_name(os.path.basename(file))
                if script == None:
                    script_status["unknown"] = "1"
                else:
                    status = wg_get_local_script_status(script)
                    if status["obsolete"]:
                        script_status["obsolete"] = "1"
                if wg_is_local_script_loaded(file):
                    script_status["running"] = "1"
                local_scripts_status.append((language + os.sep + file,
                                             script_status))
    return local_scripts_status

def wg_search_scripts(search):
    """ Search word in scripts, return list of matching scripts. """
    global wg_scripts
    if search == "":
        return wg_scripts
    scripts_matching = {}
    for id, script in wg_scripts.iteritems():
        if script["name"].lower().find(search) >= 0 \
           or script["language"].lower().find(search) >= 0 \
           or script["desc_en"].lower().find(search) >= 0 \
           or script["desc_fr"].lower().find(search) >= 0 \
           or script["tags"].lower().find(search) >= 0:
           scripts_matching[id] = script
    return scripts_matching

def wg_list_scripts(search, installed=False):
    """
    List all scripts (with optional search string).
    If installed == True, then list only installed scripts.
    For each script, display status (installed/running/new version available),
    name of script, language and description.
    For example:
      ir  buffers        pl  Sidebar with list of buffers.
      i N go             py  Quick jump to buffers.
      i   weetris        pl  Tetris-like game.
    """
    global wg_scripts
    search = search.strip().lower()
    scripts_matching = wg_search_scripts(search)
    if len(scripts_matching) == 0:
        weechat.prnt("", "%s: no script found" % SCRIPT_NAME)
    else:
        weechat.prnt("", "")
        if search != "":
            if installed:
                weechat.prnt("", "Scripts installed matching \"%s\":" % search)
            else:
                weechat.prnt("", "Scripts for WeeChat %s matching \"%s\":"
                             % (weechat.info_get("version", ""),
                                search))
        else:
            if installed:
                weechat.prnt("", "Scripts installed:")
            else:
                weechat.prnt("", "Scripts for WeeChat %s:"
                             % weechat.info_get("version", ""))
        sorted_scripts = sorted(scripts_matching.iteritems(),
                                key=lambda(k,v):v["name"])
        length_max_name = 0
        for item in sorted_scripts:
            length = len(item[1]["name"])
            if length > length_max_name:
                length_max_name = length
        str_format = "%%s%%s%%s%%s%%s%%s%%s %%s%%-%ds %%s%%-3s %%s%%s" \
                     % length_max_name
        for item in sorted_scripts:
            script = item[1]
            str_installed = " "
            str_running = " "
            str_obsolete = " "
            status = wg_get_local_script_status(script)
            if installed and not status["installed"]:
                continue
            if status["installed"]:
                str_installed = "i"
            if status["running"]:
                str_running = "r"
            if status["obsolete"]:
                str_obsolete = "N"
            weechat.prnt("", str_format
                         % (wg_config_color("installed"),
                            str_installed,
                            wg_config_color("running"),
                            str_running,
                            wg_config_color("obsolete"),
                            str_obsolete,
                            weechat.color("chat"),
                            wg_config_color("script"),
                            script["name"],
                            wg_config_color("language"),
                            SCRIPT_EXTENSION[script["language"]],
                            weechat.color("chat"),
                            script["desc_en"]))

def wg_show_script(name):
    """
    Show detailed info about a script (in repository).
    For example:
        Script: weeget.py, version 0.7, license: GPL3
        Author: Sebastien Helleu <flashcode [at] flashtux [dot] org>
        Status: installed, running
          Date: added: 2009-04-05, updated: 2009-09-07
           URL: http://www.weechat.org/files/scripts/weeget.py
           MD5: 4b0458dd5cc5c9a09ba8078f89830869
          Desc: Scripts manager.
          Tags: scripts
      Requires: python 2.5
           Min: 0.3.0
    """
    if len(wg_scripts) == 0:
        return
    script = wg_search_script_by_name(name)
    if script == None:
        weechat.prnt("", "%s: script \"%s%s%s\" not found"
                     % (SCRIPT_NAME,
                        wg_config_color("script"),
                        name,
                        weechat.color("chat")))
    else:
        weechat.prnt("", "")
        weechat.prnt("", "  Script: %s%s%s, version %s, license: %s"
                     % (wg_config_color("script"),
                        script["full_name"],
                        weechat.color("chat"),
                        script["version"],
                        script["license"]))
        weechat.prnt("", "  Author: %s <%s>" % (script["author"], script["mail"]))
        status = wg_get_local_script_status(script)
        str_status = "not installed"
        if status["installed"]:
            str_status = "installed"
            if status["running"]:
                str_status += ", running"
            else:
                str_status += ", not running"
        if status["obsolete"]:
            str_status += " (new version available)"
        weechat.prnt("",   "  Status: %s" % str_status)
        date_added = script.get("added", "")[:10]
        str_updated = script.get("updated", "")
        if str_updated != "":
            date_updated = script["updated"][:10]
            if date_updated == "0000-00-00" or date_updated == date_added:
                str_updated = ""
        if str_updated != "":
            weechat.prnt("", "    Date: added: %s, updated: %s"
                         % (date_added, date_updated))
        else:
            weechat.prnt("", "    Date: added: %s" % date_added)
        weechat.prnt("", "     URL: %s" % script.get("url", ""))
        weechat.prnt("", "     MD5: %s" % script.get("md5sum", ""))
        weechat.prnt("", "    Desc: %s" % script.get("desc_en", ""))
        weechat.prnt("", "    Tags: %s" % script.get("tags", ""))
        str_requires = script.get("requirements", "")
        if str_requires == "":
            str_requires = "(nothing)"
        weechat.prnt("", "Requires: %s" % str_requires)
        vmin = script.get("min_weechat", "")
        vmax = script.get("max_weechat", "")
        if vmin != "":
            weechat.prnt("", "     Min: %s" % vmin)
        if vmax != "":
            weechat.prnt("", "     Max: %s" % vmax)

def wg_install_next_script():
    """
    Install first script in list wg_scripts_to_install and remove it from
    list.
    """
    global wg_scripts, wg_scripts_to_install, wg_current_script_install
    global wg_hook_process
    if len(wg_scripts) == 0:
        return
    # be sure weeget is ALWAYS last script to install/update
    # otherwise we'll lose end of list when weeget is unloaded by WeeChat
    if SCRIPT_NAME in wg_scripts_to_install:
        wg_scripts_to_install.remove(SCRIPT_NAME)
        wg_scripts_to_install.append(SCRIPT_NAME)
    # loop until a script is installed, or end if list is empty
    while len(wg_scripts_to_install) > 0:
        name = wg_scripts_to_install.pop(0)
        script = wg_search_script_by_name(name)
        if script == None:
            weechat.prnt("", "%s: script \"%s%s%s\" not found"
                         % (SCRIPT_NAME,
                            wg_config_color("script"),
                            name,
                            weechat.color("chat")))
        else:
            status = wg_get_local_script_status(script)
            if status["installed"] and not status["obsolete"]:
                weechat.prnt("",
                             "%s: script \"%s%s%s\" is already "
                             "installed and up to date"
                             % (SCRIPT_NAME,
                                wg_config_color("script"),
                                script["full_name"],
                                weechat.color("chat")))
            else:
                weechat.prnt("", "%s: downloading \"%s%s%s\"..."
                             % (SCRIPT_NAME,
                                wg_config_color("script"),
                                script["full_name"],
                                weechat.color("chat")))
                if wg_hook_process["script"] != "":
                    weechat.unhook(wg_hook_process["script"])
                    wg_hook_process["script"] = ""
                wg_current_script_install = script
                filename = wg_config_get_dir() + os.sep + script["full_name"]
                python2_bin = weechat.info_get("python2_bin", "") or "python"
                wg_hook_process["script"] = weechat.hook_process(
                    python2_bin + " -c \"import urllib, urllib2\n"
                    "req = urllib2.Request('" + script["url"] + "')\n"
                    "try:\n"
                    "    response = urllib2.urlopen(req)\n"
                    "    file = open('" + filename + "', 'w')\n"
                    "    file.write(response.read())\n"
                    "    response.close()\n"
                    "    file.close()\n"
                    "except urllib2.URLError, e:\n"
                    "    print 'error:%s' % e.code\n"
                    "\"",
                    TIMEOUT_SCRIPT, "wg_process_script_cb", "")
                # this function will be called again when script will be
                # downloaded
                return

def wg_install_scripts(names):
    """ Install scripts. """
    global wg_scripts_to_install
    for name in names.split(" "):
        wg_scripts_to_install.append(name)
    wg_install_next_script()

def wg_process_script_cb(data, command, rc, stdout, stderr):
    """ Callback when reading a script from website. """
    global wg_hook_process, wg_stdout, wg_current_script_install, wg_loaded_scripts
    if stdout != "":
        wg_stdout["script"] += stdout
    if stderr != "":
        wg_stdout["script"] += stderr
    if int(rc) >= 0:
        if wg_stdout["script"].startswith("error:"):
            weechat.prnt("", "%s%s: error downloading script (%s)"
                         % (weechat.prefix("error"), SCRIPT_NAME,
                            wg_stdout["update"][6:].strip()))
        else:
            # ask C plugin to install/load script
            weechat.hook_signal_send(wg_current_script_install["language"] + "_script_install",
                                     weechat.WEECHAT_HOOK_SIGNAL_STRING,
                                     wg_config_get_dir() + os.sep + wg_current_script_install["full_name"])
        wg_hook_process["script"] = ""
        wg_install_next_script()
    return weechat.WEECHAT_RC_OK

def wg_check_scripts():
    """
    Check status of local script(s).
    For each script found, display status (unknown/running/new version available).
    For example:
       r   python/autoload/vdm.py
      ?r   python/autoload/dummy.py
       rN  python/shell.py
           perl/buffers.pl
    """
    local_scripts_status = wg_get_local_scripts_status()
    if len(local_scripts_status) == 0:
        return
    weechat.prnt("", "")
    weechat.prnt("", "Local scripts:")
    for file, status in local_scripts_status:
        str_unknown = " "
        str_running = " "
        str_obsolete = " "
        if status["unknown"]:
            str_unknown = "?"
        if status["running"]:
            str_running = "r"
        if status["obsolete"]:
            str_obsolete = "N"
        weechat.prnt("", "%s%s%s%s%s%s%s  %s%s%s%s"
                     % (wg_config_color("unknown"), str_unknown,
                        wg_config_color("running"), str_running,
                        wg_config_color("obsolete"), str_obsolete,
                        weechat.color("chat"),
                        os.path.dirname(file),
                        os.sep,
                        wg_config_color("script"),
                        os.path.basename(file)))

def wg_upgrade_scripts():
    """ Upgrade scripts. """
    global wg_scripts, wg_scripts_to_install
    if len(wg_scripts) == 0:
        return
    scripts_to_upgrade = []
    for id, script in wg_scripts.iteritems():
        status = wg_get_local_script_status(script)
        if status["installed"] and status["obsolete"]:
            scripts_to_upgrade.append(script["name"])
    if len(scripts_to_upgrade) == 0:
        weechat.prnt("", "%s: all scripts are up to date" % SCRIPT_NAME)
    else:
        wg_scripts_to_install.extend(scripts_to_upgrade)
        wg_install_next_script()

def wg_remove_scripts(names):
    """ Remove scripts. """
    if len(wg_scripts) == 0:
        return
    list_names = names.split(" ")
    scripts_to_remove = {}
    for language in SCRIPT_EXTENSION.keys():
        scripts_to_remove[language] = []
    for name in list_names:
        script = wg_search_script_by_name(name)
        if script == None:
            weechat.prnt("", "%s: script \"%s%s%s\" not found"
                         % (SCRIPT_NAME,
                            wg_config_color("script"),
                            name,
                            weechat.color("chat")))
        else:
            if script["full_name"] not in scripts_to_remove[script["language"]]:
                scripts_to_remove[script["language"]].append(script["full_name"])
    for language in SCRIPT_EXTENSION.keys():
        if len(scripts_to_remove[language]) > 0:
            # ask C plugin to remove script file(s)
            weechat.hook_signal_send(language + "_script_remove",
                                     weechat.WEECHAT_HOOK_SIGNAL_STRING,
                                    ",".join(scripts_to_remove[language]))

# ==================================[ xml ]===================================

def wg_execute_action():
    """ Execute action. """
    global wg_action, wg_action_args, wg_loaded_scripts
    if wg_action != "":
        wg_get_loaded_scripts()
        if wg_action == "list":
            wg_list_scripts(wg_action_args)
        elif wg_action == "listinstalled":
            wg_list_scripts(wg_action_args, installed=True)
        elif wg_action == "show":
            wg_show_script(wg_action_args)
        elif wg_action == "install":
            wg_install_scripts(wg_action_args)
        elif wg_action == "check":
            wg_check_scripts()
        elif wg_action == "upgrade":
            wg_upgrade_scripts()
        elif wg_action == "remove":
            wg_remove_scripts(wg_action_args)
        else:
            weechat.prnt("", "%s%s: unknown action \"%s\""
                         % (weechat.prefix("error"), SCRIPT_NAME, wg_action))
    
    # reset action
    wg_action = ""
    wg_action_args = ""
    wg_loaded_scripts = {}

def wg_check_version(script):
    """ Check if a script is designed for current running WeeChat version."""
    version = weechat.info_get("version", "")
    version = version.split("-", 1)[0]
    vmin = script.get("min_weechat", "")
    vmax = script.get("max_weechat", "")
    if vmin != "" and version < vmin:
        return False
    if vmax != "" and version > vmax:
        return False
    return True

def wg_parse_xml():
    """
    Parse XML scripts list and return dictionary with list, with key 'id'.
    Example of item return in dictionary :
      '119': { 'name'        : 'weeget',
               'version'     : '0.1',
               'url'         : 'http://www.weechat.org/files/scripts/weeget.py',
               'language'    : 'python',
               'license'     : 'GPL3',
               'md5sum'      : 'd500714fc19b0e10cc4e339e70739e4ad500714fc19b0e10cc4e339e70739e4a',
               'tags'        : 'scripts',
               'desc_en'     : 'Scripts manager.',
               'desc_fr'     : 'Gestionnaire de scripts.',
               'requirements': 'python 2.5',
               'min_weechat' : '0.3.0',
               'max_weechat' : '',
               'author'      : 'FlashCode',
               'mail'        : 'flashcode [at] flashtux [dot] org',
               'added'       : '2009-04-05 22:39:18',
               'updated'     : '0000-00-00 00:00:00' }
    """
    global wg_scripts, wg_action, wg_action_args
    wg_scripts = {}
    try:
        f = gzip.open(wg_config_get_cache_filename(), "rb")
        string = f.read()
        f.close()
    except:
        weechat.prnt("", "%s%s: unable to read xml file"
                     % (weechat.prefix("error"), SCRIPT_NAME))
    else:
        try:
            dom = xml.dom.minidom.parseString(string)
        except:
            weechat.prnt("",
                         "%s%s: unable to parse xml list of scripts"
                         % (weechat.prefix("error"), SCRIPT_NAME))
            # discard action
            wg_action = ""
            wg_action_args = ""
        else:
            for scriptNode in dom.getElementsByTagName("plugin"):
                id = scriptNode.getAttribute("id")
                script = {}
                for node in scriptNode.childNodes:
                    if node.nodeType == node.ELEMENT_NODE:
                        if node.firstChild != None:
                            nodename = node.nodeName.encode("utf-8")
                            value = node.firstChild.data.encode("utf-8")
                            script[nodename] = value
                if script["language"] in SCRIPT_EXTENSION:
                    script["full_name"] = script["name"] + "." + SCRIPT_EXTENSION[script["language"]]
                    if wg_check_version(script):
                        wg_scripts[id] = script
            wg_execute_action()

def wg_process_update_cb(data, command, rc, stdout, stderr):
    """ Callback when reading XML cache file from website. """
    global wg_hook_process, wg_stdout, wg_scripts
    if stdout != "":
        wg_stdout["update"] += stdout
    if stderr != "":
        wg_stdout["update"] += stderr
    if int(rc) >= 0:
        if wg_stdout["update"].startswith("error:"):
            weechat.prnt("", "%s%s: error downloading scripts (%s)"
                         % (weechat.prefix("error"), SCRIPT_NAME,
                            wg_stdout["update"][6:].strip()))
        else:
            weechat.prnt("", "%s: scripts downloaded" % SCRIPT_NAME)
            wg_parse_xml()
        wg_hook_process["update"] = ""
    return weechat.WEECHAT_RC_OK

def wg_update_cache():
    """ Download list of scripts and update local cache. """
    global wg_config_option, wg_hook_process, wg_stdout
    # get data from website, via hook_process
    if wg_hook_process["update"] != "":
        weechat.unhook(wg_hook_process["update"])
        wg_hook_process["update"] = ""
    weechat.prnt("", "%s: downloading list of scripts..." % SCRIPT_NAME)
    wg_stdout["update"] = ""
    wg_config_create_dir()
    url = weechat.config_string(wg_config_option["scripts_url"])
    filename = wg_config_get_cache_filename()
    python2_bin = weechat.info_get("python2_bin", "") or "python"
    wg_hook_process["update"] = weechat.hook_process(
        python2_bin + " -c \"import urllib, urllib2\n"
        "req = urllib2.Request('" + url + "')\n"
        "try:\n"
        "    response = urllib2.urlopen(req)\n"
        "    file = open('" + filename + "', 'w')\n"
        "    file.write(response.read())\n"
        "    response.close()\n"
        "    file.close()\n"
        "except urllib2.URLError, e:\n"
        "    print 'error:%s' % e.code\n"
        "\"",
        TIMEOUT_UPDATE, "wg_process_update_cb", "")

def wg_read_scripts(download_list=True):
    """ Read scripts list (download list if needed and asked). """
    global wg_scripts
    cache_file = wg_config_get_cache_filename()
    if os.path.isfile(cache_file):
        # check if local cache file is too old
        cache_expire = weechat.config_integer(wg_config_option["scripts_cache_expire"]) * 60
        if cache_expire >= 0:
            diff_time = time.time() - os.stat(cache_file)[stat.ST_MTIME]
            if download_list and diff_time >= cache_expire:
                os.unlink(cache_file)
                wg_scripts.clear()
    if len(wg_scripts) > 0:
        wg_execute_action()
    else:
        if os.path.isfile(cache_file):
            wg_parse_xml()
        elif download_list:
            wg_update_cache()

# ================================[ command ]=================================

def wg_cmd(data, buffer, args):
    """ Callback for /weeget command. """
    global wg_action, wg_action_args
    if args == "":
        weechat.command("", "/help %s" % SCRIPT_COMMAND)
        return weechat.WEECHAT_RC_OK
    argv = args.strip().split(" ", 1)
    if len(argv) == 0:
        return weechat.WEECHAT_RC_OK
    
    wg_action = ""
    wg_action_args = ""
    
    # check arguments
    if len(argv) < 2:
        if argv[0] == "show" or \
                argv[0] == "install" or \
                argv[0] == "remove":
            weechat.prnt("", "%s: too few arguments for action \"%s\""
                         % (SCRIPT_NAME, argv[0]))
            return weechat.WEECHAT_RC_OK
    
    # execute asked action
    if argv[0] == "update":
        wg_update_cache()
    else:
        wg_action = argv[0]
        wg_action_args = ""
        if len(argv) > 1:
            wg_action_args = argv[1]
        wg_read_scripts()
    
    return weechat.WEECHAT_RC_OK

def wg_completion_scripts_cb(data, completion_item, buffer, completion):
    """ Complete with known script names, for command '/weeget'. """
    global wg_scripts
    wg_read_scripts(download_list=False)
    if len(wg_scripts) > 0:
        for id, script in wg_scripts.iteritems():
            weechat.hook_completion_list_add(completion, script["name"],
                                             0, weechat.WEECHAT_LIST_POS_SORT)
    return weechat.WEECHAT_RC_OK

# ==================================[ main ]==================================

if __name__ == "__main__" and import_ok:
    if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                        SCRIPT_DESC, "wg_unload_script", ""):
        wg_config_init()
        wg_config_read()
        if weechat.config_string(wg_config_option["scripts_url"]).find("weechat.flashtux.org") >= 0:
            weechat.prnt("", "%sWarning: old site still used in URL for plugins.xml.gz, you should do:  /unset wg.scripts.url"
                         % weechat.prefix("error"))
        str_installed = wg_config_color("installed") + "i" + weechat.color("chat")
        str_unknown = wg_config_color("unknown") + "?" + weechat.color("chat")
        str_running = wg_config_color("running") + "r" + weechat.color("chat")
        str_obsolete = wg_config_color("obsolete") + "N" + weechat.color("chat")
        weechat.hook_command(SCRIPT_COMMAND,
                             "WeeChat scripts manager",
                             "[list [<text>] | listinstalled [<text>] | show <script> | "
                             "install <script> [<script>...] | check | update | "
                             "upgrade | remove <script> [<script>...]]",
                             "         list: list scripts (search text if given)\n"
                             "listinstalled: list installed scripts (search text if given)\n"
                             "         show: show detailed information about a script (in repository)\n"
                             "      install: install/upgrade script(s)\n"
                             "        check: check if local scripts needs upgrade\n"
                             "       update: update local scripts cache\n"
                             "      upgrade: upgrade all local scripts if they are obsolete\n"
                             "       remove: remove script(s)\n\n"
                             "Indicators in lists (first column):\n"
                             "  " + str_installed + "  script is installed\n"
                             "  " + str_unknown   + "  unknown script\n"
                             "  " + str_running   + "  script is running (loaded)\n"
                             "  " + str_obsolete  + "  script is obsolete (new version available)\n\n"
                             "Examples:\n"
                             "  /" + SCRIPT_COMMAND + " list            => list all scripts\n"
                             "  /" + SCRIPT_COMMAND + " list game       => list all scripts with text/tag \"game\"\n"
                             "  /" + SCRIPT_COMMAND + " install weetris => install script weetris.pl\n"
                             "  /" + SCRIPT_COMMAND + " remove weetris  => remove script weetris.pl",
                             "list %(weeget_scripts)"
                             " || listinstalled %(weeget_scripts)"
                             " || show %(weeget_scripts)"
                             " || install %(weeget_scripts)|%*"
                             " || check"
                             " || update"
                             " || upgrade"
                             " || remove %(weeget_scripts)|%*",
                             "wg_cmd", "")
        weechat.hook_completion("weeget_scripts", "list of scripts in repository",
                                "wg_completion_scripts_cb", "")

# ==================================[ end ]===================================

def wg_unload_script():
    """ Function called when script is unloaded. """
    wg_config_write()
    return weechat.WEECHAT_RC_OK
