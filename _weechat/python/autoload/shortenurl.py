# Copyright (c) 2010 by John Anderson <sontek@gmail.com>
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

# History
# 2011-10-24, Dmitry Geurkov <dmitry_627@mail.ru>
#   version 0.4.1: added: option "ignore_list" for a blacklist of shorten urls.
# 2011-01-17, nils_2 <weechatter@arcor.de>
#   version 0.4: URI will be shorten in /query, too.
#              : added: option "short_own".
# 2010-11-08, John Anderson <sontek@gmail.com>:
#   version 0.3: Get python 2.x binary for hook_process (fixes problem
#                when python 3.x is default python version, requires
#                WeeChat >= 0.3.4)

import re
import weechat
from urllib import urlencode
from urllib2 import urlopen

SCRIPT_NAME    = "shortenurl"
SCRIPT_AUTHOR  = "John Anderson <sontek@gmail.com>"
SCRIPT_VERSION = "0.4.1"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC    = "Shorten long incoming and outgoing URLs"

ISGD = 'http://is.gd/api.php?%s'
TINYURL = 'http://tinyurl.com/api-create.php?%s'

# script options
# shortener options:
#  - isgd
#  - tinyurl

settings = {
    "color": "red",
    "urllength": "30",
    "shortener": "isgd",
    "public": "off",
    "short_own": "off",
    "ignore_list": "",
}

octet = r'(?:2(?:[0-4]\d|5[0-5])|1\d\d|\d{1,2})'
ipAddr = r'%s(?:\.%s){3}' % (octet, octet)
# Base domain regex off RFC 1034 and 1738
label = r'[0-9a-z][-0-9a-z]*[0-9a-z]?'
domain = r'%s(?:\.%s)*\.[a-z][-0-9a-z]*[a-z]?' % (label, label)
urlRe = re.compile(r'(\w+://(?:%s|%s)(?::\d+)?(?:/[^\])>\s]*)?)' % (domain, ipAddr), re.I)


if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                    SCRIPT_DESC, "", ""):

    for option, default_value in settings.iteritems():
        if weechat.config_get_plugin(option) == "":
            weechat.config_set_plugin(option, default_value)

    # Hooks we want to hook
    hook_command_run = {
        "input" : ("/input return",  "command_input_callback"),
    }

    # Hook all hooks !
    for hook, value in hook_command_run.iteritems():
        weechat.hook_command_run(value[0], value[1], "")

    weechat.hook_print("", "notify_message", "://", 1, "hook_print_callback", "")
    weechat.hook_print("", "notify_private", "://", 1, "hook_print_callback", "")
    weechat.hook_print("", "notify_highlight", "://", 1, "hook_print_callback", "")


def command_input_callback(data, buffer, command):
    """ Function called when a command "/input xxxx" is run """
    if command == '/input return':
        input = weechat.buffer_get_string(buffer, 'input')
        input = match_url(input, buffer, True)

        weechat.buffer_set(buffer, 'input', input)
    return weechat.WEECHAT_RC_OK


def hook_print_callback(data, buffer, date, tags, displayed, highlight, prefix, message):
  if weechat.config_get_plugin('short_own') == 'on':
    # get servername
    infolist = weechat.infolist_get('buffer',buffer,'')
    weechat.infolist_next(infolist)
    servername,undef = weechat.infolist_string(infolist,'name').split('.',1)
    weechat.infolist_free(infolist)
    
    # get own nick
    my_nick = weechat.info_get( 'irc_nick', servername )
    if my_nick in tags:
      return weechat.WEECHAT_RC_OK

  return match_url(message, buffer, False)

  return weechat.WEECHAT_RC_OK

def match_url(message, buffer, from_self):
    new_message = message
    for url in urlRe.findall(message):
        if len(url) > int(weechat.config_get_plugin('urllength')) and not ignore_url(url):
            if from_self:
                public = weechat.config_get_plugin('public')
                if public == 'on':
                    short_url = tiny_url(url, None)
                    new_message = new_message.replace(url, short_url)
            else:
                tiny_url(url, buffer)

    if from_self:
        return new_message
    else:
        return weechat.WEECHAT_RC_OK 

def tiny_url(url, buffer):
    shortener = weechat.config_get_plugin('shortener')
    if shortener == 'isgd':
        url = ISGD % urlencode({'longurl':url})
    if shortener == 'tinyurl':
        url = TINYURL % urlencode({'url':url})
    try:
        if buffer:
            python2_bin = weechat.info_get('python2_bin', '') or 'python'
            shortenurl_hook_process = weechat.hook_process(
                        python2_bin + " -c \"import urllib2; print urllib2.urlopen('" + url + "').read()\"",
                        10 * 1000, "process_complete", buffer)
        else:
            return urlopen(url).read()
    except:
        return  url

def ignore_url(url):
  ignorelist = weechat.config_get_plugin('ignore_list').split(',')

  for ignore in ignorelist:
      if len(ignore) > 0 and ignore in url:
          return True

  return False

def process_complete(data, command, rc, stdout, stderr):
    url = stdout.strip()
    if url:
        color = weechat.color(weechat.config_get_plugin("color"))
        weechat.prnt(data, '%s[%s]' % (color, url))

    return weechat.WEECHAT_RC_OK

