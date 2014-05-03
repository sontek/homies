# Copyright (c) 2010, 2011, 2012, 2013 by John Anderson <sontek@gmail.com>
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
# 2014-5-3, John Anderson <sontek@gmail.com>
#   version 0.5.2: Fixed short_own bug introduced in 0.5, notify the short url
#                instead of appending to the message (returning to behavior
#                from 0.4)
# 2013-12-25, John Anderson <sontek@gmail.com>
#   version 0.5: Added support for latest weechat (0.4+)
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

SCRIPT_NAME = "shortenurl"
SCRIPT_AUTHOR = "John Anderson <sontek@gmail.com>"
SCRIPT_VERSION = "0.5.2"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC = "Shorten long incoming and outgoing URLs"

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
    "short_own": "off",
    "ignore_list": "http://is.gd,http://tinyurl.com",
}

octet = r'(?:2(?:[0-4]\d|5[0-5])|1\d\d|\d{1,2})'
ipAddr = r'%s(?:\.%s){3}' % (octet, octet)
# Base domain regex off RFC 1034 and 1738
label = r'[0-9a-z][-0-9a-z]*[0-9a-z]?'
domain = r'%s(?:\.%s)*\.[a-z][-0-9a-z]*[a-z]?' % (label, label)
urlRe = re.compile(
    r'(\w+://(?:%s|%s)(?::\d+)?(?:/[^\])>\s]*)?)' % (domain, ipAddr),
    re.I
)


if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                    SCRIPT_DESC, "", ""):

    for option, default_value in settings.iteritems():
        if weechat.config_get_plugin(option) == "":
            weechat.config_set_plugin(option, default_value)

    weechat.hook_print('', 'irc_privmsg', '', 1, 'notify', '')
    short_own = weechat.config_get_plugin('short_own')

    if short_own != 'off':
        weechat.hook_modifier('irc_out_privmsg', 'outgoing_hook', '')


def notify(data, buf, date, tags, displayed, hilight, prefix, msg):
    color = weechat.color(weechat.config_get_plugin('color'))
    reset = weechat.color('reset')

    my_nick = weechat.buffer_get_string(buf, 'localvar_nick')
    if prefix != my_nick:
        urls = find_and_process_urls(msg)

        for url, short_url in urls:
            weechat.prnt(buf, '%(color)s[%(url)s)]%(reset)s' % dict(
                color=color,
                url=short_url,
                reset=reset))

    return weechat.WEECHAT_RC_OK


def outgoing_hook(data, modifier, modifier_data, msg):
    urls = find_and_process_urls(msg)
    for url, short_url in urls:
        msg = msg.replace(url, '%(short_url)s [%(url)s]' % dict(
            url=url,
            short_url=short_url))

    return msg


def find_and_process_urls(new_message):
    urls = []

    for url in urlRe.findall(new_message):
        max_url_length = int(weechat.config_get_plugin('urllength'))

        if len(url) > max_url_length and not should_ignore_url(url):
            short_url = get_shortened_url(url)
            urls.append((url, short_url))

    return urls


def get_shortened_url(url):
    shortener = weechat.config_get_plugin('shortener')
    if shortener == 'isgd':
        url = ISGD % urlencode({'longurl': url})
    if shortener == 'tinyurl':
        url = TINYURL % urlencode({'url': url})
    try:
        return urlopen(url).read()
    except:
        return url


def should_ignore_url(url):
    ignorelist = weechat.config_get_plugin('ignore_list').split(',')

    for ignore in ignorelist:
        if len(ignore) > 0 and ignore in url:
            return True

    return False
