#!/usr/bin/env python
import sys
import re
import weechat
from urllib import urlencode
from urllib2 import urlopen

SCRIPT_NAME    = "shortenurl"
SCRIPT_AUTHOR  = "John Anderson <sontek@gmail.com>"
SCRIPT_VERSION = "0.1"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC    = "Shorten long incoming and outgoing URLs"

ISGD = 'http://is.gd/api.php?%s'

# script options
settings = {
    "color": "red",
    "urllength": "30",
}


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

    weechat.hook_print("", "", "", 1, "hook_print_callback", "")


def command_input_callback(data, buffer, command):
    """ Function called when a command "/input xxxx" is run """
    if command == '/input return':
        input = weechat.buffer_get_string(buffer, 'input')
        input = match_url(input, buffer, True)

        weechat.buffer_set(buffer, 'input', input)
    return weechat.WEECHAT_RC_OK


def hook_print_callback(data, buffer, date, tags, displayed, highlight, prefix, message):
    if 'notify_message' in tags.split(','):
        return match_url(message, buffer, False)

    return weechat.WEECHAT_RC_OK

def match_url(message, buffer, from_self):
    octet = r'(?:2(?:[0-4]\d|5[0-5])|1\d\d|\d{1,2})'
    ipAddr = r'%s(?:\.%s){3}' % (octet, octet)
    # Base domain regex off RFC 1034 and 1738
    label = r'[0-9a-z][-0-9a-z]*[0-9a-z]?'
    domain = r'%s(?:\.%s)*\.[a-z][-0-9a-z]*[a-z]?' % (label, label)
    urlRe = re.compile(r'(\w+://(?:%s|%s)(?::\d+)?(?:/[^\])>\s]*)?)' % (domain, ipAddr), re.I)

    new_message = message
    for url in urlRe.findall(message):
        if len(url) > int(weechat.config_get_plugin('urllength')):
            if from_self:
                short_url = tiny_url(url)
                new_message = new_message.replace(url, short_url)
            else:
                tiny_url_process(url, buffer, from_self)

    if from_self:
        return new_message
    else:
        return weechat.WEECHAT_RC_OK 

def tiny_url(url):
    url = ISGD % urlencode({'longurl':url})
    try:
        return urlopen(url).read()
    except:
        return  url

def tiny_url_process(url, buffer, from_self):
    """ Query a Tinyurl service, if not available query an alternative one
        If the alternative service is not available, don't do anything
    """ 
    url = ISGD % urlencode({'longurl':url})

    shortenurl_hook_process = weechat.hook_process(
                 "python -c \"import urllib2; print urllib2.urlopen('" + url + "').read()\"",
                 10 * 1000, "process_complete", buffer)

def process_complete(data, command, rc, stdout, stderr):
    url = stdout.strip()
    if url:
        color = weechat.color(weechat.config_get_plugin("color"))
        weechat.prnt(data, '%s[%s]' % (color, url))

    return weechat.WEECHAT_RC_OK

