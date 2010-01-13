#!/usr/bin/env python
import sys
import re

try:
    from urllib import urlencode
    from urllib2 import urlopen
except:
    raise ImportError("You must have urllib and urllib2")

try:
    import weechat
    in_weechat = True
except:
    in_weechat = False

def match_url(data, buffer, date, tags, displayed, highlight, prefix, message):
    octet = r'(?:2(?:[0-4]\d|5[0-5])|1\d\d|\d{1,2})'
    ipAddr = r'%s(?:\.%s){3}' % (octet, octet)
    # Base domain regex off RFC 1034 and 1738
    label = r'[0-9a-z][-0-9a-z]*[0-9a-z]?'
    domain = r'%s(?:\.%s)*\.[a-z][-0-9a-z]*[a-z]?' % (label, label)
    urlRe = re.compile(r'(\w+://(?:%s|%s)(?::\d+)?(?:/[^\])>\s]*)?)' % (domain, ipAddr), re.I)

    for url in urlRe.findall(message):
        if len(url) > 30:
            tiny_url(url, buffer)

    if in_weechat:
        return weechat.WEECHAT_RC_OK 


def tiny_url(url, buffer):
    """ Query a Tinyurl service, if not available query an alternative one
        If the alternative service is not available, don't do anything
    """ 
    TINYURL = 'http://tinyurl.com/apicreate.php?%s'
    ISGD = 'http://is.gd/api.php?%s'

    try:
        url = urlopen(ISGD % urlencode({'longurl':url})).read()
    except:
        url = urlopen(TINYURL % urlencode({'url':url})).read()
    finally:
        if url:
            if in_weechat:
                weechat.prnt(buffer, '[%s]' % (url))
            else:
                print url

if in_weechat:
    weechat.register( "ShortenUrl", "John Anderson (sontek)", "1.0", "GPL", "Waits for URLs and sends them to a url shrink service", "", "" )
    hook = weechat.hook_print("", "", "", 1, "match_url", "")
else:
    args = sys.argv[1:]

    if not args:
        print "Please provide at least one url to shorten"
    else:
        for arg in args:
            match_url(None, 'Buffer', None, None, None, None, None, arg)
