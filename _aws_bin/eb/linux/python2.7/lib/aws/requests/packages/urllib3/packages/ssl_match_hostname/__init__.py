
#This code has been modified. Portions copyright 2011-2012 Amazon.com, Inc. or 
#its affiliates. Please see LICENSE.txt for applicable license terms and 
#NOTICE.txt for applicable notices.  

"""The match_hostname() function from Python 3.2, essential when using SSL."""

import re

__version__ = '3.2.2'

class CertificateError(ValueError):
    pass


def _reject_wild_cert(dn):
    # Reject a wild cert. Return True if rejects otherwise False
    if re.compile('^[.*]+$').match(dn):    # reject cert only having "*" and/or "."
        return True 
    elif re.compile('^\*\.[^.]+$').match(dn):   #reject top level domain wild card
        return True
    elif re.compile('^\*\.co\.[^.]+$').match(dn):   # reject *.co.* domain cert
        return True
    else:
        return False

def _dnsname_to_pat(dn):
    pats = []
    for frag in dn.split(r'.'):
        if frag == '*':
            # When '*' is a fragment by itself, it matches a non-empty dotless
            # fragment.
            pats.append('[^.]+')
        else:
            # Otherwise, '*' matches any dotless fragment.
            frag = re.escape(frag)
            pats.append(frag.replace(r'\*', '[^.]*'))
    return re.compile(r'\A' + r'\.'.join(pats) + r'\Z', re.IGNORECASE)

def match_hostname(cert, hostname):
    """Verify that *cert* (in decoded format as returned by
    SSLSocket.getpeercert()) matches the *hostname*.  RFC 2818 rules
    are mostly followed, but IP addresses are not accepted for *hostname*.

    CertificateError is raised on failure. On success, the function
    returns nothing.
    """
    if not cert:
        raise ValueError("empty or no certificate")
    dnsnames = []
    san = cert.get('subjectAltName', ())
    for key, value in san:
        if key == 'DNS':
            if _dnsname_to_pat(value).match(hostname):
                return
            dnsnames.append(value)
    if not dnsnames:
        # The subject is only checked when there is no dNSName entry
        # in subjectAltName
        for sub in cert.get('subject', ()):
            for key, value in sub:
                # XXX according to RFC 2818, the most specific Common Name
                # must be used.
                if key == 'commonName':
                    if not _reject_wild_cert(value) and \
                        _dnsname_to_pat(value).match(hostname):
                        return
                    dnsnames.append(value)
    if len(dnsnames) > 1:
        raise CertificateError("hostname %r "
            "doesn't match either of %s"
            % (hostname, ', '.join(map(repr, dnsnames))))
    elif len(dnsnames) == 1:
        raise CertificateError("hostname %r "
            "doesn't match %r"
            % (hostname, dnsnames[0]))
    else:
        raise CertificateError("no appropriate commonName or "
            "subjectAltName fields were found")
