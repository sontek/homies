#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
certs.py
~~~~~~~~

This module returns the preferred default CA certificate bundle.

If you are packaging Requests, e.g., for a Linux distribution or a managed
environment, you can change the definition of where() to return a separately
packaged CA bundle.
"""

import os.path
from lib.utility import shell_utils
from scli.constants import CABundle

def where():
    """Return the preferred certificate bundle."""
    # vendored bundle inside Requests
    return   os.path.join(shell_utils.ori_path(), CABundle.Path, CABundle.Name)

if __name__ == '__main__':
    print(where())
