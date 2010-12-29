# .pdbrc only allows for debugger commands; you cannot insert Python
# scripts.

# To overcome this restriction, this .pdbrc executes ~/.pdbrc.py,
# which can contain arbitrary Python commands (including a call to a
# local pdbrc.py (no leading dot!) in your working directory if it
# exists).

# If ~/.pdbrc.py is missing, you get an error message (which doesn't
# hurt).

import os
execfile(os.path.expanduser("~/.pdbrc.py"))
