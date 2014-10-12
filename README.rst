Getting Started
--------------------------
Get rcm:

.. code-block: bash

     sudo apt-add-repository ppa:martin-frost/thoughtbot-rcm
     sudo apt-get update
     sudo apt-get install rcm

Then run:

.. code-block: bash

    ./install.sh


Emacs
--------------------------
The first launch of emacs will not have the plugins installed, you must
open emacs and run:

.. code-block:

    M-x eval-expression
    (setup-packaging-system)

Then you can close/re-open emacs
