#!/bin/bash
# Run this to generate all the initial makefiles, etc.

set -e

# fool automake
echo '@setfilename guile-library.info' > doc/guile-library.texi
touch -d 'jan 23 1980' doc/guile-library.texi

autoreconf -vif
exec ./configure "$@"
