#!/bin/sh

srcdir="${srcroot}/guile-lib/unit-tests"
libdir="${srcroot}/guile-lib"

export GUILE_LOAD_PATH="$libdir/src"

for test in $srcdir/*.scm; do
    guile -s $test
done

# arch-tag: b2cef361-ec4a-43aa-9e42-7611bb43b238
