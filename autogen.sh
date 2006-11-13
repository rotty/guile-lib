#!/bin/bash
# Run this to generate all the initial makefiles, etc.

DIE=0
package=guile-lib
srcfile=src/md5.scm

. ./autogen-support.sh

# fool automake
echo '@setfilename guile-library.info' > new-doc/guile-library.texi
touch -d 'jan 23 1980' new-doc/guile-library.texi

CONFIGURE_DEF_OPT='--enable-maintainer-mode'

autogen_options $@

echo -n "+ check for build tools"
if test ! -z "$NOCHECK"; then echo ": skipped version checks"; else  echo; fi
version_check "autoconf" "$AUTOCONF autoconf autoconf-2.59" \
              "ftp://ftp.gnu.org/pub/gnu/autoconf/" 2 59 || DIE=1
version_check "automake" "$AUTOMAKE automake automake-1.8 automake-1.9" \
              "ftp://ftp.gnu.org/pub/gnu/automake/" 1 8 || DIE=1

die_check $DIE

aclocal_check || DIE=1

die_check $DIE

# if no arguments specified then this will be printed
if test -z "$*"; then
  echo "+ checking for autogen.sh options"
  echo "  This autogen script will automatically run ./configure as:"
  echo "  ./configure $CONFIGURE_DEF_OPT"
  echo "  To pass any additional options, please specify them on the $0"
  echo "  command line."
fi

toplevel_check $srcfile

tool_run "$aclocal" "$GW_ACLOCAL_FLAGS"

tool_run "$autoconf"
debug "automake: $automake"
tool_run "$automake" "-a -c"

test -n "$NOCONFIGURE" && {
  echo "skipping configure stage for package $package, as requested."
  echo "autogen.sh done."
  exit 0
}

echo "+ running configure ... "
test ! -z "$CONFIGURE_DEF_OPT" && echo "  ./configure default flags: $CONFIGURE_DEF_OPT"
test ! -z "$CONFIGURE_EXT_OPT" && echo "  ./configure external flags: $CONFIGURE_EXT_OPT"
echo

./configure $CONFIGURE_DEF_OPT $CONFIGURE_EXT_OPT || {
        echo "  configure failed"
        exit 1
}

echo "Now type 'make' to compile $package."
