#!/bin/sh
# Shell script to test modules having some properties defined

CURRYBIN="../bin"

ALLTESTS="Combinatorial ShowS Sort"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
$CURRYBIN/cleancurry

CCOPTS="-m100 -dInt"
LOGFILE=xxx$$

if [ $VERBOSE = yes ] ; then
  $CURRYBIN/currycheck $CCOPTS $ALLTESTS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYBIN/currycheck $CCOPTS $ALLTESTS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in currycheck:"
    cat $LOGFILE
    exit 1
  fi
fi

################ end of tests ####################
# Clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
