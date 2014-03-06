#!/bin/sh
# $Id: bisonOut.sh,v 1.1.1.1 2009/07/20 19:38:16 profw Exp $
# Copyright 2009, The Regents of the University of Colorado

# EXEC (bisonOut.sh) (:bisonGen :name) (:bisonInfo :name) (+parser)
#   => (:bisonOut) (:bisonData);

ODIN_bison=$1;shift; ODIN_info=$1;shift; ODIN_parser=$1;shift;

if test 'bisonglr' = "$ODIN_parser"
then
  cp $ODIN_bison bisonOut
  cp $ODIN_info bisonData
else
  cp /dev/null bisonOut
  cp /dev/null bisonData
fi
