#!/bin/sh
# 
# 
# /* This file is part of the Eli translator construction system.
# 
# Eli is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2, or (at your option) any later
# version.
# 
# Eli is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License along
# with Eli; see the file COPYING.  If not, write to the Free Software
# Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 
# 

# EXEC (bisonInvoker.sh) (:bisonSpecification) (bison.h) (bison.head)
#      (mon_cprods.h)
#   => (:bisonGen) (:bisonInfo);

BISON_spec=$1;shift; BISON_h=$1;shift; BISON_head=$1;shift; BISON_mon=$1;shift;
mkdir bisonGen

echo "====================================================" >> bisonInfo
/usr/bin/bison -v $BISON_spec -o bison.c 1> bisonInfo 2>&1
cp $BISON_h bisonGen
cp $BISON_head bisonGen
if test -s bison.c
then 
  cp bison.c bisonGen
  cp $BISON_mon bisonGen
else
  echo "Generated parser not found. There must be an error during the generation process." >> ERRORS
  exit 0
fi

if test -s bison.output
then
  echo "====================================================" >> bisonInfo
  cat bison.output >> bisonInfo
  echo "====================================================" >> bisonInfo
fi

