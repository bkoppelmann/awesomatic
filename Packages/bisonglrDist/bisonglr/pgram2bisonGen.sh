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

# EXEC (pgram2bisonGen.sh) (pgram2bison.exe) (:Pgram) (:BuPgram)
#   => (:bisonSpecification);

ODIN_bison=$1;shift; ODIN_pgram=$1;shift; ODIN_bupgram=$1;shift;
SED=$ODINCACHE/PKGS/skeleton/sed.exe

$ODIN_bison $ODIN_pgram \
  | $SED -e 's/&(T_POS(/(POSITION*)\&(T_POS(/g' \
  > bisonSpecification
