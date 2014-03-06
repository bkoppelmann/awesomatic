#ifndef INIT_H1
#define INIT_H1
/* set the output file names to the strings found on
   command line position expfile, ordfile, optfile, befile
   (2nd, 3rd, 4th, 5th command line argument) (peter131192) */

	exp_fname= strng[GetClpValue(expfile,0)];
	ord_fname= strng[GetClpValue(ordfile,0)];
	opt_fname= strng[GetClpValue(optfile,0)];
	be_fname= strng[GetClpValue(befile,0)];


#endif

#ifndef INIT_H2
#define INIT_H2
/* $Id: liga.init,v 4.3 1997/09/15 14:46:48 cogito Exp $ */
/* (C) Copyright 1997 University of Paderborn */

/* This file is part of the Eli Module Library.

The Eli Module Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Eli Module Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the Eli Module Library; see the file COPYING.LIB.
If not, write to the Free Software Foundation, Inc., 59 Temple Place -
Suite 330, Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Eli into the
   directory resulting from a :source derivation, you may use that
   created file as a part of that directory without restriction. */

InitTree();

#endif

