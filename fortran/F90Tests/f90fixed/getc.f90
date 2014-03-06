      SUBROUTINE GETC (NUMBLK,IGEOM,IDMAX,JDMAX,XD,YD)
C
C**********************************************************************
C
C
C     Read in mesh information for the problem
C
C     Author: John K. Prentice, Quetzal Computational Associates.
C     written:09/12/92
C
C**********************************************************************
C
C     All real numbers are double precision.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Data to define the maximum mesh dimensions
      PARAMETER (IXMAX=1003, IYMAX=1003, IZMAX=503)
C     Data to define the maximum number of geometry blocks
      PARAMETER (IGBMAX=3)
C
C	storage required for the coordinates routine
C
      DIMENSION XCOORD(IXMAX),YCOORD(IYMAX),IDMAX(IGBMAX),JDMAX(IGBMAX),
     &          XD(IXMAX,IGBMAX),YD(IYMAX,IGBMAX)
      LOGICAL EXIST
      DATA IOVER /55/
C
C#######################################################################
C
C	see if the coordinates file exists.  If not, abort.  If so,
C	read through it and verify that the mesh information on
C	this file agrees with that on the CTH restart file	
C
C#######################################################################
C 
      INQUIRE (FILE='coordinates',EXIST=EXIST)
      IF (.NOT.EXIST) THEN
C
C	no coordinates file exists, abort
C
          WRITE (*,'(/'' Fatal error in subroutine GETC. ''/
     *                '' No coordinates file was found.  This file '',
     *                ''must be present in domain decomposition '',
     *                ''calculations.''/'' The coordinates '',
     *                '' file should be named coordinates.''
     *                /)')
          STOP
      ENDIF
C
C	open the coordinates file
C
      OPEN (UNIT=IOVER,FILE='coordinates',STATUS='OLD',
     *                                         FORM='FORMATTED')
C
C	read the number of blocks and the geometry from the coordinates file 
C	and check to see they are correct
C
      READ(IOVER,*) NUMBLK,IGEOM
C
C
C	loop over the blocks, read in the coordinate information
C
      DO IBLK=1,NUMBLK
C
          READ(IOVER,*) IMAX,JMAX
          IDMAX(IBLK) = IMAX
          JDMAX(IBLK) = JMAX
C
C	read in the x coordinates for this block 
C
          READ(IOVER,*) (XCOORD(I),I=1,IMAX+1)
          DO I = 1,IMAX+1
            XD(I,IBLK) = XCOORD(I)
          ENDDO  
C
C	read in the y coordinates for this block 
C
          IF (IGEOM.GE.20) THEN
            READ(IOVER,*) (YCOORD(J),J=1,JMAX+1)
            DO J = 1,JMAX+1
              YD(J,IBLK) = YCOORD(J)
            ENDDO
          ENDIF
C
      ENDDO
C
      CLOSE (UNIT=IOVER)
C
      END
