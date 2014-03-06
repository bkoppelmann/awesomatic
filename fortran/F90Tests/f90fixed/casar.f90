      PROGRAM CASAR
C
C**********************************************************************
C
C     Calculate various geometrical overlap information for use by the
C     Quetzal Computational Associates domain decomposition solid
C     dynamics code
C
C
C**********************************************************************
C
C
C     Author: John K. Prentice, Quetzal Computational Associates.
C             Adapted from subroutine CEDRV, written by Mike McGlaun
C     written:07/20/92
C
C     All real numbers are double precision.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Data to define the maximum mesh dimensions
      PARAMETER (IXMAX=1003, IYMAX=1003, IZMAX=503)
C     Data to define the maximum number of geometry blocks
      PARAMETER (IGBMAX=3)
C
      DIMENSION IDMAX(IGBMAX),JDMAX(IGBMAX),XD(IXMAX,IGBMAX),
     &          YD(IYMAX,IGBMAX),I1(IGBMAX),I2(IGBMAX),
     &          J1(IGBMAX),J2(IGBMAX),LEVEL(IGBMAX)
C
C	open the output file 
C
      KPT6 = 6
      OPEN (UNIT=KPT6,FILE='casar_output',STATUS='UNKNOWN',
     &                                            FORM='FORMATTED')
C
C	open the CTH restart file and get the mesh geometry
C
      CALL GETC (NDOM,IGEOM,IDMAX,JDMAX,XD,YD)  
C
C	print cell locations for this mesh
C
      DO N=1,NDOM
        WRITE(KPT6,'(/'' Cell information for domain '',i5/)') N
        WRITE(KPT6,'(//'' x cell locations: ''/)')
        DO I = 1,IDMAX(N)
            XLEFT = XD(I,N)
            XRIGHT = XD(I+1,N)
            DX = XRIGHT - XLEFT
            WRITE(KPT6,'('' i = '',i5,'' xleft = '',1pe12.5,
     &                   '' xright = '',e12.5,'' dx = '',e12.5)') 
     &                   I,XLEFT,XRIGHT,DX
        ENDDO
C
        IF (IGEOM.GE.20) THEN
            WRITE(KPT6,'(//'' y cell locations: ''/)')
            DO J = 1,JDMAX(N)
                XLEFT = YD(J,N)
                XRIGHT = YD(J+1,N)
                DX = XRIGHT - XLEFT
                WRITE(KPT6,'('' j = '',i5,'' ybottom = '',1pe12.5,
     &                       '' ytop = '',e12.5,'' dy = '',e12.5)') 
     &                       J,XLEFT,XRIGHT,DX
            ENDDO
        ENDIF
      ENDDO
C
C	open the tree input file and read in the necessary
C	geometry information
C
      OPEN (UNIT=52,FILE='domain_tree',STATUS='OLD',FORM='FORMATTED')
C
      READ (52,*) NUMBLK
      IF (NUMBLK.NE.NDOM) THEN
          WRITE(KPT6,'(/'' Wrong number of domains on the domain_tree'',
     &         '' file, abort.''/'' Number of domains from file = '',i2/
     &         '' Number of domains from restart file = '',i2/)')
     &         NUMBLK,NDOM
          STOP
      ENDIF
C
      DO NN=1,NDOM
          READ(52,*) N,IMAX,JMAX,I1SAVE,I2SAVE,J1SAVE,J2SAVE,LSAVE
C
          IF (N.GT.IGBMAX) THEN
              WRITE(KPT6,'('' Input from domain_tree file for domain '',
     &                  i2,'' is incorrect, abort. ''/'' This domain '',
     &                  ''number exceeds the maximum number of '',
     &                  ''allowed domains.''/)')
              STOP
          ENDIF
C
          I1(N) = I1SAVE
          I2(N) = I2SAVE
          J1(N) = J1SAVE
          J2(N) = J2SAVE
          LEVEL(N) = LSAVE
C
          IF (IMAX.NE.IDMAX(N)) THEN
              WRITE(KPT6,'('' Incorrect IMAX value for domain '',i2,
     &           '', abort.''/'' IMAX from restart file is '',i5/
     &           '' IMAX from domain_tree file is '',i5/)') N,
     &           IDMAX(N),IMAX
              STOP
          ENDIF
C
          IF (JMAX.NE.JDMAX(N)) THEN
              WRITE(KPT6,'('' Incorrect JMAX value for domain '',i2,
     &           '', abort.''/'' JMAX from restart file is '',i5/
     &           '' JMAX from domain_tree file is '',i5/)') N,
     &           JDMAX(N),JMAX
              STOP
          ENDIF
C
      ENDDO       
C
      CLOSE (UNIT = 52)  
C
C	open the overlap information file and write the header info
C
      OPEN (UNIT=55,FILE='overlap_information',STATUS='UNKNOWN',
     &                                                 FORM='FORMATTED')
C
      WRITE (55,*) NDOM,IGEOM
      DO N = 1,NDOM
          WRITE(55,*) IDMAX(N),JDMAX(N)
          WRITE(55,*) (XD(I,N),I=1,IDMAX(N)+1)
          IF (IGEOM.GE.20) WRITE(55,*) (YD(J,N),J=1,JDMAX(N)+1)
      ENDDO
C
C	call the overlap routine to calculate the overlaps
C
      CALL OVRLAP (NDOM,IGEOM,IDMAX,JDMAX,XD,YD,I1,I2,J1,J2,LEVEL)
C
      CLOSE (UNIT=55)
C
      END
