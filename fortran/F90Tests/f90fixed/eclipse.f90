      DOUBLE PRECISION FUNCTION ECLIPSE (IGEOM,XMIN1,XMAX1,YMIN1,YMAX1,
     &                                         XMIN2,XMAX2,YMIN2,YMAX2)
C
C     This subroutine calculates the overlap between two rectangles
C
C     Called by: OVRLAP
C     Database manager: none
C
C     Author: John K. Prentice, Quetzal Computational Associates
C     written: 4/1/93
C
C
      IMPLICIT NONE
      DOUBLE PRECISION ECLIPSE,XMIN1,XMAX1,YMIN1,YMAX1,XMIN2,XMAX2,
     &                 YMIN2,YMAX2,ZERO,DX1,DY1,DX2,DY2,XMIN,XMAX,
     &                 YMIN,YMAX,DX,DY,FOUR_THIRDS,PI,REL_X_ERROR,
     &                 REL_Y_ERROR,SMALL
      INTEGER IGEOM
C
      PARAMETER (ZERO=0.0,FOUR_THIRDS=1.3333333333,PI=31.141592654,
     &           SMALL=1.E-10)
C
      DX1 = MAX(ZERO,XMAX1-XMIN1)
      DY1 = MAX(ZERO,YMAX1-YMIN1)
      DX2 = MAX(ZERO,XMAX2-XMIN2)
      DY2 = MAX(ZERO,YMAX2-YMIN2)
C
C	the overlap is zero if any of the above are zero
C
      IF (DX1.LE.ZERO.OR.DY1.LE.ZERO.OR.DX2.LE.ZERO.OR.DY2.LE.ZERO) THEN
          ECLIPSE = ZERO
C
C	calculate the overlaps
C
      ELSE
          XMIN = MAX(XMIN1,XMIN2)
          XMAX = MIN(XMAX1,XMAX2)
          YMIN = MAX(YMIN1,YMIN2)
          YMAX = MIN(YMAX1,YMAX2)
C
          DX = MAX(ZERO,XMAX-XMIN)
          DY = MAX(ZERO,YMAX-YMIN)
C
          IF (DX.GT.ZERO.AND.DY.GT.ZERO) THEN
              REL_X_ERROR = DX/MAX(DX,DY)
              REL_Y_ERROR = DY/MAX(DX,DY)
          ENDIF
C
          IF (DX.LE.ZERO.OR.DY.LE.ZERO) THEN
C
C	no overlap
C
              ECLIPSE = ZERO
C
C	make sure the overlap is not in the noise
C
          ELSE IF (REL_X_ERROR.LT.SMALL.OR.REL_Y_ERROR.LT.SMALL) THEN
              ECLIPSE = ZERO
          ELSE
C
C	there is an overlap, calculate the volume of the overlap
C
              IF (IGEOM.EQ.10) THEN
                  ECLIPSE = XMAX - XMIN
              ELSEIF (IGEOM.EQ.11) THEN
                  ECLIPSE = PI * (XMAX**2 - XMIN**2)
              ELSEIF (IGEOM.EQ.12) THEN
                  ECLIPSE = FOUR_THIRDS * PI * (XMAX**3 - XMIN**2)
              ELSEIF (IGEOM.EQ.20) THEN
                  ECLIPSE = (XMAX - XMIN)*(YMAX - YMIN)
              ELSEIF (IGEOM.EQ.21) THEN
                  ECLIPSE = PI * (XMAX**2 - XMIN**2) * (YMAX - YMIN)
              ELSE
                  WRITE(*,'(/'' Abort in subroutine ECLIPSE.''/
     &                   '' Unsupported geometry.''/)')
                  STOP
              ENDIF
C
          ENDIF
C
      ENDIF
C
      END

