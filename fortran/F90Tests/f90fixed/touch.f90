      LOGICAL FUNCTION TOUCH (X1,X2)
C
C     This subroutine calculates whether two numbers are close to each
C     other
C
C     Called by: OVRLAP
C     Database manager: none
C
C     Author: John K. Prentice, Quetzal Computational Associates
C     written: 4/1/93
C
C
      IMPLICIT NONE
      DOUBLE PRECISION X1,X2,DX,RELATIVE_ERROR,ZERO,SMALL
      LOGICAL TOUCH
      PARAMETER (ZERO=0.0,SMALL=1.E-5)
C
      DX = ABS(X2-X1)
      IF (DX.NE.ZERO) THEN
          RELATIVE_ERROR = DX/MAX(ABS(X1),ABS(X2))
      ELSE
          RELATIVE_ERROR = ZERO
      ENDIF
C
      IF (RELATIVE_ERROR.LT.SMALL) THEN
          TOUCH = .TRUE.
      ELSE
          TOUCH = .FALSE.
      ENDIF
C
      END
