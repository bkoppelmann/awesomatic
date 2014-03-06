      SUBROUTINE CHOZDT(NODES, VEL, SOUND, DX, DT, STABF)
!                                      *********************************
!                                      CHOOSE TIME STEP
!
!                                      STABF IS A STABILITY FACTOR
!                                      *********************************
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NODES
      REAL DT, STABF
      REAL, DIMENSION(NODES) :: VEL, SOUND, DX
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: IMAX = 500
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: ISET(1)
      REAL :: VSET, SSET
      REAL, DIMENSION (NODES) :: DTEMP
!-----------------------------------------------
      DTEMP = DX/(ABS(VEL) + SOUND)
      ISET = MINLOC (DTEMP)
      DT = DTEMP(ISET(1))
      VSET = VEL(ISET(1))
      SSET = SOUND(ISET(1))
      WRITE (6,                                                         &
     &'('' CELL SETTING DT IS '',I5,'', V='',1PE12.5,                   &
     &  '', CS='',E12.5)') ISET(1), VSET, SSET
      DT = STABF*DT
      RETURN 
      END SUBROUTINE CHOZDT
