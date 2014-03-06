       SUBROUTINE EOS(NODES, IENER, DENS, PRES, TEMP, GAMMA, CS, SHEAT,  &
     &    CGAMMA, WT)
!
!        EQUATION OF STATE
!
!        INPUT:
!
!        NODES     INTEGER     NUMBER OF CELLS IN MESH
!        IENER     REAL A.     INTERNAL SPECIFIC ENERGY (J/KG)
!        DENS      REAL A.     DENSITY (KG/M**3)
!        SHEAT     REAL        CONSTANT SPECIFIC HEAT TO BE USED
!        CGAMMA    REAL        CONSTANT GAMMA TO BE USED
!
!        OUTPUT:
!
!        PRES      REAL A.     PRESSURE (PASCALS)
!        TEMP      REAL A.     TEMPERATURE (DEG K)
!        GAMMA     REAL A.     THERMODYNAMIC GAMMA
!        CS        REAL A.     SOUND SPEED (M/S)
!
!        NOTE:  THE ENTIRE MESH IS CALCULATED AT ONCE, SO THESE ARRAYS
!               CONTAIN THE VARIABLES FOR EACH CELL
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
      INCLUDE 'tsolve.int'
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NODES
      REAL SHEAT, CGAMMA, WT
      REAL, DIMENSION(NODES) :: IENER, DENS, PRES, TEMP, GAMMA, CS
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      REAL, PARAMETER :: RGAS = 8.314
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL :: CONST
!-----------------------------------------------
!     PARAMETER (RGAS=8.314)
!
!        CONSTANT SPECIFIC HEAT AND GAMMA CALCULATIONS
!
      IF (SHEAT>0.0 .AND. CGAMMA>0.0) THEN
          TEMP(:NODES) = IENER(:NODES)/SHEAT
          PRES(:NODES) = (CGAMMA - 1.0)*DENS(:NODES)*IENER(:NODES)
          GAMMA(:NODES) = CGAMMA
          CS(:NODES) = SQRT(CGAMMA*PRES(:NODES)/DENS(:NODES))
      ELSE
!
!        NON-CONSTANT SPECIFIC HEAT AND GAMMA CALCULATIONS
!
!                                      *********************************
!                                      SOLVE FOR TEMPERATURE
!                                      *********************************
          CONST = RGAS/WT
          CALL TSOLVE (NODES, IENER, TEMP)
!                                      *********************************
!                                      SOLVE FOR PRESSURE
!                                      *********************************
          PRES(:NODES) = CONST*DENS(:NODES)*TEMP(:NODES)
!                                      *********************************
!                                      SOLVE FOR GAMMA
!                                      *********************************
          GAMMA(:NODES) = 1.0 + PRES(:NODES)/(DENS(:NODES)*IENER(:NODES))
!                                      *********************************
!                                      SOLVE FOR SOUND SPEED
!                                      *********************************
          CS(:NODES) = SQRT(GAMMA(:NODES)*PRES(:NODES)/DENS(:NODES))
      ENDIF
!
      END SUBROUTINE EOS
