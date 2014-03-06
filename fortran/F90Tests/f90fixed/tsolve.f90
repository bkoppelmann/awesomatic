       SUBROUTINE TSOLVE(NODES, ENER, TEMP)
!
!        SOLVE FOR THE TEMPERATURE GIVEN THE SPECIFIC INTERNAL
!        ENERGY, ASSUMING IDEAL GAS BEHAVIOR.
!
!        INPUT:
!
!        NODES      INTEGER     NUMBER OF CELLS
!        ENER       REAL A.     INTERNAL SPECIFIC ENERGY (J/KG)
!
!        OUTPUT:
!
!        TEMP       REAL A.     TEMPERATURE (DEG KELVIN)
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NODES
      REAL, DIMENSION(NODES) :: ENER, TEMP
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: N, NN, I
      REAL, DIMENSION(28) :: TDATA, EDATA
      REAL :: ENERGY, X1, Y1, X2, Y2, SLOPE, B
      LOGICAL :: FIRST = .TRUE.
      SAVE FIRST,TDATA,EDATA
!-----------------------------------------------
!
!        CONVERT ENTHALPY IN TABLE FROM CAL/MOLE TO J/KG
!
      IF (FIRST) THEN
!
!        FAKE A T/E DATA FILE, FOR PERFORMANCE TESTS ONLY
!
          DO N = 1, NODES
              TDATA(N) = REAL(N)
              EDATA(N)  = REAL(N)
          ENDDO
!
          EDATA = 4.1840/18.016E-3*EDATA
          FIRST = .FALSE.
      ENDIF
!
!        INTERPOLATE ON INTERNAL ENERGY (ENTHALPY)
!
      DO N = 1, NODES
          ENERGY = ENER(N)
          IF (ENERGY >= EDATA(28)) THEN
              NN = 27
          ELSE IF (ENERGY < EDATA(1)) THEN
              NN = 1
          ELSE
              DO I = 27, 1, -1
                  NN = I
                  IF (ENERGY>=EDATA(I) .AND. ENERGY<EDATA(I+1)) GO TO 30
              END DO
          ENDIF
   30     CONTINUE
          X1 = EDATA(NN)
          Y1 = TDATA(NN)
          X2 = EDATA(NN+1)
          Y2 = TDATA(NN+1)
          SLOPE = (Y2 - Y1)/(X2 - X1)
          B = Y2 - SLOPE*X2
          TEMP(N) = ENERGY*SLOPE + B
      END DO
      END SUBROUTINE TSOLVE
