      SUBROUTINE TSOLVE (NODES,ENER,TEMP)
C
C        SOLVE FOR THE TEMPERATURE GIVEN THE SPECIFIC INTERNAL
C        ENERGY, ASSUMING IDEAL GAS BEHAVIOR.
C
C        INPUT:
C
C        NODES      INTEGER     NUMBER OF CELLS
C        ENER       REAL A.     INTERNAL SPECIFIC ENERGY (J/KG)
C
C        OUTPUT:
C
C        TEMP       REAL A.     TEMPERATURE (DEG KELVIN)
C
      IMPLICIT REAL (A-H,O-Z)
      REAL ENER(*),TEMP(*),TDATA(28),EDATA(28)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
  
C
C        CONVERT ENTHALPY IN TABLE FROM CAL/MOLE TO J/KG
C
      IF(FIRST) THEN
C
C        FAKE A T/E DATA FILE, FOR PERFORMANCE TESTS ONLY
C
            DO 5 N=1,28
                TDATA(N) = REAL(N)
                EDATA(N) = REAL(N)
    5       CONTINUE
C
            DO 10 N=1,28
              EDATA(N)=4.1840/18.016E-3*EDATA(N)
   10     CONTINUE
          FIRST=.FALSE.
      ENDIF
C
C        INTERPOLATE ON INTERNAL ENERGY (ENTHALPY)
C
      DO 40 N=1,NODES
          ENERGY=ENER(N)
          IF(ENERGY.GE.EDATA(28)) THEN
              NN=27
          ELSEIF (ENERGY.LT.EDATA(1)) THEN
              NN=1
          ELSE
              DO 20 I=27,1,-1
                  NN=I
                  IF(ENERGY.GE.EDATA(I).AND.ENERGY.LT.EDATA(I+1))
     *                 GO TO 30
   20         CONTINUE
          ENDIF
   30     X1=EDATA(NN)
          Y1=TDATA(NN)
          X2=EDATA(NN+1)
          Y2=TDATA(NN+1)
          SLOPE=(Y2-Y1)/(X2-X1)
          B=Y2-SLOPE*X2
          TEMP(N)=ENERGY*SLOPE+B
   40 CONTINUE
      END
