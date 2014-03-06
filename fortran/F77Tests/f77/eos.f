      SUBROUTINE EOS (NODES,IENER,DENS,PRES,TEMP,GAMMA,CS,SHEAT,CGAMMA
     *,WT)
C
C        EQUATION OF STATE
C
C        INPUT:
C
C        NODES     INTEGER     NUMBER OF CELLS IN MESH
C        IENER     REAL A.     INTERNAL SPECIFIC ENERGY (J/KG)
C        DENS      REAL A.     DENSITY (KG/M**3)
C        SHEAT     REAL        CONSTANT SPECIFIC HEAT TO BE USED
C        CGAMMA    REAL        CONSTANT GAMMA TO BE USED
C
C        OUTPUT:
C
C        PRES      REAL A.     PRESSURE (PASCALS)
C        TEMP      REAL A.     TEMPERATURE (DEG K)
C        GAMMA     REAL A.     THERMODYNAMIC GAMMA
C        CS        REAL A.     SOUND SPEED (M/S)
C
C        NOTE:  THE ENTIRE MESH IS CALCULATED AT ONCE, SO THESE ARRAYS
C               CONTAIN THE VARIABLES FOR EACH CELL
C
      IMPLICIT REAL (A-H,O-Z)
      REAL IENER(*),DENS(*),PRES(*),TEMP(*),CS(*),GAMMA(*)
C                                      *********************************
C                                      RGAS = UNIVERSAL GAS CONSTANT
C                                             (J/MOLE/DEG K)
C                                      *********************************
      PARAMETER (RGAS=8.314)
C
C        CONSTANT SPECIFIC HEAT AND GAMMA CALCULATIONS
C
      IF(SHEAT.GT.0.0.AND.CGAMMA.GT.0.0) THEN
          DO 5 I=1,NODES
              TEMP(I)=IENER(I)/SHEAT
              PRES(I)=(CGAMMA-1.0)*DENS(I)*IENER(I)
              GAMMA(I)=CGAMMA
              CS(I)=SQRT(CGAMMA*PRES(I)/DENS(I))
    5     CONTINUE
          RETURN
      ENDIF
C
C        NON-CONSTANT SPECIFIC HEAT AND GAMMA CALCULATIONS
C
C                                      *********************************
C                                      SOLVE FOR TEMPERATURE
C                                      *********************************
      CONST=RGAS/WT
      CALL TSOLVE (NODES,IENER,TEMP)
C                                      *********************************
C                                      SOLVE FOR PRESSURE
C                                      *********************************
      DO 10 N=1,NODES
          PRES(N)=CONST*DENS(N)*TEMP(N)
   10 CONTINUE
C                                      *********************************
C                                      SOLVE FOR GAMMA
C                                      *********************************
      DO 20 N=1,NODES
          GAMMA(N)=1.0+PRES(N)/(DENS(N)*IENER(N))
   20 CONTINUE
C                                      *********************************
C                                      SOLVE FOR SOUND SPEED
C                                      *********************************
      DO 30 N=1,NODES
          CS(N)=SQRT(GAMMA(N)*PRES(N)/DENS(N))
   30 CONTINUE
      END
