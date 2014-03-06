      SUBROUTINE CHOZDT (NODES,VEL,SOUND,DX,DT,STABF)
C                                      *********************************
C                                      CHOOSE TIME STEP
C
C                                      STABF IS A STABILITY FACTOR
C                                      *********************************
      IMPLICIT REAL (A-H,O-Z)
      PARAMETER (IMAX=500)
      DIMENSION VEL(*),SOUND(*),DX(*)
      DT=DX(1)/(ABS(VEL(1))+SOUND(1))
      VSET=VEL(1)
      SSET=SOUND(1)
      ISET=1
      DO 10 N = 2,NODES
          DTEMP=DX(N)/(ABS(VEL(N))+SOUND(N))
          IF(DTEMP.LT.DT) THEN
              ISET=N
              DT=DTEMP
              VSET=VEL(N)
              SSET=SOUND(N)
          ENDIF
   10 CONTINUE
      WRITE(6,'('' CELL SETTING DT IS '',I5,'', V='',1PE12.5,
     *          '', CS='',E12.5)') ISET,VSET,SSET
      DT = STABF*DT
      RETURN
      END
