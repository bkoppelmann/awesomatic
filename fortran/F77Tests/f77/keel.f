      SUBROUTINE KEEL (NODES,DX,RADIUS,RBOUND,VEL,DENS,IENER,SCREEN,NS,
     *                 NSNODE,SAREA)
C
C        CALCULATE MESH INITIAL CONDITIONS
C
      PARAMETER (IMAX=500,IMAXP1=501)
      IMPLICIT REAL (A-H,O-Z)
      REAL DX(*),RADIUS(*),VEL(*),DENS(*),IENER(*),SAREA(IMAXP1),
     *       NAREA(IMAXP1),SRAD(IMAXP1),RBOUND(0:IMAXP1)
      INTEGER NSNODE(IMAXP1)
      LOGICAL SCREEN
      COMMON /COORD/ RRIGHT,BAREA(0:IMAXP1),CAREA(IMAXP1),VOL(IMAXP1)
C
      NS=0
C
C        CALCULATE SCREEN INFORMATION IF NECESSARY
C
      IF(SCREEN) THEN
          CALL SGEOM (NS,SRAD,SAREA,NAREA)
          IF(NS.LE.0) THEN
              WRITE(6,'(//'' SCREENS WERE SELECTED, BUT SGEOM DID'',
     *                    '' NOT GENERATE ANY, ABORT'')')
              STOP
          ENDIF
      ENDIF
C
C        CONSTRUCT A UNIFORM GRID
C
      NP1=NODES+1
      DXX=RRIGHT/FLOAT(NODES-1)
C
C        CALCULATE RADIUS OF CELL CENTERS
C
      RBOUND(0)=-DXX
      DO 10 N=1,NP1
          RBOUND(N)=RBOUND(N-1)+DXX
          RADIUS(N)=RBOUND(N-1)+DXX/2.0
   10 CONTINUE
C
C        CELL WIDTHS
C
      NM1=NODES-1
      NST=1
      DO 20 N=1,NP1
          DX(N)=RBOUND(N)-RBOUND(N-1)
C
C        IF SCREENS PRESENT, SEE IF ONE FITS IN THIS CELL
C
          IF(SCREEN) THEN
  15          IF(NST.LE.NS) THEN
                  RLEFT=RBOUND(N-1)
                  RRIGHT=RBOUND(N)
                  IF(RLEFT.LE.SRAD(NST).AND.RRIGHT.GT.SRAD(NST)) THEN
                      NSNODE(NST)=N
                      NST=NST+1
                      GO TO 15
                  ENDIF
              ENDIF
          ENDIF
   20 CONTINUE
C
C        MAKE SURE ALL SCREENS HAVE BEEN ACCOUNTED FOR
C
      IF(SCREEN) THEN
          IF(NST.LE.NS) THEN
              NST=NST-1
              IF(NST.LE.0) THEN
                  WRITE(6,'(//'' NO SCREENS WERE ACCOUNTED FOR'')')
              ELSE
              WRITE(6,'(//'' NOT ALL SCREENS WERE ACCOUNTED FOR''/
     *                    '' SCREENS HAVE BEEN LOCATED AT:''/)')
              WRITE(6,'('' N='',I5,'' RADIUS='',1PE12.5,
     *                  '' LOCATED IN NODE ''
     *                 ,I5)') (NN,SRAD(NN),NSNODE(NN),NN=1,NST)
              ENDIF
              NST=NST+1
              WRITE(6,'(//'' UNACCOUNTED FOR SCREENS ARE AT:''/)')
              WRITE(6,'('' RADIUS='',1PE12.5)') (SRAD(NN),NN=NST,NS)
              STOP
          ENDIF
      ENDIF
C
C        CELL BOUNDARY AREAS
C
      NST=1
      DO 30 N=0,NP1
          BAREA(N)=AREA(RBOUND(N))
          IF(N.EQ.NP1) BAREA(N)=BAREA(N-1)
   30 CONTINUE
C
C        CELL CENTER AREAS
C
      DO 31 N=1,NP1
          CAREA(N)=AREA(RADIUS(N))
          IF(N.EQ.NP1) CAREA(N)=CAREA(N-1)
   31 CONTINUE
C
C        MODIFY CELL BOUNDARY AREAS FOR SCREENS IF NECESSARY
C
      IF(SCREEN) THEN
          DO 35 N=1,NS
              NN=NSNODE(N)
              SAREA(N)=SAREA(N)*BAREA(NN)
              BAREA(NN)=BAREA(NN)*NAREA(N)
   35     CONTINUE
      ENDIF
C
C        CELL VOLUMES.  ASSUME THE CELL IS A FRUSTRUM OF A RIGHT
C        CIRCULAR CONE
C
      DO 40 N=1,NP1
          VOL(N)=DX(N)/3.0*(BAREA(N-1)+SQRT(BAREA(N-1)*BAREA(N))+
     *           BAREA(N))
   40 CONTINUE
C
C        FILL MESH WITH AMBIENT CONDITIONS OUTSIDE CHAMBER.
C        THESE ARE 70 MICRONS OF MERCURY PRESSURE AND A TEMPERATURE
C        OF 300 DEG KELVIN.  WE ASSUME AN IDEAL GAS.  THE MOLECULAR
C        WEIGHT OF STEAM IS ASSUMED TO BE 18.016 G/MOLE.  THE GAS
C        CONSTANT IS 8.314 J/DEG K/MOLE.
C
C
C        SET CHAMBER INTERNAL ENERGY TO AMBIENT
C
C        I=2.765E5 FOR GAMMA 1.4
C        I=4.1477E5 FOR STEAM
C
      VSET=0.0
      ESET=2.765E5
      DSET=9.629E-7
      DO 50 N=2,NODES
          VEL(N)=VSET
          DENS(N)=DSET
          IENER(N)=ESET
   50 CONTINUE
C
C        PRINT OUT MESH GEOMETRY
C
      WRITE(6,'(///'' MESH GEOMETRY:''//)')
      WRITE(6,'('' CELL='',I5,'' R='',E12.5,'' DR='',
     *          E12.5,'' VOL='',
     *          E12.5,'' LEFT AREA='',E12.5,
     *          '' RIGHT AREA='',E12.5)')
     *          (N,RADIUS(N),DX(N),VOL(N),BAREA(N-1),BAREA(N),N=1,NODES)
      IF(SCREEN) THEN
          WRITE(6,'(///''1SCREEN LOCATIONS:''//)')
          DO 111 N=1,NS
          TRANS=100.0*NAREA(N)
          NN=NSNODE(N)
          WRITE(6,'('' CELL='',I5,'' SCREEN RADIUS='',1PE12.5,
     *              '' CELL RADIUS='',E12.5,'' SCREEN AREA='',
     *               E12.5,'' PERCENT TRANSMISSION='',E12.5)')
     *          NN,SRAD(N),RADIUS(NN),SAREA(N),TRANS
  111     CONTINUE
      ENDIF
      END
