      PROGRAM CORTESA
C
C        THIS PROGRAM SOLVES THE CONTINUITY EQUATIONS FOR MASS,
C        MOMENTUM, AND ENERGY TO MODEL THE FLOW OF A GAS IN ONE
C        DIMENSION.  THE SOLUTION TECHNIQUE IS A ONE DIMENSIONAL
C        VERSION OF THE HULL DIFFERENCING SCHEME, MODIFIED FOR VARIABLE
C        AREA.  WE FURTHER MODIFY THE SCHEME SO THAT THE HYDRODYNAMIC
C        FRONT IS MOVED BY FREE MOLECULAR FLOW, ASSUMING A NEAR VACUUM
C        DOWNSTREAM OF THE FRONT.
C
C        JOHN K. PRENTICE                17 SEPTEMBER 1986
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C	Original F77 code written by John Prentice to model the
C	hydrodynamics in a plasma switch.  Modified for use in F90
C	performance benchmark project.  Modifications include removing
C	calls to plot packages and hardwiring the input/output file
C 	names.
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C                                      *********************************
C                                      IMAX   = MAXIMUM NUMBER OF CELLS
C                                               IN THE MESH
C                                      MAXSTA = MAXIMUM AMOUNT OF
C                                               STATION DATA POSSIBLE
C                                      *********************************
      PARAMETER (IMAX=500,IMAXP1=501,MAXSTA=50000)
      PARAMETER (BOLTS=1.3806E-23,AVOG=6.0222E23,DIAM=4.E-10,PI=3.14159)
C
C        VMIN IS SMALLEST VELOCITY ALLOWED IN MESH
C
      PARAMETER (VMIN=0.1)
C
C                                      *********************************
C                                      VEL    = VELOCITY
C                                      PRES   = PRESSURE
C                                      DENS   = DENSITY
C                                      TENER  = TOTAL SPECIFIC ENERGY
C                                      IENER  = INTERNAL SPECIFIC ENERGY
C                                      TEMP   = TEMPERATURE
C                                      PSTAT  = STATION PRESSURE
C                                      DSTAT  = STATION DENSITY
C                                      VSTAT  = STATION VELOCITY
C                                      TSTAT  = STATION TEMPERATURE
C                                      STIME  = STATION TIME
C                                      RADIUS = RADIUS OF CELL CENTERS
C                                      STATS  = STATION CELL LOCATION
C
      IMPLICIT REAL (A-H,O-Z)
      REAL VEL(IMAXP1),PRES(IMAXP1),DENS(IMAXP1),TENER(IMAXP1),
     *     IENER(IMAXP1),MASS(IMAXP1),TEMP(IMAXP1),PSTAT(MAXSTA),
     *     DSTAT(MAXSTA),VSTAT(MAXSTA),TSTAT(MAXSTA),STIME(MAXSTA),
     *     RADIUS(IMAXP1),ZERO(IMAXP1),ILEFT,
     *     SOUND(IMAXP1),DX(IMAXP1),GAMMA(IMAXP1),MINUS1(IMAXP1),
     *     SAREA(IMAXP1),DBND(IMAXP1),
     *     IBND(IMAXP1),VBND(IMAXP1),PBND(IMAXP1),GBND(IMAXP1),
     *     RT(IMAXP1),VT(IMAXP1),PT(IMAXP1),PV(IMAXP1),VOLD(IMAXP1),
     *     EOLD(IMAXP1),OLDR(IMAXP1),RBOUND(0:IMAXP1)
      CHARACTER*15 INFIL,OUTFIL
      CHARACTER*80 TITLE
      INTEGER DCYCLE,CSTOP,FCYCLE,CYCLE,STATS(IMAXP1),
     *        SCYCLE(MAXSTA),NSNODE(IMAXP1)
      LOGICAL PHIST,DHIST,VHIST,DEBUG,PPLOT,DPLOT,VPLOT,TPLOT,
     *        SCREEN,SLIST,THIST,STOPIT
      COMMON /COORD/ RRIGHT,BAREA(0:IMAXP1),CAREA(IMAXP1),VOL(IMAXP1)
C                                      *********************************
C                                      INITIALIZATIONS
C                                      *********************************
      TIME=0.
      NLOC=0
      CYCLE=0
      DO 10 I=1,MAXSTA
          STIME(I)=0.0
          PSTAT(I)=0.0
          DSTAT(I)=0.0
          VSTAT(I)=0.0
          TSTAT(I)=0.0
   10 CONTINUE
C                                      *********************************
C                                      OPEN INPUT AND OUTPUT FILES
C                                      *********************************
      INFIL = 'INPUT_FILE'
      OUTFIL = 'OUTPUT_FILE'
      OPEN (UNIT=5,FILE=INFIL,STATUS='OLD',FORM='FORMATTED')
      OPEN (UNIT=6,FILE=OUTFIL,STATUS='UNKNOWN',FORM='FORMATTED')
C                                      *********************************
C                                      READ IN THE INITIAL
C                                      CONDITIONS.
C                                      *********************************
      CALL READIN (PROB,TITLE,CSTOP,FCYCLE,DCYCLE,DHIST,VHIST,IMAX,
     *             PHIST,DEBUG,NSTAT,STATS,MAXSTA,NCORE,PPLOT,DPLOT
     *            ,VPLOT,TPLOT,SLIST,D0,E0,NODES,SHEAT,CGAMMA,COLD,
     *            THIST,NVISC,SCREEN,WEIGHT,TSTOP,STABF)
      CLOSE(UNIT=5)
C                                      *********************************
C                                      CALCULATE NOZZLE INITIAL
C                                      CONDITIONS
C                                      *********************************
      COEFF=0.4*(18.016E-3/8.314)
      AMASS=WEIGHT/AVOG
      SIGMA=SQRT(BOLTS/(3.0*AMASS))
      SIGMA2=3.0*SQRT((PI*AMASS)/(8.0*BOLTS))
      T0=COEFF*E0
      IF(T0.GT.6000.0) T0=5212.0
C
C        IF COLD NOT SET, CALL NOZZLE TO INITIALIZE
C
      IF(COLD.LE.0.0) THEN
          CALL NOZZLE(0.0,P0,DMY1,DMY2,DMY3,DMY4,DMY5,DMY6,DMY7,0.0,
     *                DMY9)
C
C        ASSUME ISOTHERMAL CONDITIONS AT NOZZLE, T=300 DEG KELVIN
C
          IF(SHEAT.LE.0.0.OR.CGAMMA.LE.0.0) THEN
              CALL TSOLVE (1,E0,T0)
              CON=8.314/WEIGHT
              D0=P0/(CON*T0)
          ELSE
              D0=P0/((CGAMMA-1.0)*E0)
          ENDIF
      ENDIF
      CALL EOS (1,E0,D0,P0,T0,GAMMA0,C0,SHEAT,CGAMMA,WEIGHT)
C                                      *********************************
C                                      INITIALIZE THE MESH
C                                      *********************************
      CALL NOZZLE (0.0,P0,D0,E0,C0,PRES(1),DENS(1),VEL(1),IENER(1),COLD,
     *  GAMMA0)
      CALL KEEL (NODES,DX,RADIUS,RBOUND,VEL,DENS,IENER,SCREEN,NS,NSNODE,
     *           SAREA)
      DO 20 N=1,NODES
          TENER(N)=VEL(N)**2/2.0+IENER(N)
          ZERO(N)=0.0
          MINUS1(N)=-1.0
C                                      *********************************
C                                      ESTIMATE INITIAL TEMPERATURE
C                                      WITH IDEAL GAS LAW AND GAMMA 1.4
C                                      *********************************
          TEMP(N)=COEFF*IENER(N)
          IF(TEMP(N).GT.6000.0) TEMP(N)=5212.0
   20 CONTINUE
C                                      *********************************
C                                      MAKE INITIAL CALL TO EOS AND
C                                      CHOZDT
C                                      *********************************
      CALL EOS (NODES,IENER,DENS,PRES,TEMP,GAMMA,SOUND,SHEAT,CGAMMA,
     *          WEIGHT)
C                                      *********************************
C                                      PRINT INITIAL CONDITIONS
C                                      *********************************
      WRITE(6,'(//'' INITIAL HYDRODYNAMIC CONDITIONS IN MESH:''//)')
      WRITE(6,'('' X='',1PE12.5,'' P='',1PE12.5,'' D='',E12.5,
     *          '' U='',E12.5,'' I='',E12.5,'' T='',E12.5,'' CS='',
     *          E12.5,'' GAMMA='',E12.5)') (RADIUS(N),PRES(N),DENS(N),
     *          VEL(N),IENER(N),TEMP(N),SOUND(N),GAMMA(N),N=1,NODES)
      CALL CHOZDT (NODES,VEL,SOUND,DX,DT,STABF)
C***********************************************************************
C
C                       MAIN LOOP
C
C***********************************************************************
      WRITE(6,'(''1BEGINNING CALCULATION AT TIME='',1PE12.5//)') TIME
      IFRONT=1
      XFRONT=RBOUND(1)
      STOPIT=.FALSE.
      MCYCLE=MIN(50,CSTOP/2)
      DO 110 I = 1,CSTOP
          IF(I.EQ.CSTOP) STOPIT=.TRUE.
          IF(TIME+DT.GE.TSTOP) STOPIT=.TRUE.
          IF(I.EQ.1.OR.MOD(I,MCYCLE).EQ.0) THEN
          WRITE(*,'('' NOW AT CYCLE '',I5,'' AND TIME '',1PE12.5)')
     *    I,TIME
          WRITE(6,'('' ----->  CYCLE='',I5,'' TIME='',1PE12.5,'' DT='',
     *              E12.5)') I,TIME,DT
          ENDIF
C                                      *********************************
C                                      ADVANCE TO T+DT/2
C                                      *********************************
          DTHALF=DT/2.0
C                                      *********************************
C                                      CALCULATE BOUNDARY CONDITIONS.
C                                      THE LEFT BOUNDARY IS DRIVEN BY
C                                      THE NOZZLE, THE RIGHT IS
C                                      PERFECTLY REFLECTIVE.  DEFINE
C                                      CELLS 1 AND 'NODES' TO THESE
C                                      VALUES, AS WELL AS THE LEFT AND
C                                      RIGHT BOUNDARIES.  THIS
C                                      ELIMINATES A PROBLEM IN ETBFCT
C                                      WITH ZERO VELOCITY CELLS
C                                      ADJACENT TO THE BOUNDARY
C                                      *********************************
          IF(I.EQ.1) CALL NOZZLE (TIME,P0,D0,E0,C0,PLEFT,DLEFT,VLEFT,
     *                                   ILEFT,COLD,GAMMA0)
          IDC=1
          IF(STOPIT.OR.(FCYCLE.LE.I.AND.MOD(I,DCYCLE).EQ.0).OR.
     *       I.EQ.1) IDC=0
          IF(DEBUG.OR.IDC.EQ.0) THEN
              WRITE(6,'(//'' NOZZLE CONDITIONS AT BEGINNING OF TIME '',
     *                    ''STEP:''/
     *                    ''   PRESSURE='',1PE12.5/
     *                    ''   DENSITY ='',1PE12.5/
     *                    ''   ENERGY  ='',1PE12.5/
     *                    ''   VELOCITY='',1PE12.5/)') PLEFT,DLEFT,
     *                          ILEFT,VLEFT
          ENDIF
C
C        LOAD NOZZLE CONDITIONS INTO CELL 1
C
          DENS(1)=DLEFT
          PRES(1)=PLEFT
          VEL(1)=VLEFT
          TENER(1)=ILEFT+VLEFT**2/2.0
          IENER(1)=ILEFT
C
C        LOAD INITIAL LEFT HAND FLOW CONDITIONS
C
          IFLOW = 0
          IF(IFLOW.NE.2) THEN
              FML=0.0
              FEL=0.0
              VMOML=0.0
          ELSE
              FML=DLEFT*VLEFT*DT*BAREA(1)
              FEL=FML*TENER(1)
              VMOML=FML*VLEFT
          ENDIF
C
C        IF FLOW AT END OF MESH, LOAD REFLECTIVE CONDITIONS
C
          IF(IFRONT.GT.NODES) THEN
              IFRONT=NODES+1
              DENS(IFRONT)=DENS(NODES)*VOL(NODES)/VOL(IFRONT)
              PRES(IFRONT)=PRES(NODES)
              VEL(IFRONT)=-VEL(NODES)
              TENER(IFRONT)=TENER(NODES)
              IENER(IFRONT)=IENER(NODES)
          ENDIF

          NLAST=IFRONT-1
          IF(IFRONT.LE.NODES) THEN
              DXFRNT=XFRONT-RBOUND(IFRONT-1)
              RFRNT=RBOUND(NLAST)+0.5*DXFRNT
              AFRONT=AREA(RFRNT)
              VFRONT=DXFRNT/3.0*(BAREA(NLAST)+SQRT(BAREA(NLAST)*AFRONT)+
     *           AFRONT)
          ENDIF
C
C        DIFFERENCE ONLY IF FRONT HAS PROPAGATED AT LEAST INTO CELL 3
C
          IF(IFRONT.GE.3) THEN
              VOLSAV=VOL(IFRONT)
              RADSAV=RADIUS(IFRONT)
              ASAV=CAREA(IFRONT)
              DXSAV=DX(IFRONT)
              VOL(IFRONT)=VFRONT
              RADIUS(IFRONT)=RFRNT
              CAREA(IFRONT)=AFRONT
              DX(IFRONT)=DXFRNT
C
C        CALCULATE MASSES FOR ACTIVE CELLS
C
              DO 21 N=1,IFRONT
                  MASS(N)=DENS(N)*VOL(N)
   21         CONTINUE
C
C        ***** PHASE 1 *****
C
C        IN PHASE 1 WE UPDATE INTERNAL ENERGY WITH WORK TERM DUE TO
C        VELOCITIES CALCULATED AT LAST TIME STEP.  WE THEN UPDATE
C        VELOCITIES WITH ACCELERATIONS DUE TO PRESSURE GRADIENTS
C
C        DENSITY,VELOCITY,ENERGY, AND PRESSURE AT
C        CELL BOUNDARY AT INITIAL TIME
C
              DO 22 N=1,NLAST
                  DBND(N)=(MASS(N)+MASS(N+1))/(VOL(N)+VOL(N+1))
                  IBND(N)=(IENER(N)+IENER(N+1))/2.0
                  VBND(N)=(VEL(N)+VEL(N+1))/2.0
                  PBND(N)=(PRES(N)+PRES(N+1))/2.0
   22         CONTINUE
C
C        HALF TIME ADVANCED DENSITY ON CELL BOUNDARY
C
              DO 23 N=1,NLAST
                  RT(N)=DBND(N)*(1.0-DTHALF/BAREA(N)*(CAREA(N+1)*
     *                  VEL(N+1)-CAREA(N)*VEL(N))/(RADIUS(N+1)-
     *                  RADIUS(N)))
   23         CONTINUE
C
C        HALF TIME ADVANCED VELOCITIES AND EFFECTIVE GAMMA
C
              DO 24 N=1,NLAST
                  VT(N)=VBND(N)-DTHALF/RT(N)*(PRES(N+1)-PRES(N))/
     *                  (RADIUS(N+1)-RADIUS(N))
                  GBND(N)=1.0+PBND(N)/(IBND(N)*DBND(N))
   24         CONTINUE
C
C        HALF TIME ADVANCED PRESSURE ON CELL BOUNDARY
C
              DO 25 N=1,NLAST
                  PT(N)=PBND(N)*(1.0-DTHALF*GBND(N)/BAREA(N)*
     *                  (CAREA(N+1)*VEL(N+1)-CAREA(N)*VEL(N))/
     *                  (RADIUS(N+1)-RADIUS(N)))
   25         CONTINUE
C
C        ZERO VELOCITIES DUE TO TRUNCATION AND CALCULATE PV TEMPORARY.
C        STORE OTHER VARIABLES INTO TEMPORARIES
C
              DO 26 N=1,NLAST
                  IF(ABS(VT(N)).LE.VMIN) VT(N)=0.0
                  PV(N)=PT(N)*VT(N)
                  VOLD(N)=VEL(N)
                  EOLD(N)=TENER(N)
                  OLDR(N)=DENS(N)
   26         CONTINUE
C
C        CALCULATE NEW VELOCITIES
C
              DO 27 N=2,NLAST
                  VEL(N)=VOLD(N)-DT/OLDR(N)*(PT(N)-PT(N-1))/DX(N)
                  IF(ABS(VEL(N)).LE.VMIN) VEL(N)=0.0
   27         CONTINUE
C
C        UPDATE ENERGY
C
              DO 28 N=2,NLAST
                  TENER(N)=EOLD(N)-DT/(OLDR(N)*CAREA(N))*(PV(N)*BAREA(N)
     *                     -PV(N-1)*BAREA(N-1))/DX(N)
   28         CONTINUE
C
C        UPDATE CELL 1 WITH NOZZLE CONDITIONS
C
              CALL NOZZLE (TIME,P0,D0,E0,C0,PLEFT,DLEFT,VLEFT,ILEFT,
     *                     COLD,GAMMA0)
              VEL(1)=VLEFT
              TENER(1)=ILEFT+0.5*VLEFT**2
C
C        ***** PHASE 2 *****
C
C        IN PHASE 2 WE TRANSPORT MASS AND INTERNAL ENERGY WHILE
C        CONSERVING MOMENTUM AND TOTAL ENERGY
C
              FML=0.0
              FEL=0.0
              VMOML=0.0
              DO 29 N=1,NLAST
C
C        CALCULATE WEIGHTED VELOCITY ON RIGHT CELL BOUNDARY
C
                  VAR=0.5*(VEL(N)+VEL(N+1))
                  IF(FML.EQ.0.0.AND.VAR.EQ.0.0) THEN
C
C        INACTIVE CELL
C
                      FMR=0.0
                      VMOMR=0.0
                      FER=0.0
C
C        ACTIVE CELL
C
                  ELSE
C
C        SET DONOR CELL DENSITY
C
                      IDONOR=N
                      IF(VAR.LT.0.0) IDONOR=N+1
                      DAR=OLDR(IDONOR)
                      VAW=DAR*(1.0-DT/BAREA(N)*(CAREA(N+1)*VEL(N+1)-
     *                    CAREA(N)*VEL(N))/(RADIUS(N+1)-RADIUS(N)))
C
C        COMPUTE MASS FLUX TO RIGHT
C
                      RMOVE=RBOUND(N)+VAR*DT
                      AMOVE=AREA(RMOVE)
                      VMOVE=(RMOVE-RBOUND(N))/3.0*(BAREA(N)+
     *                      SQRT(BAREA(N)*AMOVE)+AMOVE)
                      FMR=VMOVE*VAW
                      VMOMR=FMR*VEL(IDONOR)
                      FER=FMR*TENER(IDONOR)
C
C        SKIP FLUXES INTO CELL 1
C
                      IF(N.NE.1) THEN
                          TMASS=MASS(N)+FML-FMR
                          TVMOM=VEL(N)*MASS(N)+VMOML-VMOMR
                          TENGY=MASS(N)*TENER(N)+FEL-FER
C
C        COMPUTE FINAL VELOCITIES, ENERGY, AND PRESSURE
C
                          VEL(N)=TVMOM/TMASS
                          TENER(N)=TENGY/TMASS
                          IENER(N)=TENER(N)-0.5*VEL(N)**2
                          MASS(N)=TMASS
                          DENS(N)=MASS(N)/VOL(N)
C
C        CHECK FOR NEGATIVE ENERGY
C
                          IF(IENER(N).LE.0.0) THEN
                               WRITE(6,'('' NEGATIVE ENERGY IN CELL '',
     *                               I5,'' I='',1PE12.5,'' FEL='',E12.5,
     *                               '' FER='',E12.5)') N,IENER(N),FEL,
     *                               FER
                               STOP
                           ENDIF
                       ENDIF
                       FML=FMR
                       FEL=FER
                       VMOML=VMOMR
                   ENDIF
   29          CONTINUE
C
C        RELOAD CELL 1
C
               DENS(1)=DLEFT
               PRES(1)=PLEFT
               VEL(1)=VLEFT
               TENER(1)=ILEFT+0.5*VLEFT**2
               IENER(1)=ILEFT
               VOL(IFRONT)=VOLSAV
               RADIUS(IFRONT)=RADSAV
               CAREA(IFRONT)=ASAV
               DX(IFRONT)=DXSAV
           ENDIF
C
C        ADVANCE HYDRODYNAMIC FRONT BY FREE MOLECULAR FLOW
C
           IF(IFRONT.LE.NODES) THEN
               FMR=0.0
               FER=0.0
               VMOMR=0.0
               FMASS=DENS(IFRONT)*VFRONT
               FMOM=FMASS*VEL(IFRONT)
               FENGY=FMASS*TENER(IFRONT)
C
C        CALCULATE ANY FLUXES INTO NEXT CELL
C
               INEW=IFRONT
               RMOVE=VEL(IFRONT)*DT+XFRONT
               IF(RMOVE.GT.RBOUND(IFRONT)) THEN
                   INEW=IFRONT+1
                   IF(IFRONT.GT.NODES) THEN
                       RMOVE=RBOUND(NODES)
                   ELSE
                       AMOVE=AREA(RMOVE)
                       VMOVE=(RMOVE-RBOUND(IFRONT))/3.0*(BAREA(IFRONT)+
     *                       SQRT(BAREA(IFRONT)*AMOVE)+AMOVE)
                       FMR=VMOVE*DENS(IFRONT)
                       VMOMR=FMR*VEL(IFRONT)
                       FER=FMR*TENER(IFRONT)
                       FMASS=FMASS-FMR
                       FMOM=FMOM-VMOMR
                       FENGY=FENGY-FER
                   ENDIF
               ENDIF
C
C        NOW ADD OLD FLUXES FROM LEFT BOUNDARY INTO OLD FRONT CELL
C
               FMASS=FMASS+FML
               FMOM=FMOM+VMOML
               FENGY=FENGY+FEL
C
C        UPDATE HYDRO FOR OLD FRONT CELL
C
               OLDX=MIN(RMOVE,RBOUND(IFRONT))
               ANEW=AREA(OLDX)
               VNEW=(OLDX-RBOUND(IFRONT-1))/3.0*(BAREA(IFRONT-1)+
     *              SQRT(BAREA(IFRONT-1)*ANEW)+ANEW)
               DENS(IFRONT)=FMASS/VNEW
               VEL(IFRONT)=FMOM/FMASS
               TENER(IFRONT)=FENGY/FMASS
               IENER(IFRONT)=TENER(IFRONT)-0.5*VEL(IFRONT)**2
               IF(IENER(IFRONT).LE.0.0) THEN
                   WRITE(6,'('' NEGATIVE ENERGY IN CELL '',I5,'' I='',
     *                   1PE12.5/'' UPDATING OLD FRONT CELL'')')IFRONT,
     *                   IENER(IFRONT)
                   STOP
               ENDIF
C
C        MOVE FRONT TO NEXT CELL IF APPROPIATE
C
               IF(IFRONT.NE.INEW) THEN
                   IFRONT=INEW
                   IF(ABS(FMR).GT.0.0) THEN
                       DENS(IFRONT)=FMR/VMOVE
                       VEL(IFRONT)=VMOMR/FMR
                       TENER(IFRONT)=FER/FMR
                       IENER(IFRONT)=TENER(IFRONT)-0.5*VEL(IFRONT)**2
                       IF(IENER(IFRONT).LE.0.0) THEN
                           WRITE(6,'('' NEGATIVE ENERGY IN CELL '',I5,
     *                     '' I='',1PE12.5/'' UPDATING NEW FRONT CELL''
     *                     )') IFRONT,IENER(IFRONT)
                           STOP
                       ENDIF
                   ENDIF
               ENDIF
               XFRONT=RMOVE
          ENDIF
          WRITE(6,'('' HYDRODYNAMIC FRONT AT X='',1PE12.5,'' IN CELL '',
     *                  I5)') XFRONT,IFRONT
C                                      *********************************
C                                      CALCULATE PRESSURE FROM EQUATION
C                                      OF STATE
C                                      *********************************
          CALL EOS (NODES,IENER,DENS,PRES,TEMP,GAMMA,SOUND,SHEAT,CGAMMA,
     *              WEIGHT)
C                                      *********************************
C                                      CALCULATE DRAG ACROSS SCREENS
C                                      *********************************
          IF(SCREEN) THEN
          DO 75 N=1,NS
              NN=NSNODE(N)
              CALL DRAG (RADIUS(NN),SAREA(NN),SIGMA2,TEMP(NN),PRES(NN),
     *                   DENS(NN),VEL(NN),REYN)
   75     CONTINUE
          ENDIF
          TIME=TIME+DT
          CYCLE=CYCLE+1
C          IF(IFRONT.GT.NODES) THEN
C              WRITE(6,'(//'' HYDRODYNAMIC FRONT HAS IMPACTED END OF '',
C     *                    ''CHAMBER, STOP CALCULATION''//)')
C              STOPIT=.TRUE.
C          ENDIF
          IDC=MOD(CYCLE,DCYCLE)
          IF(STOPIT.OR.CYCLE.LT.FCYCLE) IDC=1
          IF(DEBUG.OR.IDC.EQ.0) THEN
              TMIN=1.E10
              TMAX=-1.E10
              CMIN=1.E10
              CMAX=-1.E10
              DO 755 N=1,NODES
                  TMIN=MIN(TMIN,TEMP(N))
                  TMAX=MAX(TMAX,TEMP(N))
                  AMACH=VEL(N)/SOUND(N)
                  CMIN=MIN(AMACH,CMIN)
                  CMAX=MAX(AMACH,CMAX)
  755         CONTINUE
              WRITE(6,'('' MINIMUM TEMPERATURE = '',F10.5,'' DEG K''/
     *                  '' MAXIMUM TEMPERATURE = '',F10.5,'' DEG K''/
     *                  '' MINIMUM MACH NUMBER = '',F10.5,
     *                  '' MAXIMUM MACH NUMBER = '',F10.5/)')
     *                  TMIN,TMAX,CMIN,CMAX
              WRITE(6,'(//'' MESH AT CYCLE '',I5,'' AND TIME '',1PE12.5)
     *        ') CYCLE,TIME
          WRITE(6,'('' X='',1PE12.5,'' P='',E12.5,'' D='',E12.5,
     *              '' U='',E12.5,'' I='',E12.5,'' T='',E12.5,'' CS='',
     *              E12.5,
     *              '' GAMMA='',E12.5)') (RADIUS(N),PRES(N),DENS(N),
     *              VEL(N),
     *              IENER(N),TEMP(N),SOUND(N),GAMMA(N),N=1,NODES)
          ENDIF
C                                      *********************************
C                                      SAVE STATION DATA
C                                      *********************************
          IF(NSTAT.GT.0.AND.CYCLE.GE.FCYCLE) THEN
              NLOC=NLOC+1
              DO 80 M=1,NSTAT
                  N=STATS(M)
                  NSAVE=(M-1)*NCORE+NLOC
                  SCYCLE(NSAVE)=CYCLE
                  STIME(NSAVE)=TIME
                  PSTAT(NSAVE)=PRES(N)
                  DSTAT(NSAVE)=DENS(N)
                  VSTAT(NSAVE)=VEL(N)
                  TSTAT(NSAVE)=TEMP(N)
   80         CONTINUE
          ENDIF
C                                      *********************************
C                                      CALCULATE NEW TIME STEP
C                                      *********************************
          CALL CHOZDT (NODES,VEL,SOUND,DX,DT,STABF)
          IDC=MOD(CYCLE,DCYCLE)
          IF(STOPIT) IDC=0
          IF(TIME.LT.TSTOP.AND.I.NE.CSTOP.AND.CYCLE.LT.FCYCLE) IDC=1
          IF(STOPIT) GO TO 111
  110 CONTINUE
C
C***********************************************************************
C
C                          END OF MAIN LOOP
C
C***********************************************************************
  111 IF(SLIST) THEN
C                                      *********************************
C                                      PRINT OUT STATION DATA
C                                      *********************************
          IF(NLOC.LE.0) THEN
              WRITE(6,'(//'' NO STATION DATA FOR THIS RUN''//)')
          ELSE
              DO 120 M=1,NSTAT
                  I=STATS(M)
                  NSTART=(M-1)*NCORE+1
                  NSTOP=NSTART+NLOC-1
                  WRITE(6,'(''1STATION DATA FOR STATION NUMBER '',I5,
     *                      '' AT RADIUS '',1PE12.5,'' METERS''//)')
     *                     I,RADIUS(I)
                  WRITE(6,'('' CYCLE='',I5,'' TIME='',1PE12.5,
     *                      '' PRESSURE='',E12.5,
     *                      '' DENSITY='',E12.5,'' VELOCITY='',E12.5,
     *                      '' TEMPERATURE='',E12.5)')
     *                      (SCYCLE(I),STIME(I),PSTAT(I),DSTAT(I),
     *                       VSTAT(I),
     *                       TSTAT(I),I=NSTART,NSTOP)
  120         CONTINUE
          ENDIF
      ENDIF
C                                      *********************************
C                                      FORMATS
C                                      *********************************
 1001 FORMAT(1X,I3,61PD15.5,2X,1PD10.3)
 1002 FORMAT(2X,I4,2X,I3,2X,9(1PD10.3,2X),1PD10.3)
 1003 FORMAT(1X,///T2,'I',T8,'NODES',T16,'TIME',T24,'MODEN',T41,'VEL',
     * T53,'PRES',T65,'EDEN',T77,'RHO',T90,'TEMP',T100,'MACH NO.')
 1004 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',
     *                  'FIRST CALL TO FCT1D'/)
 1005 FORMAT(1X,I3,8(1PD12.5,2X))
 1006 FORMAT(1X,'>',I4,2X,I3,2X,9(1PD10.3,2X),1PD10.3)
 1007 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',
     *                  'FIRST CALL TO BOUND'/)
 1008 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',
     *                  'LOOP 30',/)
 1009 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER
     *SECOND CALL TO FCT1D',/)
 1010 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY SECOND CALL TO
     *BOUND'/)
 1011 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER LOOP 50'/)
 1012 FORMAT('                               RADIUS (METERS)')
 1013 FORMAT('     DENSITY VERSUS RADIUS AT ',1PE12.5,' SEC')
 1014 FORMAT('                            DENSITY (KG/METER**3)')
 1015 FORMAT('     VELOCITY VERSUS RADIUS AT ',1PE12.5,' SEC')
 1016 FORMAT('                             VELOCITY (METERS/SEC)')
 1017 FORMAT('     PRESSURE VERSUS RADIUS AT ',1PE12.5,' SEC')
 1018 FORMAT('                             PRESSURE (PASCALS)')
 1019 FORMAT('                                TIME (SEC)')
 1020 FORMAT('PRESSURE VERSUS TIME AT RADIUS = ',1PE12.5,' METERS')
 1021 FORMAT('DENSITY VERSUS TIME AT RADIUS = ',1PE12.5,' METERS')
 1022 FORMAT('VELOCITY VERSUS TIME AT RADIUS = ',1PE12.5,' METERS')
 1023 FORMAT('TEMPERATURE VERSUS TIME AT RADIUS = ',1PE12.5,' METERS')
 1024 FORMAT('                        TEMPERATURE (DEGREES KELVIN)')
 1025 FORMAT('      TEMPERATURE VERSUS RADIUS AT ',1PE12.5,' SEC')
      END
