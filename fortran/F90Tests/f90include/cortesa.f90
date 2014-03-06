      PROGRAM CORTESA 
!
!        THIS PROGRAM SOLVES THE CONTINUITY EQUATIONS FOR MASS,
!        MOMENTUM, AND ENERGY TO MODEL THE FLOW OF A GAS IN ONE
!        DIMENSION.  THE SOLUTION TECHNIQUE IS A ONE DIMENSIONAL
!        VERSION OF THE HULL DIFFERENCING SCHEME, MODIFIED FOR VARIABLE
!        AREA.  WE FURTHER MODIFY THE SCHEME SO THAT THE HYDRODYNAMIC
!        FRONT IS MOVED BY FREE MOLECULAR FLOW, ASSUMING A NEAR VACUUM
!        DOWNSTREAM OF THE FRONT.
!
!        JOHN K. PRENTICE                17 SEPTEMBER 1986
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!	Original F77 code written by John Prentice to model the
!	hydrodynamics in a plasma switch.  Modified for use in F90
!	performance benchmark project.  Modifications include removing
!	calls to plot packages and hardwiring the input/output file
! 	names.
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE VCOORD
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!                                      *********************************
!                                      IMAX   = MAXIMUM NUMBER OF CELLS
!                                               IN THE MESH
!                                      MAXSTA = MAXIMUM AMOUNT OF
!                                               STATION DATA POSSIBLE
!                                      *********************************
      INTEGER, PARAMETER :: IMAX = 500
      INTEGER, PARAMETER :: MAXSTA = 50000
      REAL, PARAMETER :: BOLTS = 1.3806E-23
      REAL, PARAMETER :: AVOG = 6.0222E23
      REAL, PARAMETER :: DIAM = 4.E-10
      REAL, PARAMETER :: PI = 3.14159
      REAL, PARAMETER :: VMIN = 0.1
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: DCYCLE, CSTOP, FCYCLE, CYCLE
      INTEGER, DIMENSION(IMAXP1) :: STATS
      INTEGER, DIMENSION(MAXSTA) :: SCYCLE
      INTEGER, DIMENSION(IMAXP1) :: NSNODE
      INTEGER :: NLOC, I, NSTAT, NCORE, NODES, NVISC, NS, N, IFRONT,    &
     &    MCYCLE, IDC, IFLOW, NLAST, IDONOR, INEW, NN, M, NSTART, NSTOP
      REAL,DIMENSION(IMAXP1)::VEL,PRES,DENS,TENER,IENER,MASS,TEMP
      REAL, DIMENSION(MAXSTA) :: PSTAT, DSTAT, VSTAT, TSTAT, STIME
      REAL, DIMENSION(IMAXP1) :: RADIUS, ZERO
      REAL :: ILEFT
      REAL, DIMENSION(IMAXP1) :: SOUND, DX, GAMMA, MINUS1, SAREA, DBND, &
     &    IBND, VBND, PBND, GBND, RT, VT, PT, PV, VOLD, EOLD, OLDR
      REAL, DIMENSION(0:IMAXP1) :: RBOUND
      REAL :: TIME, PROB, D0(1), E0(1), SHEAT,                                &
     &    CGAMMA, COLD, WEIGHT, TSTOP, STABF, COEFF, AMASS, SIGMA,      &
     &    SIGMA2, T0(1), P0(1),                                         &
     &    CON, GAMMA0(1), C0(1), DT, XFRONT, DTHALF, PLEFT, DLEFT, VLEFT,   &
     &    FML, FEL, VMOML, DXFRNT, RFRNT, AFRONT, VFRONT, VOLSAV, RADSAV&
     &    , ASAV, DXSAV, VAR, FMR, VMOMR, FER, DAR, VAW, RMOVE, AMOVE,  &
     &    VMOVE, TMASS, TVMOM, TENGY, FMASS, FMOM, FENGY, OLDX, ANEW,   &
     &    VNEW, REYN, TMIN, TMAX, CMIN, CMAX
      LOGICAL :: PHIST, DHIST, VHIST, DEBUG, PPLOT, DPLOT, VPLOT, TPLOT &
     &    , SCREEN, SLIST, THIST, STOPIT
      CHARACTER :: INFIL*15, OUTFIL*15, TITLE*80
      REAL , ALLOCATABLE :: AMAC1U(:)
      INCLUDE 'readin.int'
      INCLUDE 'nozzle.int'
      INCLUDE 'tsolve.int'
      INCLUDE 'eos.int'
      INCLUDE 'keel.int'
      INCLUDE 'chozdt.int'
      INCLUDE 'area.int'
!-----------------------------------------------
!                                      *********************************
!                                      INITIALIZATIONS
!                                      *********************************
      TIME = 0.
      NLOC = 0
      CYCLE = 0
      STIME(:MAXSTA) = 0.0
      PSTAT(:MAXSTA) = 0.0
      DSTAT(:MAXSTA) = 0.0
      VSTAT(:MAXSTA) = 0.0
      TSTAT(:MAXSTA) = 0.0
      INFIL = 'INPUT_FILE'
      OUTFIL = 'OUTPUT_FILE'
      OPEN(UNIT=5, FILE=INFIL, STATUS='OLD', FORM='FORMATTED')
      OPEN(UNIT=6, FILE=OUTFIL, STATUS='UNKNOWN', FORM='FORMATTED')
!                                      *********************************
!                                      READ IN THE INITIAL
!                                      CONDITIONS.
!                                      *********************************
      CALL READIN (PROB, TITLE, CSTOP, FCYCLE, DCYCLE, DHIST, VHIST,    &
     &    IMAX, PHIST, DEBUG, NSTAT, STATS, MAXSTA, NCORE, PPLOT, DPLOT &
     &    , VPLOT, TPLOT, SLIST, D0(1), E0(1), NODES, SHEAT, CGAMMA, COLD,    &
     &    THIST, NVISC, SCREEN, WEIGHT, TSTOP, STABF)
      CLOSE(UNIT=5)
!
!     call area to initialize some arrays.  use AFRONT as a dummy variable
!
      AFRONT = AREA(0.0)      
!                                      *********************************
!                                      CALCULATE NOZZLE INITIAL
!                                      CONDITIONS
!                                      *********************************
      COEFF = 0.4*(18.016E-3/8.314)
      AMASS = WEIGHT/AVOG
      SIGMA = SQRT(BOLTS/(3.0*AMASS))
      SIGMA2 = 3.0*SQRT((PI*AMASS)/(8.0*BOLTS))
      T0(1) = COEFF*E0(1)
      IF (T0(1) > 6000.0) T0(1) = 5212.0
!
!        IF COLD NOT SET, CALL NOZZLE TO INITIALIZE
!
      IF (COLD <= 0.0) THEN
          CALL NOZZLE (0.0, P0(1), FLOW = 0.0)
!
!        ASSUME ISOTHERMAL CONDITIONS AT NOZZLE, T=300 DEG KELVIN
!
          IF (SHEAT<=0.0 .OR. CGAMMA<=0.0) THEN
              CALL TSOLVE (1, E0(1), T0(1))
              CON = 8.314/WEIGHT
              D0(1) = P0(1)/(CON*T0(1))
          ELSE
              D0(1) = P0(1)/((CGAMMA - 1.0)*E0(1))
          ENDIF
      ENDIF
      CALL EOS (1, E0, D0, P0, T0, GAMMA0, C0, SHEAT, CGAMMA, WEIGHT)
!                                      *********************************
!                                      INITIALIZE THE MESH
!                                      *********************************
      CALL NOZZLE (0.0, P0(1), D0(1), E0(1), C0(1), PRES(1), DENS(1), VEL(1), IENER(1), &
     &    COLD, GAMMA0(1)) 
      CALL KEEL (NODES, DX, RADIUS, RBOUND, VEL, DENS, IENER, SCREEN, NS,   &
     &    NSNODE, SAREA)
      TENER(:NODES) = VEL(:NODES)**2/2.0 + IENER(:NODES)
      ZERO(:NODES) = 0.0
      MINUS1(:NODES) = -1.0
!                                      *********************************
!                                      ESTIMATE INITIAL TEMPERATURE
!                                      WITH IDEAL GAS LAW AND GAMMA 1.4
!                                      *********************************
      TEMP(:NODES) = COEFF*IENER(:NODES)
      WHERE (TEMP(:NODES) > 6000.0) TEMP(:NODES) = 5212.0
!                                      *********************************
!                                      MAKE INITIAL CALL TO EOS AND
!                                      CHOZDT
!                                      *********************************
      CALL EOS (NODES, IENER, DENS, PRES, TEMP, GAMMA, SOUND, SHEAT,    &
     &    CGAMMA, WEIGHT)
!                                      *********************************
!                                      PRINT INITIAL CONDITIONS
!                                      *********************************
      WRITE (6, '(//'' INITIAL HYDRODYNAMIC CONDITIONS IN MESH:''//)')
       WRITE (6,                                                        &
     &'('' X='',1PE12.5,'' P='',1PE12.5,'' D='',E12.5,                  &
     &  '' U='',E12.5,'' I='',E12.5,'' T='',E12.5,'' CS='',             &
     &  E12.5,'' GAMMA='',E12.5)') (RADIUS(N),PRES(N),DENS(N),VEL(N),   &
     &    IENER(N),TEMP(N),SOUND(N),GAMMA(N),N=1,NODES)
      CALL CHOZDT (NODES, VEL, SOUND, DX, DT, STABF)
!***********************************************************************
!
!                       MAIN LOOP
!
!***********************************************************************
      WRITE (6, '(''1BEGINNING CALCULATION AT TIME='',1PE12.5//)') TIME
      IFRONT = 1
      XFRONT = RBOUND(1)
      STOPIT = .FALSE.
      MCYCLE = MIN(50,CSTOP/2)
      DO I = 1, CSTOP
          IF (I == CSTOP) STOPIT = .TRUE.
          IF (TIME + DT >= TSTOP) STOPIT = .TRUE.
          IF (I==1 .OR. MOD(I,MCYCLE)==0) THEN
              WRITE (*, '('' NOW AT CYCLE '',I5,'' AND TIME '',1PE12.5)'&
     &            ) I, TIME
              WRITE (6,'('' ----->  CYCLE='',I5,'' TIME='',1PE12.5,'' DT='', &
     &                 E12.5)') I, TIME, DT
          ENDIF
!                                      *********************************
!                                      ADVANCE TO T+DT/2
!                                      *********************************
          DTHALF = DT/2.0
!                                      *********************************
!                                      CALCULATE BOUNDARY CONDITIONS.
!                                      THE LEFT BOUNDARY IS DRIVEN BY
!                                      THE NOZZLE, THE RIGHT IS
!                                      PERFECTLY REFLECTIVE.  DEFINE
!                                      CELLS 1 AND 'NODES' TO THESE
!                                      VALUES, AS WELL AS THE LEFT AND
!                                      RIGHT BOUNDARIES.  THIS
!                                      ELIMINATES A PROBLEM IN ETBFCT
!                                      WITH ZERO VELOCITY CELLS
!                                      ADJACENT TO THE BOUNDARY
!                                      *********************************
          IF (I == 1) CALL NOZZLE (TIME, P0(1), D0(1), E0(1), C0(1), PLEFT, DLEFT,  &
     &        VLEFT, ILEFT, COLD, GAMMA0(1))
          IDC = 1
          IF(STOPIT.OR.FCYCLE<=I.AND.MOD(I,DCYCLE)==0.OR.I==1)IDC=0
          IF (DEBUG .OR. IDC==0) WRITE (6,                              &
     &'(//'' NOZZLE CONDITIONS AT BEGINNING OF TIME '',                 &
     &    ''STEP:''/                                                    &
     &    ''   PRESSURE='',1PE12.5/                                     &
     &    ''   DENSITY ='',1PE12.5/                                     &
     &    ''   ENERGY  ='',1PE12.5/                                     &
     &    ''   VELOCITY='',1PE12.5/)') PLEFT, DLEFT, ILEFT, VLEFT
!
!        LOAD NOZZLE CONDITIONS INTO CELL 1
!
          DENS(1) = DLEFT
          PRES(1) = PLEFT
          VEL(1) = VLEFT
          TENER(1) = ILEFT + VLEFT**2/2.0
          IENER(1) = ILEFT
!
!        LOAD INITIAL LEFT HAND FLOW CONDITIONS
!
          IFLOW = 0
          IF (IFLOW /= 2) THEN
              FML = 0.0
              FEL = 0.0
              VMOML = 0.0
          ELSE
              FML = DLEFT*VLEFT*DT*BAREA(1)
              FEL = FML*TENER(1)
              VMOML = FML*VLEFT
          ENDIF
!
!        IF FLOW AT END OF MESH, LOAD REFLECTIVE CONDITIONS
!
          IF (IFRONT > NODES) THEN
              IFRONT = NODES + 1
              DENS(IFRONT) = DENS(NODES)*VOL(NODES)/VOL(IFRONT)
              PRES(IFRONT) = PRES(NODES)
              VEL(IFRONT) = -VEL(NODES)
              TENER(IFRONT) = TENER(NODES)
              IENER(IFRONT) = IENER(NODES)
          ENDIF
!
          NLAST = IFRONT - 1
          IF (IFRONT <= NODES) THEN
              DXFRNT = XFRONT - RBOUND(IFRONT-1)
              RFRNT = RBOUND(NLAST) + 0.5*DXFRNT
              AFRONT = AREA(RFRNT)
              VFRONT = DXFRNT/3.0*(BAREA(NLAST)+SQRT(BAREA(NLAST)*AFRONT&
     &            )+AFRONT)
          ENDIF
!
!        DIFFERENCE ONLY IF FRONT HAS PROPAGATED AT LEAST INTO CELL 3
!
          IF (IFRONT >= 3) THEN
              VOLSAV = VOL(IFRONT)
              RADSAV = RADIUS(IFRONT)
              ASAV = CAREA(IFRONT)
              DXSAV = DX(IFRONT)
              VOL(IFRONT) = VFRONT
              RADIUS(IFRONT) = RFRNT
              CAREA(IFRONT) = AFRONT
              DX(IFRONT) = DXFRNT
!
!        CALCULATE MASSES FOR ACTIVE CELLS
!
              MASS(:IFRONT) = DENS(:IFRONT)*VOL(:IFRONT)
!
!        ***** PHASE 1 *****
!
!        IN PHASE 1 WE UPDATE INTERNAL ENERGY WITH WORK TERM DUE TO
!        VELOCITIES CALCULATED AT LAST TIME STEP.  WE THEN UPDATE
!        VELOCITIES WITH ACCELERATIONS DUE TO PRESSURE GRADIENTS
!
!        DENSITY,VELOCITY,ENERGY, AND PRESSURE AT
!        CELL BOUNDARY AT INITIAL TIME
!
              DBND(:NLAST) = (MASS(:NLAST)+MASS(2:NLAST+1))/(VOL(:NLAST)&
     &            +VOL(2:NLAST+1))
              IBND(:NLAST) = (IENER(:NLAST)+IENER(2:NLAST+1))/2.0
              VBND(:NLAST) = (VEL(:NLAST)+VEL(2:NLAST+1))/2.0
              PBND(:NLAST) = (PRES(:NLAST)+PRES(2:NLAST+1))/2.0
!
!        HALF TIME ADVANCED DENSITY ON CELL BOUNDARY
!
              RT(:NLAST) = DBND(:NLAST)*(1.0 - DTHALF/BAREA(1:NLAST)*(  &
     &            CAREA(2:NLAST+1)*VEL(2:NLAST+1)-CAREA(:NLAST)*VEL(:   &
     &            NLAST))/(RADIUS(2:NLAST+1)-RADIUS(:NLAST)))
!
!        HALF TIME ADVANCED VELOCITIES AND EFFECTIVE GAMMA
!
              VT(:NLAST) = VBND(:NLAST) - DTHALF/RT(:NLAST)*(PRES(2:    &
     &            NLAST+1)-PRES(:NLAST))/(RADIUS(2:NLAST+1)-RADIUS(:    &
     &            NLAST))
              GBND(:NLAST) = 1.0 + PBND(:NLAST)/(IBND(:NLAST)*DBND(:    &
     &            NLAST))
!
!        HALF TIME ADVANCED PRESSURE ON CELL BOUNDARY
!
              PT(:NLAST) = PBND(:NLAST)*(1.0 - DTHALF*GBND(:NLAST)/BAREA&
     &            (1:NLAST)*(CAREA(2:NLAST+1)*VEL(2:NLAST+1)-CAREA(:    &
     &            NLAST)*VEL(:NLAST))/(RADIUS(2:NLAST+1)-RADIUS(:NLAST))&
     &            )
              WHERE (ABS(VT(:NLAST)) <= VMIN) VT(:NLAST) = 0.0
              PV(:NLAST) = PT(:NLAST)*VT(:NLAST)
              VOLD(:NLAST) = VEL(:NLAST)
              EOLD(:NLAST) = TENER(:NLAST)
              OLDR(:NLAST) = DENS(:NLAST)
!
!        CALCULATE NEW VELOCITIES
!
              VEL(2:NLAST) = VOLD(2:NLAST) - DT/OLDR(2:NLAST)*(PT(2:    &
     &            NLAST)-PT(:NLAST-1))/DX(2:NLAST)
              WHERE (ABS(VEL(2:NLAST)) <= VMIN) VEL(2:NLAST) = 0.0
!
!        UPDATE ENERGY
!
              TENER(2:NLAST) = EOLD(2:NLAST) - DT/(OLDR(2:NLAST)*CAREA(2&
     &            :NLAST))*(PV(2:NLAST)*BAREA(2:NLAST)-PV(:NLAST-1)*    &
     &            BAREA(1:NLAST-1))/DX(2:NLAST)
!
!        UPDATE CELL 1 WITH NOZZLE CONDITIONS
!
              CALL NOZZLE (TIME, P0(1), D0(1), E0(1), C0(1), PLEFT, DLEFT, VLEFT,   &
     &            ILEFT, COLD, GAMMA0(1))
              VEL(1) = VLEFT
              TENER(1) = ILEFT + 0.5*VLEFT**2
!
!        ***** PHASE 2 *****
!
!        IN PHASE 2 WE TRANSPORT MASS AND INTERNAL ENERGY WHILE
!        CONSERVING MOMENTUM AND TOTAL ENERGY
!
              FML = 0.0
              FEL = 0.0
              VMOML = 0.0
              DO N = 1, NLAST
!
!        CALCULATE WEIGHTED VELOCITY ON RIGHT CELL BOUNDARY
!
                  VAR = 0.5*(VEL(N)+VEL(N+1))
                  IF (FML==0.0 .AND. VAR==0.0) THEN
!
!        INACTIVE CELL
!
                      FMR = 0.0
                      VMOMR = 0.0
                      FER = 0.0
!
!        ACTIVE CELL
!
                  ELSE
!
!        SET DONOR CELL DENSITY
!
                      IDONOR = N
                      IF (VAR < 0.0) IDONOR = N + 1
                      DAR = OLDR(IDONOR)
                      VAW = DAR*(1.0 - DT/BAREA(N)*(CAREA(N+1)*VEL(N+1)-&
     &                    CAREA(N)*VEL(N))/(RADIUS(N+1)-RADIUS(N)))
!
!        COMPUTE MASS FLUX TO RIGHT
!
                      RMOVE = RBOUND(N) + VAR*DT
                      AMOVE = AREA(RMOVE)
                      VMOVE = (RMOVE - RBOUND(N))/3.0*(BAREA(N)+SQRT(   &
     &                    BAREA(N)*AMOVE)+AMOVE)
                      FMR = VMOVE*VAW
                      VMOMR = FMR*VEL(IDONOR)
                      FER = FMR*TENER(IDONOR)
!
!        SKIP FLUXES INTO CELL 1
!
                      IF (N /= 1) THEN
                          TMASS = MASS(N) + FML - FMR
                          TVMOM = VEL(N)*MASS(N) + VMOML - VMOMR
                          TENGY = MASS(N)*TENER(N) + FEL - FER
!
!        COMPUTE FINAL VELOCITIES, ENERGY, AND PRESSURE
!
                          VEL(N) = TVMOM/TMASS
                          TENER(N) = TENGY/TMASS
                          IENER(N) = TENER(N) - 0.5*VEL(N)**2
                          MASS(N) = TMASS
                          DENS(N) = MASS(N)/VOL(N)
!
!        CHECK FOR NEGATIVE ENERGY
!
                          IF (IENER(N) <= 0.0) THEN
                              WRITE (6,                                 &
     &'('' NEGATIVE ENERGY IN CELL '',                                I5&
     &,'' I='',1PE12.5,'' FEL='',E12.5,                               ''&
     & FER='',E12.5)') N, IENER(N), FEL, FER
                              STOP 
                          ENDIF
                      ENDIF
                      FML = FMR
                      FEL = FER
                      VMOML = VMOMR
                  ENDIF
              END DO
!
!        RELOAD CELL 1
!
              DENS(1) = DLEFT
              PRES(1) = PLEFT
              VEL(1) = VLEFT
              TENER(1) = ILEFT + 0.5*VLEFT**2
              IENER(1) = ILEFT
              VOL(IFRONT) = VOLSAV
              RADIUS(IFRONT) = RADSAV
              CAREA(IFRONT) = ASAV
              DX(IFRONT) = DXSAV
          ENDIF
!
!        ADVANCE HYDRODYNAMIC FRONT BY FREE MOLECULAR FLOW
!
          IF (IFRONT <= NODES) THEN
              FMR = 0.0
              FER = 0.0
              VMOMR = 0.0
              FMASS = DENS(IFRONT)*VFRONT
              FMOM = FMASS*VEL(IFRONT)
              FENGY = FMASS*TENER(IFRONT)
!
!        CALCULATE ANY FLUXES INTO NEXT CELL
!
              INEW = IFRONT
              RMOVE = VEL(IFRONT)*DT + XFRONT
              IF (RMOVE > RBOUND(IFRONT)) THEN
                  INEW = IFRONT + 1
                  IF (IFRONT > NODES) THEN
                      RMOVE = RBOUND(NODES)
                  ELSE
                      AMOVE = AREA(RMOVE)
                      VMOVE = (RMOVE - RBOUND(IFRONT))/3.0*(BAREA(IFRONT&
     &                    )+SQRT(BAREA(IFRONT)*AMOVE)+AMOVE)
                      FMR = VMOVE*DENS(IFRONT)
                      VMOMR = FMR*VEL(IFRONT)
                      FER = FMR*TENER(IFRONT)
                      FMASS = FMASS - FMR
                      FMOM = FMOM - VMOMR
                      FENGY = FENGY - FER
                  ENDIF
              ENDIF
!
!        NOW ADD OLD FLUXES FROM LEFT BOUNDARY INTO OLD FRONT CELL
!
              FMASS = FMASS + FML
              FMOM = FMOM + VMOML
              FENGY = FENGY + FEL
!
!        UPDATE HYDRO FOR OLD FRONT CELL
!
              OLDX = MIN(RMOVE,RBOUND(IFRONT))
              ANEW = AREA(OLDX)
              VNEW = (OLDX - RBOUND(IFRONT-1))/3.0*(BAREA(IFRONT-1)+    &
     &            SQRT(BAREA(IFRONT-1)*ANEW)+ANEW)
              DENS(IFRONT) = FMASS/VNEW
              VEL(IFRONT) = FMOM/FMASS
              TENER(IFRONT) = FENGY/FMASS
              IENER(IFRONT) = TENER(IFRONT) - 0.5*VEL(IFRONT)**2
              IF (IENER(IFRONT) <= 0.0) THEN
                  WRITE (6,                                             &
     &'('' NEGATIVE ENERGY IN CELL '',I5,'' I='',                     1P&
     &E12.5/'' UPDATING OLD FRONT CELL'')') IFRONT, IENER(IFRONT)
                  STOP 
              ENDIF
!
!        MOVE FRONT TO NEXT CELL IF APPROPIATE
!
              IF (IFRONT /= INEW) THEN
                  IFRONT = INEW
                  IF (ABS(FMR) > 0.0) THEN
                      DENS(IFRONT) = FMR/VMOVE
                      VEL(IFRONT) = VMOMR/FMR
                      TENER(IFRONT) = FER/FMR
                      IENER(IFRONT) = TENER(IFRONT) - 0.5*VEL(IFRONT)**2
                      IF (IENER(IFRONT) <= 0.0) THEN
                          WRITE (6,                                     &
     &'('' NEGATIVE ENERGY IN CELL '',I5,                       '' I='',&
     &1PE12.5/'' UPDATING NEW FRONT CELL''                      )')     &
     &                        IFRONT, IENER(IFRONT)
                          STOP 
                      ENDIF
                  ENDIF
              ENDIF
              XFRONT = RMOVE
          ENDIF
          WRITE (6,                                                     &
     &'('' HYDRODYNAMIC FRONT AT X='',1PE12.5,'' IN CELL '',            &
     &      I5)') XFRONT, IFRONT
!                                      *********************************
!                                      CALCULATE PRESSURE FROM EQUATION
!                                      OF STATE
!                                      *********************************
          CALL EOS (NODES, IENER, DENS, PRES, TEMP, GAMMA, SOUND, SHEAT &
     &        , CGAMMA, WEIGHT)
!                                      *********************************
!                                      CALCULATE DRAG ACROSS SCREENS
!                                      *********************************
          IF (SCREEN) THEN
              DO N = 1, NS
                  NN = NSNODE(N)
                  CALL DRAG (SAREA(NN), SIGMA2, TEMP(NN),   &
     &                PRES(NN), DENS(NN), VEL(NN), REYN)
              END DO
          ENDIF
          TIME = TIME + DT
          CYCLE = CYCLE + 1
!          IF(IFRONT.GT.NODES) THEN
!              WRITE(6,'(//'' HYDRODYNAMIC FRONT HAS IMPACTED END OF '',
!     *                    ''CHAMBER, STOP CALCULATION''//)')
!              STOPIT=.TRUE.
!          ENDIF
          IDC = MOD(CYCLE,DCYCLE)
          IF (STOPIT .OR. CYCLE<FCYCLE) IDC = 1
          IF (DEBUG .OR. IDC==0) THEN
              TMIN = 1.E10
              TMAX = -1.E10
              CMIN = 1.E10
              CMAX = -1.E10
              ALLOCATE (AMAC1U(NODES))
              TMIN = MIN(TMIN,MINVAL(TEMP(:NODES)))
              TMAX = MAX(TMAX,MAXVAL(TEMP(:NODES)))
              AMAC1U = VEL(:NODES)/SOUND(:NODES)
              CMIN = MIN(MINVAL(AMAC1U),CMIN)
              CMAX = MAX(MAXVAL(AMAC1U),CMAX)
              DEALLOCATE (AMAC1U)
              WRITE (6,                                                 &
     &'('' MINIMUM TEMPERATURE = '',F10.5,'' DEG K''/                   &
     &  '' MAXIMUM TEMPERATURE = '',F10.5,'' DEG K''/                   &
     &  '' MINIMUM MACH NUMBER = '',F10.5,                              &
     &  '' MAXIMUM MACH NUMBER = '',F10.5/)') TMIN, TMAX, CMIN, CMAX
              WRITE (6,                                                 &
     &'(//'' MESH AT CYCLE '',I5,'' AND TIME '',1PE12.5)        ') CYCLE&
     &            , TIME
              WRITE (6,                                                 &
     &'('' X='',1PE12.5,'' P='',E12.5,'' D='',E12.5,                    &
     &  '' U='',E12.5,'' I='',E12.5,'' T='',E12.5,'' CS='', E12.5,      &
     &  '' GAMMA='',E12.5)') (RADIUS(N),PRES(N),DENS(N),VEL(N),IENER(N),&
     &            TEMP(N),SOUND(N),GAMMA(N),N=1,NODES)
          ENDIF
!                                      *********************************
!                                      SAVE STATION DATA
!                                      *********************************
          IF (NSTAT>0 .AND. CYCLE>=FCYCLE) THEN
              NLOC = NLOC + 1
              SCYCLE(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = CYCLE
              STIME(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = TIME
              PSTAT(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = PRES(STATS(:NSTAT))
              DSTAT(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = DENS(STATS(:NSTAT))
              VSTAT(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE)=VEL(STATS(:NSTAT))
              TSTAT(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = TEMP(STATS(:NSTAT))
          ENDIF
!                                      *********************************
!                                      CALCULATE NEW TIME STEP
!                                      *********************************
          CALL CHOZDT (NODES, VEL, SOUND, DX, DT, STABF)
          IDC = MOD(CYCLE,DCYCLE)
          IF (STOPIT) IDC = 0
          IF (TIME<TSTOP .AND. I/=CSTOP .AND. CYCLE<FCYCLE) IDC = 1
          IF (STOPIT) EXIT 
      END DO
!
!***********************************************************************
!
!                          END OF MAIN LOOP
!
!***********************************************************************
      IF (SLIST) THEN
!                                      *********************************
!                                      PRINT OUT STATION DATA
!                                      *********************************
          IF (NLOC <= 0) THEN
              WRITE (6, '(//'' NO STATION DATA FOR THIS RUN''//)')
          ELSE
               DO M = 1, NSTAT
                  I = STATS(M)
                  NSTART = (M - 1)*NCORE + 1
                  NSTOP = NSTART + NLOC - 1
                  WRITE (6,                                             &
     &'(''1STATION DATA FOR STATION NUMBER '',I5,                       &
     &  '' AT RADIUS '',1PE12.5,'' METERS''//)') I, RADIUS(I)
                  WRITE (6,                                             &
     &'('' CYCLE='',I5,'' TIME='',1PE12.5,                              &
     &  '' PRESSURE='',E12.5,                                           &
     &  '' DENSITY='',E12.5,'' VELOCITY='',E12.5,                       &
     &  '' TEMPERATURE='',E12.5)') (SCYCLE(I),STIME(I),PSTAT(I),DSTAT(I)&
     &                ,VSTAT(I),TSTAT(I),I=NSTART,NSTOP)
              END DO
          ENDIF
      ENDIF
!                                      *********************************
!                                      FORMATS
!                                      *********************************
 1001 FORMAT(1X,I3,61PD15.5,2X,1PD10.3)
 1002 FORMAT(2X,I4,2X,I3,2X,9(1PD10.3,2X),1PD10.3)
 1003 FORMAT(1X,///T2,'I',T8,'NODES',T16,'TIME',T24,'MODEN',T41,'VEL',  &
     & T53,'PRES',T65,'EDEN',T77,'RHO',T90,'TEMP',T100,'MACH NO.')
 1004 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',           &
     &                  'FIRST CALL TO FCT1D'/)
 1005 FORMAT(1X,I3,8(1PD12.5,2X))
 1006 FORMAT(1X,'>',I4,2X,I3,2X,9(1PD10.3,2X),1PD10.3)
 1007 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',           &
     &                  'FIRST CALL TO BOUND'/)
 1008 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',           &
     &                  'LOOP 30',/)
 1009 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER              &
     &SECOND CALL TO FCT1D',/)
 1010 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY SECOND CALL TO     &
     &BOUND'/)
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
      END PROGRAM CORTESA
