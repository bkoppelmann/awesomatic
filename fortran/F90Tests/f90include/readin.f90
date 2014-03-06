      SUBROUTINE READIN(PROB, TITLE, CSTOP, FCYCLE, DCYCLE, DHIST, VHIST&
     &    , IMAX, PHIST, DEBUG, NSTAT, STATS, MAXSTA, NCORE, PPLOT,     &
     &    DPLOT, VPLOT, TPLOT, SLIST, D0, E0, NODES, SHEAT, GAMMA, COLD &
     &    , THIST, NVISC, SCREEN, WEIGHT, TSTOP, STABF)
!
!        FREE FORMAT INPUT DRIVER
!        JOHN K. PRENTICE,  11 JULY 1986
!
!        THIS ROUTINE READS THE FREE FORMAT INPUT FILE TO DETERMINE
!        PARAMETERS FOR A CALCULATION.  BELOW IS GIVEN A LIST OF THE
!        INPUT PARAMETERS SHOWING THE USER INPUT WORD(S), THE FORTRAN
!        VARIABLE NAME LOADED, AND THE DESCRIPTION OF THE PARAMETER.
!
!        INPUT WORD(S)           FORTRAN    TYPE         DESCRIPTION
!                                VARIABLE
!                                NAME
!        -------------           --------   ----   ---------------------
!
!        PROBLEM_NUMBER            PROB      R     PROBLEM NUMBER
!            -OR-
!        PROB
!
!        TITLE                     TITLE     !     80 CHARACTER TITLE
!                                                  FOR THIS CALCULATION.
!                                                  WHEN THE WORD TITLE
!                                                  IS ENCOUNTERED IN
!                                                  INPUT, SCANNING WILL
!                                                  STOP FOR THAT LINE
!                                                  AND THE ENTIRE NEXT
!                                                  LINE WILL BE READ AS
!                                                  THE TITLE.
!
!        NUMBER_OF_NODES           NODES     I     NUMBER OF CELLS IN
!              -OR-                                MESH
!        NODES
!
!        CYCLE_STOP                CSTOP     I     LAST CYCLE TO RUN
!             -OR-                                 CALCUALATION TO
!        CSTOP
!
!        TIME_STOP                 TSTOP     R     LAST TIME TO RUN
!             -OR-                                 CALCULATION TO.
!        TSTOP                                     OPTIONAL.
!
!        STABILITY_FACTOR          STABF     R     COURANT CONDITION
!             -OR-                                 STABILITY FACTOR
!        STABF
!
!        FIRST_DUMP                FCYCLE    I     FIRST CYCLE TO
!             -OR-                                 DUMP RESULTS AT.
!        FCYCLE                                    DEFAULT IS 1
!
!        DUMP_INTERVAL             DCYCLE    I     CYCLE INTERVAL TO
!             -OR-                                 DUMP RESULTS.
!        DCYCLE                                    DEFAULT IS 1
!
!        SPECIFIC_HEAT             SHEAT     R     SPECIFIC HEAT OF GAS.
!              -OR-                                IF NOT SPECIFIED, THEN
!        SHEAT                                     A VALUE WILL BE CHOSEN
!                                                  APPROPIATE TO STEAM.
!                                                  IF SET, CONSTANT VALUE
!                                                  WILL BE ASSUMED. MUST
!                                                  SET GAMMA IS SPECIFIC
!                                                  HEAT IS SET.
!
!        GAMMA                     GAMMA     R     THERMODYNAMIC GAMMA OF
!                                                  GAS.  IF NOT SPECIFIED,
!                                                  A VALUE APPROPIATE TO
!                                                  STEAM WILL BE USED.
!                                                  IF SET, CONSTANT VALUE
!                                                  WILL BE ASSUMED.  MUST
!                                                  SET SPECIFIC HEAT IF
!                                                  GAMMA IS SET.
!
!        MOLECULAR_WEIGHT          WEIGHT    R     MOLEULAR WEIGHT IN
!             -OR-                                 KG/MOLE.  IF NOT
!        WEIGHT                                    SPECIFIED, A VALUE
!                                                  APPROPIATE TO WATER
!                                                  WILL BE USED
!
!        INCLUDE_SCREENS           SCREEN    B     IF SET, SCREENS WILL
!                                                  BE INCLUDED IN
!                                                  CALCULATION.  DEFAULT
!                                                  IS NO SCREENS
!
!        CONSTANT_FLOW             COLD      R     IF SET, CONSTANT FLOW
!                                                  THROUGH THE NOZZLE
!                                                  WILL BE ASSUMED AT THE
!                                                  MASS RATE GIVEN
!
!        CHAMBER_DENSITY           D0        R     INITIAL CHAMBER
!             -OR-                                 DENSITY (KG/M**3)
!        D0
!
!        CHAMBER_ENERGY            E0        R     INITIAL CHAMBER
!             -OR-                                 SPECIFIC INTERNAL
!        E0                                        ENERGY (J/KG)
!
!        TEMPERATURE_HISTOGRAM     THIST     B     CAUSE TEMPERATURE
!             -OR-                                 HISTOGRAM TO BE
!        THIST                                     PRODUCED.  DEFAULT
!                                                  IS NO PLOT
!
!        DENSITY_HISTOGRAM         DHIST     B     CAUSE
!             -OR-                                 DENSITY HISTOGRAM TO
!        DHIST                                     BE PRODUCED. DEFAULT
!                                                  IS NO PLOT
!
!        PRESSURE_HISTOGRAM        PHIST     B     CAUSE
!             -OR-                                 PRESSURE HISTOGRAM TO
!        PHIST                                     BE PRODUCED.  DEFAULT
!                                                  IS NO PLOT
!
!        VELOCITY_HISTOGRAM        VHIST     B     CAUSE
!             -OR-                                 VELOCITY HISTOGRAM TO
!        VHIST                                     BE PRODUCED.  DEFAULT
!                                                  IS NO PLOT
!
!        DEBUG                     DEBUG     B     CAUSE
!                                                  DEBUG INFORMATION TO
!                                                  BE PRODUCED EACH DUMP
!                                                  DEFAULT NO DEBUG INFO
!
!        STATIONS                  STATS     I     LIST OF CELL NUMBERS
!          -OR-                                    FOR WHICH TO STORE
!        STATS                                     STATION DATA.  NOTE
!                                                  THAT FORTRAN VARIABLE
!                                                  STATS IS AN INTEGER
!                                                  ARRAY.  THE LENGTH
!                                                  IS COMPUTED IN THIS
!                                                  ROUTINE TO BE NSTAT
!                                                  AND THIS VALUE IS
!                                                  PASSED BACK.  DEFAULT
!                                                  IS NSTAT=0.
!                                                  NOTE:  THE NUMBER OF
!                                                  WORDS OF
!                                                  CM NEEDED FOR EACH
!                                                  STATION IS NCORE
!
!        *** IF NSTAT > 0, THEN THE FOLLOWING TYPES OF STATION PLOTS
!            MAY BE REQUESTED.  DEFAULT IN ALL CASES IS NO PLOT
!
!        PRESSURE_PLOT             PPLOT     B     PRESSURE -VS- TIME
!             -OR-                                 PLOT FOR EACH
!        PPLOT                                     STATION
!
!        DENSITY_PLOT              DPLOT     B     DENSITY -VS- TIME
!             -OR-                                 PLOT FOR EACH
!        DPLOT                                     STATION
!
!        VELOCITY_PLOT             VPLOT     B     VELOCITY -VS- TIME
!             -OR-                                 PLOT FOR EACH
!        VPLOT                                     STATION
!
!        TEMPERATURE_PLOT          TPLOT     B     TEMPERATURE -VS- TIME
!             -OR-                                 PLOT FOR EACH STATION
!        TPLOT
!
!        PRINT_STATION_DATA        SLIST     B     PRINT STATION DATA
!             -OR-                                 FOR EACH STATION
!        SLIST
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE VCIMAGE
      USE VIMAGE
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER CSTOP,FCYCLE,DCYCLE,IMAX,NSTAT,MAXSTA,NCORE,NODES,NVISC
      REAL PROB, D0, E0, SHEAT, GAMMA, COLD, WEIGHT, TSTOP, STABF
      LOGICAL DHIST, VHIST, PHIST, DEBUG, PPLOT, DPLOT, VPLOT, TPLOT,   &
     &    SLIST, THIST, SCREEN
      CHARACTER TITLE*80
      INTEGER, DIMENSION(MAXSTA) :: STATS
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: TYPE, NCYCLE, I
      REAL :: RNODES, RCSTOP, RCYCLE, AVISC, RSTAT
      LOGICAL :: ABORT
      INCLUDE 'terror.int'
      INCLUDE 'value.int'
!-----------------------------------------------
!                                      *********************************
!                                      DEFAULTS
!                                      *********************************
      NODES = -999
      COLD = -999.0
      SHEAT = -999.0
      GAMMA = -999.0
      TSTOP = 1.E10
      STABF = 0.5
      WEIGHT = 18.016E-3
      DCYCLE = 1
      FCYCLE = 1
      NVISC = 0
      SCREEN = .FALSE.
      THIST = .FALSE.
      DHIST = .FALSE.
      PHIST = .FALSE.
      VHIST = .FALSE.
      DEBUG = .FALSE.
      PPLOT = .FALSE.
      DPLOT = .FALSE.
      VPLOT = .FALSE.
      TPLOT = .FALSE.
      SLIST = .FALSE.
      NSTAT = 0
!                                      *********************************
!                                      PARSE INPUT AND COMPARE TO
!                                      EXPECTED STRINGS
!                                      *********************************
   10 CONTINUE
      CALL NEXT
   20 CONTINUE
      IF (.NOT.EOFF) THEN
!                                      *********************************
!                                      PROBLEM NUMBER
!                                      *********************************
          IF (FIELD=='PROB' .OR. FIELD=='PROBLEM_NUMBER') THEN
              CALL VALUE (PROB, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      TITLE
!                                      *********************************
          ELSE IF (FIELD == 'TITLE') THEN
              READ (5, '(A)', END=30) TITLE
              ICPNT = 9999
              GO TO 10
   30         CONTINUE
              WRITE (6,                                                 &
     &'('' END OF FILE ENCOUNTERED WHILE TRYING TO '',                  &
     &  ''READ THE TITLE, ABORT'')')
               STOP 
!                                      *********************************
!                                      NUMBER OF NODES
!                                      *********************************
          ELSE IF (FIELD=='NUMBER_OF_NODES' .OR. FIELD=='NODES') THEN
              CALL VALUE (RNODES, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              NODES = INT(RNODES)
!
!        TIME STOP
!
          ELSE IF (FIELD=='TIME_STOP' .OR. FIELD=='TSTOP') THEN
              CALL VALUE (TSTOP, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!
!        STABILITY FACTOR
!
          ELSE IF (FIELD=='STABILITY_FACTOR' .OR. FIELD=='STABF') THEN
              CALL VALUE (STABF, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      CYCLE STOP
!                                      *********************************
          ELSE IF (FIELD=='CSTOP' .OR. FIELD=='CYCLE_STOP') THEN
              CALL VALUE (RCSTOP, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              CSTOP = INT(RCSTOP)
!                                      *********************************
!                                      FIRST DUMP
!                                      *********************************
          ELSE IF (FIELD=='FCYCLE' .OR. FIELD=='FIRST_DUMP') THEN
              CALL VALUE (RCYCLE, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              FCYCLE = INT(RCYCLE)
!                                      *********************************
!                                      DUMP INTERVAL
!                                      *********************************
          ELSE IF (FIELD=='DCYCLE' .OR. FIELD=='DUMP_INTERVAL') THEN
              CALL VALUE (RCYCLE, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              DCYCLE = INT(RCYCLE)
!                                      *********************************
!                                      SPECIFIC HEAT
!                                      *********************************
          ELSE IF (FIELD=='SPECIFIC_HEAT' .OR. FIELD=='SHEAT') THEN
              CALL VALUE (SHEAT, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      MOLECULAR WEIGHT
!                                      *********************************
          ELSE IF (FIELD=='MOLECULAR_WEIGHT' .OR. FIELD=='WEIGHT') THEN
              CALL VALUE (WEIGHT, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      GAMMA
!                                      *********************************
          ELSE IF (FIELD == 'GAMMA') THEN
              CALL VALUE (GAMMA, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      CONSTANT MASS FLOW
!                                      *********************************
          ELSE IF (FIELD == 'CONSTANT_FLOW') THEN
              CALL VALUE (COLD, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      MEAN FREE PATH IN SHOCK
!                                      *********************************
          ELSE IF (FIELD == 'MEAN_FREE_PATH') THEN
              CALL VALUE (AVISC, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              NVISC = INT(AVISC)
              IF (NVISC < 0) THEN
                  WRITE (6,                                             &
     &'(//'' ILLEGAL MEAN FREE PATH OF '',I5,                       '', &
     &ABORT.''//)') NVISC
                  STOP 
              ENDIF
!                                      *********************************
!                                      INITIAL CHAMBER DENSITY
!                                      *********************************
          ELSE IF (FIELD=='CHAMBER_DENSITY' .OR. FIELD=='D0') THEN
              CALL VALUE (D0, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      INITIAL CHAMBER ENERGY
!                                      *********************************
          ELSE IF (FIELD=='CHAMBER_ENERGY' .OR. FIELD=='E0') THEN
              CALL VALUE (E0, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      INCLUDE SCREENS
!                                      *********************************
          ELSE IF (FIELD == 'INCLUDE_SCREENS') THEN
              SCREEN = .TRUE.
!                                      *********************************
!                                      TEMPERATURE HISTOGRAM
!                                      *********************************
          ELSE IF (FIELD=='THIST' .OR. FIELD=='TEMPERATURE_HISTOGRAM')  &
     &            THEN
              THIST = .TRUE.
!                                      *********************************
!                                      DENSITY HISTOGRAM
!                                      *********************************
          ELSE IF (FIELD=='DHIST' .OR. FIELD=='DENSITY_HISTOGRAM') THEN
              DHIST = .TRUE.
!                                      *********************************
!                                      PRESSURE HISTOGRAM
!                                      *********************************
          ELSE IF (FIELD=='PHIST' .OR. FIELD=='PRESSURE_HISTOGRAM') THEN
              PHIST = .TRUE.
!                                      *********************************
!                                      VELOCITY HISTOGRAM
!                                      *********************************
          ELSE IF (FIELD=='VHIST' .OR. FIELD=='VELOCITY_HISTOGRAM') THEN
              VHIST = .TRUE.
!                                      *********************************
!                                      DEBUG
!                                      *********************************
          ELSE IF (FIELD == 'DEBUG') THEN
              DEBUG = .TRUE.
!                                      *********************************
!                                      STATIONS
!                                      *********************************
          ELSE IF (FIELD=='STATS' .OR. FIELD=='STATIONS') THEN
   40         CONTINUE
              NSTAT = NSTAT + 1
              IF (NSTAT > IMAX) THEN
                  WRITE (6,                                             &
     &'('' TOO MANY STATIONS HAVE BEEN DEFINED.''/                      &
     &  '' THE MAXIMUM ALLOWED IS '',I5/                                &
     &  '' CHANGE IMAX IN THE'',                                        &
     &  '' PROGRAM AND RECOMPILE'')') IMAX
                  STOP 
              ELSE
                  CALL VALUE (RSTAT, TYPE)
                  IF (TYPE /= 1) THEN
                      NSTAT = NSTAT - 1
                      EOFF = .FALSE.
                      IF (TYPE == (-1)) EOFF = .TRUE.
                      GO TO 20
                  ELSE
                      STATS(NSTAT) = INT(RSTAT)
                      GO TO 40
                  ENDIF
              ENDIF
!                                      *********************************
!                                      STATION PRESSURE PLOT
!                                      *********************************
          ELSE IF (FIELD=='PPLOT' .OR. FIELD=='PRESSURE_PLOT') THEN
              PPLOT = .TRUE.
!                                      *********************************
!                                      STATION DENSITY PLOT
!                                      *********************************
          ELSE IF (FIELD=='DPLOT' .OR. FIELD=='DENSITY_PLOT') THEN
              DPLOT = .TRUE.
!                                      *********************************
!                                      STATION VELOCITY PLOT
!                                      *********************************
          ELSE IF (FIELD=='VPLOT' .OR. FIELD=='VELOCITY_PLOT') THEN
              VPLOT = .TRUE.
!                                      *********************************
!                                      STATION TEMPERATURE PLOT
!                                      *********************************
          ELSE IF (FIELD=='TPLOT' .OR. FIELD=='TEMPERATURE_PLOT') THEN
              TPLOT = .TRUE.
!                                      *********************************
!                                      PRINT STATION DATA
!                                      *********************************
          ELSE IF (FIELD=='SLIST' .OR. FIELD=='PRINT_STATION_DATA') THEN
              SLIST = .TRUE.
!                                      *********************************
!                                      UNRECOGNIZED WORD
!                                      *********************************
          ELSE
              WRITE (6,                                                 &
     &'('' UNRECOGNIZED WORD ENCOUNTERED IN INPUT''/                    &
     &  '' THE WORD WAS ----> '',A/'' ABORT''//)') FIELD
              STOP 
          ENDIF
      ELSE
!                                      *********************************
!                                      MAKE SURE THE NUMBER OF NODES IS
!                                      A VALID NUMBER
!                                      *********************************
          IF (NODES == -999) THEN
              WRITE (6,                                                 &
     &'('' THE NUMBER OF NODES WAS NOT SPECIFIED IN '',                 &
     &  ''INPUT, ABORT'')')
               STOP 
          ELSE IF (NODES <= 0) THEN
              WRITE (6,                                                 &
     &'('' AN ILLEGAL VALUE WAS GIVEN FOR THE NUMBER '',                &
     &  ''OF NODES, ABORT'')')
               STOP 
          ELSE IF (NODES > IMAX + 1) THEN
                WRITE (6,                                                 &
     &'('' THE NUMBER OF NODES REQUESTED WAS '',I5/                     &
     &  '' THE MAXIMUM ALLOWED IS '',I5/                                &
     &  '' TO RUN MORE THAN THE MAXIMUM, CHANGE IMAX'',                 &
     &  '' IN THE CODE AND RECOMPILE. ABORT.''//)') NODES, IMAX
              STOP 
          ENDIF
!                                      *********************************
!                                      CHECK WHETHER BOTH GAMMA AND
!                                      SPECIFIC HEAT WERE GIVEN
!                                      *********************************
          IF (GAMMA>0.0 .OR. SHEAT>0.0) THEN
              IF (GAMMA>0.0 .AND. SHEAT<=0.0) THEN
                  WRITE (6,                                             &
     &'(//'' IF GAMMA IS SPECIFIED, SO MUST BE THE''                    &
     &   ,'' THE SPECIFIC HEAT.  ABORT'')')
                   STOP 
              ELSE IF (GAMMA<0.0 .AND. SHEAT>0.0) THEN
                  WRITE (6,                                             &
     &'(//'' IF SPECIFIC HEAT IS SPECIFIED, SO '',                    ''&
     &MUST BE GAMMA.  ABORT'')')
                   STOP 
              ENDIF
          ENDIF
!                                      *********************************
!                                      SORT THE STATIONS AND BE SURE
!                                      THAT THERE IS ENOUGH STORAGE FOR
!                                      THE STATIONS
!                                      *********************************
          IF (NSTAT > 0) THEN
              NCYCLE = CSTOP - FCYCLE + 1
              NCORE = NCYCLE*NSTAT
              IF (NCORE > MAXSTA) THEN
                  WRITE (6,                                             &
     &'('' THERE IS NOT ENOUGH ARRAY STORAGE TO STORE ''               ,&
     &''ALL THE STATION DATA FOR THE STATIONS DEFINED''                /&
     &'' YOU NEED AT LEAST '',I8,'' WORDS, BUT HAVE'',                 '&
     &' ONLY '',I8/'' CHANGE PARAMETER MAXSTA IN THE ''                '&
     &'MAIN ROUTINE TO AT LEAST '',I8,'' AND RECOMPILE''               /&
     &/)') NCORE, MAXSTA, NCORE
                  STOP 
              ENDIF
              NCORE = NCYCLE
              ABORT = .FALSE.
              CALL QSORT (STATS(1:NSTAT))
              DO I = 1, NSTAT
                  IF (STATS(I)<1 .OR. STATS(I)>NODES) WRITE (6,         &
     &'(/                  '' STATION AT CELL '',I5,'' IS OUTSIDE MESH''&
     &)') STATS(I)
              END DO
          ENDIF
!                                      *********************************
!                                      PRINT OUT INPUT VALUES
!                                      *********************************
          WRITE (6,                                                     &
     &'(''1CORTESA ONE DIMENSIONAL GAS DYNAMICS CODE''//1X,A//             &
     &  '' PARAMETERS FOR THIS RUN ARE:''/)') TITLE
          WRITE (6,                                                     &
     &'('' PROBLEM NUMBER = '',F10.5/                                   &
     &  '' NUMBER OF NODES = '',I5/                                     &
     &  '' CYCLE STOP = '',I5/                                          &
     &  '' TIME STOP = '',1PE12.5/                                      &
     &  '' COURANT CONDITION STABILITY FACTOR = '',0PF5.2/              &
     &  '' FIRST CYCLE TO DUMP AT = '',I5/                              &
     &  '' DUMP INTERVAL = '',I5/)') PROB, NODES, CSTOP, TSTOP, STABF,  &
     &        FCYCLE, DCYCLE
          IF (COLD < 0.0) THEN
              WRITE (6,                                                 &
     &'(//'' CALCULATE NOZZLE FLOW USING PRESSURE-TIME HISTORY FROM INPU&
     &T''/)')
          ELSE
               WRITE (6,                                                &
     &'(//'' CALCULATE NOZZLE FLOW AS CONSTANT AT '',                 1P&
     &E12.5,'' KG/SEC''/)') COLD
          ENDIF
          IF (SCREEN) THEN
              WRITE (6, '(/'' INCLUDE SCREENS IN CALCULATION''/)')
          ELSE
               WRITE (6,                                                &
     &            '(/'' NO SCREENS ARE PRESENT IN CALCULATION''/)')
          ENDIF
           IF (SHEAT > 0.0) THEN
              WRITE (6,                                                 &
     &'(//'' USE CONSTANT SPECIFIC HEAT OF '',1PE12.5,                 '&
     &' JOULES/DEG KELVIN/KILOGRAM'')') SHEAT
              WRITE (6, '('' USE CONSTANT GAMMA OF '',1PE12.5)') GAMMA
          ELSE
              WRITE (6,                                                 &
     &            '(//'' USE THERMODYNAMICS APPROPIATE TO STEAM'')')
          ENDIF
           WRITE (6, '('' MOLECULAR WEIGHT = '',1PE12.5,'' KG/MOLE''/)')&
     &         WEIGHT
          IF (DHIST) WRITE (6, '('' PLOT DENSITY HISTOGRAM ''/)')
           IF (PHIST) WRITE (6, '('' PLOT PRESSURE HISTOGRAM ''/)')
           IF (VHIST) WRITE (6, '('' PLOT VELOCITY HISTOGRAM ''/)')
           IF (DEBUG) WRITE (6, '('' PRINT DEBUG INFORMATION ''/)')
           IF (NSTAT > 0) THEN
              IF (PPLOT) WRITE (6, '('' PLOT STATION PRESSURE ''/)')
               IF (DPLOT) WRITE (6, '('' PLOT STATION DENSITY ''/)')
               IF (VPLOT) WRITE (6, '('' PLOT STATION VELOCITY ''/)')
               IF (TPLOT) WRITE (6, '('' PLOT STATION TEMPERATURE ''/)')
               IF (SLIST) WRITE (6, '('' PRINT STATION DATA ''/)')
               WRITE (6, '(///'' STATIONS ARE LOCATED AT CELLS:''/)')
               WRITE (6, '(''       '',I5/)') (STATS(I),I=1,NSTAT)
          ELSE IF (PPLOT .OR. DPLOT .OR. VPLOT .OR. TPLOT) THEN
              WRITE (6,                                                 &
     &'(//'' STATION PLOTS HAVE BEEN REQUESTED, BUT NO'',               &
     &    '' STATIONS HAVE BEEN DEFINED.  ABORT''//)')
               STOP 
          ELSE IF (SLIST) THEN
              WRITE (6,                                                 &
     &'(//'' YOU HAVE REQUESTED A PRINTOUT OF THE STATION'',            &
     &    '' DATA, BUT NO STATIONS HAVE BEEN DEFINED. '',               &
     &    '' ABORT''//)')
               STOP 
          ENDIF
          RETURN 
      ENDIF
      GO TO 10
!
!-------------------------------------------------
!     INTERNAL SUBROUTINE
!-------------------------------------------------
!
      CONTAINS
          RECURSIVE SUBROUTINE QSORT (LIST)
!
!         RECURSIVE QUICK SORT OF THE INTEGER ARRAY LIST
!
!         THIS ROUTINE FROM "PROGRAMMER'S GUIDE TO FORTRAN 90",
!         W. S. BRAINERD, C. H. GOLDBERG, AND J. C. ADAMS, McGRAW-HILL,
!         1990, PAGE 147
!
          IMPLICIT NONE
          INTEGER, DIMENSION(:), INTENT(INOUT) :: LIST
          INTEGER, DIMENSION(SIZE(LIST)) :: SMALLER,LARGER
          INTEGER :: I,NUMBER_SMALLER,NUMBER_EQUAL,NUMBER_LARGER
          REAL :: CHOSEN
!
          IF (SIZE(LIST) > 1) THEN
              CHOSEN = LIST(1)
              NUMBER_SMALLER = 0
              NUMBER_EQUAL = 1
              NUMBER_LARGER = 0
!
              DO I = 2,SIZE(LIST)
                  IF (LIST(I) < CHOSEN) THEN
                      NUMBER_SMALLER = NUMBER_SMALLER + 1
                      SMALLER(NUMBER_SMALLER) = LIST(I)
                  ELSE IF (LIST(I) == CHOSEN) THEN
                      NUMBER_EQUAL = NUMBER_EQUAL + 1
                  ELSE
                      NUMBER_LARGER = NUMBER_LARGER + 1
                      LARGER(NUMBER_LARGER) = LIST(I)
                  END IF
              END DO
!
              CALL QSORT (SMALLER(1:NUMBER_SMALLER))
              LIST(1:NUMBER_SMALLER) = SMALLER(1:NUMBER_SMALLER)
              LIST(NUMBER_SMALLER+1:NUMBER_SMALLER+NUMBER_EQUAL) = CHOSEN
              CALL QSORT (LARGER(1:NUMBER_LARGER))
              LIST(NUMBER_SMALLER+NUMBER_EQUAL+1:) = LARGER(1:NUMBER_LARGER)
          END IF
!
          END SUBROUTINE QSORT
!
      END SUBROUTINE READIN
