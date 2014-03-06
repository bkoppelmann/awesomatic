      SUBROUTINE KEEL(NODES, DX, RADIUS, RBOUND, VEL, DENS, IENER,      &
     &    SCREEN, NS, NSNODE, SAREA)
!
!        CALCULATE MESH INITIAL CONDITIONS
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
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NODES, NS
      LOGICAL SCREEN
      INTEGER, DIMENSION(IMAXP1) :: NSNODE
      REAL, DIMENSION(NODES+1) :: DX, RADIUS
      REAL, DIMENSION(0:IMAXP1) :: RBOUND
      REAL, DIMENSION(NODES+1) :: VEL, DENS, IENER
      REAL, DIMENSION(IMAXP1) :: SAREA
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: IMAX = 500
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NP1, N, NM1, NST, NN
      REAL, DIMENSION(IMAXP1) :: NAREA, SRAD
      REAL :: DXX, RLEFT, VSET, ESET, DSET, TRANS
      INCLUDE 'area.int'
!
!-----------------------------------------------
!
      NS = 0
!
!        CALCULATE SCREEN INFORMATION IF NECESSARY
!
      IF (SCREEN) CALL SGEOM (NS, SRAD, SAREA, NAREA)
!
!        CONSTRUCT A UNIFORM GRID
!
      NP1 = NODES + 1
      DXX = RRIGHT/REAL(NODES - 1)
!
!        CALCULATE RADIUS OF CELL CENTERS
!
      RBOUND(0) = -DXX
      DO N = 1, NP1
          RBOUND(N) = RBOUND(N-1) + DXX
      END DO
      RADIUS(:NP1) = RBOUND(:NP1-1) + DXX/2.0
!
!        CELL WIDTHS
!
      NM1 = NODES - 1
      NST = 1
      IF (SCREEN) THEN
          DO N = 1, NP1
              DX(N) = RBOUND(N) - RBOUND(N-1)
!
!        IF SCREENS PRESENT, SEE IF ONE FITS IN THIS CELL
!
   15         CONTINUE
              IF (NST <= NS) THEN
                  RLEFT = RBOUND(N-1)
                  RRIGHT = RBOUND(N)
                  IF (RLEFT<=SRAD(NST) .AND. RRIGHT>SRAD(NST)) THEN
                      NSNODE(NST) = N
                      NST = NST + 1
                      GO TO 15
                  ENDIF
              ENDIF
          END DO
      ELSE
          DX(:NP1) = RBOUND(1:NP1) - RBOUND(:NP1-1)
!
!        IF SCREENS PRESENT, SEE IF ONE FITS IN THIS CELL
!
      ENDIF
!
!        MAKE SURE ALL SCREENS HAVE BEEN ACCOUNTED FOR
!
      IF (SCREEN) THEN
          IF (NST <= NS) THEN
              NST = NST - 1
              IF (NST <= 0) THEN
                  WRITE (6, '(//'' NO SCREENS WERE ACCOUNTED FOR'')')
              ELSE
                   WRITE (6,                                            &
     &'(//'' NOT ALL SCREENS WERE ACCOUNTED FOR''/                      &
     &    '' SCREENS HAVE BEEN LOCATED AT:''/)')
                   WRITE (6,                                            &
     &'('' N='',I5,'' RADIUS='',1PE12.5,                                &
     &  '' LOCATED IN NODE ''                                           &
     & ,I5)') (NN,SRAD(NN),NSNODE(NN),NN=1,NST)
              ENDIF
              NST = NST + 1
              WRITE (6, '(//'' UNACCOUNTED FOR SCREENS ARE AT:''/)')
               WRITE (6, '('' RADIUS='',1PE12.5)') (SRAD(NN),NN=NST,NS)
              STOP 
          ENDIF
      ENDIF
!
!        CELL BOUNDARY AREAS
!
      NST = 1
      DO N = 0, NP1
          BAREA(N) = AREA(RBOUND(N))
          IF (N == NP1) BAREA(N) = BAREA(N-1)
      END DO
!
!        CELL CENTER AREAS
!
      DO N = 1, NP1
          CAREA(N) = AREA(RADIUS(N))
          IF (N == NP1) CAREA(N) = CAREA(N-1)
      END DO
!
!        MODIFY CELL BOUNDARY AREAS FOR SCREENS IF NECESSARY
!
      IF (SCREEN) THEN
          DO N = 1, NS
              NN = NSNODE(N)
              SAREA(N) = SAREA(N)*BAREA(NN)
              BAREA(NN) = BAREA(NN)*NAREA(N)
          END DO
      ENDIF
!
!        CELL VOLUMES.  ASSUME THE CELL IS A FRUSTRUM OF A RIGHT
!        CIRCULAR CONE
!
      VOL(:NP1) = DX(:NP1)/3.0*(BAREA(:NP1-1)+SQRT(BAREA(:NP1-1)*BAREA(1&
     &    :NP1))+BAREA(1:NP1))
!
!        FILL MESH WITH AMBIENT CONDITIONS OUTSIDE CHAMBER.
!        THESE ARE 70 MICRONS OF MERCURY PRESSURE AND A TEMPERATURE
!        OF 300 DEG KELVIN.  WE ASSUME AN IDEAL GAS.  THE MOLECULAR
!        WEIGHT OF STEAM IS ASSUMED TO BE 18.016 G/MOLE.  THE GAS
!        CONSTANT IS 8.314 J/DEG K/MOLE.
!
!
!        SET CHAMBER INTERNAL ENERGY TO AMBIENT
!
!        I=2.765E5 FOR GAMMA 1.4
!        I=4.1477E5 FOR STEAM
!
      VSET = 0.0
      ESET = 2.765E5
      DSET = 9.629E-7
      VEL(2:NODES) = VSET
      DENS(2:NODES) = DSET
      IENER(2:NODES) = ESET
!
!        PRINT OUT MESH GEOMETRY
!
      WRITE (6, '(///'' MESH GEOMETRY:''//)')
       WRITE (6,                                                        &
     &'('' CELL='',I5,'' R='',E12.5,'' DR='',                           &
     &  E12.5,'' VOL='',                                                &
     &  E12.5,'' LEFT AREA='',E12.5,                                    &
     &  '' RIGHT AREA='',E12.5)') (N,RADIUS(N),DX(N),VOL(N),BAREA(N-1), &
     &    BAREA(N),N=1,NODES)
      IF (SCREEN) THEN
          WRITE (6, '(///''1SCREEN LOCATIONS:''//)')
           DO N = 1, NS
              TRANS = 100.0*NAREA(N)
              NN = NSNODE(N)
              WRITE (6,                                                 &
     &'('' CELL='',I5,'' SCREEN RADIUS='',1PE12.5,                      &
     &  '' CELL RADIUS='',E12.5,'' SCREEN AREA='',                      &
     &   E12.5,'' PERCENT TRANSMISSION='',E12.5)') NN, SRAD(N), RADIUS( &
     &            NN), SAREA(N), TRANS
          END DO
      ENDIF
!
!-----------------------------------------------------
!       INTERNAL SUBROUTINE
!-----------------------------------------------------
!
      CONTAINS
          SUBROUTINE SGEOM (NS, RADIUS, AREA, TRANS)
!
!        CALCULATE SCREEN LOCATIONS AND AREAS
!
!        OUTPUT
!
!        NS          INTEGER        NUMBER OF SCREENS
!        RADIUS      REAL A.        LOCATIONS OF SCREENS
!        AREA        REAL A.        FRACTION OF ORIGINAL NOZZLE AREA
!                                   TAKEN UP BY SCREENS
!        TRANS       REAL A.        FRACTION OF ORIGINAL NOZZLE AREA
!                                   STILL OPEN FOR FLOW (TRANSMISSION
!  !...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
          IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          INTEGER NS
          REAL, DIMENSION(NODES) :: RADIUS, AREA, TRANS
!-----------------------------------------------
          NS = 5
          RADIUS(1) = 0.434108
          RADIUS(2) = 0.45315
          RADIUS(3) = 0.523
          RADIUS(4) = 0.542
          RADIUS(5) = 0.548
          AREA(1) = 0.10
          AREA(2) = 0.10
          AREA(3) = 0.1412
          AREA(4) = 0.1412
          AREA(5) = 0.1412
          TRANS(:5) = 1.0 - AREA(:5)
          END SUBROUTINE SGEOM
!
      END SUBROUTINE KEEL
