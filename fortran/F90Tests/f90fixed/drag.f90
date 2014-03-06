      SUBROUTINE DRAG(AREA, SIGMA, TEMP, PRES, DENS, VEL, REY)
!
!        CALCULATE PRESSURE DROP DUE TO DRAG ACROSS SCREENS
!
!        INPUT:
!
!        AREA     REAL       AREA OF SCREEN
!        SIGMA    REAL       CONSTANT EQUAL TO 3*SQRT((PI*M)/(8*K))
!                              WHERE M=MOLECULE MASS, K=BOLTZMANN CONST
!        PRES     REAL       FLOW PRESSURE
!        DENS     REAL       FLOW DENSITY
!        TEMP     REAL       FLOW TEMPERATURE
!        VEL      REAL       FLOW VELOCITY
!
!        OUTPUT:
!
!        PRES     REAL       ADJUSTED FLOW PRESSURE
!        REY      REAL       REYNOLD'S NUMBER
!
!        WE ASSUME DRAG IS 0.5*CD*AREA*VEL**2
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL :: AREA, SIGMA, TEMP, PRES, DENS, VEL, REY
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL :: COEFF
!-----------------------------------------------
!
!        CALCULATE REYNOLD'S NUMBER
!
      REY = SIGMA*VEL/(DENS*SQRT(TEMP))
!
!        GET DRAG COEFFICIENT
!
      COEFF = CD(REY)
!
!        CALCULATE DRAG
!
      PRES = PRES + 0.5*COEFF*DENS*AREA*VEL*VEL
!
!-------------------------------------------------
!   I N T E R N A L  F U N C T I O N S
!-------------------------------------------------
!
      CONTAINS
          REAL FUNCTION CD (REY)
!
!        CALCULATE DRAG COEFFICIENT AS A FUNCTION OF REYNOLDS
!        NUMBER FOR A RIGHT CIRCULAR CYLINDER (REF:  PAGE 190
!        OF AIP 50TH ANNIVERSARY PHYSICS VADE MECUM
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
          IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          REAL REY
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
          INTEGER, PARAMETER :: NDATA = 11
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          INTEGER :: IOLD, N, NN, I
          REAL, DIMENSION(NDATA) :: R, DRAG
          REAL :: X1, Y1, X2, Y2, SLOPE, B
          LOGICAL :: FIRST = .TRUE.
          SAVE FIRST,IOLD,R,DRAG
!-----------------------------------------------
          DATA R/0.1,1.0,10.0,100.0,1.E3,2.E3,1.E4,1.E5,2.E5,5.E5,1.E6/
          DATA DRAG/60.0,10.0,3.0,1.8,1.0,1.0,1.2,1.2,1.2,0.3,0.38/
          IF (FIRST) THEN
              FIRST = .FALSE.
              IOLD = NDATA - 1
          ENDIF
!
!        INTERPOLATE ON REYNOLD'S NUMBER
!
          IF (REY >= R(NDATA)) THEN
              N = NDATA - 1
          ELSE IF (REY < R(1)) THEN
              N = 1
          ELSE
              NN = NDATA - 1
              IF (REY < R(IOLD+1)) NN = IOLD
              DO I = NN, 1, -1
                  N = I
                  IF (REY>=R(I) .AND. REY<R(I+1)) GO TO 20
              END DO
          ENDIF
   20     CONTINUE
          X1 = R(N)
          Y1 = DRAG(N)
          X2 = R(N+1)
          Y2 = DRAG(N+1)
          IOLD = N
          SLOPE = (Y2 - Y1)/(X2 - X1)
          B = Y2 - SLOPE*X2
          CD = REY*SLOPE + B
          END FUNCTION CD
!
      END SUBROUTINE DRAG
