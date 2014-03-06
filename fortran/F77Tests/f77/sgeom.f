      SUBROUTINE SGEOM (NS,RADIUS,AREA,TRANS)
C
C        CALCULATE SCREEN LOCATIONS AND AREAS
C
C        OUTPUT
C
C        NS          INTEGER        NUMBER OF SCREENS
C        RADIUS      REAL A.        LOCATIONS OF SCREENS
C        AREA        REAL A.        FRACTION OF ORIGINAL NOZZLE AREA
C                                   TAKEN UP BY SCREENS
C        TRANS       REAL A.        FRACTION OF ORIGINAL NOZZLE AREA
C                                   STILL OPEN FOR FLOW (TRANSMISSION
C                                   FRACTION)
C
      IMPLICIT REAL (A-H,O-Z)
      DIMENSION RADIUS(*),AREA(*),TRANS(*)
      NS=5
      RADIUS(1)=0.434108
      RADIUS(2)=0.45315
      RADIUS(3)=0.523
      RADIUS(4)=0.542
      RADIUS(5)=0.548
      AREA(1)=0.10
      AREA(2)=0.10
      AREA(3)=0.1412
      AREA(4)=0.1412
      AREA(5)=0.1412
      DO 10 I=1,5
          TRANS(I)=1.0-AREA(I)
   10 CONTINUE
      END
