      REAL FUNCTION AREA (RADIUS)
C
C        GIVEN A POSITION (RADIUS) IN THE CHAMBER, CALCULATE THE
C        CROSS SECTIONAL AREA AT THAT POINT
C
      IMPLICIT REAL (A-H,O-Z)
      INTEGER POINTS
      PARAMETER (IMAX=500,IMAXP1=501)
      PARAMETER (POINTS=38,PI=3.14159)
      DIMENSION X(0:POINTS),Y(0:POINTS)
      COMMON /COORD/ RRIGHT,BAREA(0:IMAXP1),CAREA(IMAXP1),VOL(IMAXP1)
C
      DATA X /   0.1524,  0.1651,  0.1778,
     *           0.1905,  0.2032,  0.2159,  0.2286,  0.2413,
     *           0.2540,  0.2667,  0.2794,  0.2921,  0.3048,
     *           0.3175,  0.3302,  0.3429,  0.3556,  0.3683,
     *           0.3810,  0.3937,  0.4064,  0.4191,  0.4318,
     *           0.4445,  0.4572,  0.4699,  0.4826,  0.4953,
     *           0.5080,  0.5207,  2.4084, 13.4988, 40.4863,
     *          43.4108, 45.3158, 52.3008, 54.2058, 54.8408,
     *          78.5809/
C
      DATA Y /   0.0206,  0.0208,  0.0211,
     *           0.0216,  0.0222,  0.0231,  0.0242,  0.0255,
     *           0.0271,  0.0289,  0.0310,  0.0335,  0.0363,
     *           0.0249,  0.0552,  0.0893,  0.1237,  0.1564,
     *           0.1868,  0.2149,  0.2410,  0.2657,  0.2897,
     *           0.3136,  0.3380,  0.3633,  0.3896,  0.4169,
     *           0.4448,  0.4725,  0.4980,  5.2099, 40.0101,
     *          40.0101, 41.8825, 41.8825, 41.8825, 41.8825,
     *          41.8825/
C
      DATA RRIGHT /78.5809E-2/
C
C        FIND TABLE ENTRY AND LINEARLY INTERPOLATE FOR CORRECT AREA.
C        IF RADIUS IS LESS THAN BEGINNING OF DATA OR LARGER THAN END
C        OF DATA, MAKE THE AREA THE SAME AS THE NEAREST DATA POINT
C
      IF(RADIUS.LE.X(0)/100.0) THEN
          AREA=Y(0)*1.E-4
          RETURN
      ENDIF
      DO 10 N=0,POINTS-1
          I=N
          IF(RADIUS.GE.X(N)/100.0.AND.RADIUS.LE.X(N+1)/100.0) GO TO 20
   10 CONTINUE
   20 X1=X(I)/100.0
      X2=X(I+1)/100.0
      IF(X1.EQ.X2) THEN
          I=I+1
          IF(I.GT.POINTS) THEN
              AREA=Y(POINTS)*1.E-4
              RETURN
          ENDIF
          GO TO 20
      ENDIF
      Y1=Y(I)*1.E-4
      Y2=Y(I+1)*1.E-4
      SLOPE=(Y2-Y1)/(X2-X1)
      B=Y2-SLOPE*X2
      AREA=RADIUS*SLOPE+B
      END
