      SUBROUTINE QSORT (X,N)
C
C        QUICKSORT AN INTEGER ARRAY X (DIMENSIONED IN CALLING ROUTINE)
C        OF LENGTH N IN ORDER OF INCREASING X
C
C        JOHN K. PRENTICE    15 DECEMBER 1982
C
C        REFERENCE:  "ADVANCED PROGRAMMING TECHNIQUES",
C        C.E. HUGHES, C.P.PFLEEGER, AND L.L. ROSE,
C        JOHN WILEY AND SONS, 1978, PAGES 75-78
C
      IMPLICIT REAL (A-H,O-Z)
      INTEGER X(*),OLD
      INTEGER LTS(2000,2),LEND,REND,LPT,RPT
C
C        INITIALIZE LIST TO SORT STACK WITH ENTIRE ARRAY
C
      LTS(1,1)=1
      LTS(1,2)=N
      ILIST=1
C
C        MAIN SORT LOOP
C
   10 IF(ILIST.GT.0) THEN
C
C        GET LAST PAIR OFF LTS STACK
C
          LEND=LTS(1,1)
          REND=LTS(1,2)
          ILIST=ILIST-1
C
C        PUSH STACK DOWN
C
          IF(ILIST.GT.0) THEN
              DO 5 I=1,ILIST
                  DO 5 J=1,2
                      LTS(I,J)=LTS(I+1,J)
    5         CONTINUE
          ENDIF
          LPT=LEND
          RPT=REND
C
   15     IF(LPT.LT.RPT) THEN
C
C        MOVE LEFT POINTER AS FAR AS POSSIBLE
C
   20         IF(X(LPT).LE.X(RPT)) THEN
                  LPT=LPT+1
                  IF(LPT.GE.RPT) GO TO 40
                  GO TO 20
              ENDIF
C
C        WE HAVE FOUND AN ELEMENT OUT OF ORDER.  MOVE
C        THE LEFT ELEMENT TO THE RIGHT SIDE
C
              OLD=X(LPT)
              X(LPT)=X(RPT)
              X(RPT)=OLD
C
C        NOW MOVE RIGHT POINTER AS FAR AS POSSIBLE
C
   30         IF(X(LPT).LE.X(RPT)) THEN
                  RPT=RPT-1
                  IF(LPT.GE.RPT) GO TO 40
                  GO TO 30
              ENDIF
C
C        RPT ELEMENT BELONGS ON OTHER SIDE, EXCHANGE
C
              OLD=X(LPT)
              X(LPT)=X(RPT)
              X(RPT)=OLD
          ENDIF
          GO TO 15
C
C        RPT IS DIVIDING POINT.  SPLIT LISTS INTO TWO, AND
C        SORT EACH INDEPENDENTLY.  CHECK TO SEE THAN THE LIST
C        IS NOT EMPTY
C
   40     IF(RPT-1.GT.LEND) THEN
              ILIST=ILIST+1
              IF (ILIST.GT.2000) THEN
C
C        STACK OVERFLOW
C
                  WRITE(6,100)
  100      FORMAT(//' STACK OVERFLOW IN QSORT, ABORT'//)
                  STOP
              ENDIF
C
C        PUSH STACK UP
C
              IF (ILIST.GT.1) THEN
                  DO 50 I=ILIST,2,-1
                      DO 50 J=1,2
                          LTS(I,J)=LTS(I-1,J)
   50             CONTINUE
              ENDIF
              LTS(1,1)=LEND
              LTS(1,2)=RPT-1
          ENDIF
C
          IF(RPT+1.LT.REND) THEN
              ILIST=ILIST+1
              IF (ILIST.GT.2000) THEN
                  WRITE(6,100)
                  STOP
              ENDIF
C
C        PUSH STACK UP
C
              IF(ILIST.GT.1) THEN
                  DO 60 I=ILIST,2,-1
                      DO 60 J=1,2
                          LTS(I,J)=LTS(I-1,J)
   60             CONTINUE
              ENDIF
              LTS(1,1)=RPT+1
              LTS(1,2)=REND
          ENDIF
          GO TO 10
      ELSE
          RETURN
      ENDIF
      END
