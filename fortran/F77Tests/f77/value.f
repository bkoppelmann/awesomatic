      SUBROUTINE VALUE (RESULT,ITYPE)
C
C        FREE-FORMAT NUMERIC/ALPHANUMERIC INPUT.  WHEN THIS ROUTINE IS
C        CALLED, IT WILL CALL SUBROUTINE NEXT TO FETCH THE NEXT
C        VALID FIELD OFF UNIT 5.  THIS FIELD WILL THEN BE PASSED
C        TO VALUE IN THE COMMON BLOCK /CIMAGE/ AS AN INTERNAL FILE.
C        VALUE THEN ATTEMPTS A READ ON THE FILE. IF THE FIELD WAS
C        INTEGER, THEN VALUE RETURNS A FLOATED RESULT.  IN THE
C        EVENT NO FURTHER DATA IS AVAILABLE, THE LOGICAL VARIABLE
C        EOFF IN COMMON /IMAGE/ WILL BE SET TRUE.  IN ALL OTHER CASES
C        IT WILL BE FALSE.  ALSO, ITYPE=-1 WILL INDICATE AN END OF
C        FILE WAS ENCOUNTERED.  FINALLY, THE FIELD BEING PROCESSED IS
C        UNCHANGED BY VALUE AND CAN BE USED BY OTHER ROUTINES SINCE
C        IT IS PASSED IN COMMON.
C
C        OUTPUT:
C
C             RESULT    REAL
C                       NUMERICAL (OR LOGICAL) VALUE OF THE FIELD.
C                       IF THE FIELD WAS AN INTEGER, RESULT IS THE
C                       FLOATED VALUE.  IF THE FIELD WAS ALPHA-
C                       NUMERIC, THIS VARIABLE IS NOT SET.
C
C             ITYPE     INTEGER
C                       IDENTIFIER SHOWING TYPE OF FIELD READ.
C                       -1 = END OF FILE ENCOUNTERED, NO DATA READ.
C                        0 = ALPHANUMERIC DATA, NO VALUE RETURNED IN
C                            RESULT.
C                        1 = INTEGER OR REAL
C
      IMPLICIT REAL (A-H,O-Z)
      CHARACTER*80 CARD,FIELD
      LOGICAL EOFF
      COMMON /CIMAGE/ CARD,FIELD
      COMMON /IMAGE/ EOFF,ICPNT
C                                      *******************************
C                                      GET NEXT FIELD OFF UNIT 5
C                                      *******************************
      CALL NEXT
      IF(EOFF) THEN
          ITYPE=-1
          RETURN
      ENDIF
C                                      *******************************
C                                      READ FIELD AS A NUMERIC
C                                      *******************************
      READ(FIELD,FMT='(BN,F20.0)',ERR=10) RESULT
      ITYPE=1
      RETURN
C                                      *******************************
C                                      THE ONLY POSSIBILITY LEFT IS
C                                      THAT THIS WAS AN ALPHANUMERIC
C                                      *******************************
   10 ITYPE=0
      RETURN
      END
