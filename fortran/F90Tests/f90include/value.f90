      SUBROUTINE VALUE(RESULT, ITYPE)
!
!        FREE-FORMAT NUMERIC/ALPHANUMERIC INPUT.  WHEN THIS ROUTINE IS
!        CALLED, IT WILL CALL SUBROUTINE NEXT TO FETCH THE NEXT
!        VALID FIELD OFF UNIT 5.  THIS FIELD WILL THEN BE PASSED
!        TO VALUE IN THE COMMON BLOCK /CIMAGE/ AS AN INTERNAL FILE.
!        VALUE THEN ATTEMPTS A READ ON THE FILE. IF THE FIELD WAS
!        INTEGER, THEN VALUE RETURNS A FLOATED RESULT.  IN THE
!        EVENT NO FURTHER DATA IS AVAILABLE, THE LOGICAL VARIABLE
!        EOFF IN COMMON /IMAGE/ WILL BE SET TRUE.  IN ALL OTHER CASES
!        IT WILL BE FALSE.  ALSO, ITYPE=-1 WILL INDICATE AN END OF
!        FILE WAS ENCOUNTERED.  FINALLY, THE FIELD BEING PROCESSED IS
!        UNCHANGED BY VALUE AND CAN BE USED BY OTHER ROUTINES SINCE
!        IT IS PASSED IN COMMON.
!
!        OUTPUT:
!
!             RESULT    REAL
!                       NUMERICAL (OR LOGICAL) VALUE OF THE FIELD.
!                       IF THE FIELD WAS AN INTEGER, RESULT IS THE
!                       FLOATED VALUE.  IF THE FIELD WAS ALPHA-
!                       NUMERIC, THIS VARIABLE IS NOT SET.
!
!             ITYPE     INTEGER
!                       IDENTIFIER SHOWING TYPE OF FIELD READ.
!                       -1 = END OF FILE ENCOUNTERED, NO DATA READ.
!                        0 = ALPHANUMERIC DATA, NO VALUE RETURNED IN
!                            RESULT.
!                        1 = INTEGER OR REAL
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
      INTEGER ITYPE
      REAL RESULT
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INCLUDE 'next.int'
!-----------------------------------------------
!                                      *******************************
!                                      GET NEXT FIELD OFF UNIT 5
!                                      *******************************
      CALL NEXT
      IF (EOFF) THEN
          ITYPE = -1
          RETURN 
      ENDIF
!                                      *******************************
!                                      READ FIELD AS A NUMERIC
!                                      *******************************
      READ (FIELD, FMT='(BN,F20.0)', ERR=10) RESULT
      ITYPE = 1
      RETURN 
!                                      *******************************
!                                      THE ONLY POSSIBILITY LEFT IS
!                                      THAT THIS WAS AN ALPHANUMERIC
!                                      *******************************
   10 CONTINUE
      ITYPE = 0
      RETURN 
      END SUBROUTINE VALUE
