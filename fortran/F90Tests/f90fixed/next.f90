      SUBROUTINE NEXT
!
!        SEQUENTIALLY SCAN 80 COLUMN CARD IMAGES FROM UNIT 5 FOR
!        NON-DELIMETER SUBSTRINGS.  OUTPUT IS PASSED THROUGH
!        COMMONS /CIMAGE/ AND /IMAGE/ AS EXPLAINED BELOW.
!        SCANNING STOPS AT THE END OF THE SUBSTRING OR AT AN END
!        OF FILE.  IF AN END OF FILE IS ENCOUNTERED, THE FILE IS
!        LEFT POSITIONED AFTER THE END OF FILE.
!
!        OUTPUT:
!
!             FIELD     CHARACTER*80
!                       BLANK FILLED STRING CONTAINING THE NEXT
!                       VALID SUBSTRING ENCOUNTERED, LEFT
!                       JUSTIFIED AND BLANK FILLED.  A VALID
!                       SUBSTRING IS ANY SET OF CONTIGUOUS
!                       NON-DELIMETERS.  DELIMETERS ARE DEFINED
!                       IN THE DECLARATIONS AT THE BEGINNING OF THE
!                       ROUTINE.  RETURNED IN COMMON /CIMAGE/.
!
!             EOFF      LOGICAL
!                       TRUE=END OF FILE ENCOUNTERED, NO VALID SUB-
!                            STRING WAS FOUND
!                       FALSE=VALID SUB-STRING FOUND
!                       THIS FLAG IS RETURNED IN COMMON /IMAGE/.
!
!             CARD      CHARACTER*80
!                       STRING CONTAINING CONTENTS OF LAST RECORD
!                       READ.  THE CONTENTS OF THIS STRING SHOULD
!                       NOT BE CHANGED IF SUBSEQUENT CALLS TO NEXT
!                       ARE GOING TO BE MADE.  THIS STRING IS PASSED
!                       IN COMMON /CIMAGE/.
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE VCIMAGE
      USE VIMAGE
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: ICEND = 80
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NODEL, I, ISTART, KMIN, J, K, IEND, ICPNT_INITIAL
      CHARACTER, DIMENSION(3) :: DELIM
      LOGICAL :: FIRST
      SAVE FIRST
!-----------------------------------------------
      DATA NODEL, DELIM/3, ' ', ',', '='/
      DATA ICPNT_INITIAL/81/
      DATA FIRST /.TRUE./
!
      IF (FIRST) THEN
           FIRST = .FALSE.
           ICPNT = ICPNT_INITIAL
      ENDIF
!                                      *******************************
!                                      IF ICPNT>ICEND, READ THE NEXT
!                                      RECORD OFF UNIT 5 INTO THE
!                                      STRING 'CARD'.  NEXT VERIFY
!                                      THAT THIS IS A NON- BLANK
!                                      CARD.  IF IT IS BLANK, SKIP IT
!                                      AND GET THE NEXT RECORD
!                                      *******************************
   10 CONTINUE
      EOFF = .FALSE.
      IF (ICPNT > ICEND) THEN
          READ (5, '(A)', END=50) CARD
          IF (CARD(1:ICEND) == ' ') GO TO 10
          ICPNT = 1
      ENDIF
!                                      *******************************
!                                      GET THE NEXT SUB-STRING.  WE
!                                      DO THIS AS FOLLOWS.  WE LOOK
!                                      FOR THE NEXT DELIMETER.  IF IT
!                                      IS AS THE SAME POSITION AS THE
!                                      CURRENT POINTER, WE ADVANCE
!                                      THE POINTER AND TRY AGAIN.  IF
!                                      NOT, THEN THE POINTER IS AT
!                                      THE BEGINNING OF A SUB-STRING
!                                      AND THE DELIMETER IS TRAILING
!                                      THIS SUB-STRING.  NOTE THAT WE
!                                      LOOK FOR ALL THE DELIMETERS
!                                      POSSIBLE BEFORE TAKING ANY
!                                      ACTION.
!                                      *******************************
      DO I = ICPNT, ICEND
          ISTART = I
          KMIN = 0
          DO J = 1, NODEL
              K = INDEX(CARD(I:ICEND),DELIM(J))
!                                      *******************************
!                                      INDEX RETURNS POSITIONS
!                                      RELATIVE THE BEGINNING OF THE
!                                      SUB-STRING.  HENCE WE ADD IN
!                                      THE APPROPIATE OFF-SET TO GIVE
!                                      THE INDEX RELATIVE TO THE
!                                      BEGINNING OF THE STRING CARD,
!                                      NOT JUST THE SUB-STRING
!                                      CARD(I:ICEND).
!                                      *******************************
              IF (K /= 0) THEN
                  K = K + I - 1
                  IF (KMIN == 0) THEN
                      KMIN = K
                  ELSE
                      KMIN = MIN(K,KMIN)
                  ENDIF
              ENDIF
          END DO
!                                      *******************************
!                                      IF KMIN IS NOT EQUAL TO THE
!                                      CURRENT POINTER POSITION, THEN
!                                      IT MUST BE POINTING AT THE
!                                      TRAILING DELIMETER OF A VALID
!                                      SUB-STRING.
!                                      *******************************
          IF (KMIN == I) THEN
              CYCLE 
          ELSE IF (KMIN > 0) THEN
              IEND = KMIN - 1
              GO TO 40
          ENDIF
!                                      *******************************
!                                      IF WE FALL THROUGH, THERE WAS
!                                      NO DELIMETER FOUND ON THE
!                                      REMAINDER OF THIS RECORD.
!                                      THIS MEANS THE ENTIRE
!                                      REMAINDER OF THIS RECORD IS A
!                                      VALID SUB-STRING
!                                      *******************************
          IEND = ICEND
          GO TO 40
      END DO
!                                      *******************************
!                                      IF WE FALL THROUGH THIS LOOP,
!                                      THERE WERE NO MORE NON-
!                                      DELIMETERS ON THIS RECORD.  GO
!                                      GET NEXT RECORD
!                                      *******************************
      ICPNT = ICEND + 1
      GO TO 10
!                                      *******************************
!                                      PUT THE SUB-STRING INTO THE
!                                      STRING 'FIELD'.  NOTE THAT
!                                      FORTRAN 77 PADS THE STRING
!                                      WITH BLANKS
!                                      *******************************
   40 CONTINUE
      FIELD = CARD(ISTART:IEND)
      ICPNT = IEND + 2
      RETURN 
!                                      *******************************
!                                      END OF FILE ENCOUNTERED, SET
!                                      FLAG AND RETURN
!                                      *******************************
   50 CONTINUE
      EOFF = .TRUE.
      ICPNT = ICEND + 1
      RETURN 
      END SUBROUTINE NEXT
