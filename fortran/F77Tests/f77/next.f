      SUBROUTINE NEXT
C
C        SEQUENTIALLY SCAN 80 COLUMN CARD IMAGES FROM UNIT 5 FOR
C        NON-DELIMETER SUBSTRINGS.  OUTPUT IS PASSED THROUGH
C        COMMONS /CIMAGE/ AND /IMAGE/ AS EXPLAINED BELOW.
C        SCANNING STOPS AT THE END OF THE SUBSTRING OR AT AN END
C        OF FILE.  IF AN END OF FILE IS ENCOUNTERED, THE FILE IS
C        LEFT POSITIONED AFTER THE END OF FILE.
C
C        OUTPUT:
C
C             FIELD     CHARACTER*80
C                       BLANK FILLED STRING CONTAINING THE NEXT
C                       VALID SUBSTRING ENCOUNTERED, LEFT
C                       JUSTIFIED AND BLANK FILLED.  A VALID
C                       SUBSTRING IS ANY SET OF CONTIGUOUS
C                       NON-DELIMETERS.  DELIMETERS ARE DEFINED
C                       IN THE DECLARATIONS AT THE BEGINNING OF THE
C                       ROUTINE.  RETURNED IN COMMON /CIMAGE/.
C
C             EOFF      LOGICAL
C                       TRUE=END OF FILE ENCOUNTERED, NO VALID SUB-
C                            STRING WAS FOUND
C                       FALSE=VALID SUB-STRING FOUND
C                       THIS FLAG IS RETURNED IN COMMON /IMAGE/.
C
C             CARD      CHARACTER*80
C                       STRING CONTAINING CONTENTS OF LAST RECORD
C                       READ.  THE CONTENTS OF THIS STRING SHOULD
C                       NOT BE CHANGED IF SUBSEQUENT CALLS TO NEXT
C                       ARE GOING TO BE MADE.  THIS STRING IS PASSED
C                       IN COMMON /CIMAGE/.
C
      CHARACTER CARD*80,FIELD*80,DELIM(3)*1
      LOGICAL EOFF
      COMMON /CIMAGE/ CARD,FIELD
      COMMON /IMAGE/ EOFF,ICPNT
C
C        ICEND=LENGTH OF EACH RECORD (80=STANDARD 80 COLUMN INPUT)
C              NOTE THAT THIS CAN BE CHANGED, BUT IT SHOULD AGREE
C              WITH THE LENGTH OF 'FIELD'.
C        NODEL=NUMBER OF DELIMETERS
C        DELIM(I)=CHARACTER*1 DELIMETERS
C        ICPNT=POINTER TO CURRENT CIMAGED POSITION ON RECORD.  WE
C              PRESET THIS TO 1 SINCE IT WILL BE ASSUMED SEARCH HAS
C              BEEN CALLED TO PREPOSITION THE FILE AND THE FIRST
C              RECORD IS NOW IN STRING CARD.  IF THIS WERE NOT THE
C              CASE, ICPNT SHOULD BE PRESET LARGER THAN ICEND TO
C              FORCE A READ ON THE FIRST CALL.
C
      PARAMETER (ICEND=80)
      DATA NODEL,DELIM /3,' ',',','='/
      DATA ICPNT /81/
C                                      *******************************
C                                      IF ICPNT>ICEND, READ THE NEXT
C                                      RECORD OFF UNIT 5 INTO THE
C                                      STRING 'CARD'.  NEXT VERIFY
C                                      THAT THIS IS A NON- BLANK
C                                      CARD.  IF IT IS BLANK, SKIP IT
C                                      AND GET THE NEXT RECORD
C                                      *******************************
   10 EOFF=.FALSE.
      IF(ICPNT.GT.ICEND) THEN
          READ(5,'(A)',END=50) CARD
          IF(CARD(1:ICEND).EQ.' ')GO TO 10
          ICPNT=1
      ENDIF
C                                      *******************************
C                                      GET THE NEXT SUB-STRING.  WE
C                                      DO THIS AS FOLLOWS.  WE LOOK
C                                      FOR THE NEXT DELIMETER.  IF IT
C                                      IS AS THE SAME POSITION AS THE
C                                      CURRENT POINTER, WE ADVANCE
C                                      THE POINTER AND TRY AGAIN.  IF
C                                      NOT, THEN THE POINTER IS AT
C                                      THE BEGINNING OF A SUB-STRING
C                                      AND THE DELIMETER IS TRAILING
C                                      THIS SUB-STRING.  NOTE THAT WE
C                                      LOOK FOR ALL THE DELIMETERS
C                                      POSSIBLE BEFORE TAKING ANY
C                                      ACTION.
C                                      *******************************
      DO 30 I=ICPNT,ICEND
          ISTART=I
          KMIN=0
          DO 20 J=1,NODEL
              K=INDEX(CARD(I:ICEND),DELIM(J))
C                                      *******************************
C                                      INDEX RETURNS POSITIONS
C                                      RELATIVE THE BEGINNING OF THE
C                                      SUB-STRING.  HENCE WE ADD IN
C                                      THE APPROPIATE OFF-SET TO GIVE
C                                      THE INDEX RELATIVE TO THE
C                                      BEGINNING OF THE STRING CARD,
C                                      NOT JUST THE SUB-STRING
C                                      CARD(I:ICEND).
C                                      *******************************
              IF(K.NE.0) THEN
                  K=K+I-1
                  IF(KMIN.EQ.0) THEN
                      KMIN=K
                  ELSE
                      KMIN=MIN(K,KMIN)
                  ENDIF
              ENDIF
   20     CONTINUE
C                                      *******************************
C                                      IF KMIN IS NOT EQUAL TO THE
C                                      CURRENT POINTER POSITION, THEN
C                                      IT MUST BE POINTING AT THE
C                                      TRAILING DELIMETER OF A VALID
C                                      SUB-STRING.
C                                      *******************************
          IF(KMIN.EQ.I) THEN
              GO TO 30
          ELSEIF(KMIN.GT.0) THEN
              IEND=KMIN-1
              GO TO 40
          ENDIF
C                                      *******************************
C                                      IF WE FALL THROUGH, THERE WAS
C                                      NO DELIMETER FOUND ON THE
C                                      REMAINDER OF THIS RECORD.
C                                      THIS MEANS THE ENTIRE
C                                      REMAINDER OF THIS RECORD IS A
C                                      VALID SUB-STRING
C                                      *******************************
          IEND=ICEND
          GO TO 40
   30 CONTINUE
C                                      *******************************
C                                      IF WE FALL THROUGH THIS LOOP,
C                                      THERE WERE NO MORE NON-
C                                      DELIMETERS ON THIS RECORD.  GO
C                                      GET NEXT RECORD
C                                      *******************************
      ICPNT=ICEND+1
      GO TO 10
C                                      *******************************
C                                      PUT THE SUB-STRING INTO THE
C                                      STRING 'FIELD'.  NOTE THAT
C                                      FORTRAN 77 PADS THE STRING
C                                      WITH BLANKS
C                                      *******************************
   40 FIELD=CARD(ISTART:IEND)
      ICPNT=IEND+2
      RETURN
C                                      *******************************
C                                      END OF FILE ENCOUNTERED, SET
C                                      FLAG AND RETURN
C                                      *******************************
   50 EOFF=.TRUE.
      ICPNT=ICEND+1
      RETURN
      END
