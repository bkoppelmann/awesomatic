      SUBROUTINE TERROR(WANTED, FIELD, GOT)
!
!        PRINT OUT AN ERROR MESSAGE
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER WANTED, GOT
      CHARACTER FIELD*80
!-----------------------------------------------
!
      IF (WANTED == 1) THEN
          IF (GOT == 0) THEN
              WRITE (6,                                                 &
     &'('' SCANNING INPUT FOR A NUMERIC FIELD.''/                       &
     &  '' INSTEAD THE ALPHANUMERIC STRING ---> '',                     &
     &  A,'' <--- WAS FOUND.''/'' ABORT''//)') FIELD
          ELSE
              WRITE (6,                                                 &
     &'('' SCANNING INPUT FOR A NUMERIC FIELD.''/                       &
     &  '' INSTEAD THE END OF FILE WAS HIT.''/                          &
     &  '' YOU ARE MISSING A NUMERICAL VALUE AT THE'',                  &
     &  '' END OF YOUR LAST INPUT STRING.  ABORT''//)')
          ENDIF
      ELSE
           WRITE (6, '('' UNKNOWN PROBLEM WITH INPUT, ABORT''//)')
      ENDIF
       END SUBROUTINE TERROR
