      SUBROUTINE TERROR (WANTED,FIELD,GOT)
C
C        PRINT OUT AN ERROR MESSAGE
C
      INTEGER WANTED,GOT
      CHARACTER*80 FIELD
C
      IF(WANTED.EQ.1) THEN
          IF(GOT.EQ.0) THEN
              WRITE(6,'('' SCANNING INPUT FOR A NUMERIC FIELD.''/
     *                  '' INSTEAD THE ALPHANUMERIC STRING ---> '',
     *                  A,'' <--- WAS FOUND.''/'' ABORT''//)') FIELD
          ELSE
              WRITE(6,'('' SCANNING INPUT FOR A NUMERIC FIELD.''/
     *                  '' INSTEAD THE END OF FILE WAS HIT.''/
     *                  '' YOU ARE MISSING A NUMERICAL VALUE AT THE'',
     *                  '' END OF YOUR LAST INPUT STRING.  ABORT''//)')
          ENDIF
      ELSE
          WRITE(6,'('' UNKNOWN PROBLEM WITH INPUT, ABORT''//)')
      ENDIF
      END
