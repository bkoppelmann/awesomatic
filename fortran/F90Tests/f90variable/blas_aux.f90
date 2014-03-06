MODULE BLAS_AUX

INTERFACE

SUBROUTINE XERBLA_F90(ICOUNT, SRNAME, INFO1, INFO2, INFO3, INFO4 )
!   USE                            F90_KIND
   CHARACTER*6, INTENT(IN)     :: SRNAME
   INTEGER, PARAMETER :: INT32 = selected_int_kind(8)
   INTEGER (KIND=INT32), INTENT(IN)                   :: INFO1
   INTEGER (KIND=INT32), OPTIONAL, INTENT(IN)         :: INFO2
   INTEGER (KIND=INT32), OPTIONAL, INTENT(IN)         :: INFO3
   INTEGER (KIND=INT32), OPTIONAL, INTENT(IN)         :: INFO4
END SUBROUTINE XERBLA_F90

LOGICAL FUNCTION LSAME_F90( CA, CB )
   CHARACTER, INTENT(IN)       :: CA, CB
END FUNCTION LSAME_F90

END INTERFACE

END MODULE BLAS_AUX

SUBROUTINE XERBLA_F90(ICOUNT, SRNAME, INFO1, INFO2, INFO3, INFO4 )
!
!   F90 LAPACK routine loosely based on:
!
!  -- LAPACK auxiliary routine (preliminary version) --
!     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
!     Courant Institute, NAG Ltd., and Rice University
!     March 26, 1990
!
!   Steve Moulton, 11/9/91
!
!   Arguments:
!   USE                            F90_KIND
!   USE                            BLAS_AUX
   CHARACTER*6, INTENT(IN)     :: SRNAME
   INTEGER, PARAMETER :: INT32 = selected_int_kind(8)
   INTEGER (KIND=INT32), INTENT(IN)                   :: INFO1
   INTEGER (KIND=INT32), OPTIONAL, INTENT(IN)         :: INFO2
   INTEGER (KIND=INT32), OPTIONAL, INTENT(IN)         :: INFO3
   INTEGER (KIND=INT32), OPTIONAL, INTENT(IN)         :: INFO4
!     ..
!
!  Purpose
!  =======
!
!  XERBLA  is an error handler for the LAPACK routines.
!  It is called by an LAPACK routine if an input parameter has an
!  invalid value.  A message is printed and execution stops.
!
!  Installers may consider modifying the STOP statement in order to
!  call system-specific exception-handling facilities.
!
!  Arguments
!  =========
!
!  ICOUNT  (input) 
!          Number of error values (up to four)
!  SRNAME  (input) CHARACTER*6
!          The name of the routine which called XERBLA.
!
!  INFO1    (input) INTEGER
!          The position of the invalid parameter in the parameter list
!          of the calling routine.
!
!  INFO2    (input) INTEGER (optional)
!          The position of the invalid parameter in the parameter list
!          of the calling routine.
!
!  INFO3    (input) INTEGER (optional)
!          The position of the invalid parameter in the parameter list
!          of the calling routine.
!
!  INFO4    (input) INTEGER (optional)
!          The position of the invalid parameter in the parameter list
!          of the calling routine.
!
!
      WRITE( *, FMT = 9999 )SRNAME, INFO1
      IF (PRESENT(INFO2)) WRITE( *, FMT = 9999 )SRNAME, INFO2
      IF (PRESENT(INFO3)) WRITE( *, FMT = 9999 )SRNAME, INFO3
      IF (PRESENT(INFO4)) WRITE( *, FMT = 9999 )SRNAME, INFO4
!
      STOP
!
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ', &
           'an illegal value (from XERBLA_F90)' )
!
!     End of XERBLA_F90
!
      END


      LOGICAL          FUNCTION LSAME_F90(CA, CB )
!
!  -- LAPACK auxiliary routine (preliminary version) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     July 26, 1991
!
!     .. Scalar Arguments ..
      CHARACTER          CA, CB
!     ..
!
!  Purpose
!  =======
!
!  LSAME returns .TRUE. if CA is the same letter as CB regardless of
!  case.
!
!  Arguments
!  =========
!
!  CA      (input) CHARACTER*1
!  CB      (input) CHARACTER*1
!          CA and CB specify the single characters to be compared.
!
!  Further Details
!  ===============
!
!  This version of the routine is only correct for ASCII code.
!  Installers must modify the routine for other character-codes.
!
!  For EBCDIC systems the constant IOFF must be changed to -64.
!  IOFF is computed as
!     IOFF = ICHAR( 'a' ) - ICHAR( 'A' )
!
!  For CDC systems using 6-12 bit representations, the system-
!  specific code in comments must be activated.
!
!  Important!:  The statement function definition for UCASE must use
!  upper case letters A and Z, and the definition for LCASE must use
!  lower case letters a and z.  It is assumed that the characters A-Z
!  are consecutive in the collating sequence, both in upper case and
!  in lower case.
!
!     .. Parameters ..
      INTEGER            IOFF
      PARAMETER          ( IOFF = 32 )
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
!     ..
!     .. Local Scalars ..
      INTEGER            IC, INTA, INTB
!     ..
!     .. Statement Functions ..
      LOGICAL            LCASE, UCASE
!     ..
!     .. Statement Function definitions ..
      UCASE( IC ) = IC.GE.ICHAR( 'A' ) .AND. IC.LE.ICHAR( 'Z' )
      LCASE( IC ) = IC.GE.ICHAR( 'a' ) .AND. IC.LE.ICHAR( 'z' )
!     ..
!     .. Executable Statements ..
!
!     Test if the characters are equal
!
      LSAME_F90 = CA.EQ.CB
      IF( LSAME_F90 ) RETURN
!
!     Now test for equivalence if both characters are alphabetic.
!
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
!
      IF( UCASE( INTA ) ) THEN
!
!        A is upper case
!
         IF( UCASE( INTB ) ) THEN
!
!           B is upper case -- test already done
!
            LSAME_F90 = .FALSE.
!
         ELSE IF( LCASE( INTB ) ) THEN
!
!           B is lower case
!
            LSAME_F90 = INTA.EQ.INTB - IOFF
         END IF
!
      ELSE IF( LCASE( INTA ) ) THEN
!
!        A is lower case
!
         IF( LCASE( INTB ) ) THEN
!
!           B is lower case -- test already done
!
            LSAME_F90 = .FALSE.
!
         ELSE IF( UCASE( INTB ) ) THEN
!
!           B is upper case
!
            LSAME_F90 = INTA - IOFF.EQ.INTB
         END IF
      END IF
!
      RETURN
!
!  The following comments contain code for CDC systems using 6-12 bit
!  representations.
!
!     .. Parameters ..
!     INTEGER            ICIRFX
!     PARAMETER        ( ICIRFX=62 )
!     .. Scalar arguments ..
!     CHARACTER*1        CB
!     .. Array arguments ..
!     CHARACTER*1        CA(*)
!     .. Local scalars ..
!     INTEGER            IVAL
!     .. Intrinsic functions ..
!     INTRINSIC          ICHAR, CHAR
!     .. Executable statements ..
!
!     See if the first character in string CA equals string CB.
!
!     LSAME = CA(1) .EQ. CB .AND. CA(1) .NE. CHAR(ICIRFX)
!
!     IF (LSAME) RETURN
!
!     The characters are not identical. Now check them for equivalence.
!     Look for the 'escape' character, circumflex, followed by the
!     letter.
!
!     IVAL = ICHAR(CA(2))
!     IF (IVAL.GE.ICHAR('A') .AND. IVAL.LE.ICHAR('Z')) THEN
!        LSAME = CA(1) .EQ. CHAR(ICIRFX) .AND. CA(2) .EQ. CB
!     END IF
!
!     RETURN
!
!     End of LSAME_F90
!
      END
