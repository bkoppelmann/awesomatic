SUBROUTINE STRTRI_F90( UPLO, DIAG, A, INFO )
!
!  -- LAPACK routine (preliminary version) --
!  Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!  Courant Institute, Argonne National Lab, and Rice University
!  December 13, 1991
!
!  Modified from LAPACK code by Steve Moulton.  1/2/91
!
   USE LA_PRECISION, ONLY:WP
   USE LU, ONLY: STRTI2_F90
   USE BLAS_AUX, ONLY: XERBLA_F90, LSAME_F90
   USE SBLAS3, ONLY:STRSM_F90, STRMM_F90

   IMPLICIT NONE

   CHARACTER*1                  :: UPLO, DIAG
   REAL(KIND=WP), INTENT(INOUT) :: A(:,:)
   INTEGER, INTENT(OUT)         :: INFO
!  ..
!
!  Purpose
!  =======
!
!  STRTRI computes the inverse of a real upper or lower triangular
!  matrix A.
!
!  This is the Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!    Specifies whether the matrix A is upper or lower triangular.
!    = 'U':  Upper triangular
!    = 'L':  Lower triangular
!
!  DIAG    (input) CHARACTER*1
!    Specifies whether or not the matrix A is unit triangular.
!    = 'N':  Non-unit triangular
!    = 'U':  Unit triangular
!
!  A    (input/output) REAL array, dimension (N,N)
!
!    On entry, the triangular matrix A.  If UPLO = 'U', the
!    leading n by n upper triangular part of the array A contains
!    the upper triangular matrix, and the strictly lower
!    triangular part of A is not referenced.  If UPLO = 'L', the
!    leading n by n lower triangular part of the array A contains
!    the lower triangular matrix, and the strictly upper
!    triangular part of A is not referenced.  If DIAG = 'U', the
!    diagonal elements of A are also not referenced and are
!    assumed to be 1.
!
!    On exit, the (triangular) inverse of the original matrix, in
!    the same storage format.
!
!    N is intiuted from A.
!
!  INFO    (output) INTEGER
!    > 0: if INFO = k, A(k,k) is exactly zero.  The triangular
!      matrix is singular and its inverse can not be computed.
!
!  =====================================================================
!
!  .. Parameters ..
   REAL(KIND=WP),PARAMETER       :: ONE = 1.0E+0, ZERO = 0.0E+0
   CHARACTER*6,PARAMETER         :: SRNAME='STRTRI'
!  ..
!  .. Local Scalars ..
   LOGICAL      NOUNIT, UPPER
   INTEGER      J, JB, NB, NN, N
!  ..
!  .. External Functions ..
   INTEGER      ILAENV
   EXTERNAL     ILAENV
!  ..
!  .. Intrinsic Functions ..
   INTRINSIC       MIN
!  ..
!  .. Executable Statements ..
!
!  Test the input parameters.
!
   INFO = 0
   UPPER = LSAME_F90( UPLO, 'U' )
   NOUNIT = LSAME_F90( DIAG, 'N' )
   N = SIZE(A,1)
   IF( .NOT.UPPER .AND. .NOT.LSAME_F90( UPLO, 'L' ) ) THEN
      INFO = -1
   ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME_F90( DIAG, 'U' ) ) THEN
      INFO = -2
   ELSE IF (N /= SIZE(A,2)) THEN
      INFO = -3
   END IF
   IF( INFO.NE.0 ) THEN
      CALL XERBLA_F90(1, SRNAME, -INFO )
      RETURN
   END IF
!
!  Quick return if possible
!
   IF( N.EQ.0 ) RETURN
!
!  Check for singularity if non-unit.
!
   IF( NOUNIT ) THEN
      DO INFO = 1, N
         IF( A( INFO, INFO ).EQ.ZERO ) RETURN
      ENDDO 
      INFO = 0
   END IF
!
!  Determine the block size for this environment.
!
   NB = ILAENV( 1, SRNAME, UPLO // DIAG, N, -1, -1, -1 )
   IF( NB.LE.1 .OR. NB.GE.N) THEN
!
!  Use unblocked code
!
      CALL STRTI2_F90( UPLO, DIAG, A)
   ELSE
!
!  Use blocked code
!
      IF( UPPER ) THEN
!
!     Compute inverse of upper triangular matrix
!
         DO J = 1, N, NB
            JB = MIN( NB, N-J+1 )
!
!     Compute rows 1:j-1 of current block column
!
            CALL STRMM_F90( 'Left', 'Upper', 'No transpose', DIAG,  &
               ONE, A(1:J-1, 1:J-1), A(1:J-1, J:J+JB-1))
            CALL STRSM_F90( 'Right', 'Upper', 'No transpose', DIAG, &
               -ONE, A(J:J+JB-1, J:J+JB-1), A(1:J-1, J:J+JB-1))
!
!     Compute inverse of current diagonal block
!
            CALL STRTI2_F90( 'Upper', DIAG, A(J:J+JB-1, J:J+JB-1))
         ENDDO 
      ELSE
!
!     Compute inverse of lower triangular matrix
!
         NN = ( ( N-1 ) / NB )*NB + 1
         DO  J = NN, 1, -NB
            JB = MIN( NB, N-J+1 )
            IF( J+JB.LE.N ) THEN
!
!        Compute rows j+jb:n of current block column
!
               CALL STRMM_F90( 'Left', 'Lower', 'No transpose', DIAG,   &
                  ONE, A(J+JB:N, J+JB:N), A(J+JB:N, J:J+JB-1))
               CALL STRSM_F90( 'Right', 'Lower', 'No transpose', DIAG,  &
                  -ONE, A(J:J+JB-1, J:J+JB-1), A(J+JB:N, J:J+JB-1))
            END IF
!
!     Compute inverse of current diagonal block
!
            CALL STRTI2_F90( 'Lower', DIAG, A(J:J+JB-1, J:J+JB-1))
        ENDDO
      END IF
   END IF
!
   RETURN
!
!  End of STRTRI
!
END
