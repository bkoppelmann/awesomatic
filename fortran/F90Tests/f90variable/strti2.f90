SUBROUTINE STRTI2_F90( UPLO, DIAG, A)
!
!  -- LAPACK routine (preliminary version) --
!  Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!  Courant Institute, Argonne National Lab, and Rice University
!  December 13, 1991
!
!  Modified from LAPACK code by Steve Moulton.  1/2/91
!
   USE LA_PRECISION, ONLY:WP
   USE LU, ONLY:STRTRI_F90
   USE SBLAS2, ONLY:STRMV_F90
   USE BLAS_AUX, ONLY: XERBLA_F90, LSAME_F90

   IMPLICIT NONE

   CHARACTER*1                  :: UPLO, DIAG
   REAL(KIND=WP), INTENT(INOUT) :: A(:,:)
!
!  Purpose
!  =======
!
!  STRTI2 computes the inverse of a real upper or lower triangular
!  matrix.
!
!  This is the Level 2 BLAS version of the algorithm.
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
!  N    (input) INTEGER
!    The order of the matrix A.  N >= 0.
!
!  A    (input/output) REAL array, dimension (N,N)
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
!    N is intuited from this argument.
!
!  =====================================================================
!
!  .. Parameters ..
   REAL(KIND=WP), PARAMETER       ::  ONE = 1.0E+0 
   CHARACTER*6, PARAMETER         ::  SRNAME = 'STRTI2'
!  ..
!  .. Local Scalars ..
   LOGICAL      NOUNIT, UPPER
   INTEGER      J, N, INFO
   REAL(KIND=WP) AJJ
!  ..
!  .. Executable Statements ..
!
!  Test the input parameters.
!
   UPPER = LSAME_F90( UPLO, 'U' )
   NOUNIT = LSAME_F90( DIAG, 'N' )
   N = SIZE(A,2)
   INFO = 0
   IF( .NOT.UPPER .AND. .NOT.LSAME_F90( UPLO, 'L' ) ) THEN
      INFO = -1
   ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME_F90( DIAG, 'U' ) ) THEN
      INFO = -2
   ELSE IF (SIZE(A,1) /= N) THEN
      INFO = -3
   END IF
   IF( INFO.NE.0 ) THEN
      CALL XERBLA_F90(1, SRNAME, -INFO )
      RETURN
   END IF
!
   IF( UPPER ) THEN
!
!  Compute inverse of upper triangular matrix.
!
      DO J = 1, N
         IF( NOUNIT ) THEN
            A( J, J ) = ONE / A( J, J )
            AJJ = -A( J, J )
         ELSE
            AJJ = -ONE
         END IF
!
!     Compute elements 1:j-1 of j-th column.
!
          CALL STRMV_F90( 'Upper', 'No transpose', DIAG,                &
             A(1:J-1, 1:J-1), A(1:J-1, J))
          A(1:J-1, J) =  A(1:J-1, J) * AJJ
      ENDDO
   ELSE
!
!  Compute inverse of lower triangular matrix.
!
      DO J = N, 1, -1
         IF( NOUNIT ) THEN
            A( J, J ) = ONE / A( J, J )
            AJJ = -A( J, J )
         ELSE
            AJJ = -ONE
         END IF
         IF( J.LT.N ) THEN
!
!     Compute elements j+1:n of j-th column.
!
            CALL STRMV_F90( 'Lower', 'No transpose', DIAG,             &
               A(J+1:N, J+1:N), A(J+1:N, J))
            A(J+1:N, J) = A(J+1:N, J) * AJJ
         END IF
      ENDDO 
   END IF
!
   RETURN
!
!  End of STRTI2
!
END
