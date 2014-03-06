SUBROUTINE SGETRS_F90( TRANS, A, IPIV, B)
!
!  -- LAPACK routine (preliminary version) --
!  Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!  Courant Institute, Argonne National Lab, and Rice University
!  December 13, 1991
!
!  Modified from LAPACK code by Steve Moulton.  1/2/91
!
   USE LA_PRECISION, ONLY:WP
   USE LU, ONLY: SLASWP_F90
   USE BLAS_AUX, ONLY: XERBLA_F90, LSAME_F90
   USE SBLAS3, ONLY:STRSM_F90

   IMPLICIT NONE

   CHARACTER*1                  :: TRANS
   REAL(KIND=WP), INTENT(IN)    :: A(:,:)
   INTEGER, INTENT(IN)          :: IPIV(:)
   REAL(KIND=WP), INTENT(INOUT) :: B(:,:)
!
!  Purpose
!  =======
!
!  SGETRS solves a system of linear equations
!  A * X = B  or  A' * X = B
!  with a general n by n matrix A using the LU factorization computed
!  by SGETRF.
!
!  Arguments
!  =========
!
!  TRANS   (input) CHARACTER*1
!    Specifies the form of the system of equations.
!    = 'N':  A * X = B  (No transpose)
!    = 'T':  A'* X = B  (Transpose)
!    = 'C':  A'* X = B  (Conjugate transpose = Transpose)
!
!  A    (input) REAL array, dimension (N,N)
!    The factors L and U from the factorization A = P*L*U
!    as computed by SGETRF.  N is intuited from A.
!
!  IPIV    (input) INTEGER array, dimension (N)
!    The pivot indices from SGETRF; for 1<=i<=N, row i of the
!    matrix was interchanged with row IPIV(i).
!
!  B    (input/output) REAL array, dimension (*,NRHS)
!    On entry, the right hand side vectors B for the system of
!    linear equations.
!    On exit, the solution vectors, X.
!    NRHS intuited from B.
!
!  =====================================================================
!
!  Parameters
!
   REAL(KIND=WP), PARAMETER    :: ONE = 1.0E+0
   CHARACTER*6, PARAMETER      :: SRNAME='SGETRS'
!
!  Local Scalars
!
   LOGICAL      NOTRAN
   INTEGER      N
! 
!  Executable Statements
!
!  Test the input parameters.
!
   NOTRAN = LSAME_F90( TRANS, 'N' )
   IF( .NOT.NOTRAN .AND. .NOT.LSAME_F90( TRANS, 'T' ) .AND. .NOT. &
         LSAME_F90( TRANS, 'C' ) ) THEN
      CALL XERBLA_F90(1, SRNAME, 1)
      RETURN
   ENDIF
   N = SIZE(A, 2)
   IF (SIZE(A,1) /= N) THEN
      CALL XERBLA_F90(1, SRNAME, 2)
      RETURN
   ENDIF
!
!  Quick return if possible
!
   IF (N == 0 .OR. SIZE(B,2) == 0) RETURN
!
   IF (NOTRAN) THEN
!
!  Solve A * X = B.
!
!  Apply row interchanges to the right hand sides.
!
      CALL SLASWP_F90(B, 1, N, IPIV, 1 )
!
!  Solve L*X = B, overwriting B with X.
!
      CALL STRSM_F90( 'Left', 'Lower', 'No transpose', 'Unit', ONE, A, B)
!
!  Solve U*X = B, overwriting B with X.
!
      CALL STRSM_F90( 'Left', 'Upper', 'No transpose', 'Non-unit', ONE, A, B)
   ELSE
!
!  Solve A' * X = B.
!
!  Solve U'*X = B, overwriting B with X.
!
      CALL STRSM_F90( 'Left', 'Upper', 'Transpose', 'Non-unit', ONE, A, B)
!
!  Solve L'*X = B, overwriting B with X.
!
      CALL STRSM_F90( 'Left', 'Lower', 'Transpose', 'Unit', ONE, A, B)
!
!  Apply row interchanges to the solution vectors.
!
      CALL SLASWP_F90(B, 1, N, IPIV, -1 )
   END IF
!
   RETURN
!
!  End of SGETRS
!
   END
