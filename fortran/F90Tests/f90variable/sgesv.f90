SUBROUTINE SGESV_F90(A, IPIV, B, INFO)
!
!  -- LAPACK driver routine (version 1.0) --
!  Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!  Courant Institute, Argonne National Lab, and Rice University
!  February 29, 1992
!
!  Modified from LAPACK code by Steve Moulton.   5/20/92
!
!  Arguments
!
   USE LA_PRECISION, ONLY:WP
   USE LU, ONLY:SGETRF_F90, SGETRS_F90
   USE BLAS_AUX, ONLY: XERBLA_F90, LSAME_F90

   IMPLICIT NONE

   REAL(KIND=WP), INTENT(INOUT) :: A(:,:), B(:,:)
   INTEGER, INTENT(OUT)         :: IPIV(:), INFO

!  Purpose
!  =======
!
!  SGESV computes the solution to a real system of linear equations
!  A * X = B,
!  where A is an N by N matrix and X and B are N by NRHS matrices.
!
!  The LU decomposition with partial pivoting and row interchanges is
!  used to factor A as
!  A = P * L * U,
!  where P is a permutation matrix, L is unit lower triangular, and U is
!  upper triangular.  The factored form of A is then used to solve the
!  system of equations A * X = B.
!
!  Arguments
!  =========
!
!  A       (input/output) REAL array, dimension (M,N)
!       On entry, the N by N matrix of coefficients A.
!       On exit, the factors L and U from the factorization
!       A = P*L*U; the unit diagonal elements of L are not stored.
!       M, N intuited from A.
!
!  IPIV    (output) INTEGER array, dimension (N)
!       The pivot indices that define the permutation matrix P;
!       row i of the matrix was interchanged with row IPIV(i).
!
!  B       (input/output) REAL array, dimension (LDB,NRHS)
!       On entry, the N by NRHS matrix of right hand side vectors B
!       for the system of equations A*X = B.
!       On exit, if INFO = 0, the N by NRHS matrix of solution
!       vectors X.
!       NRHS intuited from B.
!
!  INFO    (output) INTEGER
!       = 0: successful exit
!       < 0: if INFO = -k, the k-th argument had an illegal value
!       > 0: if INFO = k, U(k,k) is exactly zero.  The factorization
!            has been completed, but the factor U is exactly
!            singular, so the solution could not be computed.
!
!  =====================================================================
!
!  Local Scalars
!
   CHARACTER*6, PARAMETER  :: SRNAME = 'SGESV '
! 
!  Executable Statements
!
!  Test the input parameters.
!
   INFO = 0
   IF (SIZE(IPIV, 1) <  MIN(SIZE(A,1),SIZE(A,2))) THEN
      INFO = -2
   END IF
   IF (INFO /= 0) THEN
      CALL XERBLA_F90(1, SRNAME, -INFO )
      RETURN
   ENDIF
!
!  Compute the LU factorization of A.
!
   CALL SGETRF_F90(A, IPIV, INFO)
   IF (INFO == 0) THEN
!
!     Solve the system A*X = B, overwriting B with X.
!
      CALL SGETRS_F90('No transpose', A, IPIV, B)
   END IF
   RETURN
!
!  End of SGESV_F90
!
END
