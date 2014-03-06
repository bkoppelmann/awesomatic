SUBROUTINE SGETRF_F90(A, IPIV, INFO )
!
!  -- LAPACK routine (preliminary version) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
!
!  Modified from LAPACK code by Steve Moulton. 5/19/02
!
!  Arguments
!
   USE LA_PRECISION, ONLY:WP
   USE LU, ONLY:SGETF2_F90, SLASWP_F90
   USE BLAS_AUX, ONLY: XERBLA_F90
   USE SBLAS3, ONLY:STRSM_F90

   IMPLICIT NONE

   REAL(KIND=WP), INTENT(INOUT) :: A(:,:)
   INTEGER, INTENT(OUT)         :: IPIV(:)
   INTEGER, INTENT(OUT)         :: INFO
!
!  Purpose
!  =======
!
!  SGETRF computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!  A    (input/output) REAL array, dimension (M,N)
!       On entry, the m by n matrix to be factored.
!       On exit, the factors L and U from the factorization
!       A = P*L*U; the unit diagonal elements of L are not stored.
!       M and N are intuited from A.
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!       The pivot indices; for 1 <= i <= min(M,N), row i of the
!       matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!       = 0: successful exit
!       < 0: if INFO = -k, the k-th argument had an illegal value
!       > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!         has been completed, but the factor U is exactly
!         singular, and division by zero will occur if it is used
!         to solve a system of equations.
!
!  =====================================================================
!
!  Parameters
!
   REAL(KIND=WP), PARAMETER  :: ONE = 1.0_WP
!
!  Local Scalars
!
   INTEGER                      I, IINFO, J, JB, NB, M, N, MINMN
!
!  External Functions
!
   INTEGER                      ILAENV
   EXTERNAL                     ILAENV
! 
!  Intrinsic Functions
!
   INTRINSIC                    MIN
!
!  Executable Statements
!
!  Test the input parameters.
!
   INFO = 0
   M = SIZE(A, 1)
   N = SIZE(A, 2)
   MINMN = MIN(M, N)
   IF (SIZE(IPIV, 1) .LT. MINMN) THEN
      INFO = -2
      CALL XERBLA_F90(1, 'SGETRF', -INFO )
      RETURN
   END IF
!
!  Quick return if possible
!
   IF (M == 0 .OR. N == 0) RETURN
!
!  Determine the block size for this environment.
!
   NB = ILAENV( 1, 'SGETRF', ' ', M, N, -1, -1)
   IF (NB <= 1 .OR. NB >= MINMN) THEN
!
!  Use unblocked code.
!
      CALL SGETF2_F90(A, IPIV, INFO)
   ELSE
!
!  Use blocked code.
!
      DO J = 1, MINMN, NB
         JB = MIN( MINMN-J+1, NB )
!
!        Factor diagonal and subdiagonal blocks and test for exact
!        singularity.
!
         CALL SGETF2_F90(A(J:M, J:J+JB-1), IPIV(J:M), IINFO )
!
!        Adjust INFO and the pivot indices.
!
         IF (INFO == 0 .AND. IINFO > 0) INFO = IINFO + J - 1
         DO  I = J, MIN( M, J+JB-1 )
            IPIV( I ) = J - 1 + IPIV( I )
         END DO
!
!        Apply interchanges to columns 1:J-1.
!
         CALL SLASWP_F90(A(:,1:J-1), J, J+JB-1, IPIV, 1 )
!
         IF (J+JB <= N) THEN
!
!           Apply interchanges to columns J+JB:N.
!
            CALL SLASWP_F90(A(:,J+JB:N), J, J+JB-1, IPIV, 1)
!
!           Compute block row of U.
!
            CALL STRSM_F90( 'Left', 'Lower', 'No transpose', 'Unit', &
               ONE, A(J:J+JB-1, J:J+JB-1), A(J:J+JB-1, J+JB:N))
            IF( J+JB.LE.M ) THEN
!
!              Update trailing submatrix.
!
               A(J+JB:M, J+JB:N) =  A(J+JB:M, J+JB:N) -              &
                  MATMUL(A(J+JB:M,J:J+JB-1),A(J:J+JB-1, J+JB:N))
            END IF
         END IF
      END DO
   END IF
   RETURN
!
!  End of SGETRF_F90
!
END
