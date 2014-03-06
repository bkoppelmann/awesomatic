SUBROUTINE SGETF2_F90(A, IPIV, INFO )
!
!  -- LAPACK routine (preliminary version) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
!
!  Modified from LAPACK code by Steve Moulton.  5/19/92
!
!  Arguments
!
   USE LA_PRECISION, ONLY:WP
   USE LU, ONLY: ISAMAX_F90
   USE BLAS_AUX, ONLY: XERBLA_F90
   USE SBLAS2, ONLY:SGER1_F90

   IMPLICIT NONE

   REAL(KIND=WP), INTENT(INOUT) :: A(:,:)
   INTEGER, INTENT(OUT)         :: IPIV(:)
   INTEGER, INTENT(OUT)         :: INFO
!
!  Purpose
!  =======
!
!  SGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm
!
!  Arguments
!  =========
!
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
   REAL(KIND=WP),PARAMETER     :: ONE = 1.0_WP, ZERO = 0.0_WP
   CHARACTER*6, PARAMETER      :: SRNAME = 'SGETF2'
!
!  Local Arrays
!
   REAL(KIND=WP), ALLOCATABLE  :: WORK(:)
! 
!  Local Scalars 
!
   INTEGER                     :: J, JP, M, N
! 
!  Intrinsic Functions 
!
   INTRINSIC                      MIN  !, MATMUL
!
!  Executable Statements
!
!  Test the input parameters.
!
   INFO = 0
   M = SIZE(A, 1)
   N = SIZE(A, 2)
   IF (SIZE(IPIV, 1) <  MIN(M, N)) THEN
      INFO = -3
      CALL XERBLA_F90(1, SRNAME, -INFO )
      RETURN
   END IF
!
!  Quick return if possible
!
   IF( M == 0 .OR. N == 0 ) RETURN
   ALLOCATE(WORK(1:N))
!
   DO J = 1, MIN( M, N )
!
!     Find pivot and test for singularity.
!
      JP = J - 1 +  ISAMAX_F90(A( J:M, J ))
!      JP = J - 1 + ISAMAX( M-J+1, A( J, J ), 1 )
      IPIV( J ) = JP
      IF (A( JP, J) /= ZERO) THEN
!
!        Apply the interchange to columns 1:N.
!
!         IF( JP.NE.J ) CALL SSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
         IF( JP.NE.J ) THEN
            WORK = A(J, :); A(J, :) = A(JP, :); A(JP, :) = WORK
         END IF
!
!        Compute elements J+1:M of J-th column.
!
!         IF( J.LT.M ) CALL SSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
         IF( J.LT.M ) A(J+1:M, J) = A(J+1:M, J) / A(J, J)

      ELSE IF( INFO.EQ.0 ) THEN
         INFO = J
      END IF
!
      IF( J+1.LE.N ) THEN
!
!        Update trailing submatrix.
!
!         A(J+1:M, J+1:N) = A(J+1:M, J+1:N) - MATMUL(A(J+1:M,J), A(J, J+1:N))
         CALL SGER1_F90(-ONE, A(J+1:M, J), A(J, J+1:N), A(J+1:M, J+1:N))
!         CALL SGER( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ), LDA,
!     $                 A( J+1, J+1 ), LDA )
      END IF
   END DO
   DEALLOCATE(WORK)
   RETURN
!
!     End of SGETF2_F90
!
END
