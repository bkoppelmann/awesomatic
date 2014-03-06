SUBROUTINE SGETRI_F90(A, IPIV, INFO )
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
   USE BLAS_AUX, ONLY: XERBLA_F90, LSAME_F90
   USE SBLAS3, ONLY:STRSM_F90

   IMPLICIT NONE
   REAL(KIND=WP), INTENT(INOUT) :: A(:,:)
   INTEGER, INTENT(IN)          :: IPIV(:)
   INTEGER, INTENT(OUT)         :: INFO
!
!  Purpose
!  =======
!
!  SGETRI computes the inverse of a matrix using the LU factorization
!  computed by SGETRF.
!
!  This method inverts U and then computes inv(A) by solving the system
!  inv(A)*L = inv(U) for inv(A).
!
!  Arguments
!  =========
!
!  A    (input/output) REAL array, dimension (N,N)
!    On entry, the factors L and U from the factorization
!    A = P*L*U as computed by SGETRF.
!    On exit, if INFO = 0, the inverse of the original matrix A.
!    N is intuited from A.
!
!  IPIV    (input) INTEGER array, dimension (N)
!    The pivot indices from SGETRF; for 1<=i<=N, row i of the
!    matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!    = 0:  successful exit
!    < 0: if INFO = -k, the k-th argument had an illegal value
!    > 0: if INFO = k, U(k,k) is exactly zero; the matrix is
!      singular and its inverse could not be computed.
!
!  =====================================================================
!
!  Parameters 
!
   REAL(KIND=WP),PARAMETER     :: ZERO = 0.0_WP, ONE = 1.0_WP
!
!  Local Scalars
!
   INTEGER                        J, JB, JJ, JP, NB, NBMIN, NN, N
!
!   Local work array
!
   REAL(KIND=WP),ALLOCATABLE   :: WORK(:,:)
!
!  External Functions
!
   INTEGER                        ILAENV
   EXTERNAL                       ILAENV
! 
!  Intrinsic Functions
! 
   INTRINSIC                      MIN, MATMUL
! 
!  Executable Statements
!
!  Test the input parameters.
!
   INFO = 0
   N = SIZE(A,1)
   IF (SIZE(A,2) /= N) THEN
      INFO = -1
   ELSE IF (SIZE(IPIV,1) /= N) THEN
      INFO = -2
   ENDIF
   IF( INFO.NE.0 ) THEN
      CALL XERBLA_F90(1, 'SGETRI', -INFO )
      RETURN
   END IF
!
!  Quick return if possible
!
   IF( N.EQ.0 ) RETURN
!
!  Form inv(U).  If INFO > 0 from STRTRI, then U is singular,
!  and the inverse is not computed.
!
   CALL STRTRI_F90('Upper', 'Non-unit', A, INFO )
   IF (INFO > 0) RETURN
!
!  Determine the block size for this environment.
!
   NB = ILAENV( 1, 'SGETRI', ' ', N, -1, -1, -1 )
   NBMIN = 2
!
!  Solve the equation inv(A)*L = inv(U) for inv(A).
!
   IF (NB < NBMIN .OR. NB >= N) THEN
!
!  Use unblocked code.
!
      ALLOCATE(WORK(N,1))
      DO J = N, 1, -1
!
!     Copy current column of L to WORK and replace with zeros.
!
         WORK(J+1:N,1) = A(J+1:N, J)
         A(J+1:N, J) = ZERO
!
!     Compute current column of inv(A).
!
         IF( J.LT.N )  A(1:N, J) = A(1:N, J) -                    &
            MATMUL(A(1:N, J+1:N), WORK(J+1:N, 1))
      END DO
   ELSE
!
!  Use blocked code.
!
      ALLOCATE(WORK(N,NB))
      NN = ((N-1) / NB) * NB + 1
      DO J = NN, 1, -NB
         JB = MIN( NB, N-J+1 )
!
!     Copy current block column of L to WORK and replace with
!     zeros.
!
         DO JJ = J, J + JB - 1
            WORK(JJ+1:N, JJ-J+1) = A(JJ+1:N, JJ)
            A(JJ+1:N, JJ) = ZERO
         ENDDO
!
!     Compute current block column of inv(A).
!
         IF( J+JB.LE.N ) A(1:N, J:J+JB-1) = A(1:N, J:J+JB-1) - &
            MATMUL(A(1:N, J+JB:N), WORK(J+JB:N, 1:JB))
         CALL STRSM_F90('Right', 'Lower', 'No transpose', 'Unit', &
            ONE, WORK(J:J+JB-1, 1:JB), A(1:N, J:J+JB-1))
      END DO
   END IF
!
!  Apply column interchanges.
!
   DO J = N - 1, 1, -1
      JP = IPIV( J )
      IF (JP .NE. J) THEN
         WORK(:,1) = A(:,J); A(:,J) = A(:,JP); A(:,JP) = WORK(:,1)
      ENDIF
   ENDDO
!
   DEALLOCATE(WORK)
   RETURN
!
!  End of SGETRI_F90
!
END
