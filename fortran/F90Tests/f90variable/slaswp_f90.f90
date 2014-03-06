SUBROUTINE SLASWP_F90(A, K1, K2, IPIV, IDIR)
!
!  -- LAPACK auxiliary routine (preliminary version) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     December 13, 1991
!
!  Modified from LAPACK code by Steve Moulton.  12/30/91
!     .. Arguments ..
!
   USE LA_PRECISION, ONLY:WP
   USE BLAS_AUX, ONLY: XERBLA_F90

   REAL(KIND=WP), INTENT(INOUT) :: A(:,:)
   INTEGER,INTENT(IN)           :: K1, K2, IPIV(:), IDIR

!  Purpose
!  =======
!
!  SLASWP performs a series of row interchanges on the matrix A.
!  One row interchange is initiated for each of rows K1 through K2 of A.
!
!  Arguments
!  =========
!
!  A    (input/output) REAL array, dimension (M,N)
!       On entry, the matrix of column dimension N to which the row
!       interchanges will be applied.
!       On exit, the permuted matrix.
!       A as passed should be conformable with IPIV.
!       M and N are intuited from A
!
!  K1   (input) INTEGER
!       The first element of IPIV for which a row interchange will
!       be done.
!
!  K2   (input) INTEGER
!       The last element of IPIV for which a row interchange will
!       be done.
!
!  IPIV    (input) INTEGER array, dimension (M)
!       The vector of pivot indices.  Only the elements in positions
!       K1 through K2 of IPIV are accessed.
!       IPIV(K) = L implies rows K and L are to be interchanged.
!
!  IDIR (INPUT) INTEGER
!       If IDIR is negative, the pivots are applied in reverse order.
!
!     .. Local Array
!     ..
   REAL(KIND=WP),ALLOCATABLE       :: WORK(:)
!     .. Local Scalars ..
   INTEGER      I, IP, N
!     ..
!     .. Executable Statements ..
!
!     Interchange row I with row IPIV(I) for each of rows K1 through K2.
!
   N = SIZE(A, 2)
   ALLOCATE(WORK(1:N))
   IF (IDIR >= 0) THEN
      DO  I = K1, K2
         IP = IPIV( I )
         IF( IP.NE.I ) THEN
            WORK = A(I,:); A(I,:) = A(IP,:); A(IP,:) = WORK
         ENDIF
      ENDDO
   ELSE
      DO  I = K2, K1, -1
         IP = IPIV( I )
         IF( IP.NE.I ) THEN
            WORK = A(I,:); A(I,:) = A(IP,:); A(IP,:) = WORK
         ENDIF
      ENDDO
   ENDIF
!
   DEALLOCATE(WORK)
   RETURN
!
!     End of SLASWP
!
   END
