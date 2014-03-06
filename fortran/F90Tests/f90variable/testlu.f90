PROGRAM TESTLU
!
!  Author:
!   Steve Moulton 12/30/91
!
!  Purpose:
!   TESTLU is a test program to ensure that sgetrf continues to function
!   as it is converted from F77 to F90.  This program is set up 
!   so that it is easy to incrementally change from F77 to F90 calls.
!
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!  Use Statements:
!
   USE LA_PRECISION, ONLY: WP
   USE SPRTMT_F90_M
   USE LU

   IMPLICIT NONE
!
!  Data Structures:
!
   REAL (KIND=WP), DIMENSION(:,:), ALLOCATABLE ::  A, ACOPY, AFACT
   REAL (KIND=WP), DIMENSION(:,:), ALLOCATABLE ::  SOLN
   REAL (KIND=WP), DIMENSION(:), ALLOCATABLE   ::  RWORK
   INTEGER, DIMENSION(:), ALLOCATABLE          ::  IPIV

   INTEGER :: M, N, NB, I, J, INFO = 0
   REAL (KIND=WP) ::  RESID, EPS, NORMA, RAND
   REAL (KIND=WP), PARAMETER :: ZERO = 0.0
   INTEGER :: ISEED(1) = (/114827/)   ! some prime number

   REAL SLAMCH
   EXTERNAL SLAMCH,RAND
 
! ------------------------------------------------------------------ 
! 
!  Code
!
!  Get matrix dimension, block size
!
!   WRITE (6,*) "Enter M, N, NB"
!   READ (5,*) M, N, NB
!
!   define m,n, and nb for the purposes of the f90 benchmark 
!
   M = 1000
   N = 1000
   NB = 1000
   WRITE (6,*) "M = ", M, "N = ", N, "NB = ", NB
   ALLOCATE (A(M,N), ACOPY(M,N), AFACT(M,N))
   ALLOCATE (IPIV(MIN(M,N)))
   ALLOCATE(RWORK(N*NB))
!
!  Since everything looks at ilaenv, set nb there
!
   CALL XLAENV(1, NB)    ! Sets optimal blocksize
!   CALL XLAENV(2, 2)     ! Sets minimal blocksize to use blocked rtns
!   CALL XLAENV(3, 2)     ! Sets crossover blocksize
   CALL XLAENV(2, 1)     ! Sets minimal blocksize to use blocked rtns
   CALL XLAENV(3, 1)     ! Sets crossover blocksize
!
!  Load A with random data.  Make copy for residual check.
!
!   CALL RANDOM_SEED(PUT=ISEED)
!   CALL RANDOM_NUMBER(A)
!
!  Call to F90 random number routine replaced by a call to SLATEC
!  generic random number code.  This change was made by John Prentice
!  so that the same random numbers can be guaranteed when using
!  different F90 compilers
!
    DO J=1,N
        DO I=1,M
            A(I,J) = RAND(0.0)
        ENDDO
    ENDDO
!
   ACOPY = A
!
!  We will solve against a matrix uniformly one.  Arbitrarily chose 3 right
!  hand sides.
!
   ALLOCATE(SOLN(MAX(M,N), 3))
   SOLN = 1.0_WP
!
!  Factor A
!
!   CALL SPRTMT_F90(A, 'A:')

   CALL SGESV_F90(A, IPIV, SOLN,  INFO)
   IF (INFO .NE. 0) PRINT *, "TESTLU: Factorization Failed"

!   CALL SPRTMT_F90(A, "LU:")
!
!  Check result
!
   AFACT = A
   CALL SGET01(M, N, ACOPY, M, A, M, IPIV, RWORK, RESID)
   PRINT *, "||LU - A|| / ( N * ||A|| * EPS ) (SGET01)    = ", RESID
!
!  Check solution
!
   A(:, 1:3) = MATMUL(ACOPY,  SOLN(1:N, :))
   A(:, 1:3) = A(:, 1:3) - 1E0
!
!  Compute norm (AX - B ) / ( N * norm(A) * EPS ) (using 1 norm)
!
   EPS = SLAMCH( 'Epsilon' )
   NORMA = MAXVAL(SUM(ABS(ACOPY), DIM=2))
   RESID = MAXVAL(SUM(ABS(A(:,1:3)), DIM=2)) / (NORMA * FLOAT(N) * EPS)
   PRINT *, "||LUX - B|| / / ( N * ||A|| * EPS) (Solve)   = ", RESID
!
!  Invert matrix
!
!   CALL SPRTMT_F90(AFACT, "LU before invert")
   CALL SGETRI_F90(AFACT, IPIV, INFO)
   IF (INFO /= 0) PRINT *, "SGETRI_F90 failed, INFO = ", INFO
!
!  No pivoting is necessary, sgetri takes care of this.
!  Multiply.  Subtract ones off the diagonal.
!
!   CALL SPRTMT_F90(ACOPY, "Acopy:")
!   CALL SPRTMT_F90(AFACT, "Inverse of A")
   ACOPY = MATMUL(ACOPY, AFACT)
!   CALL SPRTMT_F90(ACOPY, "A*INV(A)")
   DO I = 1, N
     ACOPY(I,I) = ACOPY(I,I) - 1
   ENDDO
!
!  Compute norm (A*INV(A) - I) / (N * norm(A) * EPS) 
!
!   CALL SPRTMT_F90(ACOPY, "Product of A*INV(A)):")

   RESID = MAXVAL(SUM(ABS(ACOPY), DIM=2)) / (NORMA * FLOAT(N) * EPS)
   PRINT *, "||A*INV(A) - I|| / (N * ||A|| * EPS) (Invert)= ", RESID

   DEALLOCATE (A, ACOPY, SOLN, RWORK, IPIV, AFACT)
   STOP
END

