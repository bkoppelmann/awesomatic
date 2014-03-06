MODULE SBLAS2

INTERFACE

SUBROUTINE SGEMV_F90 ( TRANS, ALPHA, A, X, BETA, Y)
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: TRANS
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: A(:,:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
END SUBROUTINE SGEMV_F90

SUBROUTINE SGBMV_F90 ( TRANS, KL, KU, ALPHA, AB, X, BETA, Y )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: TRANS
   INTEGER       , INTENT (IN)    :: KL, KU
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: AB(:,:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
END SUBROUTINE SGBMV_F90

SUBROUTINE SSYMV_F90 ( UPLO, ALPHA, A, X, BETA, Y )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: A(:,:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
END SUBROUTINE SSYMV_F90

SUBROUTINE SSBMV_F90 ( UPLO, K, ALPHA, AB, X, BETA, Y )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: UPLO
   INTEGER       , INTENT (IN)    :: K
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: AB(:,:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
END SUBROUTINE SSBMV_F90

SUBROUTINE SSPMV_F90 ( UPLO, ALPHA, AP, X, BETA, Y )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: AP(:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
END SUBROUTINE SSPMV_F90

SUBROUTINE STRMV_F90( UPLO, TRANS, DIAG, A, X )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: A(:,:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
END SUBROUTINE STRMV_F90

SUBROUTINE STBMV_F90( UPLO, TRANS, DIAG, K, AB, X )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: AB(:,:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
END SUBROUTINE STBMV_F90

SUBROUTINE STPMV_F90( UPLO, TRANS, DIAG, AP, X )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: AP(:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
END SUBROUTINE STPMV_F90

SUBROUTINE STRSV_F90( UPLO, TRANS, DIAG, A, X )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: A(:,:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
END SUBROUTINE STRSV_F90

SUBROUTINE STBSV_F90( UPLO, TRANS, DIAG, K, AB, X )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: AB(:,:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
END SUBROUTINE STBSV_F90

SUBROUTINE STPSV_F90( UPLO, TRANS, DIAG, AP, X )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: AP(:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
END SUBROUTINE STPSV_F90

SUBROUTINE SGER1_F90 ( ALPHA, X, Y, A )
   USE                               LA_PRECISION, ONLY: WP
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:), Y(:)
   REAL (KIND=WP), INTENT (INOUT) :: A(:,:)
END SUBROUTINE SGER1_F90

SUBROUTINE SSYR1_F90 ( UPLO, ALPHA, X, A )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:)
   REAL (KIND=WP), INTENT (INOUT) :: A(:,:)
END SUBROUTINE SSYR1_F90

SUBROUTINE SSPR1_F90 ( UPLO, ALPHA, X, AP )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:)
   REAL (KIND=WP), INTENT (INOUT) :: AP(:)
END SUBROUTINE SSPR1_F90

SUBROUTINE SSYR2_F90 ( UPLO, ALPHA, X, Y, A )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:), Y(:)
   REAL (KIND=WP), INTENT (INOUT) :: A(:,:)
END SUBROUTINE SSYR2_F90

SUBROUTINE SSPR2_F90 ( UPLO, ALPHA, X, Y, AP )
   USE                               LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:), Y(:)
   REAL (KIND=WP), INTENT (INOUT) :: AP(:)
END SUBROUTINE SSPR2_F90

END INTERFACE

END MODULE SBLAS2

SUBROUTINE SGEMV_F90 ( TRANS, ALPHA, A, X, BETA, Y )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: TRANS
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: A(:,:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SGEMV '
   INTEGER                        :: J, LX, LY, M, N
   LOGICAL                        :: NOTRAN
   INTRINSIC                         DOT_PRODUCT, SIZE
   NOTRAN = LSAME_F90(TRANS,'N')
   LX = SIZE(X)
   LY = SIZE(Y)
   M = SIZE(A,1)
   N = SIZE(A,2)
   IF (.NOT.NOTRAN.AND..NOT.LSAME_F90(TRANS,'T').AND..NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF ((NOTRAN.AND.N/=LX).OR.(.NOT.NOTRAN.AND.M/=LX)) THEN
      CALL XERBLA_F90(2,SRNAME,3,4)
   ELSE IF ((NOTRAN.AND.M/=LY).OR.(.NOT.NOTRAN.AND.N/=LY)) THEN
      CALL XERBLA_F90(2,SRNAME,3,6)
   ELSE 
      IF (BETA/=ONE) THEN
         IF (BETA==ZERO) THEN
            Y(:) = ZERO
         ELSE
            Y(:) = BETA*Y(:)
         END IF
      END IF
      IF (ALPHA/=ZERO) THEN
         IF (NOTRAN) THEN
            DO J = 1, N
               IF (X(J)/=ZERO) &
                  Y(:) = Y(:) + ALPHA*X(J)*A(:,J)
            END DO
         ELSE
            DO J = 1, N
               Y(J) = Y(J) + ALPHA*DOT_PRODUCT(A(:,J),X(:))
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SGEMV_F90

SUBROUTINE SGBMV_F90 ( TRANS, KL, KU, ALPHA, AB, X, BETA, Y )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: TRANS
   INTEGER       , INTENT (IN)    :: KL, KU
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: AB(:,:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SGBMV '
   INTEGER                        :: J, JK1, JK2, JKB1, JKB2, LX, LY, M, N
   LOGICAL                        :: NOTRAN
   INTRINSIC                         DOT_PRODUCT, MAX, MIN, SIZE
   NOTRAN = LSAME_F90(TRANS,'N')
   LX = SIZE(X)
   LY = SIZE(Y)
   N = SIZE(AB,2)
   IF (.NOT.NOTRAN.AND..NOT.LSAME_F90(TRANS,'T').AND..NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (KL<0) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (KU<0) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (SIZE(AB,1)/=KL+KU+1) THEN
      CALL XERBLA_F90(2,SRNAME,5)
   ELSE IF (NOTRAN.AND.N/=LX) THEN
      CALL XERBLA_F90(2,SRNAME,5,6)
   ELSE IF (.NOT.NOTRAN.AND.N/=LY) THEN
      CALL XERBLA_F90(2,SRNAME,5,8)
   ELSE 
      IF (BETA/=ONE) THEN
         IF (BETA==ZERO) THEN
            Y(:) = ZERO
         ELSE
            Y(:) = BETA*Y(:)
         END IF
      END IF
      IF (ALPHA/=ZERO) THEN
         IF (NOTRAN) THEN
            M = LY
            DO J = 1, N
               JK1 = MAX(1,J-KU) 
               JK2 = MIN(M,J+KL)
               JKB1 = JK1 - J + KU + 1
               JKB2 = JK2 - J + KU + 1
               IF (X(J)/=ZERO) &
                  Y(JK1:JK2) = Y(JK1:JK2) + ALPHA*X(J)*AB(JKB1:JKB2,J)
            END DO
         ELSE
            M = LX
            DO J = 1, N
               JK1 = MAX(1,J-KU) 
               JK2 = MIN(M,J+KL)
               JKB1 = JK1 - J + KU + 1
               JKB2 = JK2 - J + KU + 1
               Y(J) = Y(J) + ALPHA*DOT_PRODUCT(AB(JKB1:JKB2,J),X(JK1:JK2))
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SGBMV_F90

SUBROUTINE SSYMV_F90 ( UPLO, ALPHA, A, X, BETA, Y )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: A(:,:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SSYMV '
   INTEGER                        :: J, N
   LOGICAL                        :: UPPER
   INTRINSIC                         DOT_PRODUCT, SIZE
   UPPER = LSAME_F90(UPLO,'U')
   N = SIZE(A,1)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (SIZE(A,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,3)
   ELSE IF (SIZE(X)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,3,4)
   ELSE IF (SIZE(Y)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,3,6)
   ELSE 
      IF (BETA/=ONE) THEN
         IF (BETA==ZERO) THEN
            Y(:) = ZERO
         ELSE
            Y(:) = BETA*Y(:)
         END IF
      END IF
      IF (ALPHA/=ZERO) THEN
         IF (UPPER) THEN
            DO J = 1, N
               Y(1:J-1) = Y(1:J-1) + ALPHA*X(J)*A(1:J-1,J)
               Y(J) = Y(J) + ALPHA*DOT_PRODUCT(A(1:J-1,J),X(1:J-1))
               Y(J) = Y(J) + ALPHA*X(J)*A(J,J)
            END DO
         ELSE
            DO J = 1, N
               Y(J) = Y(J) + ALPHA*X(J)*A(J,J)
               Y(J) = Y(J) + ALPHA*DOT_PRODUCT(A(J+1:N,J),X(J+1:N))
               Y(J+1:N) = Y(J+1:N) + ALPHA*X(J)*A(J+1:N,J)
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSYMV_F90

SUBROUTINE SSBMV_F90 ( UPLO, K, ALPHA, AB, X, BETA, Y )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: UPLO
   INTEGER       , INTENT (IN)    :: K
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: AB(:,:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SSBMV '
   INTEGER                        :: J, JK, N
   LOGICAL                        :: UPPER
   INTRINSIC                         DOT_PRODUCT, MAX, MIN, SIZE
   UPPER = LSAME_F90(UPLO,'U')
   N = SIZE(AB,2)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (K<0) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (SIZE(AB,1)/=K+1) THEN
      CALL XERBLA_F90(2,SRNAME,4)
   ELSE IF (SIZE(X)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4,5)
   ELSE IF (SIZE(Y)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4,7)
   ELSE 
      IF (BETA/=ONE) THEN
         IF (BETA==ZERO) THEN
            Y(:) = ZERO
         ELSE
            Y(:) = BETA*Y(:)
         END IF
      END IF
      IF (ALPHA/=ZERO) THEN
         IF (UPPER) THEN
            DO J = 1, N
               JK = MAX(1,J-K)
               Y(JK:J-1) = Y(JK:J-1) + ALPHA*X(J)*AB(JK+K-J+1:K,J)
               Y(J) = Y(J) + ALPHA*DOT_PRODUCT(AB(JK+K-J+1:K,J),X(JK:J-1))
               Y(J) = Y(J) + ALPHA*X(J)*AB(K+1,J)
            END DO
         ELSE
            DO J = 1, N
               JK = MIN(N,J+K)
               Y(J) = Y(J) + ALPHA*X(J)*AB(1,J)
               Y(J) = Y(J) + ALPHA*DOT_PRODUCT(AB(2:JK-J+1,J),X(J+1:JK))
               Y(J+1:JK) = Y(J+1:JK) + ALPHA*X(J)*AB(2:JK-J+1,J)
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSBMV_F90

SUBROUTINE SSPMV_F90 ( UPLO, ALPHA, AP, X, BETA, Y )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT (IN)    :: AP(:), X(:)
   REAL (KIND=WP), INTENT (INOUT) :: Y(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SSPMV '
   INTEGER                        :: J, JP, N
   LOGICAL                        :: UPPER
   INTRINSIC                         DOT_PRODUCT, SIZE
   UPPER = LSAME_F90(UPLO,'U')
   N = SIZE(X)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (SIZE(AP)/=N*(N+1)/2) THEN
      CALL XERBLA_F90(2,SRNAME,3,4)
   ELSE IF (SIZE(Y)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4,6)
   ELSE 
      IF (BETA/=ONE) THEN
         IF (BETA==ZERO) THEN
            Y(:) = ZERO
         ELSE
            Y(:) = BETA*Y(:)
         END IF
      END IF
      IF (ALPHA/=ZERO) THEN
         JP = 0
         IF (UPPER) THEN
            DO J = 1, N
               Y(1:J-1) = Y(1:J-1) + ALPHA*X(J)*AP(JP+1:JP+J-1)
               Y(J) = Y(J) + ALPHA*DOT_PRODUCT(AP(JP+1:JP+J-1),X(1:J-1))
               Y(J) = Y(J) + ALPHA*X(J)*AP(JP+J)
               JP = JP + J
            END DO
         ELSE
            DO J = 1, N
               Y(J) = Y(J) + ALPHA*X(J)*AP(JP+J)
               Y(J) = Y(J) + ALPHA*DOT_PRODUCT(AP(JP+J+1:JP+N),X(J+1:N))
               Y(J+1:N) = Y(J+1:N) + ALPHA*X(J)*AP(JP+J+1:JP+N)
               JP = JP + N - J
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSPMV_F90

SUBROUTINE STRMV_F90( UPLO, TRANS, DIAG, A, X )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: A(:,:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='STRMV '
   INTEGER                        :: J, N
   LOGICAL                        :: NOUNIT, NOTRAN, UPPER
   INTRINSIC                         DOT_PRODUCT, SIZE
   N = SIZE(A,1)
   UPPER = LSAME_F90(UPLO,'U')
   NOTRAN = LSAME_F90(TRANS,'N')
   NOUNIT = LSAME_F90(DIAG,'N')
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTRAN.AND..NOT.LSAME_F90(TRANS,'T').AND. &
            .NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (.NOT.NOUNIT.AND..NOT.LSAME_F90(DIAG,'U')) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (SIZE(A,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4)
   ELSE IF (SIZE(X)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4,5)
   ELSE 
      IF (NOTRAN) THEN
         IF (UPPER) THEN
            DO J = 1, N
               IF (X(J)/=ZERO) THEN
                  X(1:J-1) = X(1:J-1) + X(J)*A(1:J-1,J)
                  IF (NOUNIT) &
                     X(J) = X(J)*A(J,J)
               END IF
            END DO
         ELSE
            DO J = N, 1, -1
               IF (X(J)/=ZERO) THEN
                  X(J+1:N) = X(J+1:N) + X(J)*A(J+1:N,J)
                  IF (NOUNIT) &
                     X(J) = X(J)*A(J,J)
               END IF
            END DO
         END IF
      ELSE
         IF (UPPER) THEN
            DO J = N, 1, -1
               IF (NOUNIT) &
                  X(J) = X(J)*A(J,J)
               X(J) = X(J) + DOT_PRODUCT(A(1:J-1,J),X(1:J-1))
            END DO
         ELSE
            DO J = 1, N
               IF (NOUNIT) &
                  X(J) = X(J)*A(J,J)
               X(J) = X(J) + DOT_PRODUCT(A(J+1:N,J),X(J+1:N))
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE STRMV_F90

SUBROUTINE STBMV_F90( UPLO, TRANS, DIAG, K, AB, X )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   INTEGER       , INTENT (IN)    :: K
   REAL (KIND=WP), INTENT (IN)    :: AB(:,:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='STBMV '
   INTEGER                        :: J, JK, N
   LOGICAL                        :: NOUNIT, NOTRAN, UPPER
   INTRINSIC                         DOT_PRODUCT, MAX, MIN, SIZE
   UPPER = LSAME_F90(UPLO,'U')
   NOTRAN = LSAME_F90(TRANS,'N')
   NOUNIT = LSAME_F90(DIAG,'N')
   N = SIZE(AB,2)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTRAN.AND..NOT.LSAME_F90(TRANS,'T').AND. &
            .NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (.NOT.NOUNIT.AND..NOT.LSAME_F90(DIAG,'U')) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (K<0) THEN
      CALL XERBLA_F90(1,SRNAME,4)
   ELSE IF (SIZE(AB,1)/=K+1) THEN
      CALL XERBLA_F90(2,SRNAME,5)
   ELSE IF (SIZE(X)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,5,6)
   ELSE 
      IF (NOTRAN) THEN
         IF (UPPER) THEN
            DO J = 1, N
               JK = MAX(1,J-K)
               IF (X(J)/=ZERO) THEN
                  X(JK:J-1) = X(JK:J-1) + X(J)*AB(JK+K-J+1:K,J)
                  IF (NOUNIT) &
                     X(J) = X(J)*AB(K+1,J)
               END IF
            END DO
         ELSE
            DO J = N, 1, -1
               JK = MIN(N,J+K)
               IF (X(J)/=ZERO) THEN
                  X(J+1:JK) = X(J+1:JK) + X(J)*AB(2:JK-J+1,J)
                  IF (NOUNIT) &
                     X(J) = X(J)*AB(1,J)
               END IF
            END DO
         END IF
      ELSE
         IF (UPPER) THEN
            DO J = N, 1, -1
               JK = MAX(1,J-K)
               IF (NOUNIT) &
                  X(J) = X(J)*AB(K+1,J)
               X(J) = X(J) + DOT_PRODUCT(AB(JK+K-J+1:K,J),X(JK:J-1))
            END DO
         ELSE
            DO J = 1, N
               JK = MIN(N,J+K)
               IF (NOUNIT) &
                  X(J) = X(J)*AB(1,J)
               X(J) = X(J) + DOT_PRODUCT(AB(2:JK-J+1,J),X(J+1:JK))
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE STBMV_F90

SUBROUTINE STPMV_F90( UPLO, TRANS, DIAG, AP, X )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: AP(:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='STPMV '
   INTEGER                        :: J, JP, N
   LOGICAL                        :: NOUNIT, NOTRAN, UPPER
   INTRINSIC                         DOT_PRODUCT, SIZE
   UPPER = LSAME_F90(UPLO,'U')
   NOTRAN = LSAME_F90(TRANS,'N')
   NOUNIT = LSAME_F90(DIAG,'N')
   N = SIZE(X)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTRAN.AND..NOT.LSAME_F90(TRANS,'T').AND. &
            .NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (.NOT.NOUNIT.AND..NOT.LSAME_F90(DIAG,'U')) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (SIZE(AP,1)/=N*(N+1)/2) THEN
      CALL XERBLA_F90(2,SRNAME,4,5)
   ELSE 
      IF (NOTRAN) THEN
         IF (UPPER) THEN
            JP = 0
            DO J = 1, N
               IF (X(J)/=ZERO) THEN
                  X(1:J-1) = X(1:J-1) + X(J)*AP(JP+1:JP+J-1)
                  IF (NOUNIT) &
                     X(J) = X(J)*AP(JP+J)
               END IF
               JP = JP + J 
            END DO
         ELSE
            JP = (N*(N-1))/2
            DO J = N, 1, -1
               IF (X(J)/=ZERO) THEN
                  X(J+1:N) = X(J+1:N) + X(J)*AP(JP+J+1:JP+N)
                  IF (NOUNIT) &
                     X(J) = X(J)*AP(JP+J)
               END IF
               JP = JP - N + J - 1
            END DO
         END IF
      ELSE
         IF (UPPER) THEN
            JP = (N*(N-1))/2
            DO J = N, 1, -1
               IF (NOUNIT) &
                  X(J) = X(J)*AP(JP+J)
               X(J) = X(J) + DOT_PRODUCT(AP(JP+1:JP+J-1),X(1:J-1))
               JP = JP - J + 1 
            END DO
         ELSE
            JP = 0
            DO J = 1, N
               IF (NOUNIT) &
                  X(J) = X(J)*AP(JP+J)
               X(J) = X(J) + DOT_PRODUCT(AP(JP+J+1:JP+N),X(J+1:N))
               JP = JP + N - J 
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE STPMV_F90

SUBROUTINE STRSV_F90( UPLO, TRANS, DIAG, A, X )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: A(:,:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='STRSV '
   INTEGER                        :: J, N
   LOGICAL                        :: NOUNIT, NOTRAN, UPPER
   INTRINSIC                         DOT_PRODUCT, SIZE
   N = SIZE(A,1)
   UPPER = LSAME_F90(UPLO,'U')
   NOTRAN = LSAME_F90(TRANS,'N')
   NOUNIT = LSAME_F90(DIAG,'N')
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTRAN.AND..NOT.LSAME_F90(TRANS,'T').AND. &
            .NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (.NOT.NOUNIT.AND..NOT.LSAME_F90(DIAG,'U')) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (SIZE(A,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4)
   ELSE IF (SIZE(X)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4,5)
   ELSE 
      IF (NOTRAN) THEN
         IF (UPPER) THEN
            DO J = N, 1, -1
               IF (X(J)/=ZERO) THEN
                  IF (NOUNIT) &
                     X(J) = X(J)/A(J,J)
                  X(1:J-1) = X(1:J-1) - X(J)*A(1:J-1,J)
               END IF
            END DO
         ELSE
            DO J = 1, N
               IF (X(J)/=ZERO) THEN
                  IF (NOUNIT) &
                     X(J) = X(J)/A(J,J)
                  X(J+1:N) = X(J+1:N) - X(J)*A(J+1:N,J)
               END IF
            END DO
         END IF
      ELSE
         IF (UPPER) THEN
            DO J = 1, N
               X(J) = X(J) - DOT_PRODUCT(A(1:J-1,J),X(1:J-1))
               IF (NOUNIT) &
                  X(J) = X(J)/A(J,J)
            END DO
         ELSE
            DO J = N, 1, -1
               X(J) = X(J) - DOT_PRODUCT(A(J+1:N,J),X(J+1:N))
               IF (NOUNIT) &
                  X(J) = X(J)/A(J,J)
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE STRSV_F90

SUBROUTINE STBSV_F90( UPLO, TRANS, DIAG, K, AB, X )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   INTEGER       , INTENT (IN)    :: K
   REAL (KIND=WP), INTENT (IN)    :: AB(:,:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='STBSV '
   INTEGER                        :: J, JK, N
   LOGICAL                        :: NOUNIT, NOTRAN, UPPER
   INTRINSIC                         DOT_PRODUCT, MAX, MIN, SIZE
   UPPER = LSAME_F90(UPLO,'U')
   NOTRAN = LSAME_F90(TRANS,'N')
   NOUNIT = LSAME_F90(DIAG,'N')
   N = SIZE(AB,2)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTRAN.AND..NOT.LSAME_F90(TRANS,'T').AND. &
            .NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (.NOT.NOUNIT.AND..NOT.LSAME_F90(DIAG,'U')) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (K<0) THEN
      CALL XERBLA_F90(1,SRNAME,4)
   ELSE IF (SIZE(AB,1)/=K+1) THEN
      CALL XERBLA_F90(2,SRNAME,5)
   ELSE IF (SIZE(X)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,5,6)
   ELSE 
      IF (NOTRAN) THEN
         IF (UPPER) THEN
            DO J = N, 1, -1
               JK = MAX(1,J-K)
               IF (X(J)/=ZERO) THEN
                  IF (NOUNIT) &
                     X(J) = X(J)/AB(K+1,J)
                  X(JK:J-1) = X(JK:J-1) - X(J)*AB(JK+K-J+1:K,J)
               END IF
            END DO
         ELSE
            DO J = 1, N
               JK = MIN(N,J+K)
               IF (X(J)/=ZERO) THEN
                  IF (NOUNIT) &
                     X(J) = X(J)/AB(1,J)
                  X(J+1:JK) = X(J+1:JK) - X(J)*AB(2:JK-J+1,J)
               END IF
            END DO
         END IF
      ELSE
         IF (UPPER) THEN
            DO J = 1, N
               JK = MAX(1,J-K)
               X(J) = X(J) - DOT_PRODUCT(AB(JK+K-J+1:K,J),X(JK:J-1))
               IF (NOUNIT) &
                  X(J) = X(J)/AB(K+1,J)
            END DO
         ELSE
            DO J = N, 1, -1
               JK = MIN(N,J+K)
               X(J) = X(J) - DOT_PRODUCT(AB(2:JK-J+1,J),X(J+1:JK))
               IF (NOUNIT) &
                  X(J) = X(J)/AB(1,J)
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE STBSV_F90

SUBROUTINE STPSV_F90( UPLO, TRANS, DIAG, AP, X )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: DIAG, TRANS, UPLO
   REAL (KIND=WP), INTENT (IN)    :: AP(:)
   REAL (KIND=WP), INTENT (INOUT) :: X(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='STPSV '
   INTEGER                        :: J, JP, N
   LOGICAL                        :: NOUNIT, NOTRAN, UPPER
   INTRINSIC                         DOT_PRODUCT, SIZE
   UPPER = LSAME_F90(UPLO,'U')
   NOTRAN = LSAME_F90(TRANS,'N')
   NOUNIT = LSAME_F90(DIAG,'N')
   N = SIZE(X)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTRAN.AND..NOT.LSAME_F90(TRANS,'T').AND. &
            .NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (.NOT.NOUNIT.AND..NOT.LSAME_F90(DIAG,'U')) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (SIZE(AP,1)/=N*(N+1)/2) THEN
      CALL XERBLA_F90(2,SRNAME,4,5)
   ELSE 
      IF (NOTRAN) THEN
         IF (UPPER) THEN
            JP = N*(N-1)/2
            DO J = N, 1, -1
               IF (X(J)/=ZERO) THEN
                  IF (NOUNIT) &
                     X(J) = X(J)/AP(JP+J)
                  X(1:J-1) = X(1:J-1) - X(J)*AP(JP+1:JP+J-1)
               END IF
               JP = JP - J + 1
            END DO
         ELSE
            JP = 0
            DO J = 1, N
               IF (X(J)/=ZERO) THEN
                  IF (NOUNIT) &
                     X(J) = X(J)/AP(JP+J)
                  X(J+1:N) = X(J+1:N) - X(J)*AP(JP+J+1:JP+N)
               END IF
               JP = JP + N - J 
            END DO
         END IF
      ELSE
         IF (UPPER) THEN
            JP = 0
            DO J = 1, N
               X(J) = X(J) - DOT_PRODUCT(AP(JP+1:JP+J-1),X(1:J-1))
               IF (NOUNIT) &
                  X(J) = X(J)/AP(JP+J)
               JP = JP + J 
            END DO
         ELSE
            JP = N*(N-1)/2
            DO J = N, 1, -1
               X(J) = X(J) - DOT_PRODUCT(AP(JP+J+1:JP+N),X(J+1:N))
               IF (NOUNIT) &
                  X(J) = X(J)/AP(JP+J)
               JP = JP - N + J - 1 
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE STPSV_F90

SUBROUTINE SGER1_F90 ( ALPHA, X, Y, A )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:), Y(:)
   REAL (KIND=WP), INTENT (INOUT) :: A(:,:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SGER1 '
   INTEGER                        :: J, M, N
   INTRINSIC                         SIZE
   M = SIZE(X)
   N = SIZE(Y)
   IF (SIZE(A,1)/=M) THEN
      CALL XERBLA_F90(2,SRNAME,2,4)
   ELSE IF (SIZE(A,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,3,4)
   ELSE 
      IF (ALPHA/=ZERO) THEN
         DO J = 1, N
            IF (Y(J)/=ZERO) &
               A(:,J) = A(:,J) + ALPHA*X(:)*Y(J)
         END DO
      END IF
   END IF
END SUBROUTINE SGER1_F90

SUBROUTINE SSYR1_F90 ( UPLO, ALPHA, X, A )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:)
   REAL (KIND=WP), INTENT (INOUT) :: A(:,:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SSYR1 '
   INTEGER                        :: J, N
   LOGICAL                        :: UPPER
   INTRINSIC                         SIZE
   UPPER = LSAME_F90(UPLO,'U')
   N = SIZE(X)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (SIZE(A,1)/=SIZE(A,2)) THEN
      CALL XERBLA_F90(2,SRNAME,4)
   ELSE IF (SIZE(A,1)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,3,4)
   ELSE 
      IF (ALPHA/=ZERO) THEN
         IF (UPPER) THEN
            DO J = 1, N
               IF (X(J)/=ZERO) &
                  A(1:J,J) = A(1:J,J) + ALPHA*X(J)*X(1:J)
            END DO
         ELSE
            DO J = 1, N
               IF (X(J)/=ZERO) &
                  A(J:N,J) = A(J:N,J) + ALPHA*X(J)*X(J:N)
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSYR1_F90

SUBROUTINE SSPR1_F90 ( UPLO, ALPHA, X, AP )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:)
   REAL (KIND=WP), INTENT (INOUT) :: AP(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SSPR1 '
   INTEGER                        :: J, JP, N
   LOGICAL                        :: UPPER
   INTRINSIC                         SIZE
   UPPER = LSAME_F90(UPLO,'U')
   N = SIZE(X)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (SIZE(AP)/=N*(N+1)/2) THEN
      CALL XERBLA_F90(2,SRNAME,3,4)
   ELSE 
      IF (ALPHA/=ZERO) THEN
         JP = 0
         IF (UPPER) THEN
            DO J = 1, N
               IF (X(J)/=ZERO) &
                  AP(JP+1:JP+J) = AP(JP+1:JP+J) + ALPHA*X(J)*X(1:J)
               JP = JP + J
            END DO
         ELSE
            DO J = 1, N
               IF (X(J)/=ZERO) &
                  AP(JP+J:JP+N) = AP(JP+J:JP+N) + ALPHA*X(J)*X(J:N)
               JP = JP + N - J
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSPR1_F90

SUBROUTINE SSYR2_F90 ( UPLO, ALPHA, X, Y, A )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:), Y(:)
   REAL (KIND=WP), INTENT (INOUT) :: A(:,:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SSYR2 '
   INTEGER                        :: J, N
   LOGICAL                        :: UPPER
   INTRINSIC                         SIZE
   UPPER = LSAME_F90(UPLO,'U')
   N = SIZE(X)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (SIZE(A,1)/=SIZE(A,2)) THEN
      CALL XERBLA_F90(2,SRNAME,5)
   ELSE IF (SIZE(Y)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,3,4)
   ELSE IF (SIZE(A,1)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,3,5)
   ELSE 
      IF (ALPHA/=ZERO) THEN
         IF (UPPER) THEN
            DO J = 1, N
               IF (X(J)/=ZERO .OR. Y(J)/=ZERO) &
                  A(1:J,J) = A(1:J,J) + ALPHA*Y(J)*X(1:J) + ALPHA*X(J)*Y(1:J)
            END DO
         ELSE
            DO J = 1, N
               IF (X(J)/=ZERO .OR. Y(J)/=ZERO) &
                  A(J:N,J) = A(J:N,J) + ALPHA*Y(J)*X(J:N) + ALPHA*X(J)*Y(J:N)
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSYR2_F90

SUBROUTINE SSPR2_F90 ( UPLO, ALPHA, X, Y, AP )
!
!  Experimental Fortran 90 version of Level 2 BLAS.
!  Jeremy Du Croz, NAG, 27 September 1991.
!
   USE                               LA_PRECISION, ONLY: WP
   USE                               BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT (IN)    :: UPLO
   REAL (KIND=WP), INTENT (IN)    :: ALPHA
   REAL (KIND=WP), INTENT (IN)    :: X(:), Y(:)
   REAL (KIND=WP), INTENT (INOUT) :: AP(:)
   REAL (KIND=WP), PARAMETER      :: ZERO=0.0_WP
   CHARACTER*6   , PARAMETER      :: SRNAME='SSPR2 '
   INTEGER                        :: J, JP, N
   LOGICAL                        :: UPPER
   INTRINSIC                         SIZE
   UPPER = LSAME_F90(UPLO,'U')
   N = SIZE(X)
   IF (.NOT.UPPER.AND..NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (SIZE(Y)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,3,4)
   ELSE IF (SIZE(AP)/=N*(N+1)/2) THEN
      CALL XERBLA_F90(2,SRNAME,3,5)
   ELSE 
      IF (ALPHA/=ZERO) THEN
         JP = 0
         IF (UPPER) THEN
            DO J = 1, N
               IF (X(J)/=ZERO .OR. Y(J)/=ZERO) &
                  AP(JP+1:JP+J) = AP(JP+1:JP+J) + ALPHA*Y(J)*X(1:J) &
                                                + ALPHA*X(J)*Y(1:J)
               JP = JP + J
            END DO
         ELSE
            DO J = 1, N
               IF (X(J)/=ZERO .OR. Y(J)/=ZERO) &
                  AP(JP+J:JP+N) = AP(JP+J:JP+N) + ALPHA*Y(J)*X(J:N) &
                                                + ALPHA*X(J)*Y(J:N)
               JP = JP + N - J
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSPR2_F90

