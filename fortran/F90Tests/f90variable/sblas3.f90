MODULE SBLAS3

INTERFACE

SUBROUTINE SGEMM_F90( TRANSA, TRANSB, ALPHA, A, B, BETA, C)
   USE                              LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT(IN)    :: TRANSA, TRANSB
   REAL (KIND=WP), INTENT(IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:), B(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: C(:,:)
END SUBROUTINE SGEMM_F90

SUBROUTINE SSYMM_F90(SIDE,UPLO,ALPHA,A,B,BETA,C)
   USE                              LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT(IN)    :: SIDE, UPLO
   REAL (KIND=WP), INTENT(IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:), B(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: C(:,:)
END SUBROUTINE SSYMM_F90

SUBROUTINE SSYRK_F90( UPLO, TRANS, ALPHA, A, BETA, C )
   USE                              LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT(IN)    :: UPLO, TRANS
   REAL (KIND=WP), INTENT(IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: C(:,:)
END SUBROUTINE SSYRK_F90

SUBROUTINE SSYR2K_F90( UPLO, TRANS, ALPHA, A, B, BETA, C )
   USE                              LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT(IN)    :: UPLO, TRANS
   REAL (KIND=WP), INTENT(IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:), B(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: C(:,:)
END SUBROUTINE SSYR2K_F90

SUBROUTINE STRMM_F90( SIDE, UPLO, TRANSA, DIAG, ALPHA, A, B )
   USE                              LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT(IN)    :: SIDE, UPLO, TRANSA, DIAG
   REAL (KIND=WP), INTENT(IN)    :: ALPHA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: B(:,:)
END SUBROUTINE STRMM_F90

SUBROUTINE STRSM_F90( SIDE, UPLO, TRANSA, DIAG, ALPHA, A, B )
   USE                              LA_PRECISION, ONLY: WP
   CHARACTER*1   , INTENT(IN)    :: SIDE, UPLO, TRANSA, DIAG
   REAL (KIND=WP), INTENT(IN)    :: ALPHA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: B(:,:)
END SUBROUTINE STRSM_F90

END INTERFACE

END MODULE SBLAS3

! ------------------------------------------------------------------------k
SUBROUTINE SGEMM_F90( TRANSA, TRANSB, ALPHA, A, B, BETA, C)
!
!  Experimental Fortran 90 version of Level 3 BLAS.
!  Jeremy Du Croz, NAG, 2 October 1991.
!
   USE                              LA_PRECISION, ONLY: WP
   USE                              BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT(IN)    :: TRANSA, TRANSB
   REAL (KIND=WP), INTENT(IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:), B(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: C(:,:)
   REAL (KIND=WP), PARAMETER     :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER     :: SRNAME='SGEMM '
   REAL (KIND=WP)   TEMP
   INTEGER          I, J, K, L, M, N
   LOGICAL          NOTA, NOTB
   NOTA = LSAME_F90(TRANSA,'N')
   NOTB = LSAME_F90(TRANSB,'N')
   M = SIZE(C,1)
   N = SIZE(C,2)
   IF (.NOT.NOTA .AND. .NOT.LSAME_F90(TRANSA,'C') .AND. .NOT.LSAME_F90(TRANSA,'T')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTB .AND. .NOT.LSAME_F90(TRANSB,'C') .AND. &
            .NOT.LSAME_F90(TRANSB,'T')) &
      THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (NOTA.AND.NOTB.AND.SIZE(A,2)/=SIZE(B,1) .OR. &
            NOTA.AND..NOT.NOTB.AND.SIZE(A,2)/=SIZE(B,2) .OR. &
            .NOT.NOTA.AND.NOTB.AND.SIZE(A,1)/=SIZE(B,1) .OR. &
            .NOT.NOTA.AND..NOT.NOTB.AND.SIZE(A,1)/=SIZE(B,2)) THEN
      CALL XERBLA_F90(2,SRNAME,4,5)
   ELSE IF (NOTA.AND.SIZE(A,1)/=M .OR. .NOT.NOTA.AND.SIZE(A,2)/=M) THEN
      CALL XERBLA_F90(2,SRNAME,4,7)
   ELSE IF (NOTB.AND.SIZE(B,2)/=N .OR. .NOT.NOTB.AND.SIZE(B,1)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,5,7)
   ELSE
      IF (NOTB) THEN
         K = SIZE(B,1)
         IF (NOTA) THEN
            DO J = 1, N
               IF (BETA.EQ.ZERO) THEN
                  C(:,J) = ZERO
               ELSE IF (BETA.NE.ONE) THEN
                  C(:,J) = BETA*C(:,J)
               END IF
               DO L = 1, K
                  IF (B(L,J).NE.ZERO) &
                     C(:,J) = C(:,J) + ALPHA*B(L,J)*A(:,L)
               END DO
            END DO
         ELSE
            DO J = 1, N
               DO I = 1, M
                  TEMP = DOT_PRODUCT(A(:,I),B(:,J))
                  IF (BETA.EQ.ZERO) THEN
                     C(I,J) = ALPHA*TEMP
                  ELSE
                     C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                  END IF
               END DO
            END DO
         END IF
      ELSE
         K = SIZE(B,2)
         IF (NOTA) THEN
            DO J = 1, N
               IF (BETA.EQ.ZERO) THEN
                  C(:,J) = ZERO
               ELSE IF (BETA.NE.ONE) THEN
                  C(:,J) = BETA*C(:,J)
               END IF
               DO L = 1, K
                  IF (B(J,L).NE.ZERO) &
                     C(:,J) = C(:,J) + ALPHA*B(J,L)*A(:,L)
               END DO
            END DO
         ELSE
            DO J = 1, N
               DO I = 1, M
                  TEMP = DOT_PRODUCT(A(:,I),B(J,:))
                  IF (BETA.EQ.ZERO) THEN
                     C(I,J) = ALPHA*TEMP
                  ELSE
                     C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                  END IF
               END DO
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SGEMM_F90

SUBROUTINE SSYMM_F90(SIDE,UPLO,ALPHA,A,B,BETA,C)
!
!  Experimental Fortran 90 version of Level 3 BLAS.
!  Jeremy Du Croz, NAG, 2 October 1991.
!
   USE                              LA_PRECISION, ONLY: WP
   USE                              BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT(IN)    :: SIDE, UPLO
   REAL (KIND=WP), INTENT(IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:), B(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: C(:,:)
   REAL (KIND=WP), PARAMETER     :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER     :: SRNAME='SSYMM '
   INTRINSIC        DOT_PRODUCT, SIZE
   REAL (KIND=WP)   TEMP1, TEMP2
   INTEGER          I, J, K, M, N
   LOGICAL          LSIDE, UPPER
   LSIDE = LSAME_F90(SIDE,'L')
   UPPER = LSAME_F90(UPLO,'U')
   M = SIZE(C,1)
   N = SIZE(C,2)
   IF (.NOT.LSIDE .AND. .NOT.LSAME_F90(SIDE,'R')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.UPPER .AND. .NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (SIZE(A,1)/=SIZE(A,2)) THEN
      CALL XERBLA_F90(2,SRNAME,4)
   ELSE IF (LSIDE.AND.SIZE(A,1)/=SIZE(B,1) .OR. &
            .NOT.LSIDE.AND.SIZE(A,1)/=SIZE(B,2)) THEN
      CALL XERBLA_F90(2,SRNAME,4,5)
   ELSE IF (SIZE(B,1)/=M .OR. SIZE(B,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,5,7)
   ELSE
      IF (LSIDE) THEN
!
!        Form  C := alpha*A*B + beta*C
!
         IF (UPPER) THEN
            DO J = 1, N
               DO I = 1, M
                  TEMP1 = ALPHA*B(I,J)
                  C(1:I-1,J) = C(1:I-1,J) + TEMP1*A(1:I-1,I)
                  TEMP2 = DOT_PRODUCT(B(1:I-1,J),A(1:I-1,I))
                  IF (BETA.EQ.ZERO) THEN
                     C(I,J) = TEMP1*A(I,I) + ALPHA*TEMP2
                  ELSE
                     C(I,J) = BETA*C(I,J) + TEMP1*A(I,I) + ALPHA*TEMP2
                  END IF
               END DO
            END DO
         ELSE
            DO J = 1, N
               DO I = M, 1, -1
                  TEMP1 = ALPHA*B(I,J)
                  C(I+1:M,J) = C(I+1:M,J) + TEMP1*A(I+1:M,I)
                  TEMP2 = DOT_PRODUCT(B(I+1:M,J),A(I+1:M,I))
                  IF (BETA.EQ.ZERO) THEN
                     C(I,J) = TEMP1*A(I,I) + ALPHA*TEMP2
                  ELSE
                     C(I,J) = BETA*C(I,J) + TEMP1*A(I,I) + ALPHA*TEMP2
                  END IF
               END DO
            END DO
         END IF
      ELSE
!
!        Form  C := alpha*B*A + beta*C
!
         DO J = 1, N
            TEMP1 = ALPHA*A(J,J)
            IF (BETA.EQ.ZERO) THEN
               C(:,J) = TEMP1*B(:,J)
            ELSE
               C(:,J) = BETA*C(:,J) + TEMP1*B(:,J)
            END IF
            DO K = 1, J - 1
               IF (UPPER) THEN
                  TEMP1 = ALPHA*A(K,J)
               ELSE
                  TEMP1 = ALPHA*A(J,K)
               END IF
               C(:,J) = C(:,J) + TEMP1*B(:,K)
            END DO
            DO K = J + 1, N
               IF (UPPER) THEN
                  TEMP1 = ALPHA*A(J,K)
               ELSE
                  TEMP1 = ALPHA*A(K,J)
               END IF
               C(:,J) = C(:,J) + TEMP1*B(:,K)
            END DO
         END DO
      END IF
   END IF
END SUBROUTINE SSYMM_F90

SUBROUTINE SSYRK_F90( UPLO, TRANS, ALPHA, A, BETA, C )
!
!  Experimental Fortran 90 version of Level 3 BLAS.
!  Jeremy Du Croz, NAG, 2 October 1991.
!
   USE                              LA_PRECISION, ONLY: WP
   USE                              BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90 
   IMPLICIT NONE
   CHARACTER*1   , INTENT(IN)    :: UPLO, TRANS
   REAL (KIND=WP), INTENT(IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: C(:,:)
   REAL (KIND=WP), PARAMETER     :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER     :: SRNAME='SSYRK '
   INTRINSIC        DOT_PRODUCT, SIZE
   REAL (KIND=WP)   TEMP
   INTEGER          I, J, K, L, N
   LOGICAL          NOTA, UPPER
   UPPER = LSAME_F90(UPLO,'U')
   NOTA = LSAME_F90(TRANS,'N')
   N = SIZE(C,1)
   IF (.NOT.UPPER .AND. .NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTA .AND. .NOT.LSAME_F90(TRANS,'T') .AND. &
            .NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (SIZE(C,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,6)
   ELSE IF (NOTA.AND.SIZE(A,1)/=N .OR. .NOT.NOTA.AND.SIZE(A,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4,6)
   ELSE
      IF (NOTA) THEN
!
!        Form  C := alpha*A*A' + beta*C.
!
         K = SIZE(A,2)
         IF (UPPER) THEN
            DO J = 1, N
               IF (BETA==ZERO) THEN
                  C(1:J,J) = ZERO
               ELSE IF (BETA/=ONE) THEN
                  C(1:J,J) = BETA*C(1:J,J)
               END IF
               DO L = 1, K
                  IF (A(J,L)/=ZERO) &
                     C(1:J,J) = C(1:J,J) + ALPHA*A(J,L)*A(1:J,L)
               END DO
            END DO
         ELSE
            DO J = 1,N
               IF (BETA==ZERO) THEN
                  C(J:N,J) = ZERO
               ELSE IF (BETA/=ONE) THEN
                  C(J:N,J) = BETA*C(J:N,J)
               END IF
               DO L = 1,K
                  IF (A(J,L)/=ZERO) &
                     C(J:N,J) = C(J:N,J) + ALPHA*A(J,L)*A(J:N,L)
               END DO
            END DO
         END IF
      ELSE
!
!        Form  C := alpha*A'*A + beta*C.
!
         K = SIZE(A,1)
         IF (UPPER) THEN
            DO J = 1,N
               DO I = 1,J
                  TEMP = DOT_PRODUCT(A(1:K,I),A(1:K,J))
                  IF (BETA==ZERO) THEN
                     C(I,J) = ALPHA*TEMP
                  ELSE
                     C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                  END IF
               END DO
            END DO
         ELSE
            DO J = 1,N
               DO I = J,N
                  TEMP = DOT_PRODUCT(A(1:K,I),A(1:K,J))
                  IF (BETA==ZERO) THEN
                     C(I,J) = ALPHA*TEMP
                  ELSE
                     C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                  END IF
               END DO
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSYRK_F90

SUBROUTINE SSYR2K_F90( UPLO, TRANS, ALPHA, A, B, BETA, C )
!
!  Experimental Fortran 90 version of Level 3 BLAS.
!  Jeremy Du Croz, NAG, 2 October 1991.
!
   USE                              LA_PRECISION, ONLY: WP
   USE                              BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT(IN)    :: UPLO, TRANS
   REAL (KIND=WP), INTENT(IN)    :: ALPHA, BETA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:), B(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: C(:,:)
   REAL (KIND=WP), PARAMETER     :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER     :: SRNAME='SSYR2K'
   INTRINSIC        DOT_PRODUCT, SIZE
   REAL (KIND=WP)   TEMP1, TEMP2
   INTEGER          I, J, K, L, N
   LOGICAL          NOTA, UPPER
   UPPER = LSAME_F90(UPLO,'U')
   NOTA = LSAME_F90(TRANS,'N')
   N = SIZE(C,1)
   IF (.NOT.UPPER .AND. .NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.NOTA .AND. .NOT.LSAME_F90(TRANS,'T') .AND. &
            .NOT.LSAME_F90(TRANS,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (SIZE(C,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,7)
   ELSE IF (SIZE(A,1)/=SIZE(B,1) .OR. SIZE(A,2)/=SIZE(B,2)) THEN
      CALL XERBLA_F90(2,SRNAME,4,5)
   ELSE IF (NOTA.AND.SIZE(A,1)/=N .OR. .NOT.NOTA.AND.SIZE(A,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,4,7)
   ELSE IF (NOTA.AND.SIZE(B,1)/=N .OR. .NOT.NOTA.AND.SIZE(B,2)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,5,7)
   ELSE
      IF (NOTA) THEN
!
!        Form  C := alpha*A*B' + alpha*B*A' + beta*C
!
         K = SIZE(A,2)
         IF (UPPER) THEN
            DO J = 1, N
               IF (BETA==ZERO) THEN
                  C(1:J,J) = ZERO
               ELSE IF (BETA/=ONE) THEN
                  C(1:J,J) = BETA*C(1:J,J)
               END IF
               DO L = 1, K
                  IF (A(J,L)/=ZERO .OR. B(J,L)/=ZERO) &
                     C(1:J,J) = C(1:J,J) + ALPHA*B(J,L)*A(1:J,L) &
                                         + ALPHA*A(J,L)*B(1:J,L)
               END DO
            END DO
         ELSE
            DO J = 1, N
               IF (BETA==ZERO) THEN
                  C(J:N,J) = ZERO
               ELSE IF (BETA/=ONE) THEN
                  C(J:N,J) = BETA*C(J:N,J)
               END IF
               DO L = 1, K
                  IF (A(J,L)/=ZERO .OR. B(J,L)/=ZERO) &
                     C(J:N,J) = C(J:N,J) + ALPHA*B(J,L)*A(J:N,L) &
                                         + ALPHA*A(J,L)*B(J:N,L)
               END DO
            END DO
         END IF
      ELSE
!
!        Form  C := alpha*A'*B + alpha*B'*A + beta*C
!
         K = SIZE(A,1)
         IF (UPPER) THEN
            DO J = 1, N
               DO I = 1, J
                  TEMP1 = DOT_PRODUCT(A(1:K,I),B(1:K,J))
                  TEMP2 = DOT_PRODUCT(B(1:K,I),A(1:K,J))
                  IF (BETA==ZERO) THEN
                     C(I,J) = ALPHA*TEMP1 + ALPHA*TEMP2
                  ELSE
                     C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 + ALPHA*TEMP2
                  END IF
               END DO
            END DO
         ELSE
            DO J = 1, N
               DO I = J, N
                  TEMP1 = DOT_PRODUCT(A(1:K,I),B(1:K,J))
                  TEMP2 = DOT_PRODUCT(B(1:K,I),A(1:K,J))
                  IF (BETA==ZERO) THEN
                     C(I,J) = ALPHA*TEMP1 + ALPHA*TEMP2
                  ELSE
                     C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 + ALPHA*TEMP2
                  END IF
               END DO
            END DO
         END IF
      END IF
   END IF
END SUBROUTINE SSYR2K_F90

SUBROUTINE STRMM_F90( SIDE, UPLO, TRANSA, DIAG, ALPHA, A, B )
!
!  Experimental Fortran 90 version of Level 3 BLAS.
!  Jeremy Du Croz, NAG, 2 October 1991.
!
   USE                              LA_PRECISION, ONLY: WP
   USE                              BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT(IN)    :: SIDE, UPLO, TRANSA, DIAG
   REAL (KIND=WP), INTENT(IN)    :: ALPHA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: B(:,:)
   REAL (KIND=WP), PARAMETER     :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER     :: SRNAME='STRMM '
   INTRINSIC        DOT_PRODUCT, SIZE
   REAL (KIND=WP)   TEMP
   INTEGER          I, J, K, M, N
   LOGICAL          LSIDE, NOTA, NOUNIT, UPPER
   LSIDE = LSAME_F90(SIDE,'L')
   UPPER = LSAME_F90(UPLO,'U')
   NOTA = LSAME_F90(TRANSA,'N')
   NOUNIT = LSAME_F90(DIAG,'N')
   M = SIZE(B,1)
   N = SIZE(B,2)
   IF (.NOT.LSIDE .AND. .NOT.LSAME_F90(SIDE,'R')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.UPPER .AND. .NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (.NOT.NOTA .AND. .NOT.LSAME_F90(TRANSA,'T') .AND. &
            .NOT.LSAME_F90(TRANSA,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (.NOT.NOUNIT .AND. .NOT.LSAME_F90(DIAG,'U')) THEN
      CALL XERBLA_F90(1,SRNAME,4)
   ELSE IF (SIZE(A,1)/=SIZE(A,2)) THEN
      CALL XERBLA_F90(2,SRNAME,6)
   ELSE IF (LSIDE.AND.SIZE(A,1)/=M .OR. .NOT.LSIDE.AND.SIZE(A,1)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,6,7)
   ELSE IF (ALPHA==ZERO) THEN
      B(:,:) = ZERO
   ELSE
      IF (LSIDE) THEN
         IF (NOTA) THEN
!
!           Form  B := alpha*A*B
!
            IF (UPPER) THEN
               DO J = 1, N
                  DO K = 1, M
                     IF (B(K,J)/=ZERO) THEN
                        TEMP = ALPHA*B(K,J)
                        B(1:K-1,J) = B(1:K-1,J) + TEMP*A(1:K-1,K)
                        IF (NOUNIT) &
                           TEMP = TEMP*A(K,K)
                        B(K,J) = TEMP
                     END IF
                  END DO
               END DO
            ELSE
               DO J = 1, N
                  DO K = M, 1, -1
                     IF (B(K,J)/=ZERO) THEN
                        TEMP = ALPHA*B(K,J)
                        B(K,J) = TEMP
                        IF (NOUNIT) &
                           B(K,J) = B(K,J)*A(K,K)
                        B(K+1:M,J) = B(K+1:M,J) + TEMP*A(K+1:M,K)
                     END IF
                  END DO
               END DO
            END IF
         ELSE
!
!           Form  B := alpha*A'*B
!
            IF (UPPER) THEN
               DO J = 1, N
                  DO I = M, 1, -1
                     TEMP = B(I,J)
                     IF (NOUNIT) &
                        TEMP = TEMP*A(I,I)
                     TEMP = TEMP + DOT_PRODUCT(A(1:I-1,I),B(1:I-1,J))
                     B(I,J) = ALPHA*TEMP
                  END DO
               END DO
            ELSE
               DO J = 1, N
                  DO I = 1, M
                     TEMP = B(I,J)
                     IF (NOUNIT) &
                        TEMP = TEMP*A(I,I)
                     TEMP = TEMP + DOT_PRODUCT(A(I+1:M,I),B(I+1:M,J))
                     B(I,J) = ALPHA*TEMP
                  END DO
               END DO
            END IF
         END IF
      ELSE
         IF (NOTA) THEN
!
!           Form  B := alpha*B*A
!
            IF (UPPER) THEN
               DO J = N, 1, -1
                  TEMP = ALPHA
                  IF (NOUNIT) &
                     TEMP = TEMP*A(J,J)
                  B(:,J) = TEMP*B(:,J)
                  DO K = 1, J - 1
                     IF (A(K,J)/=ZERO) &
                        B(:,J) = B(:,J) + ALPHA*A(K,J)*B(:,K)
                  END DO
               END DO
            ELSE
               DO J = 1, N
                  TEMP = ALPHA
                  IF (NOUNIT) &
                     TEMP = TEMP*A(J,J)
                  B(:,J) = TEMP*B(:,J)
                  DO K = J + 1, N
                     IF (A(K,J)/=ZERO) &
                        B(:,J) = B(:,J) + ALPHA*A(K,J)*B(:,K)
                  END DO
               END DO
            END IF
         ELSE
!
!           Form  B := alpha*B*A'
!
            IF (UPPER) THEN
               DO K = 1, N
                  DO J = 1, K - 1
                     IF (A(J,K)/=ZERO) &
                        B(:,J) = B(:,J) + ALPHA*A(J,K)*B(:,K)
                  END DO
                  TEMP = ALPHA
                  IF (NOUNIT) &
                     TEMP = TEMP*A(K,K)
                  IF (TEMP/=ONE) THEN
                     B(:,K) = TEMP*B(:,K)
                  END IF
               END DO
            ELSE
               DO K = N, 1, -1
                  DO J = K + 1, N
                     IF (A(J,K)/=ZERO) &
                        B(:,J) = B(:,J) + ALPHA*A(J,K)*B(:,K)
                  END DO
                  TEMP = ALPHA
                  IF (NOUNIT) &
                     TEMP = TEMP*A(K,K)
                  IF (TEMP/=ONE) &
                     B(:,K) = TEMP*B(:,K)
               END DO
            END IF
         END IF
      END IF
   END IF
END SUBROUTINE STRMM_F90

SUBROUTINE STRSM_F90( SIDE, UPLO, TRANSA, DIAG, ALPHA, A, B )
!
!  Experimental Fortran 90 version of Level 3 BLAS.
!  Jeremy Du Croz, NAG, 2 October 1991.
!
   USE                              LA_PRECISION, ONLY: WP
   USE                              BLAS_AUX, ONLY: LSAME_F90, XERBLA_F90
   IMPLICIT NONE
   CHARACTER*1   , INTENT(IN)    :: SIDE, UPLO, TRANSA, DIAG
   REAL (KIND=WP), INTENT(IN)    :: ALPHA
   REAL (KIND=WP), INTENT(IN)    :: A(:,:)
   REAL (KIND=WP), INTENT(INOUT) :: B(:,:)
   REAL (KIND=WP), PARAMETER     :: ZERO=0.0_WP, ONE=1.0_WP
   CHARACTER*6   , PARAMETER     :: SRNAME='STRSM '
   INTRINSIC        DOT_PRODUCT, SIZE
   REAL (KIND=WP)   TEMP
   INTEGER          I, J, K, M, N
   LOGICAL          LSIDE, NOTA, NOUNIT, UPPER
   LSIDE = LSAME_F90(SIDE,'L')
   UPPER = LSAME_F90(UPLO,'U')
   NOTA = LSAME_F90(TRANSA,'N')
   NOUNIT = LSAME_F90(DIAG,'N')
   M = SIZE(B,1)
   N = SIZE(B,2)
   IF (.NOT.LSIDE .AND. .NOT.LSAME_F90(SIDE,'R')) THEN
      CALL XERBLA_F90(1,SRNAME,1)
   ELSE IF (.NOT.UPPER .AND. .NOT.LSAME_F90(UPLO,'L')) THEN
      CALL XERBLA_F90(1,SRNAME,2)
   ELSE IF (.NOT.NOTA .AND. .NOT.LSAME_F90(TRANSA,'T') .AND. &
            .NOT.LSAME_F90(TRANSA,'C')) THEN
      CALL XERBLA_F90(1,SRNAME,3)
   ELSE IF (.NOT.NOUNIT .AND. .NOT.LSAME_F90(DIAG,'U')) THEN
      CALL XERBLA_F90(1,SRNAME,4)
   ELSE IF (SIZE(A,1)/=SIZE(A,2)) THEN
      CALL XERBLA_F90(2,SRNAME,6)
   ELSE IF (LSIDE.AND.SIZE(A,1)/=M .OR. .NOT.LSIDE.AND.SIZE(A,1)/=N) THEN
      CALL XERBLA_F90(2,SRNAME,6,7)
   ELSE IF (ALPHA==ZERO) THEN
      B(:,:) = ZERO
   ELSE
      IF (LSIDE) THEN
         IF (NOTA) THEN
!
!           Form  B := alpha*inv(A)*B
!
            IF (UPPER) THEN
               DO J = 1, N
                  IF (ALPHA/=ONE) &
                     B(:,J) = ALPHA*B(:,J)
                  DO K = M, 1, -1
                     IF (B(K,J)/=ZERO) THEN
                        IF (NOUNIT) &
                           B(K,J) = B(K,J)/A(K,K)
                        B(1:K-1,J) = B(1:K-1,J) - B(K,J)*A(1:K-1,K)
                     END IF
                  END DO
               END DO
            ELSE
               DO J = 1, N
                  IF (ALPHA/=ONE) &
                     B(:,J) = ALPHA*B(:,J)
                  DO K = 1, M
                     IF (B(K,J)/=ZERO) THEN
                        IF (NOUNIT) &
                           B(K,J) = B(K,J)/A(K,K)
                        B(K+1:M,J) = B(K+1:M,J) - B(K,J)*A(K+1:M,K)
                     END IF
                  END DO
               END DO
            END IF
         ELSE
!
!           Form  B := alpha*inv(A')*B
!
            IF (UPPER) THEN
               DO J = 1, N
                  DO I = 1, M
                     TEMP = ALPHA*B(I,J) - DOT_PRODUCT(A(1:I-1,I),B(1:I-1,J))
                     IF (NOUNIT) &
                        TEMP = TEMP/A(I,I)
                     B(I,J) = TEMP
                  END DO
               END DO
            ELSE
               DO J = 1, N
                  DO I = M, 1, -1
                     TEMP = ALPHA*B(I,J) - DOT_PRODUCT(A(I+1:M,I),B(I+1:M,J))
                     IF (NOUNIT) &
                        TEMP = TEMP/A(I,I)
                     B(I,J) = TEMP
                  END DO
               END DO
            END IF
         END IF
      ELSE
         IF (NOTA) THEN
!
!           Form  B := alpha*B*inv(A)
!
            IF (UPPER) THEN
               DO J = 1, N
                  IF (ALPHA/=ONE) &
                     B(:,J) = ALPHA*B(:,J)
                  DO K = 1, J - 1
                     IF (A(K,J)/=ZERO) &
                        B(:,J) = B(:,J) - A(K,J)*B(:,K)
                  END DO
                  IF (NOUNIT) &
                     B(:,J) = B(:,J)/A(J,J)
               END DO
            ELSE
               DO J = N, 1, -1
                  IF (ALPHA/=ONE) &
                     B(:,J) = ALPHA*B(:,J)
                  DO K = J + 1, N
                     IF (A(K,J)/=ZERO ) &
                        B(:,J) = B(:,J) - A(K,J)*B(:,K)
                  END DO
                  IF (NOUNIT) &
                     B(:,J) = B(:,J)/A(J,J)
               END DO
            END IF
         ELSE
!
!           Form  B := alpha*B*inv(A')
!
            IF (UPPER) THEN
               DO K = N, 1, -1
                  IF (NOUNIT ) &
                     B(:,K) = B(:,K)/A(K,K)
                  DO J = 1,K - 1
                     IF (A(J,K)/=ZERO) &
                        B(:,J) = B(:,J) - A(J,K)*B(:,K)
                  END DO
                  IF (ALPHA/=ONE) &
                     B(:,K) = ALPHA*B(:,K)
               END DO
            ELSE
               DO K = 1, N
                  IF (NOUNIT) &
                     B(:,K) = B(:,K)/A(K,K)
                  DO J = K + 1, N
                     IF (A(J,K)/=ZERO) &
                        B(:,J) = B(:,J) - A(J,K)*B(:,K)
                  END DO
                  IF (ALPHA/=ONE) &
                     B(:,K) = ALPHA*B(:,K)
               END DO
            END IF
         END IF
      END IF
   END IF
END SUBROUTINE STRSM_F90

