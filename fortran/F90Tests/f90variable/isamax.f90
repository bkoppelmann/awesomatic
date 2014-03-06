INTEGER FUNCTION ISAMAX_F90(X)
!
!  Quick and dirty - find index for vector elt w/ max abs value.
!
   USE LA_PRECISION, ONLY:WP
   USE BLAS_AUX, ONLY: XERBLA_F90
   IMPLICIT NONE

   REAL(KIND=WP), INTENT(IN)    :: X(:)

   INTEGER                      :: MAXI, ILIM, I

   ILIM = SIZE(X,1)
   IF (ILIM .LT. 1) THEN
      CALL XERBLA_F90(1, 'ISAMAX', 1)
      RETURN
   ENDIF

   IF (ILIM .EQ. 1) THEN
      ISAMAX_F90 = 1
      RETURN
   ENDIF

   MAXI = 1
   DO I = 2, ILIM, 1
      IF (ABS(X(I)) .GT. ABS(X(MAXI))) MAXI = I
   ENDDO
   ISAMAX_F90 = MAXI
   RETURN
END



   
