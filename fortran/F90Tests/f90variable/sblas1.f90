!
!  Whatever BLAS1 subroutines that I have converted.
!

MODULE SBLAS1

INTERFACE

REAL FUNCTION SLAPY2_F90(X, Y)
   USE LA_PRECISION, ONLY:WP
   REAL(KIND=WP), INTENT(IN)    :: X, Y
END FUNCTION SLAPY2_F90

END INTERFACE

END MODULE SBLAS1

REAL FUNCTION SLAPY2_F90(X, Y)
!
!  -- LAPACK routine (preliminary version) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     August 6, 1991
!
!  Modified from LAPACK code by Steve Moulton.  5/13/92
!     .. Arguments ..
!

   USE LA_PRECISION, ONLY:WP

   IMPLICIT NONE

   REAL(KIND=WP), INTENT(IN)    :: X, Y
!
!  Purpose
!  =======
!
!  SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
!  overflow.
!
!  Arguments
!  =========
!
!  X   (input) REAL
!  Y   (input) REAL
!      X and Y specify the values x and y.
!
!  Parameters
!
   REAL(KIND=WP)      :: ZERO = 0.0_WP, ONE=1.0_WP
!
!  Local Scalars
!
   REAL(KIND=WP)      :: W, Z, XABS, YABS
!     ..
!     .. Intrinsic Functions ..
!
   INTRINSIC       MAX, MIN
!     ..
!     .. Executable Statements ..
!
   XABS = ABS( X )
   YABS = ABS( Y )
   W = MAX( XABS, YABS )
   Z = MIN( XABS, YABS )
   IF( Z.EQ.ZERO ) THEN
      SLAPY2_F90 = W
   ELSE
      SLAPY2_F90 = W*SQRT( ONE+( Z / W )**2 )
   END IF
   RETURN
END
