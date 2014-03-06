!
!  Need some extra modules?  Lets put them here!
!
MODULE LA_PRECISION
!
!   USE F90_KIND, ONLY:SINGLE                     ! from original code
!   INTEGER, PARAMETER:: WP=SINGLE                ! from original code
!
!   Define a WP and INT32 parameters (used to define kind).  This
!   mod made by John Prentice to make this F90 code portable
!
INTEGER, PARAMETER :: WP = selected_real_kind(6,20)
INTEGER, PARAMETER :: INT32 = selected_int_kind(8)
END MODULE LA_PRECISION

!MODULE LAPACK
!
!INTERFACE
!
!SUBROUTINE SPOTRF( UPLO, A, INFO )
!   USE  LA_PRECISION, ONLY:WP
!   CHARACTER*1, INTENT( IN )    :: UPLO
!   INTEGER    , INTENT( OUT )   :: INFO
!   REAL(KIND=WP), INTENT( INOUT ) :: A( :, : )
!END SUBROUTINE SPOTRF
!
!END INTERFACE
!
!END MODULE LAPACK
