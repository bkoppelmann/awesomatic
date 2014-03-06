MODULE SPRTMT_F90_M

INTERFACE

SUBROUTINE SPRTMT_F90(A, TEXT)
   USE LA_PRECISION, ONLY: WP
   REAL (KIND=WP), INTENT(IN) :: A(:,:)
   CHARACTER*(*)      :: TEXT
END SUBROUTINE SPRTMT_F90

END INTERFACE

END MODULE SPRTMT_F90_M

subroutine sprtmt_f90(a, text)
   USE LA_PRECISION, ONLY: WP
   real (kind=wp), intent(inout) :: a(:,:)
   character*(*) text
   integer m, n, jmin

   m = size(a,1)
   n = size(a,2)
   jmin = min(n, 11)

   print *, text
   do i = 1, min(m, 11)
      write (6,100) (a(i:i,j:j), j = 1, jmin)
 100  format(11(f7.3))
   enddo
   return
   end
