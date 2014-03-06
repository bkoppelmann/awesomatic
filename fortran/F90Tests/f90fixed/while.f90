      program test
      integer  i,j,a(20),b(40),c(10,10)
      real x
      logical  d(20)
      x = 3.1415
      j = 1
      a(1:20) = b(1:40:2) + 1
      d = .TRUE.
      do  i = 1,20
         a(i) = sin (x)
      end do
      do while (i .gt. j)
         c(i,j) = 1
         i = i - 1
      end do
      end
