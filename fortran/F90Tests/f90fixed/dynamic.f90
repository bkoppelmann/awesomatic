      implicit none

      integer :: n, i, j
      real :: time, second

      type test
        integer, pointer :: list(:)
      end type test

      type(test) :: rows
      allocatable :: rows(:)


      n = 2000
      print *,'n = ',n

      allocate(rows(n))

      do i = 1,  n
         allocate(rows(i)%list(n))
         do j = 1, n
            rows(i)%list(j) = 123.0
         end do
      end do

      time = second()
      do i = 1,  n
         do j = 1, n
            rows(i)%list(j) = rows(i)%list(j)**3 + 10.0*rows(i)%list(j)
         end do
      end do
      print*,'total time: ', second()-time, ' (sec)'


      stop
      end
      real function second()
      real junk, tarray(2)
      junk = etime(tarray)
      second = tarray(1)
      return
      end
