      program tpoch
c
c      test the F77 version of the Pochhammer code
c
      integer nmax,mmax
      double precision zero
      parameter (nmax = 100,mmax=10000,zero=0.0)
c
      double precision poch,z,result,check
      integer n,m,ntry
      external poch
c
      ntry = 0
      check = zero
      do 20 m = 1,mmax
          do 10 n = 1,nmax
              z = dble(n)
              result = poch(z,n)
              check = check + result
              ntry = ntry + 1
   10     continue
   20 continue
c
      write (*,'(/'' F77 Pochhammer test complete for '',i9,'' trys''
     &                   /'' Summation value = '',1pd15.5/)') ntry,check 
c
      end
      double precision function poch (z,n)
c
c        calculate the Pochhammer symbol (z)
c                                           n
c
c        where z is real and m is an integer.  We require that n be
c        non-negative.
c
c        Pochhammer's symbol is defined as:
c
c        (z)  = 1,
c           0
c
c        (z)  = z(z+1)(z+2) ... (z+n-1)
c           n
c
c        See "Handbook of Mathematical Functions", M. Abramowitz and
c        I. A. Stegun, Dover Publications Inc., 1972, page 256
c
      double precision one,z
      integer k,n
      parameter (one=1.0)
c
      if (n.eq.0) then
          poch = one
      else
          poch=z
          do 10 k=1,n-1
              poch=poch*(z+dble(k))
   10     continue
      endif
c
      end
