!
!  F90 dummies for testing.  Here we make sure that subroutines that
!  we have replaced are not inadvertently called, and subroutines that
!  we have not replaced, yet rely on subroutines replaced, are not
!  called.
!
!  Subroutines to ignore
!
       subroutine ignore(insub)
       character*(*) insub
       print *, "Subroutine ", insub, " called, ignored"
       return
       end

!       subroutine sgetrs
!       call ignore("sgetrs")
!       return
!       end

! ====================================================================== 
!  Subroutines to die for
!
       subroutine die(insub)
       character*(*) insub
       print *, "Subroutine ", insub, " called when a f90 subroutine",
     +   " should have been called."
       stop 
       end


       subroutine sgeqr2
       call die("sgeqr2")
       end
       
       subroutine sgeqrf
       call die("sgeqrf")
       end
       
       subroutine sgeqrs
       call die("sgeqrs")
       end
       
       subroutine sgetf2
       call die("sgetf2")
       end
       
       subroutine sgetrf
       call die("sgetrf")
       end
       
       subroutine sgetri
       call die("sgetri")
       end
       
!       subroutine sgetrs
!       call die("sgetrs")
!       end
!       
!       subroutine slarf
!       call die("slarf")
!       end
       
!       subroutine slarfb
!       call die("slarfb")
!       end
       
       subroutine slarfg
       call die("slarfg")
       end
       
!       subroutine slarft
!       call die("slarft")
!       end
       
!       subroutine slaswp
!       call die("slaswp")
!       end
       
!       subroutine sorm2r
!       call die("sorm2r")
!       end
!       
!       subroutine sormqr
!       call die("sormqr")
!       end
!       
       subroutine spotf2
       call die("spotf2")
       end
       
       subroutine spotrf
       call die("spotrf")
       end
       
!       subroutine spotrs
!       call die("spotrs")
!       end

        subroutine spotri
        call die("spotri")
        end
       
       subroutine sprtmt
       call die("sprtmt")
       end
       
!       subroutine strti2
!       call die("strti2")
!       end
       
!       subroutine strtri
!       call die("strtri")
!       end
       
