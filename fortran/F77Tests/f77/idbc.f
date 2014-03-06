        integer function idbc(i,ivert)
c
            integer ivert(4,*)
c
c       test for dirichlet boundary point
c
        idbc=0
        ii=ivert(3,i)
        if(ivert(2,ii)+ivert(1,2).le.0) idbc=1
        if(ii.eq.ivert(1,3)) idbc=1
        return
        end
