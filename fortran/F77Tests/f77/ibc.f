        integer function ibc(i,ivert)
c
            integer ivert(4,*)
c
c       test for boundary point
c
        ibc=0
        ii=ivert(3,i)
        if(ivert(2,ii).le.0) ibc=1
        return
        end
