        integer function ielty(i,itri)
c
            integer itri(4,*)
c
c       compute element type
c
        ielty=0
        if(i.ge.itri(3,1)) return
        if(i.lt.itri(2,1)) go to 10
c
c       normal refinement
c
        icent=(i/4)*4
        if(icent.eq.i) return
        ielf=itri(1,icent)
        irel=i-icent
        if(ielf.ne.0.and.ielf.ne.irel) ielty=6-ielf-irel
        return
c
c       macro triangle
c
   10   do 15 j=1,3
            if(itri(j,i).lt.0) ielty=j
   15   continue
        return
        end
