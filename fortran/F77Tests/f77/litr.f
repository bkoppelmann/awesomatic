        subroutine litr(b,nsides,c,kmin,kmid,kmax,tmin,tmid,tmax)
c
            real c(3,*),b(3,*)
c
c       compute a 0,3,4,or 5 sided ploygon 
c
        if(tmin.ge.1.0e0.or.tmax.le.0.0e0) then
            nsides=0
            return
        endif
c
        nsides=1
        t=amax1(tmin,0.0e0)
        do 10 j=1,3
   10       b(j,nsides)=(1.0e0-t)*c(j,kmin)+t*c(j,kmax)
c
        if(tmin.gt.0.0e0.and.tmin.ne.tmid) then
            nsides=nsides+1
            if(tmin.lt.tmid) then
                t=tmin/tmid
                k=kmin
            else
                t=(1.0e0-tmin)/(1.0e0-tmid)
                k=kmax
            endif
            do 20 j=1,3
   20           b(j,nsides)=(1.0e0-t)*c(j,k)+t*c(j,kmid)
        endif
c
        if(tmin.le.tmid.and.tmid.le.tmax) then
            nsides=nsides+1
            do 30 j=1,3
   30           b(j,nsides)=c(j,kmid)
        endif
c
        if(tmax.lt.1.0e0.and.tmax.ne.tmid) then
            nsides=nsides+1
            if(tmax.lt.tmid) then
                t=tmax/tmid
                k=kmin
            else
                t=(1.0e0-tmax)/(1.0e0-tmid)
                k=kmax
            endif
            do 40 j=1,3
   40           b(j,nsides)=(1.0e0-t)*c(j,k)+t*c(j,kmid)
        endif
c
        nsides=nsides+1
        t=amin1(tmax,1.0e0)
        do 50 j=1,3
   50       b(j,nsides)=(1.0e0-t)*c(j,kmin)+t*c(j,kmax)
        return
        end
