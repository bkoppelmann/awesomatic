        integer function level(i,itri)
c
            integer itri(4,*)
c
c       compute level of element i
c
        if(i.lt.itri(2,1)) go to 5
        if(i.ge.itri(3,1)) go to 10
        icent=i/4*4
        level=itri(3,icent)
        return
c
    5   level=1
        return
c
   10   icent=i/2*2
        ifath=itri(1,icent)
        level=2
        if(ifath.lt.itri(2,1)) return
        ifath=ifath/4*4
        level=itri(3,ifath)+1
        return
        end
