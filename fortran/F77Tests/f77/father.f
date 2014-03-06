        integer function father(i,itri)
c
            integer itri(4,*)
c
c       compute father of element i
c
        if(i.lt.itri(2,1)) go to 5
        if(i.ge.itri(3,1)) go to 10
        icent=i/4*4
        father=itri(2,icent)
        return
c
    5   father=0
        return
c
   10   icent=i/2*2
        father=itri(1,icent)
        return
        end
