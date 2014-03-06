        subroutine cstor(ip,itri)
c
            integer ip(100),itri(4,*)
c
c       storage allocation statistics
c
        ispd=itri(4,8)
        maxt=itri(1,6)
        maxv=itri(2,6)
        mxt=itri(4,1)-1
        mxv=itri(4,4)
        lja=itri(1,7)
        la=itri(2,7)
        lju=itri(3,7)
        lu=itri(4,7)
        iprob=ip(7)
        lenw=ip(16)
        i1=16
        if(iprob.lt.8) i1=i1+6
        if(ispd.ne.1) i1=i1+4
        nv=ip(2)
        nc=ip(3)
        ip(49)=2*(nc+nv)+640+i1*mxv+5*mxt+lja+la+lju+lu
        ip(38)=itri(4,3)
        ip(39)=mxt
        ip(40)=mxv
        ip(45)=itri(1,7)
        ip(46)=itri(2,7)
        ip(47)=itri(3,7)
        ip(48)=itri(4,7)
        return
        end
