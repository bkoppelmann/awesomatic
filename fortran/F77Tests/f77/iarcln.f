        integer function iarcln(rln)
c
            real rln(9,9)
c
c       decide whether pseudo-arclength continuation
c       is necessary
c
        iarcln=0
        theta=rln(3,6)
        if(theta.eq.0.0e0) return
        tol=0.05e0
        rl=rln(1,2)
        r=rln(2,2)
        rldot=rln(3,2)
        rdot=rln(4,2)
        unorm=rln(8,2)
        undot=rln(9,2)
        ud2=1.0e0-rldot*rldot
        udn=sqrt(ud2)
c
        if(r.ne.0.0e0) unorm=unorm/r
        if(unorm.ne.0.0e0) rdot=rdot*unorm
        if(amin1(abs(undot),abs(rdot)).lt.tol*udn) iarcln=1
        return
        end
