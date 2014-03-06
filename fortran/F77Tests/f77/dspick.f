        subroutine dspick(rln,idsp,mxfail)
c
            real rln(9,9),coeff(5),p(2),pr(2),
     1          dpr(2),pl(2),dpl(2),alr(2),al(2)
c
c       this routine computes the new ds, theta, predicted lamda
c       and r, and the coefficients for the predicted u
c
c       idsp = 0 is the normal case (go to boundary)
c       idsp > 0 is failure of some sort
c
        rtrgt=rln(2,4)
        rltrgt=rln(1,4)
        rstrt=rln(2,5)
        rlstrt=rln(1,5)
        r=rln(2,2)
        rl=rln(1,2)
        rdot=rln(4,2)
        rldot=rln(3,2)
        p(1)=r
        p(2)=rl
        rld2=rldot*rldot
        ud2=1.0e0-rld2
        call prbla(rln,coeff)
c
c       set boundary lines
c
    5   z=2.0e0**(-idsp)
        pr(1)=rtrgt
        if(idsp.gt.0) pr(1)=r+(rtrgt-r)*z
        pr(2)=0.0e0
        dpr(1)=0.0e0
        dpr(2)=1.0e0
        pl(1)=0.0e0
        pl(2)=rltrgt
        if(idsp.gt.0) pl(2)=rl+(rltrgt-rl)*z
        dpl(1)=1.0e0
        dpl(2)=0.0e0
        numr=0
        if(rstrt.ne.rtrgt)
     1      call lip(pr,dpr,alr,numr,coeff)
        numl=0
        if(rlstrt.ne.rltrgt)
     1      call lip(pl,dpl,al,numl,coeff)
        if(numr+numl.gt.0) go to 10
c
c       failed step
c
    7   idsp=idsp+1
        if(idsp.gt.mxfail) return
        go to 5
c
c       decide where to hit the boundary
c
   10   dist0=arclen(p,coeff)
c
        if(numr.eq.0) go to 15
        pr(2)=alr(1)
        dist1=arclen(pr,coeff)
        rdis=abs(dist0-dist1)
        if(numr.eq.1) go to 15
        pr(2)=alr(2)
        dist1=arclen(pr,coeff)
        pr(2)=alr(1)
        if(rdis.lt.abs(dist0-dist1)) go to 15
        rdis=abs(dist0-dist1)
        pr(2)=alr(2)
c
   15   if(numl.eq.0) go to 20
        pl(1)=al(1)
        dist1=arclen(pl,coeff)
        rldis=abs(dist0-dist1)
        if(numl.eq.1) go to 20
        pl(1)=al(2)
        dist1=arclen(pl,coeff)
        pl(1)=al(1)
        if(rldis.lt.abs(dist0-dist1)) go to 20
        rldis=abs(dist0-dist1)
        pl(1)=al(2)
c
   20   if(numr.eq.0) go to 25
        if(numl.eq.0) go to 30
        if(rdis.lt.rldis) go to 30
c
c       target in lamda
c
   25   if(rldot.eq.0.0e0) go to 7
        p(1)=pl(1)
        p(2)=pl(2)
        theta=0.0e0
c       if(idsp.ne.0.and.rld2.lt.0.25e0)  theta=1.0e0
        go to 35
c
c       target in r
c
   30   if(rdot.eq.0.0e0) go to 7
        p(1)=pr(1)
        p(2)=pr(2)
        theta=2.0e0
c       if(idsp.ne.0.and.ud2.lt.0.25e0) theta=1.0e0
c
   35   rln(3,6)=theta
        rln(4,6)=theta*rdot*(p(1)-r)+(2.0e0-theta)*rldot*(p(2)-rl)
        iarc=iarcln(rln)
        if(iarc.eq.0) return
        q=1.0e0
        if(rldot.ne.0.0e0.and.p(2).ne.rl) q=(p(2)-rl)/rldot
        rln(4,6)=(theta*ud2*q+(2.0e0-theta)*rldot*(p(2)-rl))*0.1e0
        idsp=idsp+1
        return
        end
