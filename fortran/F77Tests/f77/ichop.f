        integer function ichop(kv,maxv,vx,vy,iusr,ipoly,poly,pram)
c
            integer ipoly(3,*),iusr(6,*)
            real poly(2,*),vx(*),vy(*),pram(7)
            real p(2),dp(2),q(2),dq(2),al(2)
c
c       this routine decides whether it is a good idea to
c       chop off the triangle with vertices ka,kb,kv
c
        eps=pram(1)
        qual=pram(2)
        angmn=pram(3)
        angmx=pram(4)
        grade=pram(5)
        best=pram(7)
        pi=3.141592653589793e0+eps
        epsm=-eps
        epsp1=eps+1.0e0
        ichop=0
        ka=ipoly(1,kv)
        kb=ipoly(2,kv)
        llen=ipoly(3,maxv+1)
        currnt=1.0e0
c
c       tests for three or four vertices
c
        if(llen.eq.3) go to 30
        if(llen.eq.4) go to 20
c
c       check angles of proposed new polygon
c
        if(poly(2,kv).gt.angmx) return
        ab=cang(ka,kb,kv,vx,vy)
        aa=cang(kv,ka,kb,vx,vy)
        angle=amin1(poly(2,kb)-ab,poly(2,ka)-aa)
        currnt=amin1(currnt,angle/angmn)
        if(currnt.le.best) return
c
c       check side lengths of proposed new polygon
c
        dd=sqrt((vx(ka)-vx(kb))*(vx(ka)-vx(kb))+
     1      (vy(ka)-vy(kb))*(vy(ka)-vy(kb)))
        da=poly(1,ka)/dd
        db=poly(1,kb)/dd
        if(da.lt.1.0e0) da=1.0e0/da
        if(db.lt.1.0e0) db=1.0e0/db
        dd=amax1(da,db)
        currnt=amin1(currnt,grade/dd)
        if(currnt.le.best) return
c
c       check geometry and angles of proposed new triangle
c
        gg=geom(kb,kv,ka,vx,vy)
        currnt=amin1(currnt,abs(gg)/qual)
        if(currnt.le.best) return
c*      gg=cangmx(kb,kv,ka,vx,vy)
c*      currnt=amin1(currnt,angmx/gg)
c*      if(currnt.le.best) return
c
c       check for two boundary edges
c
        j1=ipoly(3,kb)
        if(j1.eq.0) go to 6
        j2=ipoly(3,kv)
        if(j2.eq.0) go to 6
        j1=iusr(3,j1)
        j2=iusr(3,j2)
        if(j1.gt.0.and.j2.gt.0) return
        currnt=currnt/2.0e0
        if(currnt.le.best) return
c
c       test for convexity
c
    6   ks=ipoly(1,maxv+1)
        if(poly(2,ks).le.pi) go to 30
c
c       make sure no other vertices are inside the
c       proposed triangle or are too close
c       (al(2).gt.1 means inside)
c
        hv=poly(1,kv)
        p(1)=vx(ka)
        p(2)=vy(ka)
        dp(1)=vx(kb)-p(1)
        dp(2)=vy(kb)-p(2)
        q(1)=vx(kv)
        q(2)=vy(kv)
        kk=ka
        do 10 ll=4,llen
            kk=ipoly(1,kk)
            if(poly(2,kk).le.pi) go to 10
            dq(1)=vx(kk)-q(1)
            dq(2)=vy(kk)-q(2)
            call lil(p,dp,q,dq,al,ier)
            if(ier.eq.1) go to 10
            if(al(1).lt.epsm.or.al(1).gt.epsp1) go to 10
            if(al(2).gt.1.0e0) return
            hk=poly(1,kk)
            if((hk+hv)*al(2).lt.hv) go to 10
            qq=geom(kb,ka,kk,vx,vy)
            currnt=amin1(currnt,abs(qq)/qual)
            if(currnt.le.best) return
   10   continue
        go to 30
c
c       the special case of only four vertices
c
   20   kl=ipoly(1,maxv+1)
        if(poly(2,kl).ge.pi) go to 25
        kl=ipoly(1,ka)
        g1=geom(kb,kv,ka,vx,vy)
        g2=geom(kb,kl,ka,vx,vy)
        gg=amin1(abs(g1),abs(g2))
        currnt=amin1(currnt,gg/qual)
        if(currnt.le.best) return
c*      g1=cangmx(kb,kv,ka,vx,vy)
c*      g2=cangmx(kb,kl,ka,vx,vy)
c*      gg=amax1(g1,g2)
c*      currnt=amin1(currnt,angmx/gg)
c*      if(currnt.le.best) return
c
c       check for two curved boundary edges
c
        j1=ipoly(3,kb)
        if(j1.gt.0) j1=iusr(3,j1)
        j2=ipoly(3,kv)
        if(j2.gt.0) j2=iusr(3,j2)
        if(j1.gt.0.and.j2.gt.0) return
        j1=ipoly(3,ka)
        if(j1.gt.0) j1=iusr(3,j1)
        j2=ipoly(3,kl)
        if(j2.gt.0) j2=iusr(3,j2)
        if(j1.gt.0.and.j2.gt.0) return
        go to 30
c
c       non convex case
c
   25   if(kl.ne.ka.and.kl.ne.kb) return
   30   ichop=1
        if(currnt.lt.1.0e0) ichop=-1
        pram(7)=currnt
        return
        end
