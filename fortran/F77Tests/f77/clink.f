        subroutine clink(kv,kk,ns,nv,maxv,vx,vy,ipoly,
     1      poly,irptr,irlst,pram,iflag)
c
            integer irptr(*),irlst(2,*),ipoly(3,*)
            real vx(*),vy(*),poly(2,*),pram(7)
            real p(2),dp(2),q(2),dq(2),al(2)
c
c       update the irptr and irlst arrays
c       with the two newly created subregions
c
        iflag=0
        grade=pram(5)
        hmax=pram(6)
        llen=ipoly(3,maxv+1)
c
c       compute points on linking line
c
        d=(vx(kv)-vx(kk))*(vx(kv)-vx(kk))+
     1      (vy(kv)-vy(kk))*(vy(kv)-vy(kk))
        d=sqrt(d)
        hv=poly(1,kv)
        hk=poly(1,kk)
c
c       see if increasing h towards the middle of the interval
c       is possible or worthwhile
c
        if(llen.le.8) go to 70
        if(d.le.grade*(hv+hk)) go to 70
        ds=d*(grade-1.0e0)/grade
        theta=(hv-hk)/(2.0e0*ds)
        if(abs(theta).gt.0.4e0) go to 70
        fv=0.5e0-theta
        fk=0.5e0+theta
        hm=(ds+hv+hk)/2.0e0
        hmin=fv*hk+fk*hv
        if(hmax.gt.0.0e0) hm=amin1(hm,hmax)
        if(hm.lt.hmin) go to 70
c
c       set up lines
c
        xm=fv*vx(kk)+fk*vx(kv)
        ym=fv*vy(kk)+fk*vy(kv)
        p(1)=xm
        p(2)=ym
        dp(1)=vx(kk)-vx(kv)
        dp(2)=vy(kk)-vy(kv)
        dq(1)=-dp(2)
        dq(2)=dp(1)
        k=kk
        kka=ipoly(1,kk)
        kkb=ipoly(2,kk)
        kva=ipoly(1,kv)
        kvb=ipoly(2,kv)
        do 45 i=1,llen
            k=ipoly(1,k)
            if(k.eq.kka.or.k.eq.kkb) go to 45
            if(k.eq.kva.or.k.eq.kvb) go to 45
            if(k.eq.kv.or.k.eq.kk) go to 45
            q(1)=vx(k)
            q(2)=vy(k)
            call lil(p,dp,q,dq,al,ier)
            al2=abs(al(2))*ds
            z=al2+poly(1,k)/grade
            if(al(1).lt.0.0e0) go to 43
c
            al1=al(1)/fk
            if(al1.ge.1.0e0) go to 45
            if(al2.gt.hm*(1.0e0-al1)) go to 45
            h=hm+(hk-hm)*al1
            if(h.le.z) go to 45
            h=(z-al1*hk)/(1.0e0-al1)
            go to 44
c
   43       al1=-al(1)/fv
            if(al1.ge.1.0e0) go to 45
            if(al2.gt.hm*(1.0e0-al1)) go to 45
            h=hm+(hv-hm)*al1
            if(h.le.z) go to 45
            h=(z-al1*hv)/(1.0e0-al1)
c
   44       hm=amin1(hm,h)
            if(hm.lt.hmin) go to 70
   45   continue
c
c       first set up segment between kv and (xm,ym)
c
        dv=fv*d
        call dvpram(hv,hm,dv,pram,alpha,h,np1)
        if(nv+np1+1.gt.maxv) go to 101
        if(np1.eq.0) go to 55
        p1=vx(kv)
        p2=vy(kv)
        dp1=xm-p1
        dp2=ym-p2
        qq=0.0e0
        do 50 i=1,np1
            qq=qq+h
            h=h*alpha
            nv=nv+1
            vx(nv)=p1+qq*dp1
   50       vy(nv)=p2+qq*dp2
   55   nv=nv+1
        vx(nv)=xm
        vy(nv)=ym
c
c       next set up segment between (xm,ym) and kk
c
        dk=fk*d
        call dvpram(hm,hk,dk,pram,alpha,h,np2)
        if(nv+np2.gt.maxv) go to 101
        np=np1+np2+1
        if(np2.eq.0) go to 3
        p1=xm
        p2=ym
        dp1=vx(kk)-p1
        dp2=vy(kk)-p2
        qq=0.0e0
        do 60 i=1,np2
            qq=qq+h
            h=h*alpha
            nv=nv+1
            vx(nv)=p1+qq*dp1
   60       vy(nv)=p2+qq*dp2
        go to 3
c
c       take h to be a linear function between kv and kk
c
   70   call dvpram(hv,hk,d,pram,alpha,h,np)
        if(nv+np.gt.maxv) go to 101
        if(np.eq.0) go to 3
        p1=vx(kv)
        p2=vy(kv)
        dp1=vx(kk)-p1
        dp2=vy(kk)-p2
        qq=0.0e0
        do 2 i=1,np
            qq=qq+h
            h=h*alpha
            nv=nv+1
            vx(nv)=p1+qq*dp1
    2       vy(nv)=p2+qq*dp2
c
    3   if(ns+1.eq.irlst(2,1)) go to 100
        i1=irptr(ns)
        if(i1+2*np+llen+2.gt.irlst(1,1)) go to 100
c
c       compute length of each region
c
        len1=1
        kj=kk
    4   if(kj.eq.kv) go to 6
        len1=len1+1
        kj=ipoly(1,kj)
        go to 4
    6   len2=llen-len1+2
c
c       the shortest region goes on the top of the stack
c
        if(len2.gt.len1) go to 7
        ns1=ns
        ns2=ns+1
        irptr(ns+1)=i1+np+len1
        go to 8
    7   ns1=ns+1
        ns2=ns
        irptr(ns+1)=i1+np+len2
    8   ns=ns+1
        irptr(ns+1)=i1+2*np+llen+2
c
c       save the first region in irlst
c
        i1=irptr(ns1)
        if(np.eq.0) go to 10
        do 9 i=1,np
            irlst(1,i1)=nv-np+i
            irlst(2,i1)=0
    9       i1=i1+1
   10   kj=kk
        do 15 i=1,len1
            irlst(1,i1)=kj
            irlst(2,i1)=ipoly(3,kj)
            i1=i1+1
   15       kj=ipoly(1,kj)
        irlst(2,i1-1)=0
c
c       save the second region in irlst
c
        i1=irptr(ns2)
        if(np.eq.0) go to 30
        do 25 i=1,np
            irlst(1,i1)=nv+1-i
            irlst(2,i1)=0
   25       i1=i1+1
   30   kj=kv
        do 35 i=1,len2
            irlst(1,i1)=kj
            irlst(2,i1)=ipoly(3,kj)
            i1=i1+1
   35       kj=ipoly(1,kj)
        irlst(2,i1-1)=0
        return
  100   iflag=20
        return
  101   iflag=56
        return
        end
