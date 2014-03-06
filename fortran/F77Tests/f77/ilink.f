        integer function ilink(kv,kk,maxv,vx,vy,ipoly,poly,pram,
     1      list)
c
            integer ipoly(3,*),list(*)
            real vx(*),vy(*),poly(2,*),pram(7)
            real p(2),dp(2),q(2),dq(2),al(2)
c
c       this routine determines the best point, if any, to
c       be linked with kv.  it is return in kk.
c
        pi=3.141592653589793e0
        twopi=2.0e0*pi
        eps=pram(1)
c*      epsm=1.0e0-eps
c*      qual=pram(2)
        angmn=pram(3)
        grade=pram(5)
        best=pram(7)
        ilink=0
        if(ipoly(2,kv)/(2.0e0*angmn).lt.best) return
c
c       compute vertices visible from kv  (not counting
c       the immediate neighbors of kv)
c
        ks=ipoly(1,kv)
        kf=ipoly(2,kv)
        xx=vx(kv)
        yy=vy(kv)
        llist=1
        kk=ks
        list(1)=kk
        llen=ipoly(3,maxv+1)
        ll=llen-2
        if(llen.le.3) return
        do 15 l=1,ll
            llist=llist+1
            kk=ipoly(1,kk)
            list(llist)=kk
    5       ka=list(llist)
            kb=list(llist-1)
            aa=cang(kv,kb,ka,vx,vy)
            if(aa.gt.eps.and.aa.lt.pi-eps) go to 15
            llist=llist-1
            if(aa.le.eps.or.aa.ge.twopi-eps) go to 10
            if(aa.le.pi+eps.or.llist.le.1) go to 15
            kc=list(llist-1)
            aa=cang(kc,kb,ka,vx,vy)
            if(aa.gt.pi) go to 15
   10       list(llist)=ka
            if(llist.gt.1) go to 5
   15   continue
        llist=llist-2
        if(llist.le.0) return
        do 20 l=1,llist
   20       list(l)=list(l+1)
c
c       delete points distant from kv, or vornoi points
c       whose edge is outside the region
c
c*      if(llist.le.10) go to 60
        if(llist.le.1) go to 60
        ll=llist
        llist=1
        do 40 l=2,ll
            llist=llist+1
            list(llist)=list(l)
   25       kb=list(llist-1)
            ka=list(llist)
            p(1)=(xx+vx(ka))/2.0e0
            p(2)=(yy+vy(ka))/2.0e0
            dp(1)=yy-vy(ka)
            dp(2)=vx(ka)-xx
            q(1)=(xx+vx(kb))/2.0e0
            q(2)=(yy+vy(kb))/2.0e0
            dq(1)=yy-vy(kb)
            dq(2)=vx(kb)-xx
            call lil(p,dp,q,dq,al,ier)
            if(al(1)*al(2).le.0.0e0) go to 40
            kk=ks
            if(al(1).gt.0.0e0) kk=kf
            p(1)=p(1)+al(1)*dp(1)
            p(2)=p(2)+al(1)*dp(2)
            q(1)=xx
            q(2)=yy
            dq(1)=vx(kk)-xx
            dq(2)=vy(kk)-yy
            dp(1)=-dq(2)
            dp(2)=dq(1)
            call lil(p,dp,q,dq,al,ier)
            if(al(1).ge.0.0e0.and.kk.eq.ks) go to 30
            if(al(1).le.0.0e0.and.kk.eq.kf) llist=llist-1
            go to 40
   30       llist=llist-1
            list(llist)=ka
            if(llist.gt.1) go to 25
   40   continue
c
c       of the remaining points keep only vornoi points
c
        if(llist.le.2) go to 60
        ll=llist
        llist=2
        do 50 l=3,ll
            llist=llist+1
            list(llist)=list(l)
   45       kb=list(llist-2)
            km=list(llist-1)
            ka=list(llist)
            p(1)=(xx+vx(km))/2.0e0
            p(2)=(yy+vy(km))/2.0e0
            dp(1)=yy-vy(km)
            dp(2)=vx(km)-xx
            q(1)=(xx+vx(ka))/2.0e0
            q(2)=(yy+vy(ka))/2.0e0
            dq(1)=yy-vy(ka)
            dq(2)=vx(ka)-xx
            call lil(p,dp,q,dq,al,ier)
            aa=al(1)
            q(1)=(xx+vx(kb))/2.0e0
            q(2)=(yy+vy(kb))/2.0e0
            dq(1)=yy-vy(kb)
            dq(2)=vx(kb)-xx
            call lil(p,dp,q,dq,al,ier)
            ab=al(1)
            if(aa.gt.ab) go to 50
            llist=llist-1
            list(llist)=ka
            if(llist.ge.3) go to 45
   50   continue
c
c       final selection of kk
c
   60   qs=sqrt((vx(kv)-vx(ks))*(vx(kv)-vx(ks))
     1      +(vy(kv)-vy(ks))*(vy(kv)-vy(ks)))
        qf=sqrt((vx(kv)-vx(kf))*(vx(kv)-vx(kf))
     1      +(vy(kv)-vy(kf))*(vy(kv)-vy(kf)))
        kk=0
        currnt=0.0e0
        do 70 l=1,llist
            km=list(l)
            kb=ipoly(2,km)
            ka=ipoly(1,km)
c
c       compute angles
c
            a1=cang(km,kv,ks,vx,vy)
            a2=poly(2,kv)-a1
            a3=cang(kb,km,kv,vx,vy)
            a4=poly(2,km)-a3
            angle=amin1(a1,a2)
            angle=amin1(a3,angle)
            angle=amin1(a4,angle)
            testkm=amin1(1.0e0,angle/angmn)
            if(testkm.le.currnt) go to 70
c
c       check side lengths for first and last segments on
c       the proposed link
c
            dx=vx(km)-xx
            dy=vy(km)-yy
            dd=sqrt(dx*dx+dy*dy)
            call dvpram(poly(1,kv),poly(1,km),dd,pram,qa,ha,nps)
            hh=ha*dd
            d1=qs/hh
            if(d1.lt.1.0e0) d1=1.0e0/d1
            d2=qf/hh
            if(d2.lt.1.0e0) d2=1.0e0/d2
            hh=hh*(qa**nps)
            d3=sqrt((vx(km)-vx(ka))*(vx(km)-vx(ka))
     1          +(vy(km)-vy(ka))*(vy(km)-vy(ka)))/hh
            if(d3.lt.1.0e0) d3=1.0e0/d3
            d4=sqrt((vx(km)-vx(kb))*(vx(km)-vx(kb))
     1          +(vy(km)-vy(kb))*(vy(km)-vy(kb)))/hh
            if(d4.lt.1.0e0) d4=1.0e0/d4
            rmax=amax1(d1,d2)
            rmax=amax1(d3,rmax)
            rmax=amax1(d4,rmax)
            testkm=amin1(testkm,grade/rmax)
            if(testkm.le.currnt) go to 70
c
c       check distances to the other points
c
c*          do 65 ii=1,llist
c*              kl=list(ii)
c*              if(kl.eq.km) go to 65
c*              q(1)=vx(kl)
c*              q(2)=vy(kl)
c*              call lil(p,dp,q,dq,al,ier)
c
c       check for intersection on interior segment
c
c*              if(abs(al(1)).gt.epsm) go to 65
c*              if(abs(al(2)).le.eps) go to 70
c*              if(abs(al(2)).gt.epsm) go to 65
c*              if(al(1)*al(1)+al(2)*al(2).gt.1.0e0) go to 65
c
c       compute relevant hloc values in scaled coordinate system
c
c*              z1=poly(1,kl)/dd
c*              z2=ha+(qa-1.0e0)*(al(1)+1.0e0)/2.0e0
c*              zmax=amax1(z1,z2)
c*              zmin=amin1(z1,z2)
c*              zz=abs(al(2))
c
c       we want zz .ge. zmin/grade and
c       zz .ge. (zmax-zmin)/(grade-1)
c
c*              if(zz*grade.ge.amax1(zmin,zmax-zmin+zz)) go to 65
c*              if(nps.gt.0) go to 70
c*              qq=geom(kl,kv,km,vx,vy)
c*              testkm=amin1(testkm,abs(qq)/qual)
c*              if(testkm.le.currnt) go to 70
c* 65       continue
c
c       km is the best point found so far
c
            currnt=testkm
            kk=km
            if(currnt.eq.1.0e0) go to 75
   70   continue
        if(kk.eq.0) return
        if(currnt.le.best) return
   75   ilink=1
        if(currnt.lt.1.0e0) ilink=-1
        pram(7)=currnt
        return
        end
