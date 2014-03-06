        subroutine chkcur(irgn,jrgn,iseed,list,ilist,jlist,
     1      vx,vy,jv,tol)
c
            integer list(2,*),jv(2,*),ilist(2,*),jlist(2,*)
            real vx(*),vy(*)
            real p(2),dp(2),q(2),dq(2),al(2),ang(2),c(2),t(2)
c
c       check interior vertices on the path to see if any can be
c       eliminated and replace by a circular arc
c
        eps=1.0e-3
        call getpth(irgn,iseed,length,list,jv)
        if(length.le.1) return
c
c       check each path going in only one direction
c
        last=iseed
        do 10 ii=1,length
            last=list(1,last)
   10   continue
        if(last.lt.iseed) return
c
c       find the rest of the two affected regions
c
        call getrgn(irgn,iseed,ilen,ilist,jv)
        istart=iseed
        if(list(1,iseed).eq.ilist(1,iseed)) istart=last
        istart=ilist(1,istart)
        ilen=ilen-length-2
c
        call getrgn(jrgn,iseed,jlen,jlist,jv)
        jstart=iseed
        if(list(1,iseed).eq.jlist(1,iseed)) jstart=last
        jstart=jlist(1,jstart)
        jlen=jlen-length-2
c
        minmrk=0
        if(ilen.lt.0.or.jlen.lt.0) minmrk=1
c
c       the main loop
c
        pi=3.141592653589793e0
        piby2=pi/2.0e0+eps
        llen=length
        ileft=iseed
c
c       find iright
c
   20   iright=ileft
        do 30 lrlen=1,llen
            iright=list(1,iright)
            if(list(2,iright).eq.1) go to 40
   30   continue
   40   if(lrlen.le.1) go to 100
c
c       compute coordinates
c
   50   p(1)=vx(ileft)
        p(2)=vy(ileft)
        dp(1)=vx(iright)-vx(ileft)
        dp(2)=vy(iright)-vy(ileft)
        dq(1)=-dp(2)
        dq(2)=dp(1)
c
c       look at intermediate vertices, finding the
c       largest perpendicular deviation from a straight line
c       and compute center of circle by least squares
c
        dm=0.0e0
        msave=0
        lsave=0
        mm=ileft
        lm=lrlen-1
        xx=(vx(ileft)+vx(iright))/2.0e0
        yy=(vy(ileft)+vy(iright))/2.0e0
        dd=(dq(1)*dq(1)+dq(2)*dq(2))/4.0e0
        r=0.0e0
        rr=0.0e0
        do 60 lz=1,lm
            mm=list(1,mm)
c
            rr=rr+((vx(mm)-xx)**2+(vy(mm)-yy)**2-dd)
            r=r+dq(1)*(vx(mm)-xx)+dq(2)*(vy(mm)-yy)
c
            q(1)=vx(mm)
            q(2)=vy(mm)
            call lil(p,dp,q,dq,al,iflag)
            if(iflag.ne.0) stop 112
            if(abs(al(2)).lt.abs(dm)) go to 60
            dm=al(2)
            msave=mm
            lsave=lz
   60   continue
        if(r.eq.0.0e0) go to 90
        r=rr/(2.0e0*r)
        xcen=xx+r*dq(1)
        ycen=yy+r*dq(2)
c
c       compute parameterization in terms of (r,theta)
c
        call arc(vx(ileft),vy(ileft),vx(iright),vy(iright),
     1      xcen,ycen,theta1,theta2,rad)
c
c       check deviation from proposed arc
c
        dm=0.0e0
        mm=ileft
        do 80 lz=1,lrlen
            ms=mm
            mm=list(1,mm)
            rr=(vx(mm)-xcen)**2+(vy(mm)-ycen)**2
            rr=sqrt(rr)/rad
            if(abs(rr-1.0e0).lt.dm.or.lz.gt.lm) go to 75
            dm=abs(rr-1.0e0)
            msave=mm
            lsave=lz
   75       xx=(vx(mm)+vx(ms))/2.0e0
            yy=(vy(mm)+vy(ms))/2.0e0
            rr=(xx-xcen)**2+(yy-ycen)**2
            rr=sqrt(rr)/rad
            if(abs(rr-1.0e0).lt.dm) go to 80
            dm=abs(rr-1.0e0)
            msave=mm
            lsave=lz
            if(lz.eq.1) go to 80
            if(lz*2.gt.lrlen.and.lz.ne.lrlen) go to 80
            msave=ms
            lsave=lz-1
   80   continue
        if(minmrk.gt.0) go to 90
        if(abs(theta1-theta2).ge.piby2) go to 90
        if(dm.gt.tol) go to 90
c
c       ckeck the rest of the region
c
        c(1)=xcen
        c(2)=ycen
        t(1)=amin1(theta1,theta2)
        t(2)=amax1(theta1,theta2)
        do 180 iz=1,2
            if(iz.eq.2) go to 160
            if(ilen.le.0) go to 180
            ll=ilen
            i1=istart
            go to 165
  160       if(jlen.le.0) go to 180
            ll=jlen
            i1=jstart
  165       do 170 lz=1,ll
                if(iz.eq.1) i2=ilist(1,i1)
                if(iz.eq.2) i2=jlist(1,i1)
                q(1)=vx(i1)
                q(2)=vy(i1)
                dq(1)=vx(i2)-vx(i1)
                dq(2)=vy(i2)-vy(i1)
                call lil(p,dp,q,dq,al,iflag)
                if(iflag.ne.0) go to 167
                if(al(1).gt.1.0e0+eps) go to 167
                if(al(1).lt.-eps) go to 167
                if(al(2).gt.1.0e0+eps) go to 167
                if(al(2).ge.-eps) go to 90
  167           call liarc(q,dq,c,t,rad,npts,al,ang,eps)
                if(npts.eq.0) go to 170
                if(al(1).gt.1.0e0+eps) go to 168
                if(al(1).lt.-eps) go to 168
                if(ang(1).lt.t(2)+eps) go to 90
  168           if(npts.eq.1) go to 170
                if(al(2).gt.1.0e0+eps) go to 170
                if(al(2).lt.-eps) go to 170
                if(ang(2).lt.t(2)+eps) go to 90
  170           i1=i2
  180   continue
c
c       store midpoint of arc in (vx(msave),vy(msave))
c
        list(2,msave)=-1
        call midpt(vx(ileft),vy(ileft),vx(iright),vy(iright),
     1      xcen,ycen,vx(msave),vy(msave))
        go to 100
c
   90   list(2,msave)=1
        lrlen=lsave
        iright=msave
        minmrk=minmrk-1
        if(lrlen.gt.1) go to 50
c
  100   ileft=iright
        llen=llen-lrlen
        if(llen.gt.1) go to 20
c
c       delete unmarked degree 2 vertices
c
        i=iseed
        ileft=i
        llen=length-1
        do 150 lz=1,llen
            iv1=ileft
            i=list(1,i)
            iv2=list(1,i)
            ileft=i
            if(list(2,i).lt.0) jv(2,i)=-2
            if(list(2,i).ne.0) go to 150
            ileft=iv1
            jv(2,i)=0
            do 140 ll=1,2
                i1=jv(1,iv1)+1
                i2=i1+iabs(jv(2,iv1))-1
                do 110 j=i1,i2
                    k=jv(1,j)
                    if(iabs(k).eq.i) go to 120
  110           continue
                stop 113
  120           jv(1,j)=iv2
                if(k.lt.0) jv(1,j)=-iv2
                if(jv(2,i1-1).le.0) go to 130
                jv(1,i1-1)=jv(1,i2)
                jv(1,i2+1)=jv(1,i1)
  130           ii=iv1
                iv1=iv2
                iv2=ii
  140       continue
  150   continue
        return
        end
