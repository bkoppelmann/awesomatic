        subroutine chkpth(irgn,jrgn,iseed,list,ilist,jlist,
     1      vx,vy,jv,tol)
c
            integer  list(2,*),jv(2,*),ilist(2,*),jlist(2,*)
            real vx(*),vy(*)
            real p(2),dp(2),q(2),dq(2),al(2)
c
c       check interior vertices on the path to see if any can be
c       eliminated and replace by a straight line segment
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
   40   if(lrlen.le.1) go to 70
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
c       now look at intermediate vertices, and find the
c       largest perpendicular deviation from the proposed line
c
        dm=0.0e0
        msave=0
        lsave=0
        mm=ileft
        lm=lrlen-1
        do 60 lz=1,lm
            mm=list(1,mm)
            q(1)=vx(mm)
            q(2)=vy(mm)
            call lil(p,dp,q,dq,al,iflag)
            if(iflag.ne.0) stop 110
            if(abs(al(2)).lt.abs(dm)) go to 60
            dm=al(2)
            msave=mm
            lsave=lz
   60   continue
c
        if(abs(dm).ge.tol.or.minmrk.gt.0) go to 65
c
c       ckeck the rest of the region
c
        do 150 iz=1,2
            if(iz.eq.2) go to 130
            if(ilen.le.0) go to 150
            ll=ilen
            i1=istart
            go to 135
  130       if(jlen.le.0) go to 150
            ll=jlen
            i1=jstart
  135       do 140 lz=1,ll
                if(iz.eq.1) i2=ilist(1,i1)
                if(iz.eq.2) i2=jlist(1,i1)
                q(1)=vx(i1)
                q(2)=vy(i1)
                dq(1)=vx(i2)-vx(i1)
                dq(2)=vy(i2)-vy(i1)
                call lil(p,dp,q,dq,al,iflag)
                if(iflag.ne.0) go to 140
                if(al(1).gt.1.0e0+eps) go to 140
                if(al(1).lt.-eps) go to 140
                if(al(2).gt.1.0e0+eps) go to 140
                if(al(2).ge.-eps) go to 65
  140           i1=i2
  150   continue
        go to 70
c
   65   list(2,msave)=1
        lrlen=lsave
        iright=msave
        minmrk=minmrk-1
        if(lrlen.gt.1) go to 50
c
   70   ileft=iright
        llen=llen-lrlen
        if(llen.gt.1) go to 20
c
c       delete unmarked degree 2 vertices
c
        i=iseed
        ileft=i
        llen=length-1
        do 120 lz=1,llen
            iv1=ileft
            i=list(1,i)
            iv2=list(1,i)
            ileft=i
            if(list(2,i).ne.0) go to 120
            ileft=iv1
            jv(2,i)=0
            do 110 ll=1,2
                i1=jv(1,iv1)+1
                i2=i1+jv(2,iv1)-1
                do 80 j=i1,i2
                    k=jv(1,j)
                    if(iabs(k).eq.i) go to 90
   80           continue
                stop 111
   90           jv(1,j)=iv2
                if(k.lt.0) jv(1,j)=-iv2
                if(jv(2,i1-1).le.0) go to 100
                jv(1,i1-1)=jv(1,i2)
                jv(1,i2+1)=jv(1,i1)
  100           ii=iv1
                iv1=iv2
                iv2=ii
  110       continue
  120   continue
        return
        end
