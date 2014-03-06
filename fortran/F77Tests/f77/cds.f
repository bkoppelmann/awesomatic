        subroutine cds(nb,nt,nv,nc,mxb,mxt,mxjb,mxv,mxc,jv,
     1      itnode,ibndry,jb,vx,vy,xm,ym,rx,ry,
     2      map,itri,ivert,rusr,list,maxv,maxt,iflag)
c
            integer itnode(4,*),ibndry(5,*),jb(*),jv(2,*),
     1          itri(4,*),ivert(4,*),list(2,*),map(*)
            real vx(*),vy(*),rx(*),ry(*),xm(*),ym(*),rusr(2,*)
            real p(2),dp(2),q(2),dq(2),al(2)
c
c       compute trigen date structures from the jv array
c
        nb=0
        nt=0
        nc=0
        iflag=0
        pi=3.141592653589793e0
        tol=1.e-2
c
c       find a marker for map array
c
        mark=0
        do 10 i=1,maxt
            mark=max0(iabs(map(i)),mark)
   10   continue
        mark=mark+1
c
c       make initial form of jb
c
        do 70 i=1,nv
c
c       associate an edge with higher numbered vertex
c
            if(jv(2,i).le.1) go to 70
            i1=jv(1,i)+1
            i2=i1+jv(2,i)-1
            do 60 j=i1,i2
                k=iabs(jv(1,j))
                it=i
                if(jv(2,k).ne.-2) go to 30
                it=k
                k1=jv(1,it)+1
                k=iabs(jv(1,k1))
                if(k.eq.i) k=iabs(jv(1,k1+1))
                if(iabs(jv(1,k1)).ne.i.and.
     1              iabs(jv(1,k1+1)).ne.i) stop 116
   30           if(k.gt.i) go to 60
c
c       initial form of ibndry for a new edge
c
                if(nb.ge.mxb) go to 200
                nb=nb+1
                k1=jv(1,k)+1
                k2=k1+jv(2,k)-1
                do 40 l=k1,k2
                    if(iabs(jv(1,l)).eq.it) go to 50
   40           continue
                stop 117
   50           ibndry(1,nb)=j
                ibndry(2,nb)=l
                ibndry(3,nb)=i
                ibndry(4,nb)=k
                ibndry(5,nb)=0
                kk=-min0(jv(2,j-1),jv(2,j))
                if(kk.gt.0) ibndry(5,nb)=kk
                if(it.ne.i) ibndry(5,nb)=-it
   60       continue
   70   continue
c
c       now find regions
c
        itnode(1,1)=1
        do 100 i=1,nv
            if(jv(2,i).lt.2) go to 100
            i1=jv(1,i)+1
            i2=i1+jv(2,i)-1
            do 90 j=i1,i2
                iseed=i
                irgn=jv(2,j)
                if(irgn.le.0) go to 90
                if(map(irgn).eq.mark) go to 90
                call getrgn(irgn,iseed,length,list,jv)
c
c       initialize itnode, mark region as found
c
                if(nt.ge.mxt) go to 210
                k=itnode(1,nt+1)
                if(k+length-1.gt.mxjb) go to 220
                nt=nt+1
                itnode(1,nt+1)=k+length
                itnode(2,nt)=k+length-1
                itnode(3,nt)=0
                itnode(4,nt)=map(irgn)
                map(irgn)=mark
c
c       initial form of jb for a new region
c
                m=iseed
                do 80 jj=1,length
                    jb(k)=list(2,m)
                    k=k+1
                    m=list(1,m)
   80           continue
   90       continue
  100   continue
c
c       compute vx,vy and xm,ym for interior curved edges
c
        nn=nv
        nv=0
        do 120 i=1,nn
            if(jv(2,i).le.1) go to 110
c
c       a new vertex
c
            if(nv.ge.mxv) go to 230
            nv=nv+1
            jv(1,i)=nv
            vx(nv)=rx(i)
            vy(nv)=ry(i)
            go to 120
c
 110        if(jv(2,i).ne.-2) go to 120
c
c       a new interior midpoint
c
            if(nc.ge.mxc) go to 240
            nc=nc+1
            jv(1,i)=nc
            xm(nc)=rx(i)
            ym(nc)=ry(i)
 120   continue
c
c       final form of ibndry
c
        do 140 i=1,nb
            do 130 j=1,2
                m=ibndry(j,i)
                jv(1,m)=i
                m=ibndry(j+2,i)
                ibndry(j+2,i)=0
                ibndry(j,i)=jv(1,m)
  130       continue
            kk=ibndry(5,i)
            ibndry(3,i)=0
            if(kk.lt.0) ibndry(3,i)=jv(1,-kk)
            ibndry(4,i)=0
            ibndry(5,i)=0
            if(kk.le.0) go to 140
c
c       a boundary edge
c
            call cbound(kk,ity,icen,mfi,mfe,itag,itri)
            ibndry(4,i)=ity
            ibndry(5,i)=itag
c
c       compute midpoint for a curved boundary edge
c
            if(icen.eq.0) go to 140
            if(nc.ge.mxc) go to 240
            nc=nc+1
            ibndry(3,i)=nc
            i1=ibndry(1,i)
            i2=ibndry(2,i)
            call midpt(vx(i1),vy(i1),vx(i2),vy(i2),
     1          rusr(1,icen),rusr(2,icen),xm(nc),ym(nc))
  140   continue
c
c       final form of jb
c
        l=itnode(2,nt)
        do 150 i=1,l
            m=jb(i)
            jb(i)=jv(1,m)
  150   continue
c
c       look for crack tips
c
        do 190 ir=1,nt
            i1=itnode(1,ir)
            i2=itnode(2,ir)
            ie1=jb(i2)
            do 180 kk=i1,i2
                ie2=jb(kk)
                if(ibndry(4,ie1).eq.0) go to 180
                if(ibndry(4,ie2).eq.0) go to 180
                if(ibndry(3,ie1).gt.0) go to 180
                if(ibndry(3,ie2).gt.0) go to 180
c
c       two consecutive straight boundary edges
c
                icom=ibndry(1,ie1)
                if(icom.ne.ibndry(1,ie2).and.icom.ne.ibndry(2,ie2))
     1              icom=ibndry(2,ie1)
                ibef=ibndry(1,ie1)+ibndry(2,ie1)-icom
                iaft=ibndry(1,ie2)+ibndry(2,ie2)-icom
                aa=cang(ibef,icom,iaft,vx,vy)/pi
                if(abs(aa).gt.tol) go to 180
                dx1=vx(ibef)-vx(icom)
                dy1=vy(ibef)-vy(icom)
                dd1=sqrt(dx1*dx1+dy1*dy1)
                dx2=vx(iaft)-vx(icom)
                dy2=vy(iaft)-vy(icom)
                dd2=sqrt(dx2*dx2+dy2*dy2)
                p(1)=vx(icom)
                p(2)=vy(icom)
                dp(1)=dx1/dd1+dx2/dd2
                dp(2)=dy1/dd1+dy2/dd2
                dq(1)=-dp(2)
                dq(2)=dp(1)
c
c       fixup ibef and iaft
c
                ix=ibef
                do 170 jj=1,2
                    q(1)=vx(ix)
                    q(2)=vy(ix)
                    call lil(p,dp,q,dq,al,jflag)
                    vx(ix)=vx(ix)+al(2)*(1.0e0+tol)*dq(1)
                    vy(ix)=vy(ix)+al(2)*(1.0e0+tol)*dq(2)
  170               ix=iaft
  180       ie1=ie2
  190   continue
        return
c
c       error returns
c
  200   iflag=58
        return
  210   iflag=55
        return
  220   iflag=59
        return
  230   iflag=56
        return
  240   iflag=57
        return
        end
