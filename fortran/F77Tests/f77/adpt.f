        subroutine adpt(itri,ivert,vx,vy,vz,cxy,jv,ljv,
     1      maxv,maxt,nv,nrgn,iflag)
c
            integer itri(4,*),ivert(4,*),jv(2,*),iv(3)
            real vx(*),vy(*),vz(*),cxy(3,*)
            real p(2),dp(2),q(2),dq(2),al(2)
c
c       add points along triangle edges corresponding to
c       coutour lines (these will ultimately be degree 3 or 4 vertices)
c
        tol=1.0e-1
c
        iflag=0
        nv=itri(4,4)
        nv0=nv
        do 130 n=1,nv0
            if(jv(2,n).lt.2) go to 130
            i1=jv(1,n)+1
            i2=i1+jv(2,n)-1
c
c       we compute points on edges for which vz(n) is the
c       minimum endpoint
c
            minc=int(vz(n))+1
            if(abs(float(minc)-vz(n)).lt.tol) minc=minc+1
            do 120 ii=i1,i2
                i=iabs(jv(1,ii))
                if(i.gt.nv0) go to 120
                if(vz(n).gt.vz(i)) go to 120
                maxc=int(vz(i))
                if(abs(float(maxc)-vz(i)).lt.tol) maxc=maxc-1
                if(minc.gt.maxc) go to 90
c
                iaft=jv(2,ii)
                ibef=jv(2,ii-1)
                ix=6
                icen=0
c
c       check for (curved) boundary edge
c
                if(iaft.gt.0) go to 10
                ix=5
                call cbound(-iaft,ity,icen,mfi,mfe,itag,itri)
                if(icen.eq.0) go to 10
                iz=iabs(jv(1,ii-1))
                go to 20
   10           if(ibef.gt.0) go to 30
                ix=5
                call cbound(-ibef,ity,icen,mfi,mfe,itag,itri)
                if(icen.eq.0) go to 30
                iz=iabs(jv(1,ii+1))
   20           iv(1)=iz
                iv(2)=i
                iv(3)=n
                call grad(gy,gx,vx,vy,vz,iv)
                gg=sqrt(gx*gx+gy*gy)
                q(1)=(vx(i)+vx(n))/2.0e0
                q(2)=(vy(i)+vy(n))/2.0e0
                dq(1)=vx(i)-vx(n)
                dq(2)=vy(i)-vy(n)
                p(1)=vx(iz)
                p(2)=vy(iz)
                dp(1)=-gx
                dp(2)=gy
                call lil(p,dp,q,dq,al,iflag)
                if(al(1).gt.0.0e0) gg=-gg
                gx=-gx/gg
                gy=gy/gg
c
c       the main loop ove contours for this edge
c
   30           dx=vx(i)-vx(n)
                dy=vy(i)-vy(n)
                dz=vz(i)-vz(n)
                if(icen.gt.0) dd=sqrt(dx*dx+dy*dy)
                nvsv=nv+1
                if(jv(1,ii).lt.0) nvsv=-nvsv
c
c       the new points are initialized as degree 2 vertices
c
                do 60 m=minc,maxc
                    nv=nv+1
                    if(nv.gt.maxv) go to 200
                    jv(1,nv+1)=jv(1,nv)+ix
                    if(jv(1,nv+1).gt.ljv+1) go to 210
                    jv(2,nv)=2
c
                    l=jv(1,nv)
                    jv(1,l)=nv-1
                    if(m.eq.minc) jv(1,l)=n
                    if(jv(1,ii).lt.0) jv(1,l)=-jv(1,l)
                    jv(1,l+1)=nv+1
                    if(m.eq.maxc) jv(1,l+1)=i
                    if(jv(1,ii).lt.0) jv(1,l+1)=-jv(1,l+1)
                    jv(1,l+2)=jv(1,l)
                    jv(1,l+3)=jv(1,l+1)
                    jv(2,l)=ibef
                    jv(2,l+1)=iaft
                    jv(2,l+2)=ibef
                    jv(2,l+3)=iaft
c
c       check for boundary edges and adjust as necessary
c
                    if(ibef.le.0) go to 45
                    if(iaft.gt.0) go to 50
                    do 40 k=1,2
                        jv(k,l)=jv(k,l+1)
                        jv(k,l+1)=jv(k,l+2)
   40                   jv(k,l+2)=jv(k,l)
   45               jv(2,l+3)=0
                    jv(1,l+3)=0
                    jv(1,l)=0
c
   50               qq=(float(m)-vz(n))/dz
                    vx(nv)=vx(n)+qq*dx
                    vy(nv)=vy(n)+qq*dy
                    vz(nv)=float(m)
c
c       adjust for curved boundary edge
c
                    if(icen.eq.0) go to 60
                    xx=vx(nv)+gx*dd
                    yy=vy(nv)+gy*dd
                    call inters(xx,vx(nv),yy,vy(nv),
     1                  cxy(1,icen),cxy(2,icen),cxy(3,icen))
   60           continue
c
c       fixup original edges connecting n to i
c
                jv(1,ii)=nvsv
                if(jv(2,i1-1).le.0) go to 65
                jv(1,i2+1)=jv(1,i1)
                jv(1,i1-1)=jv(1,i2)
c
   65           k1=jv(1,i)+1
                k2=k1+jv(2,i)-1
                do 70 kk=k1,k2
                     if(iabs(jv(1,kk)).eq.n) go to 80
   70           continue
                stop 103
   80           jv(1,kk)=nv
                if(jv(1,ii).lt.0) jv(1,kk)=-nv
                if(jv(2,k1-1).le.0) go to 120
                jv(1,k2+1)=jv(1,k1)
                jv(1,k1-1)=jv(1,k2)
                go to 120
c
c       see if this edge is a contour edge and mark if necessary
c
   90           if(maxc+2.ne.minc) go to 120
                if(nrgn.le.1) go to 120
                qq=float(minc-1)
                if(abs(qq-vz(n)).ge.tol) go to 120
                if(abs(qq-vz(i)).ge.tol) go to 120
c
c       if the bef and aft points are also contour pts, then skip it
c
                ibef=iabs(jv(1,ii-1))
                iaft=iabs(jv(1,ii+1))
                if(ibef.eq.0.or.iaft.eq.0) go to 120
                qq=amax1(abs(qq-vz(ibef)),abs(qq-vz(iaft)))
                if(qq.le.tol) go to 120
                jv(1,ii)=-i
                k1=jv(1,i)+1
                k2=k1+jv(2,i)-1
                do 100 kk=k1,k2
                     if(iabs(jv(1,kk)).eq.n) go to 110
  100           continue
                stop 104
  110           jv(1,kk)=-n
  120       continue
  130   continue
        return
c
c       error returns
c
  200   iflag=42
        return
  210   iflag=43
        return
        end
