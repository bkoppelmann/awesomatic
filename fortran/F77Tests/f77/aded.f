        subroutine aded(itri,ivert,vx,vy,vz,jv,map,area,
     1      list,nrgn,iflag)
c
            integer itri(4,*),ivert(4,*),map(*),jv(2,*),list(2,*)
            integer iv(3),loc(3),num(3),istart(3)
            real vx(*),vy(*),vz(*),area(*)
            real c(3),p(2),dp(2),q(2),dq(2),al(2)
c
c       add contour edges to the jv array
c
        tol=1.0e-1
c
        iflag=0
        if(nrgn.lt.1) go to 160
c
c       the main loop
c
        it1=itri(1,1)
        it2=itri(4,1)-1
        do 150 i=it1,it2
            if(istl(i,itri).eq.0) go to 150
c
c       read vertices and function values, initailize loc
c
            call knots(i,iv,itri,ivert)
            g=geom(iv(1),iv(2),iv(3),vx,vy)
            do 30 j=1,3
                jj=iv(j)
                c(j)=vz(jj)
                num(j)=0
                istart(j)=0
                k1=jv(1,jj)+1
                k2=k1+jv(2,jj)-1
                do 10 kk=k1,k2
                    if(jv(2,kk).eq.i) go to 20
   10           continue
                stop 105
   20           loc(j)=kk
   30       continue
c
c       order values c(kmin).le.c(kmid).le.c(kmax)
c
            kmin=1
            if(c(2).le.c(1)) kmin=2
            kmax=3-kmin
            if(c(3).le.c(kmin)) kmin=3
            if(c(3).gt.c(kmax)) kmax=3
            kmid=6-kmin-kmax
c
c      find min and max contour values for this triangle
c
            minc=int(c(kmin))+1
            if(abs(float(minc)-c(kmin)).lt.tol) minc=minc+1
            maxc=int(c(kmax))
            if(abs(float(maxc)-c(kmax)).lt.tol) maxc=maxc-1
            if(minc.gt.maxc) go to 150
c
c       find starting indices and number of contours for each side
c
            do 40 j=1,3
                j1=j+1
                if(j.eq.3) j1=1
                if(g.lt.0) j1=6-j-j1
                j2=6-j-j1
                n1=iv(j1)
                n2=iv(j2)
                k1=loc(j1)
                k2=loc(j2)+1
                k1=iabs(jv(1,k1))
                if(k1.eq.n2) go to 40
                k2=iabs(jv(1,k2))
                istart(j)=min0(k1,k2)
                num(j)=iabs(k1-k2)+1
   40       continue
c
c       determine if a countour will pass through middle point
c
            if(num(kmid).ne.maxc-minc+1) stop 106
            imid=0
            if(num(kmin)+num(kmax).ne.maxc-minc+1) imid=1
            if(num(kmin)+num(kmax)+imid.ne.maxc-minc+1) stop 107
c
c       compute unit vector in direction of gradient
c
            call grad(gx,gy,vx,vy,vz,iv)
            dd=sqrt(gx*gx+gy*gy)
            dp(1)=gx/dd
            dp(2)=gy/dd
            dq(1)=-dp(2)
            dq(2)=dp(1)
c
c       match up end points and define new regions from stack
c
            do 80 ic=1,2
                if(ic.eq.2) go to 50
                if(num(kmax).le.0) go to 80
                i1=istart(kmid)
                i2=istart(kmax)
                i3=i1+num(kmax)-1
                go to 60
   50           if(num(kmin).le.0) go to 80
                i1=istart(kmid)+num(kmid)-num(kmin)
                i2=istart(kmin)
                i3=i1+num(kmin)-1
   60           do 70 j=i1,i3
                    list(1,j)=i2
                    i2=i2+1
                    map1=map(1)
                    if(map1.le.0) go to 300
                    map(1)=map(map1)
                    map(map1)=map(i)
                    list(2,j)=map1
   70           continue
   80       continue
c
c       the middle point
c
            if(imid.eq.0) go to 90
            jmid=istart(kmid)+num(kmax)
            list(1,jmid)=iv(kmid)
            map1=map(1)
            if(map1.le.0) go to 300
            map(1)=map(map1)
            map(map1)=map(i)
            list(2,jmid)=map1
c
c       compute areas
c
   90       q(1)=vx(iv(kmin))
            q(2)=vy(iv(kmin))
            qd=0.0e0
            it=i
            i1=istart(kmid)
            i2=i1+num(kmid)-1
            do 91 ii=i1,i2
                p(1)=q(1)
                p(2)=q(2)
                pd=qd
                j=list(1,ii)
                q(1)=(vx(ii)+vx(j))/2.0e0
                q(2)=(vy(ii)+vy(j))/2.0e0
                dx=vx(ii)-vx(j)
                dy=vy(ii)-vy(j)
                qd=sqrt(dx*dx+dy*dy)
                call lil(p,dp,q,dq,al,iflag)
                area(it)=abs(al(1))*(qd+pd)/2.0e0
   91           it=list(2,ii)
            p(1)=vx(iv(kmax))
            p(2)=vy(iv(kmax))
            call lil(p,dp,q,dq,al,iflag)
            area(it)=abs(al(1))*qd/2.0e0
c
c       fixup odd region near kmid
c
            if(imid.eq.1) go to  95
            jmid=i1+num(kmax)-1
            k1=iv(kmid)
            k2=iv(kmin)
            if(num(kmax).gt.0) k2=list(1,jmid)
            it=i
            if(num(kmax).gt.0) it=list(2,jmid)
            k3=iv(kmax)
            if(num(kmin).gt.0) k3=list(1,jmid+1)
            x2=vx(k2)-vx(k1)
            y2=vy(k2)-vy(k1)
            x3=vx(k3)-vx(k1)
            y3=vy(k3)-vy(k1)
            area(it)=area(it)+abs(x2*y3-x3*y2)/2.0e0
c
c       now add edges
c
   95       if(kmin.eq.kmax+1) g=-g
            if(kmin.eq.1.and.kmax.eq.3) g=-g
            i1=istart(kmid)
            i2=i1+num(kmid)-1
            icur=i
            do 130 j1=i1,i2
                j2=list(1,j1)
                ilast=icur
                icur=list(2,j1)
                j=j1
                if(g.lt.0.0e0) j=j2
                do 120 ll=1,2
                    k1=jv(1,j)+1
                    k2=k1+jv(2,j)
                    jv(2,j)=jv(2,j)+1
c
c       shift jv array to make a hole for the new edge
c
                    do 100 kk=k2,k1,-1
                        jv(1,kk+1)=jv(1,kk)
                        jv(2,kk+1)=jv(2,kk)
                        if(jv(2,kk).eq.i.and.kk.ne.k2) go to 110
  100               continue
                    stop 108
c
c       add the new edge
c
  110               jj=j
                    j=j1+j2-j
                    jv(1,kk+1)=-j
                    jv(2,kk+ll-1)=icur
                    jv(2,kk+2-ll)=ilast
                    if(jv(2,k1-1).le.0) go to 120
                    jv(2,k1-1)=jv(2,k2)
                    jv(1,k1-1)=jv(1,k2)
                    jv(2,k2+1)=jv(2,k1)
                    jv(1,k2+1)=jv(1,k1)
  120           continue
  130       continue
c
c       fixup kmax
c
            j=iv(kmax)
            kk=loc(kmax)
            jv(2,kk)=icur
            if(ibc(j,ivert).eq.1) go to 140
            k1=jv(1,j)+1
            k2=k1+jv(2,j)-1
            if(jv(2,k1-1).eq.i) jv(2,k1-1)=jv(2,k2)
            if(jv(2,k2+1).eq.i) jv(2,k2+1)=jv(2,k1)
c
c       fixup kmid
c
  140       if(imid.eq.1) go to 150
            if(num(kmax).eq.0) go to 150
            j=iv(kmid)
            kk=loc(kmid)
            icur=i1+num(kmax)-1
            icur=list(2,icur)
            jv(2,kk)=icur
            if(ibc(j,ivert).eq.1) go to 150
            k1=jv(1,j)+1
            k2=k1+jv(2,j)-1
            if(jv(2,k1-1).eq.i) jv(2,k1-1)=jv(2,k2)
            if(jv(2,k2+1).eq.i) jv(2,k2+1)=jv(2,k1)
  150   continue
  160   itri(1,3)=itri(3,3)
        return
c
c       error return
c
  300   iflag=41
        return
        end
