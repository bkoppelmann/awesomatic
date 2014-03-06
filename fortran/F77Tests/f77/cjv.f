        subroutine cjv(vx,vy,vz,jv,map,area,maxv,maxt,ljv,
     1      itri,ivert,iflag)
c
            integer itri(4,*),ivert(4,*),jv(2,*),map(*)
            integer iv(3),iadj(3),tag,vlev
            real vx(*),vy(*),vz(*),area(*),c(3)
c
c       begin construction of jv array using the triangular mesh
c
c       make list of triangles as a function of vertex
c
        tol=1.0e-1
        iflag=0
c
        lvl= itri(1,3)
        nv=itri(4,4)
        it1=itri(1,1)
        it2=itri(4,1)-1
        do 10 i=1,nv
            jv(1,i)=2
            if(ibc(i,ivert).eq.1) jv(1,i)=3
            if(vlev(i,itri,ivert).gt.lvl) jv(1,i)=0
   10   continue
c
        atot=0.0e0
        do 20 i=it1,it2
            if(istl(i,itri).eq.0) go to 20
            call knots(i,iv,itri,ivert)
            do 15 j=1,3
                ii=iv(j)
                c(j)=vz(ii)
   15           jv(1,ii)=jv(1,ii)+1
c
c       compute area
c
            iv1=iv(1)
            iv2=iv(2)
            iv3=iv(3)
            x2=vx(iv2)-vx(iv1)
            y2=vy(iv2)-vy(iv1)
            x3=vx(iv3)-vx(iv1)
            y3=vy(iv3)-vy(iv1)
            area(i)=abs(x2*y3-x3*y2)/2.0e0
            atot=atot+area(i)
c
c       look for future incoming contour line for middle edge
c
            kmin=1
            if(c(2).le.c(1)) kmin=2
            kmax=3-kmin
            if(c(3).le.c(kmin)) kmin=3
            if(c(3).gt.c(kmax)) kmax=3
            kmid=6-kmin-kmax
c
            minc=int(c(kmin))+1
            if(abs(float(minc)-c(kmin)).lt.tol) minc=minc+1
            maxc=int(c(kmax))
            if(abs(float(maxc)-c(kmax)).lt.tol) maxc=maxc-1
            if(minc.gt.maxc) go to 20
            maxm=int(c(kmid))
            minm=maxm+1
            if(abs(float(maxm)-c(kmid)).lt.tol) maxm=maxm-1
            if(abs(float(minm)-c(kmid)).lt.tol) minm=minm+1
c
c       compute number of points to be added on each edge
c
            nmid=maxc-minc+1
            nmax=max0(0,maxm-minc+1)
            nmin=max0(0,maxc-minm+1)
            if(nmid.eq.nmax+nmin) go to 20
            if(nmid.ne.nmax+nmin+1) stop 102
            imid=iv(kmid)
            jv(1,imid)=jv(1,imid)+1
   20   continue
        area(1)=atot
c
c       initailize pointers
c
        l=maxv+2
        do 30 i=1,nv
            ii=jv(1,i)
            jv(1,i)=l
            jv(2,i)=l+1
   30       l=l+ii
        jv(1,nv+1)=l
        if(l.gt.ljv+1) go to 210
c
        do 50 i=it1,it2
            if(istl(i,itri).eq.0) go to 50
            call knots(i,iv,itri,ivert)
            do 40 j=1,3
                ii=iv(j)
                k=jv(2,ii)
                jv(2,ii)=k+1
   40           jv(1,k)=i
   50   continue
c
c       convert this list to a circular list of vertices
c       (jv(1,*)) and triangles (jv(2,*))
c       in counter clockwise order (first and last
c       vertices are the same for interior points)
c
        do 80 n=1,nv
            i1=jv(1,n)+1
            i2=jv(2,n)-1
            if(i1.gt.i2) go to 80
            i=jv(1,i1)
            if(ibc(n,ivert).eq.0) go to 60
c
c       starting element for a boundary point
c
            do 55 ii=i1,i2
                i=jv(1,ii)
                num=itbc(i,iadj,itri,ivert)
                if(num.le.0) go to 55
                call knots(i,iv,itri,ivert)
                j1=1
                if(iv(2).eq.n) j1=2
                if(iv(3).eq.n) j1=3
                j2=(5-j1)/2
                j3=6-j1-j2
                if(geom(n,iv(j2),iv(j3),vx,vy).lt.0.0e0) j3=j2
                if(iadj(j3).ne.0) go to 60
   55       continue
c
c       compute list for knot n
c
   60       do 70 ii=i1,i2
                call knots(i,iv,itri,ivert)
                call ledges(i,iv,iadj,itri,ivert)
                j1=1
                if(iv(2).eq.n) j1=2
                if(iv(3).eq.n) j1=3
                j2=(5-j1)/2
                j3=6-j1-j2
                if(geom(n,iv(j2),iv(j3),vx,vy).lt.0.0e0) j3=j2
                j2=6-j3-j1
                jv(1,ii)=iv(j2)
                jv(1,ii+1)=iv(j3)
                jv(2,ii-1)=iadj(j3)
                jv(2,ii)=i
                jv(2,ii+1)=iadj(j2)
                i=iadj(j2)
   70       continue
            i=jv(1,i2)
            if(ibc(n,ivert).eq.1) i=0
            jv(1,i1-1)=i
   80   continue
c
c       compute degrees
c
        do 90 i=1,nv
            ideg=jv(2,i)-jv(1,i)-1
            if(ibc(i,ivert).eq.1) ideg=ideg+1
            if(vlev(i,itri,ivert).gt.lvl) ideg=0
            jv(2,i)=ideg
   90   continue
c
c       initiialize a stack of unused loactions in map
c
        do 100 i=1,it1-1
  100       map(i)=i+1
        map(it1-1)=0
        if(it2.ge.maxt) go to 120
        map(it1-1)=it2+1
        do 110 i=it2+1,maxt
  110       map(i)=i+1
        map(maxt)=0
c
c       initialize the rest of map
c
  120   do 130 i=it1,it2
            if(istl(i,itri).eq.1) go to 125
            map(i)=map(1)
            map(1)=i
            go to 130
  125       map(i)=tag(i,itri)
  130   continue
c
c       mark interfaces specified by user
c
        do 160 n=1,nv
            i1=jv(1,n)+1
            i2=i1+jv(2,n)-1
            if(i1.gt.i2) go to 160
            do 150 i=i1,i2
                irgn=jv(2,i-1)
                jrgn=jv(2,i)
                if(min0(irgn,jrgn).le.0) go to 140
                if(map(irgn).eq.map(jrgn))go to 150
  140           jv(1,i)=-jv(1,i)
  150       continue
  160   continue
        return
c
c       error return
c
  210   iflag=43
        return
        end
