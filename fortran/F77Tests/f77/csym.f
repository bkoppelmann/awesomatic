        subroutine csym(ns,nv,nt,nb,maxv,maxt,maxb,vx,vy,itnode,
     1      ibndry,jb,iusr,ipoly,poly,irgn,pram,iflag)
c
            integer itnode(4,*),ibndry(5,*),jb(*),iusr(6,*),
     1          irgn(6,*),ipoly(3,*)
            real vx(*),vy(*),poly(2,*),pram(7)
c
c       triangluate a region similar to a previously triangulated region
c
        iflag=0
        eps=pram(1)
        nvo=irgn(2,1)-1
        nso=iabs(irgn(5,ns))
        if(nso.eq.0.or.nso.ge.ns) go to 100
        i1=irgn(3,ns)
        i2=irgn(4,ns)
        j1=irgn(3,nso)
        j2=irgn(4,nso)
        if(i2-i1.ne.j2-j1) go to 100
        ie1=jb(i1)
        ie2=jb(i2)
        je1=jb(j1)
        je2=jb(j2)
c
c       find common vertex
c
        iv1=iusr(1,ie1)
        if(iv1.ne.iusr(1,ie2).and.iv1.ne.iusr(2,ie2)) iv1=iusr(2,ie1)
        jv1=iusr(1,je1)
        if(jv1.ne.iusr(1,je2).and.jv1.ne.iusr(2,je2)) jv1=iusr(2,je1)
c
c       find second common vertex
c
        iv2=iusr(1,ie1)+iusr(2,ie1)-iv1
        if(irgn(5,ns).lt.0) go to 10
        jv2=iusr(1,je1)+iusr(2,je1)-jv1
        j=j1
        inc=1
        go to 11
   10   jv2=iusr(1,je2)+iusr(2,je2)-jv1
        j=j2
        inc=-1
c
c       compute affine transformation
c
   11   dxi=vx(iv2)-vx(iv1)
        dyi=vy(iv2)-vy(iv1)
        dxj=vx(jv2)-vx(jv1)
        dyj=vy(jv2)-vy(jv1)
        dd=dxj*dxj+dyj*dyj
        a11=(dxi*dxj+dyi*dyj*float(inc))/dd
        a12=(dxi*dyj-dyi*dxj*float(inc))/dd
        a21=-a12*float(inc)
        a22=a11*float(inc)
        xx=vx(iv1)-a11*vx(jv1)-a12*vy(jv1)
        yy=vy(iv1)-a21*vx(jv1)-a22*vy(jv1)
c
c       check all boundary points...set up ipoly
c
        iv=iv1
        jv=jv1
        do 20 i=i1,i2
            jbi=jb(i)
            jbj=jb(j)
            ipoly(1,jv)=iv
            ipoly(3,iv)=jbi
            dx=a11*vx(jv)+a12*vy(jv)+xx-vx(iv)
            dy=a21*vx(jv)+a22*vy(jv)+yy-vy(iv)
            if(dx*dx+dy*dy.gt.(vx(iv)*vx(iv)+vy(iv)*vy(iv)+1.0e0)
     1          *eps) go to 100
            n1=iusr(6,jbi)
            n2=iusr(6,jbi+1)-1
            if(n2-n1.ne.iusr(6,jbj+1)-1-iusr(6,jbj)) go to 100
            if(n1.gt.n2) go to 19
            is=1
            if(iv.ne.iusr(1,jbi)) is=2
            jnc=1
            nn=iusr(6,jbj)
            if(jv.eq.iusr(is,jbj)) go to 13
            jnc=-1
            nn=iusr(6,jbj+1)-1
   13       jj=jbi
            if(iusr(4,jbi).eq.0) jj=0
            do 18 n=n1,n2
                ipoly(1,nn)=n
                ipoly(3,n)=jj
                dx=a11*vx(nn)+a12*vy(nn)+xx-vx(n)
                dy=a21*vx(nn)+a22*vy(nn)+yy-vy(n)
                if(dx*dx+dy*dy.gt.(vx(n)*vx(n)+vy(n)*vy(n)+1.0e0)
     1              *eps) go to 100
   18           nn=nn+jnc
   19       iv=iusr(1,jbi)+iusr(2,jbi)-iv
            jv=iusr(1,jbj)+iusr(2,jbj)-jv
   20       j=j+inc
c
c       compute new interior vertices
c
        k1=irgn(2,nso)
        k2=irgn(2,nso+1)-1
        if(k1.gt.k2) go to 23
        if(nv+k2-k1+1.gt.maxv) go to 101
        do 22 k=k1,k2
            nv=nv+1
            vx(nv)=a11*vx(k)+a12*vy(k)+xx
            vy(nv)=a21*vx(k)+a22*vy(k)+yy
            ipoly(3,nv)=0
   22       ipoly(1,k)=nv
   23   irgn(2,ns+1)=nv+1
c
c       compute new triangles
c
        k1=irgn(1,nso)
        k2=irgn(1,nso+1)-1
        if(nt+k2-k1+1.gt.maxt) go to 102
        do 30 k=k1,k2
            nt=nt+1
            do 29 j=1,3
                jv=itnode(j,k)
   29           itnode(j,nt)=ipoly(1,jv)
   30   continue
        irgn(1,ns+1)=nt+1
c
c       compute intermediate form of ibndry
c
        k1=irgn(1,ns)
        do 35 k=k1,nt
            do 34 j=1,3
                j1=(5-j)/2
                j2=6-j-j1
                j1=itnode(j1,k)
                j2=itnode(j2,k)
                if(ipoly(3,j1).eq.0.or.ipoly(3,j2).eq.0) go to 34
                jmax=max0(j1,j2)
                jmin=min0(j1,j2)
                ll=ipoly(3,jmax)
                if(ipoly(3,jmin).eq.ll) go to 32
                if(jmin.gt.nvo) go to 34
                if(iusr(1,ll).eq.jmin.or.iusr(2,ll).eq.jmin) go to 32
                if(jmax.gt.nvo) go to 34
                ll=ipoly(3,jmin)
                if(iusr(1,ll).ne.jmax.and.iusr(2,ll).ne.jmax) go to 34
   32           if(iusr(4,ll).eq.0) go to 34
                nb=nb+1
                if(nb.gt.maxb) go to 103
                ibndry(1,nb)=j1
                ibndry(2,nb)=j2
                ibndry(3,nb)=ll
                ibndry(4,nb)=itnode(j,k)
   34       continue
   35   continue
        return
  100   iflag=100+ns
        return
  101   iflag=56
        return
  102   iflag=55
        return
  103   iflag=58
        return
        end
