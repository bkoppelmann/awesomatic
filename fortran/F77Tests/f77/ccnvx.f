        subroutine ccnvx(nv,nt,nb,maxv,maxb,
     1      vx,vy,iusr,itnode,ibndry,ipoly,iflag)
c
            integer itnode(4,*),ibndry(5,*),iusr(6,*),ipoly(3,*)
            real vx(*),vy(*)
c
c       triangulate a convex region
c
        iflag=0
        llen=ipoly(3,maxv+1)
        k=ipoly(1,maxv+1)
        x=0.0e0
        y=0.0e0
c
c       compute centroid
c
        do 10 i=1,llen
            x=x+vx(k)
            y=y+vy(k)
   10       k=ipoly(1,k)
        nv=nv+1
        vx(nv)=x/float(llen)
        vy(nv)=y/float(llen)
        nto=nt+1
        nt=nt+llen
        do 30 i=nto,nt
            ka=ipoly(1,k)
            itnode(1,i)=k
            itnode(2,i)=ka
            itnode(3,i)=nv
            l=ipoly(3,k)
            if(l.le.0) go to 30
            if(iusr(4,l).eq.0) go to 30
            nb=nb+1
            if(nb.gt.maxb) go to 101
            ibndry(1,nb)=k
            ibndry(2,nb)=ka
            ibndry(3,nb)=l
            ibndry(4,nb)=nv
   30       k=ka
        ipoly(3,maxv+1)=0
        return
  101   iflag=58
        return
        end
