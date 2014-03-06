        integer function icnvx(nv,nt,maxv,maxt,vx,vy,
     1      ipoly,poly,pram)
c
            integer  ipoly(3,*)
            real vx(*),vy(*),poly(2,*),pram(7)
c
c       this routine checks if a convex region can be
c       triangulated by adding one vertex at the centriod
c
        icnvx=0
        llen=ipoly(3,maxv+1)
        if(llen.le.3.or.llen.gt.8) return
        nv1=nv+1
        if(nv1.gt.maxv) return
        if(nt+llen.gt.maxt) return
        eps=pram(1)
        qual=pram(2)
c*      angmx=pram(4)
        best=pram(7)
        pi=3.141592653589793e0+eps
        currnt=1.0e0
        if(llen.eq.7) currnt=0.9e0
        if(llen.eq.8) currnt=0.8e0
        if(currnt.le.best) return
        k=ipoly(1,maxv+1)
        if(poly(2,k).gt.pi) return
        x=0.0e0
        y=0.0e0
c
c       compute centroid
c
        do 10 i=1,llen
            x=x+vx(k)
            y=y+vy(k)
   10       k=ipoly(1,k)
        vx(nv1)=x/float(llen)
        vy(nv1)=y/float(llen)
c
c       check geometry
c
        do 20 i=1,llen
            ka=ipoly(1,k)
            gk=geom(ka,k,nv1,vx,vy)
            currnt=amin1(currnt,abs(gk)/qual)
            if(currnt.le.best) return
c*          gk=cangmx(ka,k,nv1,vx,vy)
c*          currnt=amin1(currnt,angmx/gk)
c*          if(currnt.le.best) return
   20       k=ka
        icnvx=1
        if(currnt.lt.1.0e0) icnvx=-1
        pram(7)=currnt
        return
        end
