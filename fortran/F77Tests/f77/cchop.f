        subroutine cchop(kv,nv,nt,nb,maxv,maxt,maxb,vx,vy,
     1      itnode,ibndry,iusr,ipoly,poly,iflag)
c
            integer itnode(4,*),ibndry(5,*),ipoly(3,*),
     1          iusr(6,*),iadj(3)
            real poly(2,*),vx(*),vy(*)
c
c       create a new triangle and updates arrays as required
c
        iflag=0
        llen=ipoly(3,maxv+1)-1
        ipoly(3,maxv+1)=llen
        nt=nt+1
        if(nt.gt.maxt) go to 101
        ka=ipoly(1,kv)
        kb=ipoly(2,kv)
        kka=ipoly(1,ka)
        kkb=ipoly(2,kb)
        itnode(1,nt)=kv
        itnode(2,nt)=ka
        itnode(3,nt)=kb
        iadj(1)=0
        if(kka.eq.kb) iadj(1)=ipoly(3,ka)
        iadj(2)=ipoly(3,kb)
        iadj(3)=ipoly(3,kv)
        ipoly(1,kb)=ka
        ipoly(2,ka)=kb
        ipoly(3,kb)=0
        poly(2,ka)=cang(kb,ka,kka,vx,vy)
        poly(2,kb)=cang(kkb,kb,ka,vx,vy)
        poly(1,ka)=chloc(kb,ka,kka,vx,vy)
        poly(1,kb)=chloc(kkb,kb,ka,vx,vy)
c
c       recompute largest, smallest angles
c
        mx=ka
        mn=ka
        kv=ka
        do 30 i=1,llen
            if(poly(2,mx).lt.poly(2,kv)) mx=kv
            if(poly(2,mn).gt.poly(2,kv)) mn=kv
   30       kv=ipoly(1,kv)
        ipoly(1,maxv+1)=mx
        ipoly(2,maxv+1)=mn
c
c       a triangle with two curved boundary edges
c
        icc=0
        nbsv=nb+1
        do 55 j=1,3
            k=iadj(j)
            if(k.le.0) go to 55
            if(iusr(4,k).eq.0) go to 55
            if(iusr(3,k).gt.0) icc=icc+1
            j1=(5-j)/2
            j2=6-j-j1
            nb=nb+1
            if(nb.gt.maxb) go to 102
            ibndry(1,nb)=itnode(j1,nt)
            ibndry(2,nb)=itnode(j2,nt)
            ibndry(3,nb)=k
            ibndry(4,nb)=itnode(j,nt)
   55   continue
        if(icc.le.1) return
        nv=nv+1
        if(nv.gt.maxv) go to 100
        nto=nt
        nt=nt+2
        if(nt.gt.maxt) go to 101
        xx=0.0e0
        yy=0.0e0
        do 70 j=1,3
            kv=itnode(j,nto)
            xx=xx+vx(kv)
            yy=yy+vy(kv)
            do 65 i=nto,nt
   65           itnode(j,i)=kv
            ntj=nto+j-1
   70       itnode(j,ntj)=nv
        vx(nv)=xx/3.0e0
        vy(nv)=yy/3.0e0
        do 75 j=nbsv,nb
   75       ibndry(4,j)=nv
        return
  100   iflag=56
        return
  101   iflag=55
        return
  102   iflag=58
        return
        end
