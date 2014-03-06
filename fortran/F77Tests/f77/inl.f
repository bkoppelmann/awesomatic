        subroutine inl(ns,maxv,vx,vy,ipoly,poly,irptr,irlst)
c
            integer irptr(*),irlst(2,*),ipoly(3,*)
            real vx(*),vy(*),poly(2,*)
c
c       initialize ipoly and poly
c
        i1=irptr(ns)
        i2=irptr(ns+1)-1
        ka=maxv+1
        kb=maxv+1
c
c       set up ipoly
c
        do 5 i=i1,i2
            ii=i2+i1-i
            ipoly(1,ka)=irlst(1,i)
            ka=irlst(1,i)
            ipoly(2,kb)=irlst(1,ii)
    5       kb=irlst(1,ii)
        ipoly(1,ka)=irlst(1,i1)
        ipoly(2,kb)=irlst(1,i2)
c
c       set up poly
c
        kv=ipoly(1,maxv+1)
        do 20 i=i1,i2
            ipoly(3,kv)=irlst(2,i)
            ka=ipoly(1,kv)
            kb=ipoly(2,kv)
            poly(1,kv)=chloc(kb,kv,ka,vx,vy)
            poly(2,kv)=cang(kb,kv,ka,vx,vy)
   20       kv=ka
c
c       ipoly(1,maxv+1) = vertex with largest angle
c       ipoly(2,maxv+1) = vertex with smallest angle
c       ipoly(3,maxv+1) = length of polygon
c
        mx=ipoly(1,maxv+1)
        mn=ipoly(2,maxv+1)
        kv=mx
        do 30 i=i1,i2
            if(poly(2,mx).lt.poly(2,kv)) mx=kv
            if(poly(2,mn).gt.poly(2,kv)) mn=kv
   30       kv=ipoly(1,kv)
        ipoly(1,maxv+1)=mx
        ipoly(2,maxv+1)=mn
        ipoly(3,maxv+1)=i2-i1+1
        return
        end
