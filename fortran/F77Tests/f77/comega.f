        subroutine comega(ja,a,itri,ivert,z,omega)
c
            integer itri(4,*),ivert(4,*),ja(*),getlvl
            real a(*),z(*),omega(*)
c
c       compute the relaxation parameter for each level
c
        lvl=itri(1,3)
        level1=itri(2,3)
        if(level1.ge.lvl) return
        nv=itri(1,4)
        ispd=itri(4,8)
        ishift=0
        lshift=0
        if(ispd.eq.0) lshift=ja(nv+1)-ja(1)
        if(ispd.eq.-1) ishift=ja(nv+1)-ja(1)
c
        do 10 i=1,nv
   10        z(i)=0.0e0
c
        n1=nv+1
        do 80 ll=lvl,level1+1,-1
            n2=n1-1
            n1=getlvl(ll-1,itri)+1
            do 30 i=n1,n2
                j1=ja(i)
                j2=ja(i+1)-1
                do 20 jj=j1,j2
                    j=ja(jj)
                    if(j.lt.n1) go to 20
                    sym=(a(jj+lshift)+a(jj+ishift))/2.0e0
                    skew=(a(jj+lshift)-a(jj+ishift))/2.0e0
                    z(i)=z(i)+abs(skew)+abs(sym)
                    z(j)=z(j)+abs(skew)+abs(sym)
   20           continue
   30       continue
c
c       this formula is empirical
c
            q=1.4e0
            do 40 i=n1,n2
                if(z(i).gt.0.0e0) q=amin1(q,abs(a(i))/z(i))
   40       continue
            omega(ll)=amax1(1.0e-2,q)
   80   continue
        return
        end
