        subroutine mtxmlt(ja,a,x,y,z,v,itri,ivert)
c
            integer itri(4,*),ivert(4,*),ja(*),getlvl
            real a(*),x(*),y(*),v(*),z(*)
c
c       ispd = 1   symmetric
c            = 0   non-symmetric
c            =-1   non-symmetric for a-transpose
c
c       compute y=a*x
c
        lvl=itri(1,3)
        level1=itri(2,3)
        nv=itri(1,4)
        n2=itri(2,4)
        ispd=itri(4,8)
        ishift=0
        lshift=0
        if(ispd.eq.0) lshift=ja(nv+1)-ja(1)
        if(ispd.eq.-1) ishift=ja(nv+1)-ja(1)
c
        do 10 i=1,nv
        z(i)=x(i)
        if(idbc(i,ivert).eq.1) z(i)=0.0e0
        y(i)=0.0e0
   10   v(i)=0.0e0
c
c       level1 matrix
c
        do 30 i=1,n2
            j1=ja(i)
            j2=ja(i+1)-1
            s=a(i)*x(i)
            if(j1.gt.j2) go to 30
            do 20 jj=j1,j2
                j=ja(jj)
                jl=jj+lshift
                ju=jj+ishift
                s=s+a(jl)*x(j)
   20           y(j)=y(j)+a(ju)*x(i)
   30       y(i)=y(i)+s
c
c       the remaining levels
c
        if(lvl.le.level1) return
        l1=level1+1
        do 80 ll=l1,lvl
            n1=n2+1
            n2=getlvl(ll,itri)
            do 40 i=n1,n2
                j=ja(i)
                ivf1=ja(j)
                ivf2=ja(j+1)
                if(idbc(i,ivert).eq.0)
     1              z(i)=(z(ivf1)+z(ivf2))/2.0e0
   40       continue
            do 60 i=n1,n2
                j1=ja(i)
                j2=ja(i+1)-1
                s=a(i)*x(i)
                t=a(i)*z(i)
                do 50 jj=j1,j2
                    j=ja(jj)
                    jl=jj+lshift
                    ju=jj+ishift
                    s=s+a(jl)*x(j)
                    y(j)=y(j)+a(ju)*x(i)
                    t=t+a(jl)*z(j)
   50               v(j)=v(j)+a(ju)*z(i)
                y(i)=y(i)+s
   60           v(i)=v(i)+t
            do 70 i=n1,n2
                j=ja(i)
                ivf1=ja(j)
                ivf2=ja(j+1)
                z(i)=0.0e0
                q=v(i)/2.0e0
                v(i)=0.0e0
                if(idbc(i,ivert).eq.1) go to 70
                z(i)=x(i)
                if(idbc(ivf1,ivert).eq.0) v(ivf1)=v(ivf1)+q
                if(idbc(ivf2,ivert).eq.0) v(ivf2)=v(ivf2)+q
   70       continue
   80   continue
        do 90 i=1,nv
   90       y(i)=y(i)-v(i)
        return
        end
