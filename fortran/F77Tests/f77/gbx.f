        subroutine gbx(tu,tv,iv,gmin,gmax,tmin,tmax,eps)
c
            real tu(*),tv(*),rr(3),ang(3)
            integer iv(3)
c
c        compute min and max of vector function modulus on triangle
c
        pi2=3.141592653589793e0*2.0e0
c
c    check vertices
c
        do 5 j=1,3
            ivj=iv(j)
            rr(j)=sqrt(tu(ivj)*tu(ivj)+tv(ivj)*tv(ivj))
    5   continue
        gmax=rr(1)
        gmin=gmax
        tmin=pi2
        tmax=0.0e0
        do 15 j=1,3
            gmax=amax1(gmax,rr(j))
            gmin=amin1(gmin,rr(j))
            ang(j)=0.0e0
            if(rr(j).gt.0.0e0) then
                arg=amin1(tu(iv(j))/rr(j),1.0e0)
                arg=amax1(-1.0e0,arg)
                theta=acos(arg)
                if(tv(iv(j)).lt.0.0e0) theta=pi2-theta
                tmin=amin1(tmin,theta)
                tmax=amax1(tmax,theta)
                ang(j)=theta
            endif
   15   continue  
        if(gmax.le.0.0e0) then
            tmin=0.0e0
            tmax=0.0e0
            return
        endif
c
c       check bari center
c
        u1=tu(iv(1))-tu(iv(3))
        u2=tu(iv(2))-tu(iv(3))
        u3=-tu(iv(3))
        v1=tv(iv(1))-tv(iv(3))
        v2=tv(iv(2))-tv(iv(3))
        v3=-tv(iv(3))
        a1=u1*u1+v1*v1
        a12=u1*u2+v1*v2
        a2=u2*u2+v2*v2
        r1=u1*u3+v1*v3
        r2=u2*u3+v2*v3
        det=a1*a2-a12*a12
        if(det.gt.0.0e0) then
            c1=(r1*a2-r2*a12)/det
            if(c1.lt.-eps.or.c1.gt.1.0e0+eps) go to 20
            c2=(a1*r2-a12*r1)/det
            if(c2.lt.-eps.or.c2.gt.1.0e0+eps) go to 20
            c3=1.0e0-c1-c2
            if(c3.lt.-eps.or.c3.gt.1.0e0+eps) go to 20
            gmin=0.0e0
            tmin=0.0e0
            tmax=pi2
            return
        endif
c
c    look on edges
c
   20   umax=0.0e0
        do 25 j=1,3
            j1=(5-j)/2
            j2=6-j-j1
            u1=tu(iv(j1))-tu(iv(j2))
            v1=tv(iv(j1))-tv(iv(j2))
c
c    check for min radius
c
            a1=u1*u1+v1*v1
            if(a1.gt.0.0e0) then
                c1=-(u1*tu(iv(j2))+v1*tv(iv(j2)))/a1
                if(c1.ge.0.0e0.and.c1.le.1.0e0) then 
                    ut=tu(iv(j2))+c1*u1
                    vt=tv(iv(j2))+c1*v1
                    s=sqrt(ut*ut+vt*vt)
                    gmin=amin1(gmin,s)
                endif
            endif
c
c      check for crossing of positive x axis
c
            if(v1.ne.0.0e0) then
                c1=-tv(iv(j2))/v1
                if(c1.ge.0.0e0.and.c1.le.1.0e0) 
     1              umax=amax1(umax,tu(iv(j2))+c1*u1)
            endif
c
   25   continue
        if(umax.gt.0.0e0) then
            do 30 j=1,3
                if(tv(iv(j)).ge.0.0e0) ang(j)=ang(j)+pi2
   30       continue
            tmin=amin1(ang(1),ang(2))
            tmin=amin1(ang(3),tmin)
            tmax=amax1(ang(1),ang(2))
            tmax=amax1(ang(3),tmax)
        endif
        return
        end
