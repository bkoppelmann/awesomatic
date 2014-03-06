        subroutine liarc(p,dp,q,t,r,npts,al,ang,eps)
c
            real p(2),dp(2),q(2),t(2),al(2),ang(2)
c
c       compute the intersection,if any, between the line
c       given by p and dp and the arc with center q , radius r
c       and theta range t.
c
        pi=3.141592653589793e0
        twopi=2.0e0*pi
        rr=abs(r)
        x=(p(1)-q(1))/rr
        y=(p(2)-q(2))/rr
        dx=dp(1)/rr
        dy=dp(2)/rr
c
c       solve quadratic
c
        c=x*x+y*y-1.0e0
        b=-(x*dx+y*dy)
        a=dx*dx+dy*dy
        disc=b*b-a*c
        if(disc) 10,20,30
c
   10   npts=0
        return
   20   npts=1
        al(1)=b/a
        go to 40
   30   d=sqrt(disc)
        npts=2
        if(b.lt.0.0e0) go to 35
        al(1)=(b+d)/a
        al(2)=c/(b+d)
        go to 40
   35   al(1)=c/(b-d)
        al(2)=(b-d)/a
c
c       compute theta values
c
   40   tmin=amin1(t(1),t(2))
        tmax=amax1(t(1),t(2))
        tol=eps*twopi
        do 70 i=1,npts
            x=(p(1)-q(1)+al(i)*dp(1))/rr
            y=(p(2)-q(2)+al(i)*dp(2))/rr
            x=amin1(1.0e0,x)
            x=amax1(-1.0e0,x)
            th=acos(x)
            if(y.lt.0.0e0) th=-th
            do 50 j=1,5
                theta=th+float(j-3)*twopi
                if(abs(theta-tmin).le.tol) theta=tmin
                if(theta.ge.tmin) go to 60
   50       continue
   60       if(abs(theta-tmax).le.tol) theta=tmax
            ang(i)=theta
   70   continue
        if(npts.eq.2.and.ang(2).lt.ang(1)) then
            a=ang(1)
            ang(1)=ang(2)
            ang(2)=a
            a=al(1)
            al(1)=al(2)
            al(2)=a
        endif
        if(ang(1).gt.tmax) npts=0
        if(npts.eq.2.and.ang(2).gt.tmax) npts=1
        return
        end
