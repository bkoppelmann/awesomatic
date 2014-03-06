        subroutine arc(x1,y1,x2,y2,xc,yc,theta1,theta2,r)
c
c       compute the parametric representation of the arc of
c       the circle passing through (x1,y1) and (x2,y2) with
c       center at (xc,yc).
c
        v1=x1-xc
        w1=y1-yc
        v2=x2-xc
        w2=y2-yc
        r=sqrt(v1*v1+w1*w1)
        theta1=amax1(-1.0e0,v1/r)
        theta1=amin1(1.0e0,theta1)
        theta1=acos(theta1)
        if(w1.lt.0.0e0) theta1=-theta1
        q=(v1*v2+w1*w2)/(r*r)
        q=amin1(1.0e0,q)
        q=amax1(-1.0e0,q)
        theta=acos(q)
        theta2=theta1+theta
        q=v2/r-cos(theta2)
        s=w2/r-sin(theta2)
        if(q*q+s*s.le.1.e-6) return
        theta2=theta1-theta
        return
        end
