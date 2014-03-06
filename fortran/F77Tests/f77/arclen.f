        function arclen(p,coeff)
c
            real coeff(5),p(2)
c
c       this routine computes the arclength (and curvature)
c       at the point p on the parabola (arclength relative to
c       the turning point of the parabola, signed the samed as u)
c
        dr=p(1)-coeff(3)
        dl=p(2)-coeff(4)
        c=coeff(1)
        s=coeff(2)
        q=coeff(5)
        u=c*dr+s*dl
        v=c*dl-s*dr
        if(q.eq.0.0e0) go to 10
        z=abs(q*u)
        t=sqrt(1.0e0+z*z)
        arclen=u*(t+alog(z+t))/2.0e0
c       crvtre=abs(q)/(t*t*t)
        return
   10   arclen=u
c       crvtre=0.0e0
        return
        end
