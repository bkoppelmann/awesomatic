        subroutine lip(p,dp,al,num,coeff)
c
            real p(2),dp(2),al(2),coeff(5)
c
c       this routine determines the number of point (0,1,or 2)
c       and the locations (al(j)) where the line specified by
c       p and dp intersects the parabola given in coeff
c
        num=0
        dr=p(1)-coeff(3)
        dl=p(2)-coeff(4)
        c=coeff(1)
        s=coeff(2)
        q=coeff(5)
        u=c*dr+s*dl
        v=c*dl-s*dr
        du=c*dp(1)+s*dp(2)
        dv=c*dp(2)-s*dp(1)
        a=-q*du*du/2.0e0
        b=(q*u*du-dv)/2.0e0
        c=v-q*u*u/2.0e0
        if(a.eq.0.0e0) go to 10
        d=b*b-a*c
        if(d.lt.0.0e0) return
        num=2
        if(d.eq.0.0e0) num=1
        d=sqrt(d)
        if(b.lt.0.0e0) d=-d
        al(1)=(b+d)/a
        al(2)=c/(b+d)
        return
c
c       special case - 2 lines or a vertical line
c
   10   if(b.eq.0.0e0) return
        num=1
        al(1)=c/(2.0e0*b)
        return
        end
