        subroutine cpth(l0,l1,r0,r1,l0dot,l1dot,r0dot,r1dot,
     1      xscale,yscale,xshift,yshift,xl,xr,yb,yt)
c
            real l0,l1,l0dot,l1dot,x(101),y(101)
c
c       compute the parabola thru two points and evaluate
c
        dr=r1-r0
        dl=l1-l0
        al=sqrt(dr*dr+dl*dl)
        if(al.eq.0.0e0) return
        c1=dr/al
        s1=dl/al
        xd1=c1*r1dot+s1*l1dot
        yd1=c1*l1dot-s1*r1dot
        xd0=c1*r0dot+s1*l0dot
        yd0=c1*l0dot-s1*r0dot
c
c
c       we are solving 4 eqns in 4 unknowns (c,q,pr,pl)
c       the eqns are (x,y) = m d (u,v) + (pr,pl)
c           m= 2x2 orthogonal
c           d = diag(1 q)
c           v= u**2
c           (solve by first eliminating pr,pl using data points
c            and the solving tangent equations for c,q)
c           it is ok to consider one point at the origin and one at (al,0)
c
        w0=2.0e0*yd0*yd1
        w1=-(xd0*yd1+xd1*yd0)
        a=sqrt(w0*w0+w1*w1)
        if(abs(a).lt.1.e-2) go to 10
        c2=w0/a
        s2=w1/a
        ud0=c2*xd0+s2*yd0
        if(abs(ud0).le.1.e-2) go to 10
        vd0=c2*yd0-s2*xd0
        c=c1*c2-s1*s2
        s=c1*s2+s1*c2
        b=((yd0*yd1)/a)*((yd1*al)/a)
        q=-1.0e0/(4.0e0*b*ud0)
        z=b*vd0/ud0
        pr=r0+(c*2.0e0*ud0-s*vd0)*z
        pl=l0+(c*vd0+s*2.0e0*ud0)*z
c
c     compute number of points
c
        num=int(abs(r0-r1)*yscale*50.0e0)+
     1      int(abs(l0-l1)*xscale*50.0e0)
        num=min0(101,num)
        if(num.le.2) go to 10
        u0=c*(r0-pr)+s*(l0-pl)
        u1=c*(r1-pr)+s*(l1-pl)
        h=(u1-u0)/float(num-1)
        do 5 i=1,num
            u=u0+float(i-1)*h
            v=q*u*u
            y(i)=(pr+c*u-s*v)*yscale+yshift
            x(i)=(pl+s*u+c*v)*xscale+xshift
            if(x(i).lt.xl.or.x(i).gt.xr) go to 10
            if(y(i).lt.yb.or.y(i).gt.yt) go to 10
    5   continue
        call pline(x,y,num)
        return
c
c    use straight line approximation
c
   10   num=2
        y(1)=r0*yscale+yshift
        y(2)=r1*yscale+yshift
        x(1)=l0*xscale+xshift
        x(2)=l1*xscale+xshift
        call pline(x,y,num)
        return
        end
