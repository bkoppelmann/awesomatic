        subroutine csv(itri,ivert,ja,a,u,au,x,ax,
     1      sv,relerr,z1,z2)
c
            integer itri(4,*),ivert(4,*),ja(*)
            real a(*),u(*),au(*),x(*),ax(*),z1(*),z2(*)
c
        sv=0.0e0
        relerr=0.0e0
        nv=itri(1,4)
        call mtxmlt(ja,a,u,au,z1,z2,itri,ivert)
c
c       orthogonalize
c
        dd=0.0e0
        ds=0.0e0
        aa=0.0e0
        do 10 i=1,nv
            aa=amax1(aa,abs(au(i)))
            ds=ds+x(i)*u(i)
   10       dd=dd+x(i)*x(i)
        dd=dd-ds*ds
        if(aa.le.0.0e0) return
        if(dd.le.0.0e0) go to 80
        dd=0.0e0
        do 15 i=1,nv
            x(i)=x(i)-ds*u(i)
   15       dd=dd+x(i)*x(i)
        dd=1.0e0/sqrt(dd)
        do 20 i=1,nv
   20       x(i)=x(i)*dd
        call mtxmlt(ja,a,x,ax,z1,z2,itri,ivert)
c
c       compute inner producte for quadratic equation
c
        d0=0.0e0
        d1=0.0e0
        d2=0.0e0
        do 30 i=1,nv
            qu=au(i)/aa
            qx=ax(i)/aa
            d1=d1+qu*qu
            d2=d2+qx*qx
   30       d0=d0+qu*qx
c
c       coefficients of quadratic
c
        b=(d1+d2)/2.0e0
        c=amax1(d1*d2-d0*d0,0.0e0)
        d=(d1-d2)/2.0e0
        d=sqrt(d*d+d0*d0)
        ev=c/(b+d)
        sv=sqrt(ev)*aa
c
c       reset u
c
        if(abs(d2-ev).lt.abs(d0)) then
            sd=d0
            sn=d1-ev
        else
            sn=d0
            sd=d2-ev
        end if
        if(abs(sd).le.0.0e0) go to 100
        ss=-sn/sd
        dd=1.0e0/sqrt(1.0e0+ss*ss)
        relerr=abs(ss)*dd
        do 70 i=1,nv
             u(i)=(u(i)+ss*x(i))*dd
   70        au(i)=(au(i)+ss*ax(i))*dd
        return
c
c       the case of parallel vectors
c
   80   d1=0.0e0
        do 90 i=1,nv
   90       d1=d1+(au(i)/aa)**2
        relerr=0.0e0
        sv=sqrt(d1)*aa
        return
c
c       the case where new singular vector has
c       no component in u direction
c
  100   relerr=1.0e0
        do 105 i=1,nv
             u(i)=x(i)
  105        au(i)=ax(i)
        return
        end
