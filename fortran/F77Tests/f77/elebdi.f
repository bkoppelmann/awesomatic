        subroutine elebdi(vx,vy,u,rl,iv,jtag,iside,p,isw,p2xy)
c
            integer iv(3)
            real vx(*),vy(*),u(*),p(8)
            real c(2,3),wt(3),cp(5),gv(6),gx(6),gy(6)
            external p2xy
c
            save npts,wt,c
c
c       data
c
        data npts/2/
        data wt(1),wt(2)/0.5e0,0.5e0/
c
        data c(1,1),c(2,1)/0.788675134594813e0,0.211324865405187e0/
        data c(1,2),c(2,2)/0.211324865405187e0,0.788675134594813e0/
c
c*      data npts/3/
c*      data wt(1),wt(2),wt(3)/0.277777777777778e0,
c*   1      0.27777770,0.444444444444444e0/
c
c*      data c(1,1),c(2,1)/0.887298334629742e0,0.112701665379238e0/
c*      data c(1,2),c(2,2)/0.112701665379238e0,0.887298334620742e0/
c*      data c(1,3),c(2,3)/0.5e0,0.5e0/
c
c       this routine computes element wise boundary integrals
c
c       j1,j2 are the values of 1,2,3, other than iside
c
        j3=iside
        j2=(5-iside)/2
    1   j1=j2
        j2=6-iside-j1
c
c       read vertex numbers
c
        iv1=iv(j1)
        iv2=iv(j2)
        iv3=iv(j3)
c
c       compute matrix elements for affine transformation
c
        x1=vy(iv2)-vy(iv3)
        x2=vy(iv3)-vy(iv1)
        x3=vy(iv1)-vy(iv2)
        y1=vx(iv3)-vx(iv2)
        y2=vx(iv1)-vx(iv3)
        y3=vx(iv2)-vx(iv1)
        det=x2*y3-x3*y2
        if(det.lt.0.0e0) go to 1
        h=sqrt(x3*x3+y3*y3)
        dx=x3/h
        dy=y3/h
c
        x2=x2/det
        y2=y2/det
        x3=x3/det
        y3=y3/det
        x1=-(x2+x3)
        y1=-(y2+y3)
c
c       compute gradient
c
        u2=u(iv2)-u(iv1)
        u3=u(iv3)-u(iv1)
        ux=u2*x2+u3*x3
        uy=u2*y2+u3*y3
c
c
        istart=1
        ifin=6
        if(isw.eq.0) ifin=3
        if(isw.eq.1) istart=4
        j4=3+j1
        j5=3+j2
        j6=3+j3
c
        do 8 i=1,npts
c
c       evaluate linear basis functions
c
            gv(j1)=c(1,i)
            gv(j2)=c(2,i)
            gv(j3)=0.0e0
            gx(j1)=x1
            gy(j1)=y1
            gx(j2)=x2
            gy(j2)=y2
            gx(j3)=x3
            gy(j3)=y3
            if(isw.eq.0) go to 5
c
c       evaluate quadratic bump functions
c
            c1=4.0e0*c(1,i)
            c2=4.0e0*c(2,i)
            gv(j4)=0.0e0
            gv(j5)=0.0e0
            gv(j6)=c1*c(2,i)
            gx(j4)=c2*x3
            gy(j4)=c2*y3
            gx(j5)=x3*c1
            gy(j5)=y3*c1
            gx(j6)=x1*c2+c1*x2
            gy(j6)=y1*c2+c1*y2
c
c       function evaluations
c
    5       we=wt(i)*h
            x=c(1,i)*vx(iv1)+c(2,i)*vx(iv2)
            y=c(1,i)*vy(iv1)+c(2,i)*vy(iv2)
            uu=c(1,i)*u(iv1)+c(2,i)*u(iv2)
            do 6 k=1,5
                cp(k)=p2xy(x,y,dx,dy,uu,ux,uy,rl,jtag,k)
    6       continue
            do 7 j=istart,ifin
                p(j)=p(j)+(cp(2)*gv(j)+cp(3)*gx(j)+
     1              cp(4)*gy(j))*we
    7       continue
            p(7)=p(7)+cp(5)*we
            p(8)=p(8)+cp(1)*we
    8   continue
        return
        end
