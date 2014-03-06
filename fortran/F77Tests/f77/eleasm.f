        subroutine eleasm(vx,vy,u,rl,sh,iv,itag,
     1      a,b,d,p,isw,ifirst,a1xy,a2xy,fxy,p1xy)
c
            integer iv(3)
            real vx(*),vy(*),u(*),a(6,6),b(6),d(6),p(8)
            real c(3,6),wt(6),we(6),gv(6,6),gx(6,6),gy(6,6),
     1          ca1(5,6),ca2(5,6),cf(5,6),cp1(5,6),s(5),
     2          dx(2,6),dy(2,6)
            external a1xy,a2xy,fxy,p1xy
c
            save npts,wt,c
c
c       data
c
        data npts/3/
        data wt(1),wt(2),wt(3)/0.333333333333333e0,
     1      0.333333333333333e0,0.333333333333333e0/
c
        data c(1,1),c(2,1),c(3,1)/0.666666666666667e0,
     1      0.166666666666667e0,0.166666666666667e0/
        data c(1,2),c(2,2),c(3,2)/0.166666666666667e0,
     1      0.666666666666667e0,0.166666666666667e0/
        data c(1,3),c(2,3),c(3,3)/0.166666666666667e0,
     1      0.166666666666667e0,0.666666666666667e0/
c
c*      data npts/6/
c*      data wt(1),wt(2),wt(3),wt(4),wt(5),wt(6)/
c*   1      0.109951743655332e0,0.109951743655322e0,
c*   2      0.109951743655332e0,0.223381589678011e0,
c*   3      0.223381589678011e0,0.223381589678011e0/
c
c*      data c(1,1),c(2,1),c(3,1)/0.816847572980459e0,
c*   1      0.091576213509771e0,0.091576213509771e0/
c*      data c(1,2),c(2,2),c(3,2)/0.091576213509771e0,
c*   1      0.816847572980459e0,0.091576213509771e0/
c*      data c(1,3),c(2,3),c(3,3)/0.091576213509771e0,
c*   1      0.091576213509771e0,0.816847572980459e0/
c*      data c(1,4),c(2,4),c(3,4)/0.108103018168070e0,
c*   1      0.445948490915965e0,0.445498490915965e0/
c*      data c(1,5),c(2,5),c(3,5)/0.445498490915965e0,
c*   1      0.108103018168070e0,0.445498490915965e0/
c*      data c(1,6),c(2,6),c(3,6)/0.445498490915965e0,
c*   1      0.445948490915965e0,0.108103018168070e0/
c
c       this routine computes the element stiffness matrix and
c       right hand side
c
c       isw = 0   -  piecewise linear basis functions
c             1   -  piecewise quadratic bump functions
c             2   -  piecewise quadratic functions
c
c       read vertex numbers
c
        iv1=iv(1)
        iv2=iv(2)
        iv3=iv(3)
c
c       compute tangent vectors and dot products
c
        tx1=vx(iv3)-vx(iv2)
        ty1=vy(iv3)-vy(iv2)
        tx2=vx(iv1)-vx(iv3)
        ty2=vy(iv1)-vy(iv3)
        tx3=vx(iv2)-vx(iv1)
        ty3=vy(iv2)-vy(iv1)
        det=tx2*ty3-tx3*ty2
c
c       compute normal directions
c
        x2=-ty2/det
        y2=tx2/det
        x3=-ty3/det
        y3=tx3/det
        x1=-(x2+x3)
        y1=-(y2+y3)
        det=abs(det)/2.0e0
c
c       compute gradient
c
        u2=u(iv2)-u(iv1)
        u3=u(iv3)-u(iv1)
        ux=u2*x2+u3*x3
        uy=u2*y2+u3*y3
c
        istart=1
        ifin=6
        if(isw.eq.0) ifin=3
        if(isw.eq.1) istart=4
c
c
        p(7)=0.0e0
        p(8)=0.0e0
        do 7 i=1,npts
c
c       evaluate linear basis functions
c
            if(istart.eq.1) then
                gv(1,i)=c(1,i)
                gv(2,i)=c(2,i)
                gv(3,i)=c(3,i)
                gx(1,i)=x1
                gy(1,i)=y1
                gx(2,i)=x2
                gy(2,i)=y2
                gx(3,i)=x3
                gy(3,i)=y3
            endif
c
c       evaluate quadratic bump functions
c
            if(ifin.eq.6) then
                c1=4.0e0*c(1,i)
                c2=4.0e0*c(2,i)
                c3=4.0e0*c(3,i)
                gv(4,i)=c2*c(3,i)
                gv(5,i)=c3*c(1,i)
                gv(6,i)=c1*c(2,i)
                gx(4,i)=x2*c3+c2*x3
                gy(4,i)=y2*c3+c2*y3
                gx(5,i)=x3*c1+c3*x1
                gy(5,i)=y3*c1+c3*y1
                gx(6,i)=x1*c2+c1*x2
                gy(6,i)=y1*c2+c1*y2
            endif
c
c       function evaluations
c
            we(i)=wt(i)*det
            x=c(1,i)*vx(iv1)+c(2,i)*vx(iv2)+c(3,i)*vx(iv3)
            y=c(1,i)*vy(iv1)+c(2,i)*vy(iv2)+c(3,i)*vy(iv3)
            uu=c(1,i)*u(iv1)+c(2,i)* u(iv2)+c(3,i)* u(iv3)
            do 5 j=1,5
                ca1(j,i)=a1xy(x,y,uu,ux,uy,rl,itag,j)
                ca2(j,i)=a2xy(x,y,uu,ux,uy,rl,itag,j)
                cp1(j,i)=p1xy(x,y,uu,ux,uy,rl,itag,j)
    5           cf(j,i)=fxy(x,y,uu,ux,uy,rl,itag,j)
c
c       upwind terms
c
            do 6 j=1,2
                if(j.eq.1) then
                    bx=ca1(2,i)
                    by=ca2(2,i)
                else
                    bx=cf(3,i)
                    by=cf(4,i)
                endif
                if(abs(bx)+abs(by).gt.0.0e0) then
                    a11=ca1(3,i)
                    a22=ca2(4,i)
                    if(j.eq.1) then
                        a12=ca1(4,i)
                        a21=ca2(3,i)
                    else
                        a21=ca1(4,i)
                        a12=ca2(3,i)
                    endif
                    aa=a11*a22-a12*a21
                    abx=(a22*bx-a12*by)/aa
                    aby=(a11*by-a21*bx)/aa
                    z1=-(vx(iv1)*abx+vy(iv1)*aby)
                    z2=-(vx(iv2)*abx+vy(iv2)*aby)
                    z3=-(vx(iv3)*abx+vy(iv3)*aby)
                    zmax=amax1(z1,z2,z3)
                    z1=exp(z1-zmax)
                    z2=exp(z2-zmax)
                    z3=exp(z3-zmax)
                    zx=z1*x1+z2*x2+z3*x3
                    zy=z1*y1+z2*y2+z3*y3
                    zz=z1*c(1,i)+z2*c(2,i)+z3*c(3,i)
                    qq=-(zx*bx+zy*by)
                    dx(j,i)=(a11*zx+a12*zy+zz*bx)/qq
                    dy(j,i)=(a21*zx+a22*zy+zz*by)/qq
                else
                    dx(j,i)=0.0e0
                    dy(j,i)=0.0e0
                endif
    6       continue
c
c       functional of solution
c
            p(7)=p(7)+cp1(5,i)*we(i)
            p(8)=p(8)+cp1(1,i)*we(i)
c
    7       cf(2,i)=cf(2,i)+sh
c
c       element assembly
c
        do 12 i=istart,ifin
            b(i)=0.0e0
            d(i)=0.0e0
            p(i)=0.0e0
            do 8 j=istart,ifin
    8           a(i,j)=0.0e0
            do 11 k=1,npts
                qx=we(k)*gx(i,k)
                qy=we(k)*gy(i,k)
                qv=we(k)*gv(i,k)
                upa=dx(1,k)*qx+dy(1,k)*qy
                upf=cf(3,k)*qx+cf(4,k)*qy
                upx=ca1(2,k)*upa+dx(2,k)*upf
                upy=ca2(2,k)*upa+dy(2,k)*upf
                do 9 j=1,5
    9               s(j)=ca1(j,k)*qx+ca2(j,k)*qy+cf(j,k)*qv
                b(i)=b(i)+s(1)+ux*upx+uy*upy
                d(i)=d(i)+s(5)
                p(i)=p(i)+cp1(2,k)*qv+cp1(3,k)*qx+cp1(4,k)*qy
                do 10 j=istart,ifin
   10               a(i,j)=a(i,j)+s(2)*gv(j,k)
     1                  +(s(3)+upx)*gx(j,k)+(s(4)+upy)*gy(j,k)
   11       continue
   12   continue
        return
        end
