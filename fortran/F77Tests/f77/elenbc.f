        subroutine elenbc(vx,vy,u,iv,rl,itag,iside,a,b,d,isw,gxy)
c
            integer iv(3)
            real vx(*),vy(*),u(*),a(6,6),b(6),d(6)
            real c(2,3),wt(3),we(3),gv(3,3),g1(3),g2(3),g3(3)
            external gxy
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
c*   1      0.277777777777778e0,0.444444444444444e0/
c
c*      data c(1,1),c(2,1)/0.887298334629742e0,0.112701665379238e0/
c*      data c(1,2),c(2,2)/0.112701665379238e0,0.887298334620742e0/
c*      data c(1,3),c(2,3)/0.5e0,0.5e0/
c
c       this routine computes the contribution to the element
c       from the natural boundary conditions.
c
c       isw = 0   -  piecewise linear basis functions
c             1   -  piecewise quadratic bump functions
c             2   -  piecewise quadratic functions
c
c       j1,j2 are the values of 1,2,3, other than iside
c
        j1=(5-iside)/2
        j2=6-iside-j1
        jj=iside+3
c
c       do basis function and gxy evaluations
c
        iv1=iv(j1)
        iv2=iv(j2)
        h=(vx(iv1)-vx(iv2))*(vx(iv1)-vx(iv2))+
     1      (vy(iv1)-vy(iv2))*(vy(iv1)-vy(iv2))
        h=sqrt(h)
c
        do 4 i=1,npts
            gv(1,i)=c(1,i)
            gv(2,i)=c(2,i)
            gv(3,i)=4.0e0*c(1,i)*c(2,i)
            we(i)=wt(i)*h
            x=c(1,i)*vx(iv1)+c(2,i)*vx(iv2)
            y=c(1,i)*vy(iv1)+c(2,i)*vy(iv2)
            uu=c(1,i)*u(iv1)+c(2,i)* u(iv2)
            g1(i)=gxy(x,y,uu,rl,itag,1)
            g2(i)=gxy(x,y,uu,rl,itag,2)
    4       g3(i)=gxy(x,y,uu,rl,itag,3)
c
c       element assembly
c
        if(isw.eq.1) go to 8
        ii=j2
        do 7 i=1,2
            ii=6-ii-iside
            do 6 k=1,npts
                q=we(k)*gv(i,k)
                b(ii)=b(ii)-g1(k)*q
                d(ii)=d(ii)-g3(k)*q
                a(ii,j1)=a(ii,j1)-g2(k)*q*gv(1,k)
                a(ii,j2)=a(ii,j2)-g2(k)*q*gv(2,k)
    6       continue
    7   continue
        if (isw.eq.0) return
    8   do 9 k=1,npts
            q=we(k)*gv(3,k)
            b(jj)=b(jj)-g1(k)*q
            d(jj)=d(jj)-g3(k)*q
    9       a(jj,jj)=a(jj,jj)-g2(k)*q*gv(3,k)
        return
        end
