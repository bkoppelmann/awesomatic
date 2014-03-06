        subroutine eleun(vx,vy,u,iv,rl,ux1,uy1,tag1,
     1      ux2,uy2,tag2,iside,r,a1xy,a2xy)
c
            integer iv(3),tag1,tag2
            real vx(*),vy(*),u(*),r(6)
            real c(2,3),wt(3)
            external a1xy,a2xy
c
            save npts,wt,c
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
c       this routine computes a weighted average normal
c       derivative along an interior triangle edge
c
c       theta is the weighting factor
c
c       (dx,dy) is unit normal x length of edge
c       connecting (vx(iv1),vy(iv1)) with (vx(iv2),vy(iv2))
c
        j1=(5-iside)/2
        j2=6-iside-j1
        jj=iside+3
        iv1=iv(j1)
        iv2=iv(j2)
        ivj=iv(iside)
c
        q1=ux1*ux1+uy1*uy1
        q2=ux2*ux2+uy2*uy2
        theta=0.5e0
        if(q1+q2.gt.0.0e0) theta=q1/(q1+q2)
c
c       determine normal direction
c
        dx=vy(iv2)-vy(iv1)
        dy=vx(iv1)-vx(iv2)
        det=(vx(iv1)-vx(ivj))*dx+(vy(iv1)-vy(ivj))*dy
        if(det.gt.0.0e0) go to 2
        dx=-dx
        dy=-dy
c
    2   s1=0.0e0
        s2=0.0e0
        s3=0.0e0
        do 4 i=1,npts
            x=c(1,i)*vx(iv1)+c(2,i)*vx(iv2)
            y=c(1,i)*vy(iv1)+c(2,i)*vy(iv2)
            uu=c(1,i)*u(iv1)+c(2,i)*u(iv2)
            ax1=a1xy(x,y,uu,ux1,uy1,rl,tag1,1)
            ay1=a2xy(x,y,uu,ux1,uy1,rl,tag1,1)
            ax2=a1xy(x,y,uu,ux2,uy2,rl,tag2,1)
            ay2=a2xy(x,y,uu,ux2,uy2,rl,tag2,1)
            un=((ax1*dx+ay1*dy)*theta+
     1          (ax2*dx+ay2*dy)*(1.0e0-theta))*wt(i)
            s1=s1+un*c(1,i)
            s2=s2+un*c(2,i)
            s3=s3+un*c(1,i)*c(2,i)*4.0e0
    4   continue
        r(j1)=r(j1)-s1
        r(j2)=r(j2)-s2
        r(jj)=r(jj)-s3
        return
        end
