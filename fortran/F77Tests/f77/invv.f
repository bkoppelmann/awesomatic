        subroutine invv(nv,ne,maxv,vx,vy,iusr,poly,cxy,pram,iflag)
c
            integer iusr(6,*)
            real vx(*),vy(*),poly(2,*),cxy(5,*),pram(7)
c
c       divide user specified edges
c
        iflag=0
        iusr(6,1)=nv+1
        do 10 i=1,ne
            j1=iusr(1,i)
            j2=iusr(2,i)
c
c       the case of a curves edge
c
            if(iusr(3,i).le.0) go to 5
            j=iusr(3,i)
            xc=cxy(1,j)
            yc=cxy(2,j)
            theta1=cxy(3,j)
            theta2=cxy(4,j)
            r=cxy(5,j)
            d=abs(theta2-theta1)*r
            call dvpram(poly(1,j1),poly(1,j2),d,pram,al,h,np)
            iusr(6,i+1)=iusr(6,i)+np
            if(nv+np.gt.maxv) go to 20
c
c       add new points on circular arc
c
            if(np.eq.0) go to 10
            dt=theta2-theta1
            q=0.0e0
            do 2 j=1,np
                q=q+h
                h=h*al
                arg=theta1+q*dt
                nv=nv+1
                vx(nv)=xc+r*cos(arg)
    2           vy(nv)=yc+r*sin(arg)
            go to 10
c
c       the case of a straight edge
c
    5       d=sqrt((vx(j1)-vx(j2))*(vx(j1)-vx(j2))+
     1          (vy(j1)-vy(j2))*(vy(j1)-vy(j2)))
            call dvpram(poly(1,j1),poly(1,j2),d,pram,al,h,np)
            iusr(6,i+1)=iusr(6,i)+np
            if(nv+np.gt.maxv) go to 20
c
c       add new vertices along a line segment
c
             if(np.eq.0) go to 10
             p1=vx(j1)
             p2=vy(j1)
             dp1=vx(j2)-p1
             dp2=vy(j2)-p2
             q=0.0e0
             do 7 j=1,np
                 q=q+h
                 h=h*al
                 nv=nv+1
                 vx(nv)=p1+q*dp1
    7            vy(nv)=p2+q*dp2
   10       continue
        return
   20   iflag=56
        return
        end
