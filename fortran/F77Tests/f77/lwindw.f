        subroutine lwindw(x,y,n,t)
c
            real x(*),y(*),t(25)
            real xx(2),yy(2)
c
c       draw the part of the picture within the current window
c
        rmag=t(12)
        if(rmag.le.1.0e0) go to 200
c
        xl=t(8)
        xr=t(9)
        yb=t(10)
        yt=t(11)
        shift=0.05e0
c
c       the main loop
c
        do 100 i=2,n
            xx(1)=x(i-1)
            yy(1)=y(i-1)
            xx(2)=x(i)
            yy(2)=y(i)
c
c       fit line into window in x direction
c
            jl=1
            if(xx(2).lt.xx(1)) jl=2
            jr=3-jl
            if(xx(jr).le.xl.or.xx(jl).ge.xr) go to 100
c
c       left side
c
            if(xx(jl).ge.xl) go to 10
            dx=xx(jr)-xx(jl)
            if(dx.le.0.0e0) go to 20
            f=(xx(jr)-xl)/dx
            xx(jl)=xl
            yy(jl)=yy(jl)*f+yy(jr)*(1.0e0-f)
c
c       right side
c
   10       if(xx(jr).le.xr) go to 20
            dx=xx(jr)-xx(jl)
            if(dx.le.0.0e0) go to 20
            f=(xr-xx(jl))/dx
            xx(jr)=xr
            yy(jr)=yy(jr)*f+yy(jl)*(1.0e0-f)
c
c       fit line into window in y direction
c
   20       jb=1
            if(yy(2).lt.yy(1)) jb=2
            jt=3-jb
            if(yy(jt).le.yb.or.yy(jb).ge.yt) go to 100
c
c       bottom
c
            if(yy(jb).ge.yb) go to 30
            dy=yy(jt)-yy(jb)
            if(dy.le.0.0e0) go to 40
            f=(yy(jt)-yb)/dy
            yy(jb)=yb
            xx(jb)=xx(jb)*f+xx(jt)*(1.0e0-f)
c
c       top
c
   30       if(yy(jt).le.yt) go to 40
            dy=yy(jt)-yy(jb)
            if(dy.le.0.0e0) go to 40
            f=(yt-yy(jb))/dy
            yy(jt)=yt
            xx(jt)=xx(jt)*f+xx(jb)*(1.0e0-f)
c
c       rescale and then draw
c
   40       do 50 j=1,2
                xx(j)=(xx(j)-xl)*rmag+shift
   50           yy(j)=(yy(j)-yb)*rmag+shift
            call pline(xx,yy,2)
  100   continue
        return
  200   call pline(x,y,n)
        return
        end
