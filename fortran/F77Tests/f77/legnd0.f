        subroutine legnd0(t)
c
            real x(6),y(6),t(25)
c
c       helps locating current window (draw boundary in small window)
c
        size=t(16)
        xm=t(17)
        ym=t(18)
        x0=xm-size/2.0e0
        x1=xm+size/2.0e0
        y0=ym-size/2.0e0
        y1=ym+size/2.0e0
c
c       mark magnified area
c
        if(t(15).gt.1.0e0) then
            xl=amax1(x0,t(8)*size+x0)
            xr=amin1(x1,t(9)*size+x0)
            yb=amax1(y0,t(10)*size+y0)
            yt=amin1(y1,t(11)*size+y0)
c
c           mark the box in the window
c
            if(abs(xl-xr).lt.0.02e0) then
                x(1)=(xl+xr)/2.0e0
                x(2)=x(1)
                y(1)=y0
                y(2)=y1
                call pline(x,y,2)
                x(1)=x0
                x(2)=x1
                y(1)=(yb+yt)/2.0e0
                y(2)=y(1)
                call pline(x,y,2)
            else
                x(1)=xl
                y(1)=yb
                x(2)=xr
                y(2)=y(1)
                x(3)=x(2)
                y(3)=yt
                x(4)=x(1)
                y(4)=y(3)
                x(5)=x(1)
                y(5)=y(1)
                call pline(x,y,5)
            endif
        endif
c
        x(1)=x0
        y(1)=y0
        x(2)=x1
        y(2)=y0
        x(3)=x1
        y(3)=y1
        x(4)=x0
        y(4)=y1
        x(5)=x0
        y(5)=y0
        call pline(x,y,5)
        return
        end
