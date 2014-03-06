        subroutine grinit(ip,jp,red,green,blue)
c
            integer ip(100),jp(25)
            real red(10),green(10),blue(10)
c
        iprob=ip(7)
        mxcolr=max0(2,ip(72))
        igrsw=ip(61)
        if(igrsw.lt.-1.or.igrsw.gt.14) igrsw=0
c
c       background
c
        red(1)=1.0e0
        green(1)=1.0e0
        blue(1)=1.0e0
c
c       foreground
c
        red(2)=0.0e0
        green(2)=0.0e0
        blue(2)=0.0e0
c
c       red, blue, green, cyan, magenta, yellow
c
        red(3)=1.0e0
        green(3)=0.0e0
        blue(3)=0.0e0
c
        red(4)=0.0e0
        green(4)=0.0e0
        blue(4)=1.0e0
c
        red(5)=0.0e0
        green(5)=1.0e0
        blue(5)=0.0e0
c
        red(6)=0.0e0
        green(6)=1.0e0
        blue(6)=1.0e0
c
        red(7)=1.0e0
        green(7)=0.0e0
        blue(7)=1.0e0
c
        red(8)=1.0e0
        green(8)=1.0e0
        blue(8)=0.0e0
c
        if(mxcolr.le.2.or.igrsw.eq.0) then
            do 5 i=1,6
    5           jp(i)=1
        else
            ic=3
            do 10 i=1,6
                jp(i)=ic
                ic=ic+1
                if(ic.gt.mxcolr) ic=3
   10   continue
        endif
c
        jp(7)=iprob
        jp(8)=mxcolr
        jp(9)=min0(mxcolr,8)
        jp(9)=max0(jp(9),2)
        jp(10)=igrsw
        return
        end
