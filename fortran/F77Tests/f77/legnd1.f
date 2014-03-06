        subroutine legnd1(jp,t)
c
            integer ichr(4),jp(25),ccolor
            real x(4),y(4),tt(25),qq(3,3),t(25)
            save tt,qq
c
            data tt/0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     1              0.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,
     2              0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     3              0.0e0,0.0e0,0.0e0,0.0e0/
            data qq/1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,
     1              0.0e0,0.0e0,1.0e0/
c
        ncolor=jp(5)
        size=t(16)
        xs=t(19)
        ys=t(20)
        xl=xs-size/2.0e0
        xr=xs+size/2.0e0
        yb=ys-size/2.0e0
        yt=ys+size/2.0e0
        s3=sqrt(3.0e0)/2.0e0
c
c       compute ncol and nrow
c
        s=sqrt(float(ncolor)/3.0e0)
        is=int(s)
        if(s-float(is).gt.1.e-3) is=is+1
        ncol=max0(is,2)
        nrow=ncolor/ncol
        if(nrow*ncol.lt.ncolor) nrow=nrow+1
        nrow=max0(nrow,1)
        dx=(xr-xl)/float(ncol)
        dy=(yt-yb)/float(nrow)
        if(dx.gt.3.0e0*dy) dx=3.0e0*dy
        if(dx.lt.3.0e0*dy) dy=dx/3.0e0
c
c       the main loop
c
        icolor=0
        do 20 nr=1,nrow
        do 20 nc=1,ncol
            icolor=icolor+1
            if(icolor.gt.ncolor) go to 20
c
c       level number
c
            do 5 i=1,4
    5           ichr(i)=0
            if(icolor.lt.10) then
                call htextm(2,nchr,ichr(3),0,ks,icolor,0,xk)
            else
                call htextm(2,nchr,ichr(2),0,ks,icolor,0,xk)
            endif
c
            x1=xl+float(nc-1)*dx
            x2=xl+float(nc)*dx
            xm=(2.0e0*x2+x1)/3.0e0
            y1=yt-float(nr)*dy
            y2=yt-float(nr-1)*dy
            ym=(y1+y2)/2.0e0
            call htext(x1,y1,xm,ym,4,ichr,1,qq,tt)
c
c       triangle icon
c
            x(1)=xm
            x(2)=(xm+x2)/2.0e0
            x(3)=x2
            x(4)=xm
            y(1)=y1
            y(2)=y1+s3*(x2-xm)
            y(3)=y1
            y(4)=y1
            ii=ccolor(icolor,0,jp)
            call pfill(x,y,3,ii)
            call pline(x,y,4)
   20   continue
        return
        end
