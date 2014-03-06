        subroutine legnd2(jp,t)
c
            integer ichr(20),jp(25),ccolor,mcic(3,6)
            real x(4),y(4),tt(25),qq(3,3),t(25)
            character*7 label(5)
            character*15 title(3)
            save tt,qq,label,mcic,title
c
            data title(1)/'element quality'/
            data title(2)/'maximum angle  '/
            data title(3)/'minimum angle  '/
            data label(1)/'good   '/
            data label(2)/'fair   '/
            data label(3)/'poor   '/
            data label(4)/'worst  '/
            data label(5)/'average'/
            data tt/0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     1              0.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,
     2              0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     3              0.0e0,0.0e0,0.0e0,0.0e0/
            data qq/1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,
     1              0.0e0,0.0e0,1.0e0/
            data mcic/2,2,1,1,3,2,2,1,3,3,2,4,3,2,5,4,2,6/
c
        size=t(16)
        xs=t(19)
        ys=t(20)
        xl=xs-size/2.0e0
        xr=xs+size/2.0e0
        yb=ys-size/2.0e0
        yt=ys+size/2.0e0
        s3=2.0e0/sqrt(3.0e0)
        dx=(xr-xl)/14.5e0
        dy=(yt-yb)/6.0e0
        h=amin1(0.9e0*dy,dx)
c
        mxcolr=jp(17)
        if(mxcolr.ge.3.and.mxcolr.lt.8) then
            ic=mxcolr-2
        else
            ic=6
        endif
c
        ifun=jp(6)-8
        call htextm(1,nchr,ichr,15,title(ifun),kk,0,xk)
        if(nchr.lt.15) then
            do 5 j=nchr+1,15
    5           ichr(j)=0
        endif
        xxl=xl+2.25e0*dx
        xxr=xxl+15.0e0*dx
        yyl=yt-dy
        yyr=yyl+h
        call htext(xxl,yyl,xxr,yyr,15,ichr,-1,qq,tt)
c
        do 25 i=1,5
            yy=yt-float(i+1)*dy
c
c       triangle icon
c
            if(i.le.3) then
                x(1)=xl+0.25e0*dx
                x(2)=x(1)+s3*h
                x(3)=(x(1)+x(2))/2.0e0
                x(4)=x(1)
                y(1)=yy
                y(2)=yy
                y(3)=yy+h
                y(4)=yy
                icolor=mcic(i,ic)
                ii=ccolor(icolor,0,jp)
                call pfill(x,y,3,ii)
                call pline(x,y,4)
            endif
c
c     label
c
            call htextm(1,nchr,ichr,7,label(i),kk,0,xk)
            if(nchr.lt.7) then
                do 15 j=nchr+1,7
   15               ichr(j)=0
            endif
            xxl=xl+2.25e0*dx
            xxr=xxl+7.0e0*dx
            yyl=yy
            yyr=yyl+h
            call htext(xxl,yyl,xxr,yyr,7,ichr,-1,qq,tt)
c
c      value
c
           do 20 j=1,3
   20           ichr(j)=0
            call htextm(3,nchr,ichr(4),0,label(i),kk,3,t(20+i))
            if(nchr.lt.7) then
                ii=nchr-3
                nchr=7
            else
                ii=4
            endif
            xxl=xl+9.25e0*dx
            xxr=xxl+5.0e0*dx
            call htext(xxl,yyl,xxr,yyr,nchr,ichr(ii),1,qq,tt)
   25   continue
        return
        end
