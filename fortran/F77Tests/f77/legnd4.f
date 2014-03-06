        subroutine legnd4(jp,t)
c
            integer ichr(15),jp(25),ccolor
            real z(12),x(5),y(5),t(25),tt(25),qq(3,3)
            save tt,qq
c
            data tt/0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     1              0.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,
     2              0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     3              0.0e0,0.0e0,0.0e0,0.0e0/
            data qq/1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,
     1              0.0e0,0.0e0,1.0e0/
c
        icolor=jp(5)
        if(icolor.le.0) return
        ncolor=min0(icolor,11)
        iscale=jp(19)
c
c       set function values
c
        fmin=fscale(t(13),iscale,0)
        fmax=fscale(t(14),iscale,0)
        df=(fmax-fmin)/float(ncolor)
        do 10 i=1,ncolor+1
            zz=fmin+df*float(i-1)
            z(i)=fscale(zz,iscale,1)
  10    continue
c
c       make boxes for each color
c
        size=t(16)
        xs=t(19)
        ys=t(20)
        xc=xs-size/2.0e0
        xx=xc+size*0.4e0
        xi=xx+size*0.05e0
        xf=xi+size*0.5e0
        yi=ys-size*0.45e0
        yf=ys+size*0.45e0
        yinc=0.04e0*size
        tic=0.02e0*size
        if(icolor.eq.ncolor) yf=yi+(yf-yi)*ncolor/11.0e0
c
        x(1)=xi
        x(2)=xf
        x(3)=xf
        x(4)=xi
        x(5)=xi
        dy=(yf-yi)/float(icolor)
        do 30 i=1,icolor
            y(1)=yi+dy*float(i)
            y(2)=y(1)
            y(3)=yi+dy*float(i-1)
            y(4)=y(3)
            ii=ccolor(i,0,jp)
            call pfill(x,y,4,ii)
   30   continue
c
c       draw the border and tick marks
c
        y(1)=yi
        y(2)=yi
        y(3)=yf
        y(4)=yf
        y(5)=yi
        call pline(x,y,5)
c
c
        scale=(yf-yi)/float(ncolor)
        do 40 i=0,ncolor
            yp=yi+scale*i
            x(2)=xi-tic
            y(1)=yp
            y(2)=yp
            call pline(x,y,2)
   40   continue
c
c       label the tick marks
c
        do 90 i=0,ncolor
            yc=yi+scale*float(i)-yinc/2.0e0
            yf=yc+yinc
            zc=z(i+1)
            ichr(1)=0
            ichr(11)=0
            if(zc.lt.0.0e0) then
                call htextm(4,nchr,ichr(2),0,zc,zc,3,zc)
            else
                ichr(2)=0
                call htextm(4,nchr,ichr(3),0,zc,zc,3,zc)
            endif
            call htext(xc,yc,xx,yf,11,ichr,-1,qq,tt)
   90   continue
        return
c
        end
