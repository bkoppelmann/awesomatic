        subroutine legnd3(jp,t)
c
            integer jp(25),ccolor,ichr(15)
            real x(65),y(65),t(25),tt(25),qq(3,3),xx(65),yy(65),
     1          z(12)
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
        if(ncolor.le.1) return
        nshade=jp(16)      
        iscale=jp(19)
        size=t(16)
        xs=t(19)
        ys=t(20)
c
c       set function values
c
        fmin=fscale(t(13),iscale,0)
        fmax=fscale(t(14),iscale,0)
        df=(fmax-fmin)/float(2*nshade+1)
        do 10 i=1,2*nshade+2
            zz=fmin+df*float(i-1)
            z(i)=fscale(zz,iscale,1)
  10    continue
c
        xm=xs+0.15e0*size
        ym=ys
        rmax=0.35e0*size
        pi=3.141592653589793e0
c
        if(t(13).gt.0.0e0) then
            rmin=rmax*0.15e0
        else
            rmin=0.0e0
        endif
        dr=(rmax-rmin)/float(2*nshade+1)
        dt=2.0e0*pi/float(ncolor)
        nn=max0(64/ncolor,2)
        dq=dt/float(nn-1)
        n2=2*nn
        n3=n2+1
c
c        draw  regions
c
        do 20 i=1,ncolor
        do 20 j=1,2*nshade+1
            theta=float(i-1)*dt
            r1=rmin+float(j-1)*dr
            r2=rmin+float(j)*dr
            k=j-nshade-1
            ic=ccolor(i,k,jp)
            do 5 k=1,nn
                ang=theta+float(k-1)*dq
                c=cos(ang)
                s=sin(ang)
                x(k)=xm+r2*c
                y(k)=ym+r2*s
                xx(k)=x(k)
                yy(k)=y(k)
                x(n3-k)=xm+r1*c
                y(n3-k)=ym+r1*s
                xx(n3-k)=x(n3-k)
                yy(n3-k)=y(n3-k)
    5       continue
            xx(n3)=x(1)
            yy(n3)=y(1)
            call pfill(x,y,n2,ic)
            call pline(xx,yy,n3)
   20   continue
c
c       draw band across the bottom
c
        yb=ys-size*0.45e0
        yt=ys+size*0.45e0
        xl=xs-size*0.5e0
        xr=xl+size*0.05e0
        xc=xr+0.02e0*size
        xf=xc+0.2e0*size
c
        dy=(yt-yb)/float(2*nshade+1)
        do 30 i=1,2*nshade+1
            k=i-nshade-1
            ic=ccolor(1,k,jp)
            x(1)=xl
            y(1)=yb+float(i-1)*dy
            x(2)=xr
            y(2)=y(1)
            x(3)=x(2)
            y(3)=yb+float(i)*dy
            x(4)=x(1)
            y(4)=y(3)
            x(5)=x(1)
            y(5)=y(1)
            do 25 k=1,5
                xx(k)=x(k)
   25           yy(k)=y(k)
            call pfill(x,y,4,ic)
            call pline(xx,yy,5)
   30   continue
c
        do 90 i=0,2*nshade+1
            yc=yb+dy*float(i)-dy/2.0e0
            yf=yc+dy
            zc=z(i+1)
            if(zc.lt.0.0e0) then
                call htextm(4,nchr,ichr,0,zc,zc,3,zc)
            else
                ichr(1)=0
                call htextm(4,nchr,ichr(2),0,zc,zc,3,zc)
            endif
            call htext(xc,yc,xf,yf,9,ichr,-1,qq,tt)
   90   continue
        return
        end
