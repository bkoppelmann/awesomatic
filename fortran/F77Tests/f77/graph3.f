        subroutine graph3(jp,path,xx,yy,scale)
c
c
            integer ichr(80),jp(25)
            real path(51,5),x(5),y(5)
            real t(25),q(3,3)
            character*20 label
            character*1 kc
            save t,q,label
c
            data label/'continuation path   '/
            data t/0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     1              0.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,
     2              0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     3              0.0e0,0.0e0,0.0e0,0.0e0/
            data q/1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,
     1              0.0e0,0.0e0,1.0e0/
c
c     plot continuation path
c
        iprob=jp(7)
        if(iprob.ge.8) return
        num=int(path(51,1))
        if(num.le.0)  return
        rlmax=path(1,1)
        rlmin=rlmax
        rmax=path(1,2)
        rmin=rmax
        do 10 i=1,num
            rlmax=amax1(rlmax,path(i,1))
            rlmin=amin1(rlmin,path(i,1))
            rmax=amax1(rmax,path(i,2))
            rmin=amin1(rmin,path(i,2))
   10   continue
        dr=(rlmax-rlmin)/20.0e0
        if(dr.eq.0.0e0) dr=abs(rlmax)/20.0e0
        if(dr.eq.0.0e0) dr=1.0e0
        rlmax=rlmax+dr
        rlmin=rlmin-dr
        dr=(rmax-rmin)/20.0e0
        if(dr.eq.0.0e0) dr=abs(rmax)/20.0e0
        if(dr.eq.0.0e0) dr=1.0e0
        rmax=rmax+dr
        rmin=rmin-dr
c
        h=0.025*scale
        xl=xx+7.0e0*h
        xr=xx+scale-h
        yb=yy+2.5e0*h
        yt=yb+scale-5.5e0*h
c
        srl=(xr-xl)/(rlmax-rlmin)
        sr=(yt-yb)/(rmax-rmin)
        xshift=(xr+xl)/2.0e0-srl*(rlmax+rlmin)/2.0e0
        yshift=(yb+yt)/2.0e0-sr*(rmax+rmin)/2.0e0
c
c       banners
c
        call htextm(1,nchr,ichr,20,label,k,0,xk)
        xxl=xx
        xxr=xx+scale
        yyl=yy+scale-1.25e0*h
        yyr=yyl+h
        call htext(xxl,yyl,xxr,yyr,nchr,ichr,0,q,t)
c
c    horizontal axis
c
        x(1)=xl
        x(2)=xr
        y(1)=yb
        y(2)=y(1)
        call pline(x,y,2)
        dx=(xr-xl)/10.0e0
        dr=(rlmax-rlmin)/10.0e0
        do 25 i=1,11
            x(1)=xl+float(i-1)*dx
            x(2)=x(1)
            y(1)=yb
            y(2)=yb-0.5e0*h
            call pline(x,y,2)
            if(i-(i/2)*2.eq.0) go to 25
            xk=rlmin+float(i-1)*dr
            call htextm(4,nchr,ichr,0,kc,k,3,xk)
            xxl=x(1)-float(nchr)*h/4.0e0
            xxr=x(1)+float(nchr)*h/4.0e0
            yyl=y(2)-1.75e0*h/2.0e0
            yyr=yyl+h/2.0e0
            call htext(xxl,yyl,xxr,yyr,nchr,ichr,0,q,t)
   25   continue
c
c       vertical axis
c
        x(1)=xl
        x(2)=x(1)
        y(1)=yb
        y(2)=yt
        call pline(x,y,2)
        dy=(yt-yb)/10.0e0
        dr=(rmax-rmin)/10.0e0
        do 30 i=1,11
            xk=rmin+float(i-1)*dr
            call htextm(4,nchr,ichr,0,kc,k,3,xk)
            x(1)=xl
            x(2)=x(1)-0.5e0*h
            y(1)=yb+float(i-1)*dy
            y(2)=y(1)
            call pline(x,y,2)
            xxl=amax1(x(1)-float(nchr+1)*h/2.0e0,xx)
            xxr=x(1)-h
            yyl=y(1)-h/4.0e0
            yyr=y(1)+h/4.0e0
            call htext(xxl,yyl,xxr,yyr,nchr,ichr,1,q,t)
   30   continue
c
c        mark points
c
        do 35 i=1,num
            x(1)=path(i,1)*srl+xshift-h/2.0e0
            x(2)=x(1)+h
            x(3)=x(2)
            x(4)=x(1)
            x(5)=x(1)
            y(1)=path(i,2)*sr+yshift-h/2.0e0
            y(2)=y(1)
            y(3)=y(1)+h
            y(4)=y(3)
            y(5)=y(1)
            if(jp(9).eq.3) then
                ic=1
                if(path(i,5).ne.0.0e0) ic=jp(1)
            else if(jp(9).eq.4) then
                ic=jp(2)
                if(path(i,5).ne.0.0e0) ic=jp(1)
            else
                ic=jp(3)
                if(path(i,5).lt.0.0e0) ic=jp(1)
                if(path(i,5).gt.0.0e0) ic=jp(2)
            endif
            call pfill(x,y,4,ic)
            call pline(x,y,5)
   35   continue
c
c       draw interpolant
c
        if(num.gt.1) then
            do 45 i=1,num-1
                call cpth(path(i,1),path(i+1,1),path(i,2),
     1              path(i+1,2),path(i,3),path(i+1,3),path(i,4),
     2              path(i+1,4),srl,sr,xshift,yshift,xl,xr,yb,yt)
   45      continue
        endif
c
        return
        end
