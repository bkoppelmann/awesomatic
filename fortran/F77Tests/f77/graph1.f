        subroutine graph1(r,e,label,ic,xx,yy,scale)
c
c
            integer ichr(80)
            real rn(22),en(22),r(22),e(22)
            character*40 label
            character*20 order
            character*1 kc
            real t(25),q(3,3),x(25),y(25),xn(5),yn(5)
            save q,order,t
c
            data order/'convergence order   '/
            data q/1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,
     1              0.0e0,0.0e0,1.0e0/
            data t/0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     1              0.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,
     2              0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     3              0.0e0,0.0e0,0.0e0,0.0e0/
c
c     graph error
c
        num=int(r(22))
        if(num.le.0) return
        en1=abs(e(22))
        if(en1.gt.0.0e0) en1=1.0e0/en1
        a1=0.0e0
        a2=0.0e0
        r1=0.0e0
        r2=0.0e0
        do 5 i=1,num
            rn(i)=alog10(r(i))
            en(i)=0.0e0
            qq=abs(e(i))*en1
            if(qq.gt.0.0e0) en(i)=alog10(qq)
            qq=rn(i)/2.0e0
            a1=a1+qq*qq
            a2=a2+qq
            r1=r1+en(i)*qq
            r2=r2+en(i)
    5   continue
c
c       least squares fit for convergence curves
c
        if(num.gt.1) then
            det=a1*float(num)-a2*a2
            sl=-(r1*float(num)-r2*a2)/det
        else
            sl=0.0e0
        endif
c
c
        h=0.025e0*scale
        xl=xx+4.0e0*h
        xr=xx+scale-2.0e0*h
        yb=yy+3.0e0*h
        yt=yy+scale-3.0e0*h
        numx=max0(5,int(rn(num))+2)
        emx=en(1)
        emn=en(1)
        do 10 i=1,num
            emx=amax1(en(i),emx)
            emn=amin1(en(i),emn)
   10   continue
        imin=int(emn)
        if(emn.lt.float(imin)) imin=imin-1
        imax=int(emx)
        if(emx.gt.float(imax)) imax=imax+1
        if(imax-imin.lt.4) then
            imin=imin-(4+imin-imax)/2
            imax=imin+4
        endif
        numy=imax-imin+1
        dx=(xr-xl)/float(numx-1)
        dy=(yt-yb)/float(numy-1)
c
c       banners
c
        call htextm(1,nchr,ichr,40,label,k,0,xk)
        xxl=xx
        xxr=xx+scale
        yyl=yt+1.8e0*h
        yyr=yyl+h
        call htext(xxl,yyl,xxr,yyr,nchr,ichr,0,q,t)
c
        call htextm(1,nchr,ichr,20,order,k,0,xk)
        yyl=yt+0.4e0*h
        yyr=yyl+h
        xxl=(xr+xl)/2.0e0-10.0e0*h
        xxr=xxl+15.5e0*h
        call htext(xxl,yyl,xxr,yyr,nchr,ichr,-1,q,t)
        call htextm(3,nchr,ichr,0,order,k,2,sl)
        xxl=xxr
        xxr=xxl+4.5e0*h
        call htext(xxl,yyl,xxr,yyr,nchr,ichr,-1,q,t)
c
c    horizontal axis
c
       x(1)=xl
       x(2)=xr
       y(1)=yb
       y(2)=y(1)
       call pline(x,y,2)
        do 25 i=1,numx
            k=i-1
            call htextm(2,nchr,ichr,0,kc,k,3,xk)
            x(1)=xl+float(k)*dx
            x(2)=x(1)
            y(1)=yb
            y(2)=yb-0.5e0*h
            call pline(x,y,2)
            xxl=x(1)-float(nchr)*h/2.0e0
            xxr=x(1)+float(nchr)*h/2.0e0
            yyl=y(2)-1.75e0*h
            yyr=yyl+h
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
       do 30 i=1,numy
            k=imin+i-1
            call htextm(2,nchr,ichr,0,kc,k,3,xk)
            x(1)=xl
            x(2)=x(1)-0.5e0*h
            y(1)=yb+float(i-1)*dy
            y(2)=y(1)
            call pline(x,y,2)
            xxl=amax1(x(1)-float(nchr+1)*h,xx)
            xxr=x(1)-h
            yyl=y(1)-h/2.0e0
            yyr=y(1)+h/2.0e0
            call htext(xxl,yyl,xxr,yyr,nchr,ichr,1,q,t)
   30   continue
c
c        graph
c
        do 35 i=1,num
            x(i)=xl+dx*rn(i)
            y(i)=yb+dy*(en(i)-float(imin))
            xn(1)=x(i)-h/2.0e0
            xn(2)=x(i)+h/2.0e0
            xn(3)=xn(2)
            xn(4)=xn(1)
            xn(5)=xn(1)
            yn(1)=y(i)-h/2.0e0
            yn(2)=yn(1)
            yn(3)=y(i)+h/2.0e0
            yn(4)=yn(3)
            yn(5)=yn(1)
            call pfill(xn,yn,4,ic)
            call pline(xn,yn,5)
   35   continue
        if(num.gt.1) call pline(x,y,num)
        return
        end
