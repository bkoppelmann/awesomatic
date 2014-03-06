        subroutine graph0(jp,time,xx,yy,scale)
c
            integer jp(25),ichr(80)
            real time(2,20),x(5),y(5)
            character*6 name(20)
            character*40 tmst
            character*1 kc
            real t(25),q(3,3)
            save t,q,name,tmst
c
            data tmst/'time statistics  last call - accumulated'/
c
            data t/0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     1              0.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,0.0e0,
     2              0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,0.0e0,
     3              0.0e0,0.0e0,0.0e0,0.0e0/
            data q/1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,0.0e0,
     1              0.0e0,0.0e0,1.0e0/
c
            data name/'triang','marka ','regrup','order ',
     1                'setgr ','predct','linsys','sclasm',
     2                'swbrch','energy','rlerr ','interp',
     3                'snf   ','cev   ','mg    ','*     ',
     4                'total ','      ','      ','      '/
c
c       print time statistics
c
        s1=0.0e0
        s2=0.0e0
        do 5 i=1,15
            s1=s1+time(1,i)
    5       s2=s2+time(2,i)
        time(1,16)=time(1,17)-s1
        time(2,16)=time(2,17)-s2
        ss=time(2,17)
        if(ss.ne.0.0e0) ss=1.0e0/abs(ss)
c
        xxl=xx
        xxr=xx+scale
        yyb=yy
        yyt=yy+scale
        dx=(xxr-xxl)/4.3e0
        dy=(yyt-yyb)/20.75e0
        h=scale/43.0e0
c
c       banners
c
        call htextm(1,nchr,ichr,40,tmst,kk,0,xk)
        xl=xxl
        xr=xxr
        yl=yyt-dy
        yr=yl+h
        call htext(xl,yl,xr,yr,nchr,ichr,0,q,t)
c
c        horizontal axis
c
        x(1)=xxl+2.2e0*dx
        x(2)=xxr
        x(3)=x(2)
        x(4)=x(1)
        x(5)=x(1)
        y(1)=yyt-18.75e0*dy
        y(2)=y(1)
        y(3)=yyt-1.75*dy
        y(4)=y(3)
        y(5)=y(1)
        call pline(x,y,5)
        dd=(xxr-xxl-2.2e0*dx)/5.0e0
        do 10 i=1,6
            k=(i-1)*20
            call htextm(2,nchr,ichr,0,kc,k,3,xk)
            x(1)=xxl+2.2e0*dx+float(i-1)*dd
            x(2)=x(1)
            y(1)=yyt-18.75e0*dy
            y(2)=y(1)-0.02e0*scale
            call pline(x,y,2)
            xl=x(1)-float(nchr)*h/2.0e0
            xr=x(1)+float(nchr)*h/2.0e0
            yl=y(2)-2.0e0*h
            yr=y(2)-h
            call htext(xl,yl,xr,yr,nchr,ichr,0,q,t)
   10   continue
c
        do 50 i=1,17
c
c        subroutine names
c
            call htextm(1,nchr,ichr,6,name(i),kk,0,xk)
            if(nchr.lt.6) then
                do 15 j=nchr+1,6
   15               ichr(j)=0
            endif
            xl=xxl
            xr=xl+0.6e0*dx
            yl=yyt-float(i+1)*dy-0.75e0*dy
            yr=yl+h
            call htext(xl,yl,xr,yr,6,ichr,-1,q,t)
c
c       times
c
            do 25 k=1,2
                do 20 j=1,3
   20               ichr(j)=0
                call htextm(3,nchr,ichr(4),0,name,kc,3,time(k,i))
                if(nchr.lt.8) then
                    ii=nchr-4
                    nchr=8
                else
                    ii=4
                endif
                xl=xr+0.05e0*dx
                xr=xl+0.7e0*dx
                call htext(xl,yl,xr,yr,nchr,ichr(ii),1,q,t)
   25       continue
c
c        histogram
c
            do 30 k=2,1,-1
                if(time(k,i).gt.0.0e0) then
                    x(1)=xr+0.1e0*dx
                    x(2)=x(1)+time(k,i)*(xxr-x(1))*ss
                    x(3)=x(2)
                    x(4)=x(1)
                    x(5)=x(1)
                    y(1)=yl
                    y(2)=y(1)
                    y(3)=yl+dy
                    y(4)=y(3)
                    y(5)=y(1)
                    ic=jp(k)
                    if(jp(9).eq.3.and.k.eq.1) ic=1
                    call pfill(x,y,4,ic)
                    call pline(x,y,5)
                endif
   30       continue
   50   continue
        return
        end
