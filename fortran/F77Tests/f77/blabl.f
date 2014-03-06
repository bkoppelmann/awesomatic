        subroutine blabl(jp,ibndry,vx,vy,xm,ym,t)
c
            integer jp(25),ibndry(5,*),ichr(10)
            real t(16),vx(*),vy(*),q(3,3),xm(*),ym(*)
            character*1 kc
            save q
            data q/1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,
     1          0.0e0,0.0e0,0.0e0,1.0e0/
c
c       print edge numbers or midpoint numbers
c
        nb=jp(4)
        numbrs=jp(7)
c
        scale=t(3)
        xshift=t(1)
        yshift=t(2)
        xl=t(8)
        xr=t(9)
        yb=t(10)
        yt=t(11)
        rmag=t(12)
c
c
c
        rmax=0.05e0
        do 20 it=1,nb
            j1=ibndry(1,it)
            j2=ibndry(2,it)
            jm=ibndry(3,it)
c
            i=it
            if(jm.gt.0) then
                if(numbrs.eq.4) i=jm
                call centre(vx(j1),vy(j1),vx(j2),
     1              vy(j2),xm(jm),ym(jm),xs,ys)
                call midpt(vx(j1),vy(j1),vx(j2),
     1              vy(j2),xs,ys,xc,yc)
            else
                if(numbrs.eq.4) go to 20
                xc=(vx(j1)+vx(j2))/2.0e0
                yc=(vy(j1)+vy(j2))/2.0e0
            endif
c
            r=sqrt(((vx(j1)-vx(j2))*scale)**2
     1          +((vy(j1)-vy(j2))*scale)**2)/3.0e0
            r=amin1(r,rmax)
c
            if(rmag.gt.1.0e0) then
                xx=xc*scale+xshift
                yy=yc*scale+yshift
                if(xx+r.lt.xl.or.xx-r.gt.xr) go to 20
                if(yy+r.lt.yb.or.yy-r.gt.yt) go to 20
            endif
c
            call htextm(2,nchr,ichr,0,kc,i,0,xk)
c
            ratio=float(nchr)*20.0e0/21.0e0
            delta=r/(scale*sqrt(1.0e0+ratio*ratio))
            x1=xc-ratio*delta
            x2=xc+ratio*delta
            y1=yc-delta
            y2=yc+delta
c
            call htext(x1,y1,x2,y2,nchr,ichr,0,q,t)
   20   continue
c
        return
        end
