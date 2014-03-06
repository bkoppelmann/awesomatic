        subroutine binit(ip,vx,vy,xm,ym,itnode,
     1      ibndry,t,tl,jp)
c
            integer ip(100),jp(25),itnode(4,*),ibndry(5,*)
            real vx(*),vy(*),xm(*),ym(*),t(25),tl(25)
c
c       find box containing the solution
c
        nt=jp(1)
        nv=jp(2)
        nc=jp(3)
        nb=jp(4)
        pi=3.141592653589793e0
        ax=vx(1)
        bx=ax
        ay=vy(1)
        by=ay
        do 5 i=1,nv
            ax=amin1(ax,vx(i))
            bx=amax1(bx,vx(i))
            ay=amin1(ay,vy(i))
            by=amax1(by,vy(i))
    5   continue
c
c       curved edges
c
        do 40 i=1,nb
            if(ibndry(3,i).le.0) go to 40
            i1=ibndry(1,i)
            i2=ibndry(2,i)
            imid=ibndry(3,i)
            call arc0(vx(i1),vy(i1),vx(i2),vy(i2),
     1          xm(imid),ym(imid),xc,yc,theta0,thetaf,r)
            aa=abs(thetaf-theta0)*32.0e0/pi
            m1=max0(int(aa)+1,2)-1
            if(m1.eq.1) go to 40
            dtheta=(thetaf-theta0)/float(m1)
            do 30 j=2,m1
                th1=theta0+dtheta*float(j-1)
                xs=xc+r*cos(th1)
                ys=yc+r*sin(th1)
                ax=amin1(ax,xs)
                bx=amax1(bx,xs)
                ay=amin1(ay,ys)
                by=amax1(by,ys)
   30       continue
   40   continue
c
c       compute scaled coordinates
c
        dx=bx-ax
        dy=by-ay
        dd=amax1(dx,dy)
        scale=0.9e0/dd
        t(1)=0.5e0-scale*(ax+bx)/2.0e0
        t(2)=0.5e0-scale*(ay+by)/2.0e0
        t(3)=scale
        t(7)=1.0e2*ceps(ibit)
c
c       compute the window
c
        mag=max0(1,ip(62))
        ix=max0(1,ip(63))
        ix=min0(2*mag-1,ix)
        iy=max0(1,ip(64))
        iy=min0(2*mag-1,iy)
        t(8)=0.05e0+0.9e0*float(ix-1)/float(2*mag)
        t(9)=0.05e0+0.9e0*float(ix+1)/float(2*mag)
        t(10)=0.05e0+0.9e0*float(iy-1)/float(2*mag)
        t(11)=0.05e0+0.9e0*float(iy+1)/float(2*mag)
        t(12)=float(mag)
c
c
        do 110 i=1,12
  110       tl(i)=t(i)
        size=0.45e0
        xs=1.25e0
        ys=0.25e0
        sscale=size*scale
        tl(1)=xs-sscale*(ax+bx)/2.0e0
        tl(2)=ys-sscale*(ay+by)/2.0e0
        tl(3)=sscale
        tl(15)=t(12)
        tl(12)=1.0e0
        tl(16)=size
        tl(17)=xs
        tl(18)=ys
        tl(19)=xs
        tl(20)=0.75e0
c
c       comput number of colors for the case of triangles
c
        if(itnode(3,1).eq.0) go to 130
        nt=jp(1)
        mntag=itnode(4,1)
        mxtag=itnode(4,1)
        do 120 i=1,nt
            mntag=min0(itnode(4,i),mntag)
            mxtag=max0(itnode(4,i),mxtag)
  120   continue
        jp(5)=mxtag-mntag+1
        jp(9)=mntag-1
c
  130   mxcolr=jp(17)
        jp(18)=min0(mxcolr,jp(5)+2)
c
        return
        end
