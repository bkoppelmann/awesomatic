        subroutine dtri(vx,vy,ibndry,
     1      itnode,xm,ym,t,jp,itedge,z)
c
            integer itnode(4,*),ibndry(5,*),itedge(3,*),jp(25),
     1          ccolor
            real vx(*),vy(*),xm(*),ym(*),t(12),z(*),x(101),y(101)
c
c       draw triangle data
c
        nt=jp(1)
        nv=jp(2)
        nc=jp(3)
        nb=jp(4)
        mntag=jp(9)
c
        xshift=t(1)
        yshift=t(2)
        scale=t(3)
        pi=3.141592653589793e0
c
c       compute itedge
c
        call cedge(nv,nt,nb,itnode,ibndry,
     1      itedge,vx,vy,z,iflag)
        if(iflag.ne.0) return
c
c       color triangles
c
        i1=1
        i2=2
        i3=3
        do 50 ii=1,nt
            nn=0
            do 40 jj=1,3
                ivc=itnode(i1,ii)
                ivn=itnode(i2,ii)
                nn=nn+1
                if(nn.gt.101) go to 50
                x(nn)=vx(ivc)*scale+xshift
                y(nn)=vy(ivc)*scale+yshift
                if(itedge(i3,ii).ge.0) go to 35
                jmid=-itedge(i3,ii)
                jmid=ibndry(3,jmid)
                if(jmid.le.0) go to 35
                call arc0(vx(ivc),vy(ivc),vx(ivn),vy(ivn),
     1              xm(jmid),ym(jmid),xc,yc,theta0,thetaf,r)
                aa=abs(thetaf-theta0)*32.0e0/pi
                m1=max0(int(aa),2)
                dtheta=(thetaf-theta0)/float(m1)
                do 30 j=2,m1
                    nn=nn+1
                    if(nn.gt.101) go to 50
                    th1=theta0+dtheta*float(j-1)
                    xs=xc+r*cos(th1)
                    ys=yc+r*sin(th1)
                    x(nn)=xs*scale+xshift
                    y(nn)=ys*scale+yshift
   30           continue
   35           it=i1
                i1=i2
                i2=i3
   40           i3=it
            ic=itnode(4,ii)-mntag
            icolor=ccolor(ic,0,jp)
            call pwindw(x,y,nn,t,icolor)
   50   continue
c
c       draw triangle edges
c
        do 90 i=1,nt
            do 85 jj=1,3
                if(itedge(jj,i).gt.i) go to 85
                if(jp(6).eq.1) then
                    kk=itedge(jj,i)
                    if(kk.gt.0) then
                       if(itnode(4,kk).eq.itnode(4,i)) go to 85
                    endif
                endif
                j1=(5-jj)/2
                j2=6-jj-j1
                j1=itnode(j1,i)
                j2=itnode(j2,i)
                if(itedge(jj,i).lt.0) then
                    jmid=-itedge(jj,i)
                    jmid=ibndry(3,jmid)
                    if(jmid.gt.0) go to 70
                endif
                do 60 j=1,2
                    x(j)=vx(j1)*scale+xshift
                    y(j)=vy(j1)*scale+yshift
   60               j1=j2
                call lwindw(x,y,2,t)
                go to 85
   70           call arc0(vx(j1),vy(j1),vx(j2),vy(j2),
     1              xm(jmid),ym(jmid),xc,yc,theta0,thetaf,r)
                aa=abs(thetaf-theta0)*32.0e0/pi
                m1=max0(int(aa)+1,2)
                dtheta=(thetaf-theta0)/float(m1-1)
                do 80 j=1,m1
                    th1=theta0+dtheta*float(j-1)
                    xs=xc+r*cos(th1)
                    ys=yc+r*sin(th1)
                    x(j)=xs*scale+xshift
                    y(j)=ys*scale+yshift
   80               continue
                call lwindw(x,y,m1,t)
   85       continue
   90   continue
        return
        end
