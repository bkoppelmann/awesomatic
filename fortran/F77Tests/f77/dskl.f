        subroutine dskl(vx,vy,ibndry,itnode,xm,ym,jb,t,jp,x,y)
c
            integer itnode(4,*),ibndry(5,*),jb(*),jp(25),ccolor
            real vx(*),vy(*),xm(*),ym(*),t(25),x(*),y(*)
c
c       draw trigen data
c
        nt=jp(1)
        nv=jp(2)
        nc=jp(3)
        nb=jp(4)
c
        xshift=t(1)
        yshift=t(2)
        scale=t(3)
        pi=3.141592653589793e0
c
c       color regions
c
        do 50 ii=1,nt
            i1=itnode(1,ii)
            i2=itnode(2,ii)
            ie1=jb(i1)
            ie2=jb(i2)
            ivc=ibndry(1,ie1)
            if(ivc.ne.ibndry(1,ie2).and.ivc.ne.ibndry(2,ie2))
     1          ivc=ibndry(2,ie1)
            nn=0
            do 40 jj=i1,i2
                it=jb(jj)
                ivn=ibndry(1,it)+ibndry(2,it)-ivc
                nn=nn+1
                if(nn.gt.301) go to 50
                x(nn)=vx(ivc)*scale+xshift
                y(nn)=vy(ivc)*scale+yshift
                if(ibndry(3,it).le.0) go to 40
                jmid=ibndry(3,it)
                call arc0(vx(ivc),vy(ivc),vx(ivn),vy(ivn),
     1              xm(jmid),ym(jmid),xc,yc,theta0,thetaf,r)
                aa=abs(thetaf-theta0)*32.0e0/pi
                m1=max0(int(aa)+1,2)-1
                if(m1.eq.1) go to 40
                dtheta=(thetaf-theta0)/float(m1)
                do 30 j=2,m1
                    nn=nn+1
                    if(nn.gt.301) go to 50
                    th1=theta0+dtheta*float(j-1)
                    xs=xc+r*cos(th1)
                    ys=yc+r*sin(th1)
                    x(nn)=xs*scale+xshift
                    y(nn)=ys*scale+yshift
   30           continue
   40           ivc=ivn
            icolor=ccolor(ii,0,jp)
            call pwindw(x,y,nn,t,icolor)
   50   continue
c
c       draw skeleton
c
        do 90 i=1,nb
            if(ibndry(3,i).gt.0) go to 70
            do 60 j=1,2
                k=ibndry(j,i)
                x(j)=vx(k)*scale+xshift
   60           y(j)=vy(k)*scale+yshift
            call lwindw(x,y,2,t)
            go to 90
   70       j1=ibndry(1,i)
            j2=ibndry(2,i)
            jmid=ibndry(3,i)
            call arc0(vx(j1),vy(j1),vx(j2),vy(j2),
     1          xm(jmid),ym(jmid),xc,yc,theta0,thetaf,r)
            aa=abs(thetaf-theta0)*32.0e0/pi
            m1=max0(int(aa)+1,2)
            dtheta=(thetaf-theta0)/float(m1-1)
            do 80 j=1,m1
                th1=theta0+dtheta*float(j-1)
                xs=xc+r*cos(th1)
                ys=yc+r*sin(th1)
                x(j)=xs*scale+xshift
                y(j)=ys*scale+yshift
   80           continue
            call lwindw(x,y,m1,t)
   90   continue
        return
        end
