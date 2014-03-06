        subroutine cplot(jp,itnode,itedge,itag,order,ce,
     1      vx,vy,vz,cxy,u,label,rad,q,t,qxy)
c
            integer itnode(3,*),itedge(3,*),itag(*),jp(25),
     1          ibdy(30),order(*),ccolor,label(*)
            real vx(*),vy(*),vz(*),q(3,3),t(25),cxy(3,*),
     1          u(*),x(30),y(30),bx(30),by(30),
     2          bz(30),ce(*),rad(*),c(3,30),b(3,5)
            external qxy
c
c       color surface plot
c       initialize
c
        nt=jp(1)
        icont=jp(4)
        ncolor=jp(5)
        ifun=jp(6)
        nshade=jp(16)
        ishade=0
        iscale=jp(19)
        lines=jp(20)
        numbrs=jp(21)
        i3d=jp(22)
c
        pi=3.141592653589793e0
        xshift=t(1)
        yshift=t(2)
        scale=t(3)
        if(i3d.eq.0) then
            zratio=0.0e0
        else
            zratio=t(4)
        endif
        rl=t(6)
        eps=t(7)
        zmin=fscale(t(13),iscale,0)
        zmax=fscale(t(14),iscale,0)
        if(zmax.gt.zmin) then
            zscale=(1.0e0-eps)*float(ncolor)/(zmax-zmin)
        else
            zscale=0.0e0
        endif
c
c       the main loop
c
        do 200 ii=1,nt
            it=order(ii)
            iv1=itnode(1,it)
            iv2=itnode(2,it)
            iv3=itnode(3,it)
c
c       the case of a discontinuous function
c
            if(icont.eq.0) then
                if(ifun.eq.5) then
                    call grad(ux,uy,vx,vy,u,itnode(1,it))
                    do 5 j=1,3
                        ivj=itnode(j,it)
                        vz(ivj)=qxy(vx(ivj),vy(ivj),u(ivj),
     1                      ux,uy,rl,itag(it),1)
    5               continue
                elseif (ifun.eq.6) then
                    do 6 j=1,3
                        ivj=itnode(j,it)
                        vz(ivj)=ce(it)
    6               continue
                endif
            endif
c
c       compute the shade
c
            if(nshade.gt.0)then
                x2=vx(iv2)-vx(iv1)
                y2=vy(iv2)-vy(iv1)
                z2=(vz(iv2)-vz(iv1))*zratio
                x3=vx(iv3)-vx(iv1)
                y3=vy(iv3)-vy(iv1)
                z3=(vz(iv3)-vz(iv1))*zratio
                xx=y2*z3-y3*z2
                yy=z2*x3-z3*x2
                zz=x2*y3-x3*y2
                qq=sqrt(xx*xx+yy*yy+zz*zz)
                aa=(q(1,3)*xx+q(2,3)*yy+q(3,3)*zz)/qq
                aa=amin1(1.0e0,abs(aa))
                aa=(1.0e0-4.0e0*acos(aa)/pi)*float(nshade+1)
                ishade=min0(int(abs(aa)),nshade)
                if(aa.lt.0.0e0) ishade=-ishade
            endif
c
c     compute triangle boundary
c
            call tbdy(c,ibdy,nsides,it,itnode,itedge,itag,
     1          vx,vy,cxy)
c
c       set up coordinates, scale bv to lie on (0,ncolor)
c
            do 7 m=1,nsides+1
                xx=c(1,m)*vx(iv1)+c(2,m)*vx(iv2)+c(3,m)*vx(iv3)
                yy=c(1,m)*vy(iv1)+c(2,m)*vy(iv2)+c(3,m)*vy(iv3)
                zz=c(1,m)*vz(iv1)+c(2,m)*vz(iv2)+c(3,m)*vz(iv3)
                bz(m)=(fscale(zz,iscale,0)-zmin)*zscale
                xm=q(1,1)*xx+q(2,1)*yy
                ym=q(1,2)*xx+q(2,2)*yy+q(3,2)*zz*zratio
                bx(m)=xm*scale+xshift
                by(m)=ym*scale+yshift
    7       continue
                
c
            do 70 mt=2,nsides-1
c
c        order function values
c
                kmin=1
                if(bz(kmin).gt.bz(mt)) kmin=mt
                if(bz(kmin).gt.bz(mt+1)) kmin=mt+1
                kmid=(mt+1)/kmin
                kmax=2*mt+2-kmid-kmin
                if(bz(kmid).gt.bz(kmax)) then
                    kmid=kmax                    
                    kmax=2*mt+2-kmid-kmin
                endif
c
c      find min and max color values for this triangle
c
                minc=int(bz(kmin))+1
                maxc=int(bz(kmax))+1
                if(bz(kmax).eq.float(maxc-1)) maxc=max0(maxc-1,minc)
c
                if(minc.eq.maxc) then
                    do 15 m=1,3
                        b(m,1)=c(m,kmin)
                        b(m,2)=c(m,kmid)
   15                   b(m,3)=c(m,kmax)
                    do 20 m=1,3
                        xx=b(1,m)*vx(iv1)+b(2,m)*vx(iv2)
     1                      +b(3,m)*vx(iv3)
                        yy=b(1,m)*vy(iv1)+b(2,m)*vy(iv2)
     1                      +b(3,m)*vy(iv3)
                        zz=(b(1,m)*vz(iv1)+b(2,m)*vz(iv2)
     1                      +b(3,m)*vz(iv3))*zratio
                        xm=q(1,1)*xx+q(2,1)*yy
                        ym=q(1,2)*xx+q(2,2)*yy+q(3,2)*zz
                        x(m)=xm*scale+xshift
                        y(m)=ym*scale+yshift
   20               continue
                    mc=ccolor(minc,ishade,jp)
                    call pwindw(x,y,3,t,mc)
                else
                    db=1.0e0/(bz(kmax)-bz(kmin))
                    tmid=(bz(kmid)-bz(kmin))*db
                    do 40 mm=minc,maxc
                        tmin=(float(mm-1)-bz(kmin))*db
                        tmax=(float(mm)-bz(kmin))*db
                        call litr(b,msides,c,kmin,kmid,kmax,
     1                      tmin,tmid,tmax)
                        if(msides.lt.2) go to 40
                        do 30 m=1,msides
                            xx=b(1,m)*vx(iv1)+b(2,m)*vx(iv2)
     1                          +b(3,m)*vx(iv3)
                            yy=b(1,m)*vy(iv1)+b(2,m)*vy(iv2)
     1                          +b(3,m)*vy(iv3)
                            zz=(b(1,m)*vz(iv1)+b(2,m)*vz(iv2)
     1                          +b(3,m)*vz(iv3))*zratio
                            xm=q(1,1)*xx+q(2,1)*yy
                            ym=q(1,2)*xx+q(2,2)*yy+q(3,2)*zz
                            x(m)=xm*scale+xshift
                            y(m)=ym*scale+yshift
   30                   continue
                        mc=ccolor(mm,ishade,jp)
                        call pwindw(x,y,msides,t,mc)
   40               continue
                endif
c
c        contour lines
c
                if(lines.ne.2) go to 70
                if(bz(kmin).eq.bz(kmax)) go to 70
                minc=int(bz(kmin))+1
                if(bz(kmin).gt.float(minc-1)) minc=minc+1
                maxc=int(bz(kmax))+1
                if(bz(kmax).lt.float(maxc-1)) maxc=maxc-1
                if(minc.gt.maxc) go to 70
                db=1.0e0/(bz(kmax)-bz(kmin))
                tmid=(bz(kmid)-bz(kmin))*db
                do 60 mm=minc,maxc
                    tmin=(float(mm-1)-bz(kmin))*db
                    tmax=tmin
                    call litr(b,msides,c,kmin,kmid,kmax,
     1                  tmin,tmid,tmax)
                    if(msides.lt.2) go to 60
                    do 50 m=1,2
                        xx=b(1,m)*vx(iv1)+b(2,m)*vx(iv2)
     1                      +b(3,m)*vx(iv3)
                        yy=b(1,m)*vy(iv1)+b(2,m)*vy(iv2)
     1                      +b(3,m)*vy(iv3)
                        zz=(b(1,m)*vz(iv1)+b(2,m)*vz(iv2)
     1                      +b(3,m)*vz(iv3))*zratio
                        xm=q(1,1)*xx+q(2,1)*yy
                        ym=q(1,2)*xx+q(2,2)*yy+q(3,2)*zz
                        x(m)=xm*scale+xshift
                        y(m)=ym*scale+yshift
   50               continue
                    call lwindw(x,y,2,t)
   60           continue
   70       continue
c
c        line drawing
c
            if(lines.eq.0) then
                call lwindw(bx,by,nsides+1,t)
            else
                do 80 m=1,nsides
                    if(ibdy(m).eq.1) call lwindw(bx(m),by(m),2,t)
                    if(ibdy(m).eq.2.and.lines.eq.1)
     1                  call lwindw(bx(m),by(m),2,t)
   80           continue
            endif
  200   continue
c
c       labeling options
c
        if(numbrs.eq.1) call tlabel(jp,itnode,label,vx,vy,q,t)
        if(numbrs.eq.2) call vlabel(jp,itnode,vx,vy,rad,q,t)
c
        return
        end
