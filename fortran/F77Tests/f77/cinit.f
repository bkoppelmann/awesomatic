        subroutine cinit(jp,itnode,itedge,itag,ce,
     1      vx,vy,vz,cxy,u,z,t,qxy)
c
            integer jp(25),itnode(3,*),itedge(3,*),itag(*),ibdy(30)
            real vx(*),vy(*),vz(*),cxy(3,*),c(3,30),u(*),z(*),ce(*),
     1          t(25)
            external qxy
c
c       process input parameters
c
        nt=jp(1)
        nv=jp(2)
        ifun=jp(6)
        icont=jp(4)
c
c       the function to be plotted is stored in vz
c
        if(ifun.le.4) then
            do 10 i=1,nv
   10           vz(i)=u(i)
            qq=vz(1)
        else
c
c       the cases of qxy (ifun=5) and error (ifun=6)
c
            do 12 i=1,nv
                vz(i)=0.0e0
   12           z(i)=0.0e0
            rl=t(6)
            do 20 i=1,nt
                if(ifun.eq.5) then
                    call grad(ux,uy,vx,vy,u,itnode(1,i))
                else
                    qq=ce(i)
                endif
c
                area=abs((vx(itnode(2,i))-vx(itnode(1,i)))*
     1              (vy(itnode(3,i))-vy(itnode(1,i)))-
     2              (vx(itnode(3,i))-vx(itnode(1,i)))*
     3              (vy(itnode(2,i))-vy(itnode(1,i))))
c
                do 15 j=1,3
                    iv1=itnode(j,i)
                    if(ifun.eq.5) qq=qxy(vx(iv1),vy(iv1),u(iv1),
     1                      ux,uy,rl,itag(i),1)
                    if(icont.eq.0) go to 30
c
                    z(iv1)=z(iv1)+area
                    vz(iv1)=vz(iv1)+area*qq
   15           continue
   20       continue
            do 25 i=1,nv
                if(z(i).ne.0.0e0) then
                    vz(i)=vz(i)/z(i)
                    qq=vz(i)
                endif
   25       continue
        endif
c
c       find a box containing the solution
c
   30   zmin=qq
        zmax=qq
        xmin=vx(1)
        xmax=xmin
        ymin=vy(1)
        ymax=ymin
        do 50 i=1,nt
            if(icont.eq.0.and.ifun.ge.5) then
                if(ifun.eq.5.) then
                    call grad(ux,uy,vx,vy,u,itnode(1,i))
                    do 35 j=1,3
                        ivj=itnode(j,i)
                        vz(ivj)=qxy(vx(ivj),vy(ivj),u(ivj),
     1                      ux,uy,rl,itag(i),1)
   35               continue
                else
                    do 36 j=1,3
                        ivj=itnode(j,i)
                        vz(ivj)=ce(i)
   36               continue 
                endif
            endif
c
            xmin=amin1(xmin,vx(itnode(1,i)),
     1          vx(itnode(2,i)),vx(itnode(3,i)))
            xmax=amax1(xmax,vx(itnode(1,i)),
     1          vx(itnode(2,i)),vx(itnode(3,i)))
            ymin=amin1(ymin,vy(itnode(1,i)),
     1          vy(itnode(2,i)),vy(itnode(3,i)))
            ymax=amax1(ymax,vy(itnode(1,i)),
     1          vy(itnode(2,i)),vy(itnode(3,i)))
            zmin=amin1(zmin,vz(itnode(1,i)),
     1          vz(itnode(2,i)),vz(itnode(3,i)))
            zmax=amax1(zmax,vz(itnode(1,i)),
     1          vz(itnode(2,i)),vz(itnode(3,i)))
c
c       check for curved edge
c
            if(min0(itedge(1,i),itedge(2,i),itedge(3,i)).ge.0) 
     1          go to 50
            call tbdy(c,ibdy,nsides,i,itnode,itedge,itag,
     1          vx,vy,cxy)
            if(nsides.le.3) go to 50
            do 45 j=2,nsides-1
                xx=c(1,j)*vx(itnode(1,i))+
     1              c(2,j)*vx(itnode(2,i))+c(3,j)*vx(itnode(3,i)) 
                xmin=amin1(xmin,xx)
                xmax=amax1(xmax,xx)
                yy=c(1,j)*vy(itnode(1,i))+
     1              c(2,j)*vy(itnode(2,i))+c(3,j)*vy(itnode(3,i)) 
                ymin=amin1(ymin,yy)
                ymax=amax1(ymax,yy)
                zz=c(1,j)*vz(itnode(1,i))+
     1              c(2,j)*vz(itnode(2,i))+c(3,j)*vz(itnode(3,i)) 
                zmin=amin1(zmin,zz)
                zmax=amax1(zmax,zz)
   45       continue
   50   continue
        if(zmax.gt.zmin) then
            t(4)=amax1(xmax-xmin,ymax-ymin)/(zmax-zmin)
        else
            jp(5)=min0(jp(5),1)
            t(4)=0.0e0
        endif
        t(13)=zmin
        t(14)=zmax
        return
        end
