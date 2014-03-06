        subroutine cite(nt,nb,nc,nr,maxc,vx,vy,xm,ym,
     1      itnode,ibndry,iusr,irgn,cxy,iflag)
c
            integer itnode(4,*),ibndry(5,*),iusr(6,*),
     1          irgn(6,*),iv(3)
            real vx(*),vy(*),xm(*),ym(*),cxy(5,*),c(3)
c
c       compute the final version of ibndry
c
        iflag=0
        nc=0
        do 10 i=1,nb
            ied=ibndry(3,i)
            j3=ibndry(4,i)
            ibndry(3,i)=iusr(3,ied)
            ibndry(4,i)=iusr(4,ied)
            ibndry(5,i)=iusr(5,ied)
            if(iusr(3,ied).le.0) go to 10
c
c       compute a new midpt
c
            nc=nc+1
            if(nc.gt.maxc) go to 100
            jm=iusr(3,ied)
            xc=cxy(1,jm)
            yc=cxy(2,jm)
            j1=ibndry(1,i)
            j2=ibndry(2,i)
            call midpt(vx(j1),vy(j1),vx(j2),vy(j2),xc,yc,
     1          xm(nc),ym(nc))
            ibndry(3,i)=nc
            iv(1)=j1
            iv(2)=j2
            iv(3)=j3
            call bari(xm(nc),ym(nc),vx,vy,iv,c)
            if(c(3).le.0.0e0) go to 10
c
c       check that concave edges stay inside straight edge triangle
c
            do 5 j=1,2
               dx=vx(j1)-vx(j3)
               dy=vy(j1)-vy(j3)
               d=dx*(vx(j1)-xc)+dy*(vy(j1)-yc)
               r=dx*(xm(nc)-xc)+dy*(ym(nc)-yc)
               if(d/r.lt.0.0e0) go to 30
    5          j1=j2
   10       continue
c
c       mark regions in itnode
c
        do 20 i=1,nr
            i1=irgn(1,i)
            i2=irgn(1,i+1)-1
            do 15 j=i1,i2
   15           itnode(4,j)=irgn(6,i)
   20   continue
        return
   30   iflag=200+nt
        return
  100   iflag=57
        return
        end
