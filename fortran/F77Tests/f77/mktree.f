        subroutine mktree(qtree,list,llen,p,
     1      vx,vy,vz,cxy,itri,ivert,iflag)
c
            integer qtree(5,*),list(*),itri(4,*),ivert(4,*),iv(3)
            real p(4),vx(*),vy(*),vz(*),cxy(3,*)
c
c       set up quad-tree data structure
c
        iflag=0
        it1=itri(1,1)
        it2=itri(4,1)-1
        p(1)=vx(1)
        p(2)=p(1)
        p(3)=vy(1)
        p(4)=p(3)
        llen=0
        do 10 i=it1,it2
            if(istl(i,itri).eq.0) go to 10
            llen=llen+1
            list(llen)=i
            call knots(i,iv,itri,ivert)
            call vari(i,iv,xmin,xmax,ymin,ymax,
     1          vx,vy,vz,cxy,itri,ivert)
            p(1)=amin1(p(1),xmin)
            p(2)=amax1(p(2),xmax)
            p(3)=amin1(p(3),ymin)
            p(4)=amax1(p(4),ymax)
   10   continue
c
c
c       now make a quadtree
c
        qtree(2,1)=4
c
        qtree(1,3)=1
        qtree(2,3)=llen
        qtree(3,3)=0
        qtree(4,3)=0
        qtree(5,3)=0
c
c       create refined elements
c
        i=3
   20   call refnbx(i,p,qtree,list,vx,vy,vz,cxy,
     1          itri,ivert,iflag)
        if(iflag.ne.0) return
        i=i+1
        if(i.lt.qtree(2,1)) go to 20
        return
        end
