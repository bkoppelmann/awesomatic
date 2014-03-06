        subroutine cvert(itri,ivert,vx,vy,rusr)
c
            integer itri(4,*),ivert(4,*),ivf(2)
            real rusr(2,*),vx(*),vy(*)
c
c       compute the (x,y) coordinates of the vertices
c
        nv0=itri(4,5)
        nv=itri(4,4)
        nc=itri(3,4)
        nv1=nv0+1
        do 1 i=1,nv0
            j=nc+i
            k=ivert(4,i)
            vx(k)=rusr(1,j)
    1       vy(k)=rusr(2,j)
        if(nv1.gt.nv) return
        do 2 i=nv1,nv
            call vf(i,ivf,itri,ivert)
            ivf1=ivf(1)
            ivf2=ivf(2)
            ivf1=ivert(4,ivf1)
            ivf2=ivert(4,ivf2)
            k=ivert(4,i)
            vx(k)=(vx(ivf1)+vx(ivf2))/2.0e0
            vy(k)=(vy(ivf1)+vy(ivf2))/2.0e0
            if(ivert(2,i).ge.0) go to 2
            iedge=-ivert(2,i)
            icen=itri(4,iedge)
            if(icen.gt.0) call midpt(vx(ivf1),vy(ivf1),vx(ivf2),
     1          vy(ivf2),rusr(1,icen),rusr(2,icen),vx(k),vy(k))
    2   continue
        return
        end
