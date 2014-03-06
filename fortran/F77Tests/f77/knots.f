        subroutine knots(i,iv,itri,ivert)
c
            integer iv(3),itri(4,*),ivert(4,*)
c
c       compute knots of triangle i
c
        if(i.ge.itri(3,1)) go to 50
        if(i.lt.itri(2,1)) go to 40
c
c       regular element
c
        icent=i/4*4
        ielf=itri(1,icent)
        if(icent.eq.i) go to 10
c
c       corner triangle
c
        irel=i-icent
        iv(irel)=itri(3,i)
        j1=(5-irel)/2
        do 5 j=2,3
            j1=6-j1-irel
            j2=j1+icent
    5       iv(j1)=itri(2,j2)
        if(ielf.ne.irel) go to 60
        k=irel-1
        if(k.eq.0) k=3
        iv(k)=itri(2,i)
        go to 60
c
c       center triangle
c
   10   do 15 j=1,3
            j1=i+j
   15       iv(j)=itri(2,j1)
        if(ielf.eq.0) go to 60
        k=ielf+1
        if(k.eq.4) k=1
        kk=icent+ielf
        iv(k)=itri(3,kk)
        go to 60
c
c       macro-triangle
c
   40   do 45 j=1,3
   45       iv(j)=iabs(itri(j,i))
        go to 60
c
c       green triangle
c
   50   ig=i/2*2
        iv(1)=itri(2,ig+1)
        iv(2)=itri(2,ig)
        iv(3)=itri(1,ig+1)
        if(ig.ne.i) iv(3)=itri(3,ig+1)
   60   do 70 j=1,3
            ii=iv(j)
   70       iv(j)=ivert(4,ii)
        return
        end
