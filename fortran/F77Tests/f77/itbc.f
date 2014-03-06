        integer function itbc(i,iadj,itri,ivert)
c
            integer iadj(3),itri(4,*),ivert(4,*),jadj(3)
c
c       count boundary edges for element i
c       iadj is set only if itbc is positive
c
        itbc=0
        if(i.ge.itri(3,1)) go to 30
c
c       regular triangle
c
        if(i.lt.itri(2,1)) go to 10
        icent=(i/4)*4
        if(itri(1,icent+2).eq.0) return
   10   call edges(i,jadj,itri,ivert)
        do 20 j=1,3
            iadj(j)=0
            k=-jadj(j)
            if(k.le.0) go to 20
            iadj(j)=k
            itbc=itbc+1
   20   continue
        return
c
c       green triangles
c
   30   icent=i/2*2
        ifath=itri(1,icent)
        if(ifath.lt.itri(2,1)) go to 40
        ifcent=(i/4)*4
        if(itri(1,ifcent+2).eq.0) return
   40   irel=i-icent
        icom=itri(3,icent)
        iedge=(5-icom)/2
        if(irel.eq.0) iedge=6-iedge-icom
        call edges(ifath,jadj,itri,ivert)
        if(jadj(iedge).ge.0) return
        itbc=1
        iadj(1)=0
        iadj(2)=-jadj(iedge)
        iadj(3)=0
        return
        end
