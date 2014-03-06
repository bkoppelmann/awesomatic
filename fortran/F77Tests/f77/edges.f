        subroutine edges(i,iadj,itri,ivert)
c
            integer iadj(3),itri(4,*),ivert(4,*)
c
c       compute edges for triangle i
c
        do 10 j=1,3
   10       iadj(j)=0
c
c       branch if i is a green or macro triangle
        if(i.ge.itri(3,1)) return
        if(i.lt.itri(2,1)) go to 80
c
c       process regular triangle
c
        icent=i/4*4
        ielf=itri(1,icent)
        if(icent.ne.i) go to 30
c
c       process center triangle
c
        do 20 j=1,3
   20       iadj(j)=icent+j
        if(ielf.eq.0) return
        irel=ielf+1
        if(irel.eq.4) irel=1
        iside=6-irel-ielf
        iadj(iside)=icent+ielf
        ivfath=itri(3,icent+ielf)
        jj=1
        go to 50
c
c       process corner triangle
c
   30   irel=i-icent
        ivfath=itri(3,i)
        if(ielf.eq.irel) go to  40
        iadj(irel)=icent
        iside=(5-irel)/2
        jj=2
        if(ielf.eq.0) go to 50
        k=ielf-1
        if(k.eq.0) k=3
        if(irel.eq.k) iadj(irel)=icent+ielf
        go to 50
   40   iside=irel+1
        if(iside.eq.4) iside=1
        iadj(iside)=icent
        k=6-irel-iside
        iadj(irel)=icent+k
        jj=1
c
c       find adjacent elements outside this quartet
c
   50   do 70 j=1,jj
            iv=iside
            iside=6-irel-iv
            itnew=icent+iv
            ivnew=itri(2,itnew)
            n=ivert(2,ivnew)
            nq=ivert(1,ivnew)
c
c      branch if edge is on boundary or neighbor has smaller level
c
            if(n.le.0.or.nq.le.0) go to 60
            if(n.eq.itnew) n=nq
            ncent=n/4*4
            nrel=n-ncent
            k=(5-nrel)/2
            n=ncent+k
            if(itri(3,n).ne.ivfath) k=6-k-nrel
            n=ncent+k
            if(itri(1,ncent).ne.k) go to 60
            kk=k+1
            if(kk.eq.4) kk=1
            if(kk.ne.nrel) n=ncent
   60       iadj(iside)=n
   70   continue
        return
c
c       process macro triangle i
c
   80   k=i-itri(3,5)
        do 90 j=1,3
   90       iadj(j)=itri(j,k)
        return
        end
