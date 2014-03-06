        subroutine lgraph(i,num,list,itri,ivert)
c
            integer list(4),itri(4,*),ivert(4,*)
c
c       compute the adjacency list for vertex i
c
        nv0=itri(4,5)
        num=0
        ii=ivert(3,i)
        if(ii.le.nv0) return
c
c       loop over quartets
c
        do 30 iqt=1,2
            if (iqt.eq.2) then
                if(ivert(1,ii).lt.0) return
                if(ivert(2,ii).le.0) return
            end if
            it=iabs(ivert(iqt,ii))
            icent=it/4*4
            irel=it-icent
            ielf=itri(1,icent)
            if(ielf.eq.irel) ielf=0
            j1=(5-irel)/2
            do 20 jj=1,2
                if(ielf.ne.0.and.ielf.ne.j1) go to 20
                k=itri(2,icent+j1)
                k=ivert(4,k)
                num=num+1
                list(num)=k
   20           j1=6-irel-j1
   30   continue
        return
        end
