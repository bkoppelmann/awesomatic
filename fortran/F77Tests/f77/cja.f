        subroutine cja(itri,ivert,iflag)
c
            integer itri(4,*),ivert(4,*),son,vlev
c
c       compute sparse matrix storage
c
        iflag=0
        ispd=itri(4,8)
        maxz=itri(3,6)
        itemp=itri(4,6)
        level1=itri(2,3)
        nv=itri(4,4)
c
c       count triangles, boundary points and vertices
c
        nt1=0
        nb1=0
        nb2=0
        nv1=0
        nv2=0
c
        istrt=itri(1,1)
        istop=itri(4,1)-1
        maxl=0
        do 10 i=istrt,istop
            ison=son(i,itri)
            ilevel=level(i,itri)
            maxl=max0(maxl,ilevel)
            if(ilevel.gt.level1) go to 10
            if(ilevel.eq.level1.or.ison.le.0) nt1=nt1+1
   10   continue
        do 30 i=1,nv
            ilevel=vlev(i,itri,ivert)
            ii=ibc(i,ivert)
            if(ilevel.gt.level1) go to 20
            nv1=nv1+1
            if(ii.eq.1) nb1=nb1+1
            go to 30
   20       nv2=nv2+1
            if(ii.eq.1) nb2=nb2+1
   30   continue
c
c       now compute storage requirements
c
        ledge=(3*nt1+nb1)/2
        nedge=ledge+4*nv2-nb2
        lju=ledge+nv1+1
        lja=nedge+nv+1
        la=lja
        if(ispd.ne.1) la=lja+nedge
        itri(1,7)=lja
        itri(2,7)=la
        itri(3,7)=lju
        itri(4,3)=maxl
        ll=itemp*nv+lja+la+lju+itri(4,7)
        if(ll.gt.maxz) iflag=43
        return
        end
