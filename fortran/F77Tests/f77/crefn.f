        subroutine crefn(itri)
c
            integer itri(4,*)
c
c       check storage for user grids and reset level1
c       and irefn as required
c
        iadapt=itri(4,9)
        if(iadapt.eq.3) return
        irefn=itri(2,8)
        level1=itri(2,3)
c
        nt=itri(3,5)
        nhole=itri(4,2)
        nb=itri(2,2)-itri(1,2)
c
        if(irefn.le.1) go to 40
        maxt=itri(1,6)
        maxv=itri(2,6)
        maxz=itri(3,6)
        itemp=itri(4,6)
        ishift=itri(2,1)-1
        ispd=itri(4,8)
c
        do 20 i=2,irefn
            ntf=nt*(4**(i-1))
            nbf=nb*(2**(i-1))
            nvf=(ntf+nbf+2-2*nhole)/2
            ntt=ishift+(4*(ntf-nt))/3
            nedge=(3*ntf+nbf)/2
            nedge1=nedge
            nv1=nvf
            if(i.le.level1) go to 10
c
            nt1=nt*(4**(level1-1))
            nb1=nb*(2**(level1-1))
            nv1=(nt1+nb1+2-2*nhole)/2
            nedge1=(3*nt1+nb1)/2
            nedge=nedge1+4*(nvf-nv1)-(nbf-nb1)
c
   10       lja=nedge+nvf+1
            lju=nedge1+nv1+1
            la=lja
            if(ispd.ne.1) la=lja+nedge
            lu=lju
            if(ispd.ne.1) lu=lju+nedge1
            lu=max0(lu,itri(4,7))
            lz=itemp*nvf+la+lja+lu+lju
c
            if(ntt.gt.maxt) go to 30
            if(nvf.gt.maxv) go to 30
            if(lz.gt.maxz) go to 30
   20   continue
        go to 40
   30   irefn=i-1
        itri(2,8)=irefn
   40   level1=min0(irefn,level1)
        nt1=nt*(4**(level1-1))
        nb1=nb*(2**(level1-1))
        nv1=(nt1+nb1+2-2*nhole)/2
        itri(1,3)=level1
        itri(2,3)=level1
        itri(1,4)=nv1
        itri(2,4)=nv1
        return
        end
