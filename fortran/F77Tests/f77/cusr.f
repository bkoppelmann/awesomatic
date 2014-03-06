        subroutine cusr(ip,itri,ivert,itnode,ibndry,itedge,
     1      vx,vy,rusr,time,hist,path,xm,ym)
c
            integer itnode(4,*),ibndry(5,*),itedge(3,*),
     1          itri(4,*),ivert(4,*),ip(100)
            real vx(*),vy(*),xm(*),ym(*),rusr(2,*),time(2,20)
            real h(3),hist(22,12),path(51,5)
c
c       layout for integer parameters in itri(j,i)
c
c               i=1          i=2         i=3         i=4         i=5
c       j=1     1st macro    ibdy        lvl         nvl         ntold
c       j=2     1st reg      itedge      level1      n1          nvold
c       j=3     1st green    nd          lvlsv       nc          nt0
c       j=4     1st empty    nhole       maxl        nv          nv0
c
c               i=6         i=7         i=8          i=9         i=10
c       j=1     maxt        lja         nvtrgt       mxvmk
c       j=2     maxv        la          irefn        acall
c       j=3     maxz        lju         iter         rtype
c       j=4     itemp       lu          ispd         iadapt
c
c       initialize
c
        maxt=ip(41)
        maxv=ip(42)
        maxz=ip(43)
        itemp=ip(44)
        nt=ip(1)
        nv=ip(2)
        nc=ip(3)
        nb=ip(4)
        ispd=ip(5)
        nvtrgt=ip(6)
        iadapt=ip(8)
        level1=max0(1,ip(9))
        irefn=max0(1,ip(10))
        idbcpt=ip(14)
        if(idbcpt.lt.0.or.idbcpt.gt.nv) idbcpt=0
        iflag=0
        tol=0.99e0
        do 5 i=1,20
        do 5 j=1,2
    5       time(j,i)=0.0e0
        do 6 i=1,12
        do 6 j=1,22
    6       hist(j,i)=0.0e0
        do 7 i=1,5
        do 7 j=1,51
    7       path(j,i)=0.0e0
c
c       initial check of data
c
        call tchek(itnode,ibndry,nb,nv,nc,nt,itri,iflag)
        if(iflag.ne.0) go to 70
c
c       compute rusr
c
        do 10 i=1,nv
            ivert(1,i)=0
            ivert(2,i)=0
            ivert(3,i)=i
            ivert(4,i)=i
            j=nc+i
            rusr(1,j)=vx(i)
   10       rusr(2,j)=vy(i)
        nd=0
        do 15 i=1,nb
            if(ibndry(4,i).lt.0) nd=nd+1
   15   continue
c
c       compute itedge
c
        call cedge(nv,nt,nb,itnode,ibndry,
     1      itedge,vx,vy,itri,iflag)
        if(iflag.ne.0) go to 70
c
c       initialize pointers in itri and ivert
c
        k=((nb+2*nt+16)/4)*4
        itri(1,1)=k-nt
        itri(2,1)=k
        itri(3,1)=k
        itri(4,1)=k
        itri(1,2)=k-2*nt-nb
        itri(2,2)=k-2*nt
        itri(3,2)=nd
        itri(4,2)=(nt+nb+2-2*nv)/2
        itri(1,3)=level1
        itri(2,3)=level1
        itri(3,3)=level1
        itri(4,3)=level1
        itri(1,4)=nv
        itri(2,4)=nv
        itri(3,4)=nc
        itri(4,4)=nv
        itri(1,5)=0
        itri(2,5)=0
        itri(3,5)=nt
        itri(4,5)=nv
        itri(1,6)=maxt
        itri(2,6)=maxv
        itri(3,6)=maxz
        itri(4,6)=itemp
        itri(1,7)=0
        itri(2,7)=0
        itri(3,7)=0
        itri(4,7)=0
        itri(1,8)=nvtrgt
        itri(2,8)=irefn
        itri(3,8)=0
        itri(4,8)=ispd
        itri(1,9)=0
        itri(2,9)=0
        itri(3,9)=0
        itri(4,9)=iadapt
c
        ivert(1,1)=nv
        ivert(1,2)=itri(1,2)+nb-nd
        ivert(1,3)=idbcpt
c
        do 20 i=1,nt
        do 20 j=1,3
            ii=itnode(j,i)
            ivert(2,ii)=i
   20   continue
c
        nd=ivert(1,2)
        nn=itri(1,2)
        itn=itri(1,1)
        ite=itri(2,2)
        ishift=itn-1
        do 60 i=1,nt
            do 50 j=1,3
                itri(j,itn)=itnode(j,i)
                itri(j,ite)=itedge(j,i)+ishift
c
c       compute length of side j
c
                j2=(5-j)/2
                j3=6-j-j2
                j2=itnode(j2,i)
                j3=itnode(j3,i)
                h(j)=(vx(j2)-vx(j3))*(vx(j2)-vx(j3))+
     1              (vy(j2)-vy(j3))*(vy(j2)-vy(j3))
c
c       branch for boundary conditions
c
                if(itedge(j,i).gt.0) go to 50
                k=-itedge(j,i)
                if(ibndry(4,k).lt.0) go to 30
c
c       natural bc
c
                itri(j,ite)=-nn
                itri(1,nn)=itn
                itri(2,nn)=j
                itri(3,nn)=ibndry(5,k)
                itri(4,nn)=ibndry(3,k)
                ivert(2,j2)=min0(ivert(2,j2),-nn)
                ivert(2,j3)=min0(ivert(2,j3),-nn)
                nn=nn+1
                go to 40
c
c       dirichlet bc
c
   30           itri(j,ite)=-nd
                itri(1,nd)=itn
                itri(2,nd)=j
                itri(3,nd)=ibndry(5,k)
                itri(4,nd)=ibndry(3,k)
                ivert(2,j2)=-nd
                ivert(2,j3)=-nd
                nd=nd+1
   40           k=ibndry(3,k)
                if(k.gt.0) call centre(vx(j2),vy(j2),vx(j3),
     1              vy(j3),xm(k),ym(k),rusr(1,k),rusr(2,k))
   50           continue
c
c       sort side lengths
c
                j1=1
                if(h(2).lt.h(1)) j1=2
                if(h(3).lt.h(j1)) j1=3
                j2=(5-j1)/2
                j3=6-j1-j2
                if(h(j3).lt.h(j2)) j2=j3
                j3=6-j1-j2
c
c       mark obtuse angles
c
                itri(4,itn)=0
                itri(4,ite)=itnode(4,i)
                if(h(j1)+h(j2).lt.h(j3)*tol)
     1              itri(j3,itn)=-itri(j3,itn)
                ite=ite+1
                itn=itn+1
   60       continue
c
c       compute uniformly refined grids
c
        call crefn(itri)
        tt=timer(it)
        call triang(itri,ivert,iflag)
        tt=timer(it)-tt
        time(1,1)=time(1,1)+tt
        time(2,1)=time(2,1)+tt
        ip(38)=itri(4,3)
        ip(39)=itri(4,1)-1
        ip(40)=itri(4,4)
  70    ip(31)=iflag
        return
        end
