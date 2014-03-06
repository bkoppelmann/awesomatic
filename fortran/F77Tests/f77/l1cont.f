        subroutine l1cont(ip,itri,ivert,rusr,z,u,udot,u0,
     1      u0dot,ur,ul,b,rln,time,hist,path,
     2      a1xy,a2xy,fxy,gxy,p1xy,p2xy,rtrgt,rltrgt)
c
            integer itri(4,*),ivert(4,*),ip(100),getlvl
            real u(*),udot(*),u0(*),u0dot(*),ur(*),ul(*),
     1          z(*),b(*),rusr(2,*),time(2,20),rln(9,9),
     2          hist(22,12),path(51,5)
            real bisc(12)
            external a1xy,a2xy,fxy,gxy,p1xy,p2xy
c
c       level one continuation
c
        tx=timer(it)
        iunit=ip(15)
        iprob=ip(7)
        iadapt=ip(8)
        maxv=ip(42)
        itri(4,9)=iadapt
        idbcpt=ip(14)
        if(idbcpt.lt.0.or.idbcpt.gt.ip(2)) idbcpt=0
        ivert(1,3)=idbcpt
c
c       compute level1
c
        level1=max0(1,ip(9))
        level1=min0(itri(1,3),level1)
        itri(1,3)=level1
        itri(2,3)=level1
        call cja(itri,ivert,iflag)
        if(iflag.ne.0) go to 100
c
c       initialize timing array
c
        do 4 i=1,20
    4       time(1,i)=0.0e0
        if(iadapt.eq.1) then
             time(1,1)=time(2,1)
             path(51,1)=float(0)
        endif
c
c       order vertices
c
        i1=maxv+1
        i2=i1+maxv
        tt=timer(it)
        call order(z,z(i1),u,u0,udot,u0dot,
     1      ur,ul,itri,ivert,z(i2))
        tt=timer(it)-tt
        time(1,4)=time(1,4)+tt
        time(2,4)=time(2,4)+tt
c
        nv=getlvl(level1,itri)
        itri(2,4)=nv
        itri(1,4)=nv
        if(iadapt.eq.1) call iguess(itri,ivert,rusr,u,u0,
     1      udot,u0dot,ur,ul,z,rln,rltrgt,gxy)
        write(iunit,97) nv
        istep=0
        mxbis=10
        mxfail=10
        mxstep=10
        idsp=0
c
        nv=itri(2,6)
        ivx=1
        ivy=ivx+nv
        igm=ivy+nv
        ipp=igm+nv
        izz=ipp+nv
c
c       restore level1  solution
c
        do 5 i=1,nv
            u(i)=u0(i)
    5       udot(i)=u0dot(i)
        do 10 i=1,9
            rln(i,2)=rln(i,3)
   10       rln(i,1)=rln(i,3)
        rln(1,4)=rltrgt
        rln(2,4)=rtrgt
        rln(1,5)=rln(1,2)
        rln(2,5)=rln(2,2)
c
c       switch branches at bifurcation point
c
        if(iprob.ne.2) go to 15
        tt=timer(it)
        call swbrch(itri,ivert,z(ivx),z(ivy),rusr,
     1      ul,ur,u0dot,u0,z(izz),z(ipp),rln,a1xy,
     2      a2xy,fxy,gxy,p1xy,p2xy)
        tt=timer(it)-tt
        time(1,9)=time(1,9)+tt
        time(2,9)=time(2,9)+tt
        ip(7)=0
        ip(37)=0
        write(iunit,98) ip(35),ip(37),rln(1,2),rln(2,2),
     1      rln(3,2),rln(4,2),rln(5,2),int(rln(6,2)),rln(7,2)
c
c        path history
c
        num=int(path(51,1))
        if(num.ge.50) then
            do 12 i=1,50
                do 12 j=1,5
   12               path(i,j)=path(i+1,j)
            num=49
        endif
        num=num+1
        path(num,1)=rln(1,2)
        path(num,2)=rln(2,2)
        path(num,3)=rln(3,2)
        path(num,4)=rln(4,2)
        path(num,5)=-1.0e0
        path(51,1)=float(num)
        go to 110
c
c       switch functional and/or parameters
c
   15   if(iadapt.ne.1.and.iprob.lt.3) go to 25
        rln(4,6)=0.0e0
        rln(3,6)=0.0e0
        rln(6,5)=1.0e0
        if(iprob.eq.4) rln(3,6)=2.0e0
        ip(7)=0
        ip(8)=0
        rln(1,2)=rltrgt
        rln(2,2)=rtrgt
        rln(3,2)=1.0e0
        rln(4,2)=1.0e0
        call nwtt(ip,itri,ivert,rusr,z,u,udot,u0,u0dot,
     1      ur,ul,b,rln,time,hist,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
        write(iunit,98) ip(35),ip(37),rln(1,1),rln(2,1),
     1      rln(3,1),rln(4,1),rln(5,1),int(rln(6,1)),rln(7,1)
        if(ip(35).ne.0) go to 101
c
c        path history
c
        path(1,1)=rln(1,1)
        path(1,2)=rln(2,1)
        path(1,3)=rln(3,1)
        path(1,4)=rln(4,1)
        path(1,5)=0.0e0
        path(51,1)=float(1)
c
        rln(1,5)=rln(1,1)
        rln(2,5)=rln(2,1)
        go to 56
c
c       get set for an arc length continuation step
c
   25   istep=istep+1
        if(istep.gt.mxstep) go to 101
c
c       step picker
c
   27   tt=timer(it)
        call predct(itri,ivert,z(ivx),z(ivy),rusr,z(ipp),z(izz),
     1      z(igm),u0,u0dot,rln,idsp,mxfail,
     2      a1xy,a2xy,fxy,gxy,p1xy,p2xy)
        tt=timer(it)-tt
        time(1,6)=time(1,6)+tt
        time(2,6)=time(2,6)+tt
        if(idsp.gt.mxfail) go to 101
c
c       solve nonlinear equations
c
        iarc=iarcln(rln)
        call nwtt(ip,itri,ivert,rusr,z,u,udot,u0,u0dot,
     1      ur,ul,b,rln,time,hist,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
        write(iunit,98) ip(35),ip(37),rln(1,1),rln(2,1),
     1      rln(3,1),rln(4,1),rln(5,1),int(rln(6,1)),rln(7,1)
        if(ip(35).eq.0) go to 30
c
c       unsuccessful step
c
        if(ip(33).ne.0) go to 110
        idsp=idsp+1
        if(idsp.gt.mxfail) go to 101
        go to 27
c
c       successful step
c
   30   sn=rln(5,1)
        qsn=rln(5,2)
c
c        path history
c
        if(iarc.eq.0) then
            num=int(path(51,1))
            if(num.ge.50) then
                do 32 i=1,50
                    do 32 j=1,5
   32                   path(i,j)=path(i+1,j)
                num=49
            endif
            num=num+1
            path(num,1)=rln(1,1)
            path(num,2)=rln(2,1)
            path(num,3)=rln(3,1)
            path(num,4)=rln(4,1)
            path(num,5)=0.0e0
            path(51,1)=float(num)
        endif
        if(qsn*sn.ge.0.0e0.or.iprob.eq.0) go to 56
c
c       change in sign in determinent
c
        write(iunit,99)
        idsp=0
        isw=0
        eps=1.0e2*rln(4,4)
        bisc(1)=amax1(1.0e-6,eps)
        hist(21,11)=0.0e0
        hist(22,11)=rln(4,6)
        do 50 istep=1,mxbis
            call bisect(rln,isw,bisc)
c
            if(istep.gt.20) then
                do 45 i=1,19
   45               hist(i,11)=hist(i+1,11)
                hist(20,11)=abs(bisc(3)-bisc(4))
            else
                hist(istep,11)=abs(bisc(3)-bisc(4))
            endif
            hist(21,11)=float(istep)
            if(isw.eq.-1) go to 55
            call nwtt(ip,itri,ivert,rusr,z,u,udot,u0,u0dot,
     1          ur,ul,b,rln,time,hist,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
            write(iunit,98) ip(35),ip(37),rln(1,1),rln(2,1),
     1          rln(3,1),rln(4,1),rln(5,1),int(rln(6,1)),rln(7,1)
            if(ip(35).ne.0) go to 60
   50   continue
   55   ip(7)=0
c
c        path history
c
        num=int(path(51,1))
        path(num,1)=rln(1,1)
        path(num,2)=rln(2,1)
        path(num,3)=rln(3,1)
        path(num,4)=rln(4,1)
        path(num,5)=1.0e0
c
c       successful continuation
c
   56   do 57 i=1,9
            rln(i,2)=rln(i,1)
   57       rln(i,3)=rln(i,1)
        do 58 i=1,nv
            u0(i)=u(i)
   58       u0dot(i)=udot(i)
        if(idsp.eq.0) go to 60
        idsp=0
        go to 25
   60   rltrgt=rln(1,2)
        rtrgt=rln(2,2)
        rln(1,4)=rltrgt
        rln(2,4)=rtrgt
c
   97   format(/ 24x,'level 1 continuation   nv =',i4//
     1  8x,'   lambda       rho    lambda dot',
     2  '   rho dot  determinant eigenvalue')
   98   format(1x,i4,i3,4(1x,e10.3),1x,f6.3,'e',i3,2x,e10.3)
   99   format(/ 24x,'find limit / bifurcation point' /)
        go to 110
  100   ip(31)=iflag
        go to 110
  101   ip(35)=7
        ip(7)=iprob
        ip(8)=iadapt
        do 102 i=1,9
  102      rln(2,i)=rln(3,i)
  110   tx=timer(it)-tx
        time(1,17)=time(1,17)+tx
        time(2,17)=time(2,17)+tx
        call cstor(ip,itri)
        return
        end
