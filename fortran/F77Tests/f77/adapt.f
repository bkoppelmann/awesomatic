        subroutine adapt(ip,itri,ivert,rusr,z,u,udot,u0,u0dot,
     1      ur,ul,e,rln,time,hist,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
c
            integer ip(100),itri(4,*),ivert(4,*),getlvl
            real u(*),udot(*),u0(*),u0dot(*),ur(*),ul(*),rusr(2,*),
     1          z(*),time(2,20),rln(9,9),e(*),hist(22,12)
            external a1xy,a2xy,fxy,gxy,p1xy,p2xy
c
c       check to see if we have solved problem on current finest grid
c
        iprob=ip(7)
        iadapt=ip(8)
c
        itmax=20
        itmax1=40
        maxl=itri(4,3)
        lvlmx=maxl+4
        lvlmx1=maxl+6
        maxt=itri(1,6)
        maxv=itri(2,6)
        maxz=itri(3,6)
        ivx=1
        ivy=ivx+maxv
        nt0=itri(3,1)-1
        nv0=itri(1,4)
        iflag=0
        jflag=0
        irefn=itri(2,8)
        nvtrgt=itri(1,8)
        nv=itri(4,4)
        lvl=itri(1,3)
c
        itri(2,9)=itri(2,9)+1
        if(itri(2,9).eq.1) then
            i=getlvl(irefn,itri)
            nvtrgt=max0(i,nvtrgt)
            itri(1,8)=nvtrgt
            itri(3,9)=0
            if(max0((nv*7)/8,nv-10).gt.nvtrgt) itri(3,9)=-1
            if(min0((nv*9)/8,nv+10).lt.nvtrgt) itri(3,9)=1
        endif
c
        if(lvl.lt.maxl) then    
            i=nxtlvl(lvl,itri)
            itri(1,3)=i
            nv=getlvl(i,itri)
            itri(1,4)=nv
            iter=itmax+itmax1+1
            go to 50
        endif
c
c       adaptive refinement/error estimates
c
        if(itri(3,9).eq.1) then
            mxvmk=min0(4*nv,nvtrgt)
            if(mxvmk.lt.nvtrgt.and.4*mxvmk.gt.nvtrgt) then
                if(nvtrgt.lt.5*nv) then
                    mxvmk=nvtrgt
                else
                    q=sqrt(float(nvtrgt*nv))
                    mxvmk=int(q)
                endif
            endif
            mxvmk=min0(mxvmk,maxv)
        else 
            mxvmk=min0(nvtrgt,maxv)
        endif
c
        eps=rln(4,4)
        tol=amax1(10.0e0**(-ip(13)),eps)
        iter=0
        itri(1,9)=mxvmk
        itri(1,5)=itri(1,1)
        enorm0=rln(1,7)
c
c       compute the error in each triangle...
c
        if(iprob.lt.8) then
            tt=timer(it)
            call rlerr(itri,ivert,z(ivx),z(ivy),rusr,u,
     1          udot,rln,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
            tt=timer(it)-tt
            time(1,11)=time(1,11)+tt
            time(2,11)=time(2,11)+tt
        endif
   10   tt=timer(it)
        call energy(itri,ivert,z(ivx),z(ivy),rusr,u,udot,e,
     1      rln,iprob,nt0,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
        tt=timer(it)-tt
        time(1,10)=time(1,10)+tt
        time(2,10)=time(2,10)+tt
c
c       save convergence history
c
        if(iter.eq.0) then
            num=int(hist(22,1))
            if(num.eq.21) then
                do 20 i=1,20
                    do 20 j=1,5
   20                   hist(i,j)=hist(i+1,j)
                num=20
            endif
            if(num.gt.0) then
                do 30 i=num,1,-1
                    if(float(nv).gt.hist(i,1)*1.05e0) go to 40
   30           continue
                i=0
            else
                i=0
            endif
   40       num=i+1
            hist(num,1)=float(nv)
            hist(num,2)=rln(1,7)
            hist(num,3)=rln(3,7)
            hist(num,4)=rln(5,7)
            hist(num,5)=rln(1,8)
            hist(22,1)=float(num)
            hist(22,2)=rln(2,7)
            hist(22,3)=rln(4,7)
            hist(22,4)=rln(6,7)
            hist(22,5)=rln(2,8)
        endif
c
        if(iadapt.eq.2.or.iadapt.eq.4) go to 106
c
c       compute the error estimate
c
        if(iter.eq.0) then
            enorm=rln(1,7)
            unorm=rln(2,7)
            relerr=enorm
            if(unorm.gt.0.0e0) relerr=enorm/unorm
            if(nv.eq.itri(2,4).and.relerr.le.0.0e0) relerr=1.0e0
            if(itri(3,9).ne.-1.and.relerr.le.tol) go to 102
        endif
c
c       mark triangles with big errors
c
        tt=timer(it)
        itri(3,8)=iter
        call marka(itri,ivert,e,z,z(maxt+1),eps,jflag)
        tt=timer(it)-tt
        time(1,2)=time(1,2)+tt
        time(2,2)=time(2,2)+tt
        if(jflag.eq.6) go to 100
c
c       compute next triangulation
c
        tt=timer(it)
        call regrup(z,z(maxt+1),u,u0,udot,u0dot,ur,ul,
     1      e,itri,ivert,1)
        tt=timer(it)-tt
        time(1,3)=time(1,3)+tt
        time(2,3)=time(2,3)+tt
        nv0=itri(4,4)
        tt=timer(it)
        call triang(itri,ivert,iflag)
        tt=timer(it)-tt
        time(1,1)=time(1,1)+tt
        time(2,1)=time(2,1)+tt
        if(iflag.ne.0) go to 104
c
        itri(1,3)=itri(4,3)
        itri(1,4)=itri(4,4)
        nv=itri(4,4)
c
c       update current solution as necessary
c
   50   if(nv0.lt.nv) then     
            tt=timer(it)
            call intrp(itri,ivert,z(ivx),z(ivy),
     1          rusr,u,u0,ur,ul,udot,u0dot,
     2          rln,nv0,iprob,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
            tt=timer(it)-tt
            time(1,12)=time(1,12)+tt
            time(2,12)=time(2,12)+tt
        endif
c
        iter=iter+1
        maxl=itri(4,3)
        if(iabs(nv-mxvmk)*5.lt.mxvmk) then
            itmax=min0(itmax+1,itmax1)
            lvlmx=min0(lvlmx+1,lvlmx1)
        endif
        if(iter.lt.itmax.and.maxl.lt.lvlmx.and.jflag.eq.0) go to 10
c
c       set parameters for a normal return
c
   99   ip(31)=0
        ip(32)=0
        return
c
c       exceptional returns
c
  100   ip(32)=6
        return
  102   if(iter.gt.0) go to 99
        ip(32)=13
        return
  104   if(iter.gt.0) go to 99
        ip(31)=iflag
        ip(32)=16
        return
  106   ip(32)=8
        return
        end
