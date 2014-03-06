        subroutine marka(itri,ivert,e,lunref,lref,eps,iflag)
c
            integer itri(4,*),ivert(4,*),lref(*),
     1          lunref(*),rtype,ncall,kref(21),kunref(21)
            real e(*)
c
c       marke triangles for reginement and unrefinement
c
c       iflag = 0 : continue refinement/unrefinement
c             = 1 : return from adapt/get new solution
c             = 6 : return from pltmg/mesh is ok.
c
        iflag=0
        maxl=itri(4,3)
        nv=itri(4,4)
        nvtrgt=itri(1,9)
        irefn=itri(2,8)
        iter=itri(3,8)
        rtype=itri(3,9)
        ncall=itri(2,9)
        mnv=max0((nvtrgt*7)/8,nvtrgt-10)
        mxv=min0((nvtrgt*9)/8,nvtrgt+10)
        if(iter.eq.0) then
            if(rtype.eq.1.and.nv.ge.mnv) iflag=6
            if(rtype.eq.-1.and.ncall.ge.2) iflag=6
            if(rtype.eq.0.and.ncall.ge.3) iflag=6
            if(rtype.ne.1.and.maxl.le.irefn) iflag=6
            if(iflag.eq.6) return
        endif
        rat0=16.0e0
        q=1.0e0/float(ncall)
        rat1=2**(1.0e0-q)
c
        call ungren(itri)
        i1=itri(1,1)
        i2=itri(2,1)
        i3=itri(3,1)-1
c
c       compute emaxr, eave, emax, emin
c
        emax=0.0e0
        emaxr=0.0e0
        eave=0.0e0
        eaveu=0
        ic=0
        do 20 i=i1,i3
            if(itri(4,i).le.0) then
                ic=ic+1
                emaxr=amax1(emaxr,e(i))
                if(e(i).gt.0.0e0) eave=eave+alog(e(i))
            endif
            if(level(i,itri).gt.irefn) emax=amax1(emax,e(i))
   20   continue
        eave=exp(eave/float(ic))
        emin=emax 
        eminr=emaxr  
        do 30 i=i1,i3
            if(e(i).le.0.0e0) go to 30
            if(itri(4,i).le.0) eminr=amin1(eminr,e(i))
            if(level(i,itri).gt.irefn) emin=amin1(emin,e(i))
   30   continue
        if(emaxr.gt.0.0e0) then
            qmaxr=alog(emaxr)
            qminr=alog(eminr)
        else
            qmaxr=0.0e0
            qminr=0.0e0
        endif
        if(emax.gt.0.0e0) then
            qmax=alog(emax)
            qmin=alog(emin)
        else
            qmax=0.0e0
            qmin=0.0e0
        endif
c
c       compute lref, lunref arrays
c
        call clref(itri,ivert,lref,e,qminr,qmaxr,kref)
        call clunrf(itri,ivert,lunref,e,qmin,qmax,kunref)
c
c       compute thresholds
c
        q=1.0e0/float(iter+1)
        thrshr=(0.5e0**q)*(0.85e0**(1.0e0-q))*emaxr
        thrshr=amax1(thrshr,eave*0.95e0)
        thrshu=((0.95e0*thrshr)**q)*(eminr**(1.0e0-q))
        q=1.0e0/float(iter+ncall)
        if(rtype.ne.-1) thrshu=amin1(thrshu,(4.0e0**q)*eave)
        tmin=thrshr
c
c       set up hyperbola
c
        emx=amax1(emax,emaxr)*1.1e0
        emn=amin1(emin,eminr)/1.1e0
        if(rtype.eq.-1) then
            tmax=emx
        else
            tmax=emaxr*1.05e0
        endif
        dd=emx-emn
        qr=(emx-thrshr)/dd
        qu=(thrshu-emn)/dd    
        qd=1.0e0-qu-qr
        qn=qr*qu
        sr=(emx-emaxr)/dd
        su=(emin/1.05e0-emn)/dd    
        sd=(1.0e0-su-sr)*2.0e0
        sn=sr*su
        if(qd.gt.0.0e0.and.sn.gt.1.0e-15*qd) then
            ktype=0
            cf=qn/qd
            cmin=sn/sd
            cmax=cf
        else
c
c       in degenerate cases, just refine or unrefine or return
c
            cmin=1.0e-1
            cmax=1.0e0
            if(rtype.eq.0) then
                if(iter.eq.0) then
                    iflag=6
                    call rset(itri,ivert,lref,lunref,0)
                    return
                else
                    if(nv.lt.nvtrgt) then
                        rtype=1
                    else
                        rtype=-1
                    endif
                endif
            endif
            if(rtype.eq.1) then
                ktype=1
                thrshu=-emx
            else
                ktype=-1
                tmin=emn
                tmax=emx
                thrshr=emx
                thrshu=tmax*sqrt(tmin/tmax)
            endif
        endif
c 
c       the main loop
c
        itnum=0
        itmax=40
   40   call markr(itri,ivert,e,lref,nvref,thrshr)
        if(ktype.eq.0) then
            qr=(emx-thrshr)/dd
            thrshu=((1.0e0-qr)/(cf+qr))*cf*dd+emn
        endif
        call marku(itri,ivert,e,lunref,lref,nvkept,thrshu)
        nvnew=nvref+nvkept
c
c       compute eref, eunref
c
        if(rtype.eq.0) then
            eref=emaxr 
            do 50 it=lref(1),lref(maxl+1)-1
                i=lref(it)
                if(itri(4,i).eq.-1) then
                    if(e(i).ge.thrshr) eref=amin1(e(i),eref)
                endif
   50       continue
            eunref=0.0e0
            do 60 it=lunref(1),lunref(maxl+1)-1
                    i=lunref(it)
                    if(itri(1,i+3).eq.1) then
                        do 55 j=i,i+3
                            if(e(j).gt.0.0e0) 
     1                          eunref=amax1(eunref,e(i))
   55                   continue
                    endif
   60       continue
        endif
c
c       convergence checks
c 
        if(rtype.eq.1) then
            if(nvnew.ge.mnv.and.nvnew.le.mxv) then
                iflag=1
                return
            endif
            if(nvkept.eq.nv.and.nvnew.le.mxv.and.nvref.gt.0) return
        else if(rtype.eq.-1) then
            if(nvnew.ge.mnv.and.nvnew.le.mxv) then
                iflag=1
                return
            endif
        else
            if(nvnew.ge.mnv.and.nvnew.le.mxv) then
                iflag=1
                if(iter.eq.0) then
                    if(eunref.gt.eref/rat0.or.nvref.le.
     1                  nv/20.or.nvkept.ge.(nv*19)/20) then
                        iflag=6
                        call rset(itri,ivert,lref,lunref,0)
                    endif
                endif
                return
            endif
            if(iter.eq.0.and.nvnew.lt.mnv.and.nvnew.ge.nv/2
     1          .and.eunref.le.amin1(eref/rat0,eave/rat1)) return
            if(nvkept.eq.nv.and.nvnew.le.mxv.and.nvref.gt.0) return
        endif
        if(itnum.gt.itmax) then
            iflag=1
            return 
        endif
c
c       bisection, using geometric average as next threshold
c
        if(ktype.eq.0) then
            if(nvnew.ge.nvtrgt) then
                if(nvref.eq.0.and.cf.lt.cmax*0.99e0) then
                    cmin=cf
                    qr=(emx-thrshr)/dd
                    tt=cthrsh(kunref,thrshu,qmin,qmax,-1)
                    qu=(amin1(tt,emax*1.05e0)-emn)/dd    
                    qd=1.0e0-qu-qr
                    qn=qr*qu
                    cc=qn/qd
                    cf=cmax*sqrt(cmin/cmax)
                    if(cc-cmin.gt.cmin*eps) cf=amin1(cf,cc)
                    if(cmax-cmin.lt.cmax*eps) cf=cmax
                else
                    tmin=thrshr
                    tt=cthrsh(kref,thrshr,qminr,qmaxr,-1)
                    thrshr=tmax*sqrt(tmin/tmax)
                    if(tt-tmin.gt.tmin*eps) thrshr=amin1(thrshr,tt)
                    if(tmax-tmin.lt.tmax*eps) then
                        if(thrshr.lt.tmax) then
                            thrshr=tmax
                        else
                            mnv=mnv-10
                            mxv=mxv+10
                        endif
                    endif
                endif
            else
                if(thrshr.lt.tmin*1.01e0.and.nvkept.lt.nv) then
                    cmax=cf
                    qr=(emx-thrshr)/dd
                    tt=cthrsh(kunref,thrshu,qmin,qmax,1)
                    qu=(amax1(tt,emin/1.05e0)-emn)/dd    
                    qd=1.0e0-qu-qr
                    qn=qr*qu
                    cc=qn/qd
                    cf=cmax*sqrt(cmin/cmax)
                    if(cmax-cc.gt.cmax*eps) cf=amax1(cf,cc)
                    if(cmax-cmin.lt.cmax*eps) then
                        if(cf.ne.cmin) then
                            cf=cmin
                        else
                            mnv=mnv-10
                            mxv=mxv+10
                        endif
                    endif
                else
                    tmax=thrshr
                    tt=cthrsh(kref,thrshr,qminr,qmaxr,1)
                    thrshr=tmax*sqrt(tmin/tmax)
                    if(tmax-tt.gt.tmax*eps) thrshr=amax1(thrshr,tt)
                    if(tmax-tmin.lt.tmax*eps) thrshr=tmin
                endif
            endif
        else if(ktype.eq.1) then
            if(nvnew.ge.nvtrgt) then
                tmin=thrshr
            else
                tmax=thrshr
            endif
            thrshr=tmax*sqrt(tmin/tmax)
            if(tmax-tmin.lt.eps*tmax) then
                mnv=mnv-10
                mxv=mxv+10
            endif
        else
            if(nvnew.gt.nvtrgt) then
                tmin=thrshu
            else
                tmax=thrshu
            endif
            thrshu=tmax*sqrt(tmin/tmax)
            if(tmax-tmin.lt.eps*tmax) then
                mnv=mnv-10
                mxv=mxv+10
            endif
        endif
        itnum=itnum+1
c
c       reset element markers
c
        call rset(itri,ivert,lref,lunref,1)
        go to 40
c
        end
