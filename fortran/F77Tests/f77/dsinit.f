        subroutine dsinit(ip,itri,ivert,rusr,rln,jp,t,
     1      itnode,itedge,itag,ilevel,label,map,vx,vy,cxy,ce,e)
c
            integer itri(4,*),ivert(4,*),ip(100),
     1          jp(25),itnode(3,*),itedge(3,*),ilevel(*),
     2          itag(*),label(*),map(*),tag
            real vx(*),vy(*),cxy(3,*),rusr(2,*),
     1          rln(9,9),t(25),ce(*),e(*)
c
c       compute the triplt data structures
c       construct vx,vy,cxy
c
        nv=itri(4,4)
        nc=itri(3,4)
        call crad(cxy,rusr,itri,ivert)
        call cvert(itri,ivert,vx,vy,rusr)
c
c       triangle data structures itnode, itedge,
c       ilevel, itag, and label
c
        it1=itri(1,1)
        it2=itri(4,1)-1
        nt=0
        lvls=max0(1,ip(65))
        lvlsv=itri(1,3)
        lvls=min0(lvlsv,lvls)
        itri(1,3)=lvls
        do 20 i=it1,it2
            if(istl(i,itri).eq.0) go to 20
            nt=nt+1
            call knots(i,itnode(1,nt),itri,ivert)
            call ledges(i,itnode(1,nt),itedge(1,nt),itri,ivert)
            ilevel(nt)=level(i,itri)
            label(nt)=i
            itag(nt)=tag(i,itri)
            map(i)=nt
            ce(nt)=e(i)
   20   continue
        itri(1,3)=lvlsv
c
c       compute final form of itedge
c
        mntag=itag(1)
        mxtag=itag(1)
        do 40 i=1,nt
            mntag=min0(itag(i),mntag)
            mxtag=max0(itag(i),mxtag)
            do 30 j=1,3
                if(itedge(j,i).lt.0) then
                    k=-itedge(j,i)
                    call cbound(k,i0,ll,i1,i2,i3,itri)
                    if(ll.gt.0) then
                        itedge(j,i)=-ll
                    else
                        itedge(j,i)=0
                    endif
                else
                    k=itedge(j,i)
                    itedge(j,i)=map(k)
                endif
   30       continue
   40   continue
c
c       check control papameters in ip
c
        mag=max0(1,ip(62))
        ix=max0(0,ip(63))
        ix=min0(2*mag,ix)
        iy=max0(0,ip(64))
        iy=min0(2*mag,iy)
        ifun=iabs(ip(66))
        icont=1
        if(ip(66).lt.0) icont=0
        ncon=ip(67)
        if(ifun.gt.17) ifun=0
        iscale=ip(68)
        if(iscale.lt.0.or.iscale.gt.2) iscale=0
        lines=ip(75)
        if(lines.lt.0.or.lines.gt.2) lines=0
        numbrs=ip(74)
        if(numbrs.lt.0.or.numbrs.gt.2) numbrs=0
        nx=ip(69)
        ny=ip(70)
        nz=ip(71)
        mxcolr=max0(2,ip(72))
        if(ifun.ge.9.and.ifun.le.11) ncolor=6
        if(ifun.eq.8) ncolor=mxtag-mntag+1
        if(ifun.eq.7) ncolor=lvls
        if(ifun.le.6.or.ifun.gt.11) ncolor=max0(1,ncon)
        i3d=1
        if(ifun.ge.7) i3d=0
        if(numbrs.ne.0) i3d=0
        if(nx.eq.0.and.ny.eq.0) i3d=0
c
c       set up jp
c
        jp(1)=nt
        jp(2)=nv
        jp(3)=nc
        jp(4)=icont
        jp(5)=ncolor
        jp(6)=ifun
c
        jp(9)=mntag-1
        jp(10)=mag
        jp(11)=ix
        jp(12)=iy
        jp(13)=nx
        jp(14)=ny
        jp(15)=nz
c
        jp(17)=mxcolr
        jp(19)=iscale
        jp(20)=lines
        jp(21)=numbrs
        jp(22)=i3d
c
c       lambda for function qxy
c
        t(6)=rln(1,1)
c
        return
        end
