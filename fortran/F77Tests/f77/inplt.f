        subroutine inplt(title,vx,vy,ibndry,
     1      itnode,xm,ym,jb,ip,w)
c
            integer itnode(4,*),ibndry(5,*),jb(*),ip(100),jp(25)
            real vx(*),vy(*),xm(*),ym(*),w(*),t(25),tl(25)
            character*80 title
c
c       draw input data
c
        numbrs=ip(74)
        if(numbrs.lt.0.or.numbrs.gt.4) numbrs=0
        mxcolr=max0(2,ip(72))
        ired=max0(ip(95),1)
        igreen=ired+mxcolr
        iblue=igreen+mxcolr
        irad=iblue+mxcolr
c
        if(itnode(3,1).eq.0) then
            nt=ip(51)
            nv=ip(52)
            nc=ip(53)
            nb=ip(54)
            ixx=irad+nv
            llen=(ip(16)-ixx+1)/2
            iyy=ixx+llen
c
c      x and y might be too short if llen = nv and there
c      are big regions with many curved edges
c
            if(llen.lt.nv) return
        else
            nt=ip(1)
            nv=ip(2)
            nc=ip(3)
            nb=ip(4)
            itedge=irad+nv
            izz=itedge+3*nt
            if(izz+nv+nb+3*nt.gt.ip(16)) return
        endif
c
c       initialize
c
    2   jp(5)=nt
        jp(6)=0
        jp(7)=numbrs
        jp(16)=0
        jp(17)=mxcolr
c
        jp(1)=nt
        jp(2)=nv
        jp(3)=nc
        jp(4)=nb
c
        call binit(ip,vx,vy,xm,ym,itnode,
     1      ibndry,t,tl,jp)
        call clrmap(w(ired),w(igreen),w(iblue),jp)
c
        call pltutl(jp(18),w(ired),w(igreen),w(iblue))
c
        call title0(title,0)
        call legnd1(jp,tl)
c
c       draw skeleton
c
        if(itnode(3,1).eq.0) then
            call dskl(vx,vy,ibndry,
     1          itnode,xm,ym,jb,t,jp,w(ixx),w(iyy))
            call dskl(vx,vy,ibndry,
     1          itnode,xm,ym,jb,tl,jp,w(ixx),w(iyy))
        else
c
c       draw triangulation
c
            call dtri(vx,vy,ibndry,
     1          itnode,xm,ym,t,jp,w(itedge),w(izz))
            jp(6)=1
            call dtri(vx,vy,ibndry,
     1          itnode,xm,ym,tl,jp,w(itedge),w(izz))
            if(numbrs.eq.1) call tlabl(jp,itnode,vx,vy,t)
        endif
c
        call legnd0(tl)
        if(numbrs.eq.2) call vlabl(jp,itnode,ibndry,vx,vy,
     1      w(irad),t)
        if(numbrs.ge.3) call blabl(jp,ibndry,vx,vy,xm,ym,t)
c
        call pltutl(-1,w(ired),w(igreen),w(iblue))
        return
        end
