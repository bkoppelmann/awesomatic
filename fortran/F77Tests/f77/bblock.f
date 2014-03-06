        subroutine bblock(jp,itnode,itedge,
     1      list,tlist,vx,vy,q,cen,eps,iflag)
c
            integer jp(25),itnode(3,*),itedge(3,*),
     1          list(*),tlist(2,*),tblock,tlen,endin,endout
            real vx(*),vy(*),q(3,3),cen(*)
c
c       compute boundary types
c
        iflag=0
c
        nt=jp(1)
        llen=jp(7)
        tlen=jp(8)
c
        c=q(2,1)
        s=-q(1,1)
        eps1=amax1(1.0e-4,eps*8.0e0)
c
c       make a list of triangles with boundary edges
c
        mm=tlen+1
        ll=0
        hmax=0.0e0
        do 20 i=1,nt
            kin=0
            kout=0
            iv1=itnode(1,i)
            iv2=itnode(2,i)
            iv3=itnode(3,i)
            xm=(vx(iv1)+vx(iv2)+vx(iv3))/3.0e0
            ym=(vy(iv1)+vy(iv2)+vy(iv3))/3.0e0
            cen(i)=c*ym-s*xm
            do 10 j=1,3
                ivj=itnode(j,i)
                ym=c*vy(ivj)-s*vx(ivj)
                hmax=amax1(hmax,abs(cen(i)-ym))
                if(itedge(j,i).gt.0) go to 10
                ity=tblock(itnode,i,j,vx,vy,q,eps1)
                if(ity.eq.0) go to 10
                if(ity.eq.1) then
                    if(kin.eq.0) then
                        ll=ll+1
                        tlist(1,ll)=i
                        tlist(2,ll)=j
                        kin=j
                    else
                       tlist(2,ll)=6-j-kin
                    endif
                else
                    if(kout.eq.0) then
                        mm=mm-1
                        tlist(1,mm)=i
                        tlist(2,mm)=j
                        kout=j
                    else
                       tlist(2,mm)=6-j-kout
                    endif
                endif
   10       continue
   20   continue
        if(mm.le.ll) go to 160
        if(2*(tlen-mm+1+ll).gt.llen) go to 160
c
c       sort edges
c
       do 50 ic=1,2
            if(ic.eq.1) then
                iptr=1
                kl=ll
            else
                iptr=mm
                kl=tlen-mm+1
            endif
            l2=kl/2
            do 30 i=l2,1,-1
                call mkheap(i,kl,cen,tlist(1,iptr))
   30       continue
            do 40 i=kl,1,-1
                i1=tlist(1,iptr)
                i2=tlist(2,iptr)
                tlist(1,iptr)=tlist(1,iptr+i-1)
                tlist(2,iptr)=tlist(2,iptr+i-1)
                tlist(1,iptr+i-1)=i1
                tlist(2,iptr+i-1)=i2
                call mkheap(1,i-1,cen,tlist(1,iptr))
   40       continue
   50   continue
c
c       move to list
c
        endin=0
        do 55 i=1,ll
            list(endin+1)=tlist(1,i)
            list(endin+2)=tlist(2,i)
   55       endin=endin+2
        endout=endin
        do 60 i=mm,tlen
            list(endout+1)=tlist(1,i)
            list(endout+2)=tlist(2,i)
   60       endout=endout+2
c
c       now make list of triangle pairs that interfere
c
        left=1
        right=1
        num=0
        do 80 ii=endin+1,endout,2
            it=list(ii)
            iedge=list(ii+1)
            j1=(5-iedge)/2
            j2=6-iedge-j1
            j1=itnode(j1,it)
            j2=itnode(j2,it)
            x1i=c*vx(j1)+s*vy(j1)
            y1i=c*vy(j1)-s*vx(j1)
            x2i=c*vx(j2)+s*vy(j2)
            y2i=c*vy(j2)-s*vx(j2)
            ximax=amax1(x1i,x2i)
            yimax=amax1(y1i,y2i)
            ximin=amin1(x1i,x2i)
            yimin=amin1(y1i,y2i)
            epsi=eps*(yimax-yimin+ximax-ximin)
c
c
            imid=(left+right)/2
            imid=min0((imid/2)*2+1,endin-1)
            do 75 ic=1,2
                if(ic.eq.1) then
                    istrt=imid-2
                    if(imid.eq.1) istrt=1
                    iend=1
                    istep=-2
                else
                    istrt=imid
                    if(imid.eq.1) istrt=3
                    iend=endin-1
                    istep=2
                endif
c
                do 70 jj=istrt,iend,istep
                    jt=list(jj)
                    jedge=list(jj+1)
                    if(it.eq.jt) go to 70
                    j1=(5-jedge)/2
                    j2=6-jedge-j1
                    j1=itnode(j1,jt)
                    j2=itnode(j2,jt)
                    je=itnode(jedge,jt)
                    y1j=c*vy(j1)-s*vx(j1)
                    y2j=c*vy(j2)-s*vx(j2)
                    ymj=c*vy(je)-s*vx(je)
                    ymj=(ymj+y1j+y2j)/3.0e0
c
c       tests for end of relevant list
c
                    if(ic.eq.1) then
                        if(yimin-hmax.ge.ymj) then
                            left=jj
                            go to 75
                        endif
                    else
                        if(yimax+hmax.le.ymj) then
                            right=jj
                            go to 75
                        endif
                    endif
c
c       tests to disregard this element
c
                    x1j=c*vx(j1)+s*vy(j1)
                    x2j=c*vx(j2)+s*vy(j2)
                    xjmax=amax1(x1j,x2j)
                    xjmin=amin1(x1j,x2j)
                    yjmax=amax1(y1j,y2j)
                    yjmin=amin1(y1j,y2j)
                    epsj=eps*(yjmax-yjmin+xjmax-xjmin)+epsi
                    if((ximin-xjmax).ge.epsj) go to 70
                    if((yimin-yjmax).ge.epsj) go to 70
                    if((yjmin-yimax).ge.epsj) go to 70
c
c       final check for conflict
c
                    yy=(amax1(yimin,yjmin)+amin1(yimax,yjmax))/2.0e0
                    xi=(x1i*(y2i-yy)+x2i*(yy-y1i))/(y2i-y1i)
                    xj=(x1j*(y2j-yy)+x2j*(yy-y1j))/(y2j-y1j)
                    if((xi-xj).ge.epsj) go to 70
c
c       we have found a conflicting pair
c
                    num=num+1
                    if(num.gt.tlen) go to 160
                    tlist(1,num)=it
                    tlist(2,num)=jt
c
   70           continue
                if(ic.eq.1) then
                    left=1
                else
                    right=endin-1
                endif
   75       continue
   80   continue
        if(num.le.0) go to 140
c
c       make final list
c
        do 90 i=1,nt+1
   90       list(i)=0
c
        do 100 i=1,num
            j=tlist(1,i)
  100       list(j+1)=list(j+1)+1
c
        list(1)=nt+2
        do 110 i=2,nt+1
  110       list(i)=list(i)+list(i-1)
        if(list(nt+1).gt.llen+1) go to 160
c
        do 120 i=1,num
            j=tlist(1,i)
            k=list(j)
            list(j)=k+1
  120       list(k)=tlist(2,i)
c
        do 130 i=nt+1,1,-1
  130       list(i)=list(i-1)
        list(1)=nt+2
        return
c
c       no testing required
c
  140   do 150 i=1,nt+1
  150       list(i)=0
        return
  160   iflag=-1
        return
        end
