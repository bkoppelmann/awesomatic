        subroutine marku(itri,ivert,e,lunref,
     1      lref,nvkept,thresh)
c
            integer itri(4,*),ivert(4,*)
            integer son,iadj(3),father,lunref(*),lref(*)
            real e(*)
c
c       determine which elements to unrefine
c
        irefn=itri(2,8)
        nv=itri(4,4)
        maxl=itri(4,3)
        do 150 lvl=maxl,irefn+1,-1
c
c       1-irregular rule with repscet to refined elements for this level
c
            it1=lref(lvl)
            it2=lref(lvl+1)-1
            if(it1.le.it2) then
                do 50 it=it1,it2
                    i=lref(it)
                    if(itri(4,i).lt.0) then
                       call edges(i,iadj,itri,ivert)
                       do 40 j=1,3
                           k=iadj(j)
                           if(k.le.0) go to 40
                           if(level(k,itri).lt.lvl) go to 40
                           k=(k/4)*4+3
                           itri(1,k)=0
   40                  continue
                    endif
   50           continue
            endif
c
c       mark triangles with large errors
c
            it1=lunref(lvl)
            it2=lunref(lvl+1)-1
            if(it1.gt.it2) go to 150
            do 70 it=it1,it2
                ii=lunref(it)
                if(itri(1,ii+3).eq.0) go to 70
                ifath=father(ii,itri)
                emx=amax1(e(ifath),e(ii),e(ii+1),e(ii+2),e(ii+3))
                if(emx.ge.thresh) itri(1,ii+3)=0
   70       continue
c
c       2-neighbor rule for triangles at this level
c
            do 110 it=it1,it2
                ii=lunref(it)
                if(itri(1,ii+3).eq.0) go to 110
                i=ii
   80           ifath=father(i,itri)
                call edges(ifath,iadj,itri,ivert)
                next=0
                ncount=0
                do 90 j=1,3
                    k=iadj(j)
                    if(k.le.0) go to 90
                    kson=son(k,itri)
                    if(kson.lt.0) then
                        ncount=ncount+1
                    elseif(kson.gt.0) then
                        if(itri(1,kson+3).eq.1) then
                            next=kson
                        else
                            ncount=ncount+1
                        endif
                    endif
   90           continue
                if(ncount.lt.2) go to 110
                itri(1,i+3)=0
                if(next.ne.0) then
                    i=next
                    go to 80
                endif
  110       continue
c
c       now apply 1-irregular rule to next coarser level
c
            do 140 it=it1,it2
                i=lunref(it)
                if(itri(1,i+3).eq.1) go to 140
                ifath=father(i,itri)
                if(level(ifath,itri).le.irefn) go to 140
                k=(ifath/4)*4
                itri(1,k+3)=0
                call edges(ifath,iadj,itri,ivert)
                do 130 j=1,3
                    if(iadj(j).gt.0) then
                        k=(iadj(j)/4)*4
                        itri(1,k+3)=0
                    endif
  130           continue
  140       continue
  150   continue
c
c       count vertices
c
        nvkept=0
        do 170 i=1,nv
            if(istat(i,itri,ivert).eq.0) nvkept=nvkept+1
  170   continue
        return
        end
