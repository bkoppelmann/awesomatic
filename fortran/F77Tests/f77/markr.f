        subroutine markr(itri,ivert,e,lref,nvnew,thresh)
            integer itri(4,*),ivert(4,*)
            integer iadj(3),level,lref(*)
            real e(*)
c
c       determine which triangles to refine
c
        nvnew=0
        maxl=itri(4,3)
        do 150 lvl=maxl,1,-1
c
            it1=lref(lvl)
            it2=lref(lvl+1)-1
            if(it1.gt.it2) go to 150 
c
c       mark triangles with large errors
c
            do 70 it=it1,it2
                i=lref(it)
                if(e(i).ge.thresh) then
                    itri(4,i)=-1
                endif
   70       continue
c
c       2-neighbor rule for triangles at this level
c
            do 110 it=it1,it2
                i=lref(it)
                if(itri(4,i).lt.0) go to 110
   80           call edges(i,iadj,itri,ivert)
                next=0
                ncount=0
                do 90 j=1,3
                    k=iadj(j)
                    if(k.le.0) go to 90
                    if(level(k,itri).lt.lvl) go to 90
                    if(itri(4,k).eq.0) then
                        next=k
                    else
                        ncount=ncount+1
                    endif
   90           continue
                if(ncount.lt.2) go to 110
                itri(4,i)=-1
                if(next.ne.0) then
                    i=next
                    go to 80
                endif
  110       continue
c
c       now apply 1-irregular rule to next coarser level
c       and count vertices to be added
c
            do 140 it=it1,it2
                i=lref(it)
                if(itri(4,i).eq.0) go to 140
                call edges(i,iadj,itri,ivert)
                do 130 j=1,3
                    k=iadj(j)
                    if(k.le.0) then
                        nvnew=nvnew+1
                    else
                        if(level(k,itri).lt.lvl) then
                            itri(4,k)=-1
                            nvnew=nvnew+1
                        else
                            kson=itri(4,k)
                            if(kson.eq.0) then
                                nvnew=nvnew+1
                            elseif(kson.lt.0) then
                                if(k.lt.i)  nvnew=nvnew+1
                            else
                                itri(1,kson+3)=0
                            endif
                        endif
                    endif
  130           continue
  140       continue
  150   continue
c
        return
        end
