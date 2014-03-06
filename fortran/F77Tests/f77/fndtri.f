        subroutine fndtri(x,y,i,iv,c,itri,ivert,
     1      vx,vy,cxy,queue,llen,qtree,list,eps)
c
            integer itri(4,*),ivert(4,*),list(*),
     1          queue(*),qtree(5,*),iv(3),iadj(3)
            real vx(*),vy(*),cxy(3,*),c(3)
c
c       find the triangle containing (x,y)
c
        i=0
        if(llen.le.0) return
        icount=0
        do 50 kk=llen,1,-1
            call bindex(queue(kk),i1,klen,qtree)
            if(klen.le.0) go to 50
            i2=i1+klen-1
            do 40 kl=i2,i1,-1
c
c       get a seed element
c
                icount=icount+1
                i=list(kl)
c
c       check if (x,y) is in straight edge triangle i
c
   10           call knots(i,iv,itri,ivert)
                call bari(x,y,vx,vy,iv,c)
                do 20 j=1,3
                    if(c(j)+eps.lt.0.0e0) go to 30
   20           continue
c
c       check for curved edges
c
                ii=icurv(ie,i,itri,ivert)
                if(ii.eq.0) return
                if(cxy(3,ii).gt.0.0e0) return
                z=(x-cxy(1,ii))*(x-cxy(1,ii))
     1              +(y-cxy(2,ii))*(y-cxy(2,ii))
                if(z*(1.0e0+eps)+cxy(3,ii).lt.0.0e0) i=0
                return
c
c       (x,y) is not in triangle i
c
   30           if(icount.eq.1) then
                    call ledges(i,iv,iadj,itri,ivert)
                    if(iadj(j).gt.0) then
                        i=iadj(j)
                        go to 10
                    endif
                endif
c
c       check for curved edges
c
                ii=icurv(ie,i,itri,ivert)
                if(ii.eq.0) go to 40
                if(cxy(3,ii).le.0.0e0) go to 40
                if(c(ie).ge.0.0e0) go to 40
                z=(x-cxy(1,ii))*(x-cxy(1,ii))
     1              +(y-cxy(2,ii))*(y-cxy(2,ii))
                if(z*(1.0e0-eps).le.cxy(3,ii)) return
   40       continue
   50   continue
        i=0
        return
        end
