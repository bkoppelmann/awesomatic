        subroutine getlst(x,y,queue,iptr,qtree,list,p)
c
            integer qtree(5,*),list(*),queue(*)
            real p(4)
c
c       make a list of elements
c
        iptr=0
        if(x.lt.p(1).or.x.gt.p(2)) return
        if(y.lt.p(3).or.y.gt.p(4)) return
c
        xx=(x-p(1))/(p(2)-p(1))
        yy=(y-p(3))/(p(4)-p(3))
        iptr=1
        queue(iptr)=3
        jptr=1
c
   50   if(iptr.lt.jptr) return
        i=queue(jptr)
        jptr=jptr+1
c
c       check for son
c
        i=qtree(5,i)
        if(i.gt.0) then
            ity=qtree(2,i+1)
            if(ity.eq.0) then
                ix=int(xx*float(qtree(4,i)))
                ir=qtree(3,i)
                if(ir.ge.ix.and.ir.le.ix+2) then
                    iptr=iptr+1
                    queue(iptr)=i
                endif
                if(ir+1.ge.ix.and.ir+1.le.ix+2) then
                    iptr=iptr+1
                    queue(iptr)=i+1
                endif
            else
                iy=int(yy*float(qtree(4,i+1)))
                ir=qtree(3,i+1)
                if(ir.ge.iy.and.ir.le.iy+2) then
                    iptr=iptr+1
                    queue(iptr)=i
                endif
                if(ir+1.ge.iy.and.ir+1.le.iy+2) then
                    iptr=iptr+1
                    queue(iptr)=i+1
                endif
            endif
        endif
        go to 50
        end
