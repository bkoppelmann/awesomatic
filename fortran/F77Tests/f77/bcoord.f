        subroutine bcoord(i,ib,qtree)
c
            integer qtree(5,*),ib(4)
c
c       integer (x,y) coordinates with respect to
c       a uniform refinement
c
        if(i.le.3) then
            do 20 j=1,4
   20           ib(j)=1
            return
        else
            icent=(i/2)*2
            ib(1)=qtree(3,icent)
            ib(2)=qtree(3,icent+1)
            ib(3)=qtree(4,icent)
            ib(4)=qtree(4,icent+1)
            if(i.ne.icent) then
                ity=qtree(2,icent+1)
                if(ity.eq.0) then
                    ib(1)=ib(1)+1
                else
                    ib(2)=ib(2)+1
                endif
            endif
        endif
        return
        end
