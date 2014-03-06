        subroutine bindex(i,istart,llen,qtree)
c
            integer qtree(5,*)
c
c       compute coordinates for the list
c
        istart=qtree(1,i)
        ison=qtree(5,i)
        if(ison.gt.0) then
             llen=qtree(1,ison)-istart
             return
        else
            if(i.eq.3) then
                llen=qtree(2,3)
                return
            else
                icent=(i/2)*2
                if(i.eq.icent) then
                    llen=qtree(1,i+1)-istart
                    return
                else
                    llen=qtree(2,icent)-istart
                    return
                endif
            endif
        endif
        end
