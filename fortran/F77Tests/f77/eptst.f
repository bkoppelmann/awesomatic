        integer function eptst(x)
c
c       this is to force a store of eps1 to memory
c
        if(x.eq.1.0e0) then
            eptst=1
        else
            eptst=0
        endif
        return
        end
