        integer function getlvl(lvl,itri)
c
            integer itri(4,*)
c
c       compute number of vertices for level lvl
c
        if(lvl.gt.1) go to 10
        getlvl=0
        if(lvl.eq.1) getlvl=itri(4,5)
        return
   10   iptr=itri(2,1)+3+4*(lvl-2)
        getlvl=itri(1,iptr)
        return
        end
