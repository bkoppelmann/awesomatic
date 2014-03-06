        integer function istl(i,itri)
c
            integer itri(4,*),son
c
c       determine in element is in level lvl grid
c
        istl=0
        lvl=itri(1,3)
        ilev=level(i,itri)
        if(ilev.gt.lvl) return
        ison=son(i,itri)
        if(ilev.eq.lvl.or.ison.le.0) istl=1
        return
        end
