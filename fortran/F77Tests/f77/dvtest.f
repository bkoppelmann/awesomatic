        integer function dvtest(i,itri)
c
            integer itri(4,*),son
c
c       divide test
c
        dvtest=0
        if(son(i,itri).lt.0) dvtest=1
        if(level(i,itri).lt.itri(2,8)) dvtest=1
        return
        end
