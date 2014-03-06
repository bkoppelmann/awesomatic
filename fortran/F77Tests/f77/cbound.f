        subroutine cbound(iedge,ity,icen,mfi,mfe,itag,itri)
c
            integer itri(4,*)
c
c       compute information on boundary edge iedge
c
        ity=1
        if(iedge.ge.itri(2,2)-itri(3,2)) ity=-1
        mfi=itri(1,iedge)
        mfe=itri(2,iedge)
        itag=itri(3,iedge)
        icen=itri(4,iedge)
        return
        end
