        integer function ccolor(icolor,ishade,jp)
            integer jp(25)
c
c       compute the color index
c
        ncolor=jp(5)
        nshade=jp(16)
        mxcolr=jp(17)
        if(ishade.eq.0) then
            ccolor=icolor+2-((icolor-1)/(mxcolr-1))*(mxcolr-1)
            if(ccolor.gt.mxcolr) ccolor=1
        else
            if(ishade.gt.0) then
                ccolor=icolor+2+ncolor*ishade
            else
                ccolor=icolor+2+ncolor*(nshade-ishade)
            endif
        endif
        return
        end
