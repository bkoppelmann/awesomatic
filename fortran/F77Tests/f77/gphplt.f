        subroutine gphplt(title,ip,w)
c
            integer ip(100),jp(25)
            real w(*),red(10),green(10),blue(10)
            character*80 title
c
c       array pointers...in the order that they
c       occur in the w array
c
        jtime=ip(81)
        jhist=ip(82)
        jpath=ip(83)
        jusr=ip(84)
        iuu=ip(85)
        iudot=ip(86)
        iu0=ip(87)
        iu0dot=ip(88)
        iur=ip(89)
        iul=ip(90)
        itri=ip(91)
        ivert=ip(92)
        iee=ip(93)
        ibb=ip(94)
        iz=ip(95)
c
        call grinit(ip,jp,red,green,blue)
c
        call pltutl(jp(9),red,green,blue)
c
        igrsw=jp(10)
        if(igrsw.eq.0) then
            call title0(title,1)
            call prtip(ip)
        else
            if(igrsw.ge.13) then
                call title0(title,1)
            else
                call title0(title,0)
            endif
            call pgraph(jp,w(jhist),w(jtime),w(jpath))
        endif
c
        call pltutl(-1,red,green,blue)
        return
        end
