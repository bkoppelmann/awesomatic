c******************** machine dependent routine *******************
c------------------------------------------------------------------
c
c           piecewise linear triangle multi grid package
c
c                 edition 6.0 - - - february, 1990
c
c------------------------------------------------------------------
        subroutine gutl(ncolor,red,green,blue)
c
            real red(*),green(*),blue(*)
            integer eunit,wtype,wkid,conid,tnr,fstyle
            character*80 strg
            common /gks1/iflag,maxmap
            data  eunit,wkid,conid,tnr,iclear,jflag/14,1,0,1,1,0/
            data  wxmin,wxmax,wymin,wymax/0.0e0,1.5e0,0.0e0,1.0e0/
            data  vxmin,vxmax,vymin,vymax/0.0e0,1.0e0,0.0e0,0.66666667/
            data  ltype,lcolor,fstyle,junit/1,1,1,15/
            data  idev,imode,iecho/1,0,0/
c
c       gks graphis interface
c       create the display and set attributes
c
c       wtype=13 vt240
c       wtype=34 la210
c       wtype=41 microvax ii color workstation
c       wtype=61 postscript file
c       wtype=72 tek 4014
c
        wtype=41
        if(ncolor.lt.0) go to 20
c
c       initialize
c
        open(unit=eunit,file='gks.log',status='new')
        call gopks(eunit)
        call gqwkca(wtype,ierr,icat)
        if(icat.eq.1.or.icat.eq.2) then
            call gopwk(wkid,conid,wtype)
        else
            open(unit=junit,file='lpr.dat',status='new')
            call gopwk(wkid,junit,wtype)
        endif
        call gacwk(wkid)
        call gclrwk(wkid,iclear)
c
c       set up window
c
        call gswn(tnr,wxmin,wxmax,wymin,wymax)
        call gsvp(tnr,vxmin,vxmax,vymin,vymax)
        call gselnt(tnr)
        call gswkwn(wkid,vxmin,vxmax,vymin,vymax)
c
c       set up device coordinates
c
        call gqdsp(wtype,ierr,idcunt,rx,ry,lx,ly)
        dd=amin1(rx*2.0e0/3.0e0,ry)
        dxmin=(rx-dd*1.5e0)/2.0e0
        dxmax=(rx+dd*1.5e0)/2.0e0
        dymin=(ry-dd)/2.0e0
        dymax=(ry+dd)/2.0e0
        call gswkvp(wkid,dxmin,dxmax,dymin,dymax)
c
c       set up colors
c
        call gqcf(wtype,ierr,mxcolr,iflag,maxmap)
        nn=min0(ncolor,maxmap)
        do 10 i=1,nn
            index=i-1
            call gscr(wkid,index,red(i),green(i),blue(i))
   10   continue
c
c       line attributes
c
        call gsln(ltype)
        call gsplci(lcolor)
c
c       fill attributes
c
        call gsfais(fstyle)
c
        return
c
c       delete the display and window.
c
   20   call gqwkca(wtype,ierr,icat)
        if(icat.eq.1.or.icat.eq.2) then
c*          call gqli(wtype,ierr,i1,i2,i3,i4,i5,nn)
            call gsstm(wkid,idev,imode,iecho)
            call grqst(wkid,idev,istat,num,strg)
        else
            call guwk(wkid,jflag)
            close(unit=junit)
        endif
c
        call gdawk(wkid)
        call gclwk(wkid)
        call gclks
        close(unit=eunit)
        return
        end
c******************** machine dependent routine *******************
c------------------------------------------------------------------
c
c           piecewise linear triangle multi grid package
c
c                 edition 6.0 - - - february, 1990
c
c------------------------------------------------------------------
        subroutine gline(x,y,n)
c
            real x(*),y(*)
c
c       pline implementation for gks
c
        call gpl(n,x,y)
        return
        end
c******************** machine dependent routine *******************
c------------------------------------------------------------------
c
c           piecewise linear triangle multi grid package
c
c                 edition 6.0 - - - february, 1990
c
c------------------------------------------------------------------
        subroutine gfill(x,y,n,icolor)
c
            common /gks1/iflag,maxmap
            real x(*),y(*)
c
c       pfill implementation for gks
c
        if(iflag.eq.0) then
             index=0
        else
            if(icolor.le.maxmap) then
                index=icolor-1
            else
                index=icolor-1-((icolor-2)/(maxmap-1))*(maxmap-1)
                if(index.eq.1) index=0
            endif
        endif
        call gsfaci(index)
        call gfa(n,x,y)
        return
        end
