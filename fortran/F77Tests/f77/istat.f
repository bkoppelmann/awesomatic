        integer function istat(i,itri,ivert)
            integer itri(4,*),ivert(4,*)
c
c       istat = 0 keep vertex
c       istat = 1 delete vertex
c
        istat=0
        ii=ivert(3,i)
        if(ii.le.itri(4,5)) return
        ivf1=ivert(1,ii)
        ivf2=ivert(2,ii)
        k1=(iabs(ivf1)/4)*4+3
        if(ivf2.le.0.or.ivf1.lt.0) then
            if(itri(1,k1).eq.1) istat=1
        else
            k2=(ivf2/4)*4+3
            if(itri(1,k1).eq.1.and.itri(1,k2).eq.1) istat=1
        endif
        return
        end
