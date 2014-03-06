        integer function icurv(iedge,i,itri,ivert)
c
            integer itri(4,*),ivert(4,*),iadj(3)
c
c       this routine looks for a curved boundary edge
c       the center is returned in icur, the edge number in iedge
c
        icurv=0
        iedge=0
        if(itri(3,4).le.0) return
        num=itbc(i,iadj,itri,ivert)
        if(num.eq.0) return
        do 10 j=1,3
           iaj=iadj(j)
           if(iaj.eq.0) go to 10
           m=itri(4,iaj)
           if(m.gt.0) go to 20
   10   continue
        return
   20   icurv=m
        iedge=j
        return
        end
