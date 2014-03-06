        subroutine dltri(itri,ivert,itype)
c
            integer itri(4,*),ivert(4,*),getlvl
c
c       restore the triangualtion to its original state
c
        ntold=itri(1,5)
        nvold=itri(2,5)
        imin=ntold
        imax=itri(3,1)-1
c
c       delete green triangles
c
        call ungren(itri)
c
c       reset ivert and son pointers
c
        if(imin.gt.imax) go to 50
        do 30 i=imin,imax,4
            ifath=itri(2,i)
            if(ifath.ge.ntold) go to 30
            do 20 j=1,3
                iv=i+j
                iv=itri(2,iv)
                if(iv.le.0.or.iv.gt.nvold) go to 20
                ivf1=ivert(1,iv)
                ivf2=ivert(2,iv)
                ivert(2,iv)=ifath
                if(ivf2.eq.i+j) then
                    ivert(1,iv)=-ivf1
                else
                    ivert(1,iv)=-ivf2
                endif
   20       continue
            itri(4,ifath)=0
   30   continue
c
   50   itri(4,1)=ntold
        itri(3,1)=ntold
        itri(4,4)=nvold
c
c       reset remaining sons
c
        if(itype.gt.0) return
c
        imin=itri(1,1)
        imax=itri(3,1)-1
        do 60 i=imin,imax
            if(itri(4,i).lt.0) itri(4,i)=0
   60   continue
c
c       remake green triangles
c
        call green(itri,ivert,iflag)
        call cja(itri,ivert,iflag)
c
c       reset levels
c
        i=min0(itri(1,3),itri(4,3))
        itri(1,3)=i
        itri(1,4)=getlvl(i,itri)
        i=min0(itri(2,3),itri(1,3))
        itri(2,3)=i
        itri(2,4)=getlvl(i,itri)
        return
        end
