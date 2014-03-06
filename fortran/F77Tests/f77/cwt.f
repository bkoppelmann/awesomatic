        subroutine cwt(i,iqdsw,iv,itri,ivert)
c
            integer iv(8),itri(4,*),ivert(4,*)
            integer ivf(2),fv(3),mv(3),father,vlev
c
c       interpolation from coarse to fine grid
c
c       iv(1)= pointer to vertex i
c       iv(2-3) = vertex fathers of i
c       iv(4-5) = linear interpolation points
c       iv(6-8) = quadratic interpolation points
c
        call vf(i,ivf,itri,ivert)
        ivf1=max0(ivf(1),ivf(2))
        ivf2=min0(ivf(1),ivf(2))
        k1=ivert(4,ivf1)
        k2=ivert(4,ivf2)
        k=ivert(4,i)
        iqdsw=0
        iv(1)=k
        iv(2)=k1
        iv(3)=k2
        iv(4)=k1
        iv(5)=k2
        iv(6)=k
        iv(7)=k
        iv(8)=k
c
c       see if quadratic interpolation is possible
c
        ilev=vlev(k,itri,ivert)
        if(ilev.le.2) return
        iqdsw=1
        kk=iabs(ivert(1,i))
        kf=father(kk,itri)
        kf=(kf/4)*4
        i2=0
        j1=0
        j2=0
        do 10 j=1,3
            mv(j)=itri(2,kf+j)
            fv(j)=itri(3,kf+j)
            if(fv(j).eq.ivf2) i2=j
            if(mv(j).eq.ivf2) j2=j
            if(mv(j).eq.ivf1) j1=j
   10   continue
        if(i2.eq.0) go to 30
        if(i2.eq.j1) go to 20
c
c       an edge knot
c
        iv(6)=k1
        iv(7)=k2
        kr=fv(6-i2-j1)
        iv(8)=ivert(4,kr)
        return
c
c       the case of an swapped interior knot
c
   20   j2=(5-j1)/2
        j1=6-j2-j1
        k1=mv(j1)
        iv(4)=ivert(4,k1)
        k2=mv(j2)
        iv(5)=ivert(4,k2)
c
c       the case of a normal interior knot
c
   30   km=mv(6-j1-j2)
        iv(6)=ivert(4,km)
        kr=fv(j1)
        iv(7)=ivert(4,kr)
        kl=fv(j2)
        iv(8)=ivert(4,kl)
        return
        end
