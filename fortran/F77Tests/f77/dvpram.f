        subroutine dvpram(hl,hr,d,pram,al,h,np)
c
            real pram(7)
c
c       this routine determines the number of points and the
c       spacing parameters for dividing up a line segment
c
        epsm=1.0e0-pram(1)
        grade=pram(5)
        np=0
        al=1.0e0
        h=1.0e0
        hmax=amax1(hl,hr)/d
        if(hmax.ge.epsm) return
        hmin=amin1(hl,hr)/d
        if(hmin*grade.ge.epsm) return
c
c       find np by increasing hmin as quickly as possible
c
        q=hmin
    3   np=np+1
        hmin=amin1(hmin*grade,hmax)
        q=q+hmin
        if(q.lt.epsm) go to 3
        if(q.gt.1.0e0+hmax/2.0e0) np=np-1
c
c       hr=hl*al**(np+1) and h*(1-al**(np+1))/(1-al)=1
c       are the two equations that determine al and h
c
        if(np.eq.0) return
        r=hr/hl
        if(abs(r-1.0e0).lt.1.e-3) go to 5
        al=r**(1.0e0/float(np+1))
        al=amin1(grade,al)
        al=amax1(1.0e0/grade,al)
        h=(al-1.0e0)/(r-1.0e0)
        return
    5   h=1.0e0/float(np+1)
        return
        end
