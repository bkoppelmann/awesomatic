        subroutine bisect(rln,isw,bisc)
c
            real rln(9,9),bisc(12)
c
c       this routine carries out a bisection
c       or secant iteration
c
c       isw = 0   initialize
c           > 0   update
c           < 0   converged
c
c       bisc( 1) = tol
c       bisc( 2) = ds(original)
c       bisc( 3) = ds(upper)
c       bisc( 4) = ds(lower)
c       bisc( 5) = ds(new)
c       bisc( 6) = ds(old)
c       bisc( 7) = l-dot(upper)
c       bisc( 8) = l-dot(lower)
c       bisc( 9) = l-dot(new)
c       bisc(10) = l-dot(old)
c       bisc(11) = rq(new)
c       bisc(12) = rq(old)
c
        tol=bisc(1)
        if(isw.ne.0) go to 10
        bisc(2)=rln(4,6)
        bisc(3)=rln(4,6)
        bisc(4)=0.0e0
        bisc(5)=rln(4,6)
        bisc(6)=0.0e0
        bisc(7)=rln(3,1)
        bisc(8)=rln(3,2)
        bisc(9)=rln(3,1)
        bisc(10)=rln(3,2)
        bisc(11)=rln(7,1)
        bisc(12)=rln(7,2)
        isw=1
        go to 20
   10   k=1
        if(rln(5,1)*rln(5,2).lt.0.0e0) k=0
        bisc(3+k)=rln(4,6)
        bisc(6)=bisc(5)
        bisc(5)=rln(4,6)
c*      bisc(7+k)=rln(3,1)
        bisc(10)=bisc(9)
        bisc(9)=rln(3,1)
        bisc(12)=bisc(11)
        bisc(11)=rln(7,1)
   20   dsup=bisc(3)
        dslow=bisc(4)
        dsnew=bisc(5)
        dsold=bisc(6)
        ds=dslow+(dsup-dslow)/2.0e0
        k=0
        if(bisc(7)*bisc(8).gt.0.0e0) k=2
        fnew=bisc(9+k)
        fold=bisc(10+k)
        al=abs(dsup-dslow)
   50   if(fnew-fold.eq.0.0e0) go to 55
        qq=dsnew-fnew*(dsnew-dsold)/(fnew-fold)
        dism=amax1(abs(dslow-qq),abs(dsup-qq))
        d=(al/bisc(2))*(al/bisc(2))
        if(dism.gt.al*(1.0e0-d/4.0e0)) 
     1      qq=qq+(ds-qq)*amax1(d/4.0e0,10.0e0*tol)
        dism=amax1(abs(dslow-qq),abs(dsup-qq))
        if(dism.lt.al*(1.0e0-tol)) ds=qq
   55   if(ds.eq.dsnew.or.al.lt.tol*abs(ds)) go to 80
        rln(4,6)=ds
        return
   80   isw=-1
        return
        end
