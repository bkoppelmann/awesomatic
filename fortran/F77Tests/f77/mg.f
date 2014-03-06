        subroutine mg(ip,itri,ivert,ja,a,dr,br,dl,bl,
     1      pr,apr,pl,apl,x,y,z,hist,isw,iflag)
c
            integer itri(4,*),ivert(4,*),ja(*),ip(100)
            real a(*),dr(*),br(*),pr(*),apr(*),x(*),y(*),z(*),
     1          bl(*),dl(*),pl(*),apl(*),omega(100),hist(22)
c
c       initialize
c
        call comega(ja,a,itri,ivert,z,omega)
        itmax=ip(12)
        nv=itri(1,4)
        ispd=itri(4,8)
c
        iflag=0
        eps=1.0e-2
        epsmin=0.5e0
        relerr=0.0e0
        anorm=a(nv+1)
c
c       compute initial norm of b
c
        brnorm=0.0e0
        do 20 i=1,nv
            pr(i)=0.0e0
            apr(i)=0.0e0
            if(idbc(i,ivert).eq.1) then
                dr(i)=br(i)
                br(i)=0.0e0
            else
                dr(i)=0.0e0
                brnorm=brnorm+(br(i)/anorm)**2
            end if
   20   continue
        brnorm=sqrt(brnorm)*anorm
        hist(22)=brnorm
        hist(21)=0.0e0
        rrnorm=brnorm
        itnum=0
        if(brnorm.le.0.0e0) return
c
        if(ispd.ne.1) go to 400
c
c       conjugate gradient iteration
c
        thresh=0.5e0
        call vcycle(ja,a,pr,br,x,y,itri,ivert,omega)
c
        do 250 itnum=1,itmax
c
c       matrix multiply
c
            call mtxmlt(ja,a,pr,apr,x,y,itri,ivert)
c
c       compute alpha
c
            pap=0.0e0
            bp=0.0e0
            do 110 i=1,nv
                pap=pap+pr(i)*(apr(i)/anorm)
  110           bp=bp+pr(i)*(br(i)/anorm)
            if(abs(pap).lt.abs(bp)*thresh) go to 150
c
c       the case of a 1 x 1 pivot
c       update residual and solution
c
            rrnorm=0.0e0
            alpha=bp/pap
            do 120 i=1,nv
                dr(i)=dr(i)+alpha*pr(i)
                br(i)=br(i)-alpha*apr(i)
  120           rrnorm=rrnorm+(br(i)/anorm)**2
c
c       convergence test
c
            rrnorm=sqrt(rrnorm)*anorm
            if(itnum.gt.20) then
                do 125 i=1,19
  125               hist(i)=hist(i+1)
                hist(20)=rrnorm
            else
                hist(itnum)=rrnorm
            endif
            hist(21)=float(itnum)
            relerr=rrnorm/brnorm
            if(relerr.le.eps) return
c
c       compute new direction
c
            call vcycle(ja,a,z,br,x,y,itri,ivert,omega)
            bap=0.0e0
            do 130 i=1,nv
  130           bap=bap+z(i)*(apr(i)/anorm)
            beta=-bap/pap
            do 140 i=1,nv
  140           pr(i)=z(i)+beta*pr(i)
            go to 250
c
c       the case of a 2 x 2 pivot
c       compute the next 'psuedo residual' and precondition
c
  150       alpha=pap/bp
            do 160 i=1,nv
  160           bl(i)=apr(i)-alpha*br(i)
            call vcycle(ja,a,z,bl,x,y,itri,ivert,omega)
            call mtxmlt(ja,a,z,bl,x,y,itri,ivert)
c
c       compute alphas
c
            zaz=0.0e0
            zap=0.0e0
            bz=0.0e0
            do 170 i=1,nv
                zaz=zaz+z(i)*(bl(i)/anorm)
                zap=zap+z(i)*(apr(i)/anorm)
  170           bz=bz+z(i)*(br(i)/anorm)
            zz=amax1(abs(pap),abs(zaz))
            zz=amax1(zz,abs(zap))
            pap=pap/zz
            zap=zap/zz
            zaz=zaz/zz
            det=(pap*zaz-zap*zap)*zz
            alphap=(bp*zaz-bz*zap)/det
            alphaz=(pap*bz-zap*bp)/det
c
c       update residual and solution
c
            rrnorm=0.0e0
            do 180 i=1,nv
                dr(i)=dr(i)+alphap*pr(i)+alphaz*z(i)
                br(i)=br(i)-alphap*apr(i)-alphaz*bl(i)
  180           rrnorm=rrnorm+(br(i)/anorm)**2
c
c       convergence test
c
            rrnorm=sqrt(rrnorm)*anorm
            if(itnum.gt.20) then
                do 185 i=1,19
  185               hist(i)=hist(i+1)
                hist(20)=rrnorm
            else
                hist(itnum)=rrnorm
            endif
            hist(21)=float(itnum)
            relerr=rrnorm/brnorm
            if(relerr.le.eps) return
c
c       compute next direction
c
            call vcycle(ja,a,bl,br,x,y,itri,ivert,omega)
            call mtxmlt(ja,a,bl,apr,x,y,itri,ivert)
            bap=0.0e0
            baz=0.0e0
            do 190 i=1,nv
                bap=bap+pr(i)*(apr(i)/anorm)
  190           baz=baz+z(i)*(apr(i)/anorm)
            betap=-(bap*zaz-baz*zap)/det
            betaz=-(pap*baz-zap*bap)/det
            do 200 i=1,nv
  200           pr(i)=bl(i)+betap*pr(i)+betaz*z(i)
  250   continue
        if(relerr.gt.epsmin) iflag=12
        itnum=itmax
        return
c
c       biconjugate gradient method for nonsymmetric problems
c
  400   jspd=-(1+ispd)
        if(isw.eq.0) call vcycle(ja,a,bl,br,x,y,itri,ivert,omega)
        blnorm=0.0e0
        do 410 i=1,nv
            pl(i)=0.0e0
            apl(i)=0.0e0
            if(idbc(i,ivert).eq.1) then
                dl(i)=bl(i)
                bl(i)=0.0e0
            else
                dl(i)=0.0e0
                blnorm=blnorm+(bl(i)/anorm)**2
            end if
  410   continue
        blnorm=sqrt(blnorm)*anorm
        if(blnorm.eq.0.0e0) return
        brbl=0.0e0
c
        do 500 itnum=1,itmax
c
c       compute beta
c
            call vcycle(ja,a,z,br,x,y,itri,ivert,omega)
            sum=0.0e0
            do 420 i=1,nv
  420           sum=sum+bl(i)*z(i)
            if(brbl.ne.0.0e0) then
                 beta=sum/brbl
            else
                 beta=0.0e0
            endif
            brbl=sum
c
c       update pr and pl
c
            do 430 i=1,nv
  430          pr(i)=z(i)+beta*pr(i)
c
            itri(4,8)=jspd
            call vcycle(ja,a,z,bl,x,y,itri,ivert,omega)
            do 435 i=1,nv
  435          pl(i)=z(i)+beta*pl(i)
c
c       matrix multiplies
c
            call mtxmlt(ja,a,pl,apl,x,y,itri,ivert)
            itri(4,8)=ispd
            call mtxmlt(ja,a,pr,apr,x,y,itri,ivert)
c
c       compute alpha
c
            sum=0.0e0
            do 440 i=1,nv
  440           sum=sum+pr(i)*apl(i)
            if(sum.eq.0.0e0) return
            alpha=brbl/sum
            rrnorm=0.0e0
            rlnorm=0.0e0
            do 450 i=1,nv
                dr(i)=dr(i)+alpha*pr(i)
                dl(i)=dl(i)+alpha*pl(i)
                br(i)=br(i)-alpha*apr(i)
                bl(i)=bl(i)-alpha*apl(i)
                rrnorm=rrnorm+(br(i)/anorm)**2
  450           rlnorm=rlnorm+(bl(i)/anorm)**2
c
c       convergence test
c
            rrnorm=sqrt(rrnorm)*anorm
            if(itnum.gt.20) then
                do 455 i=1,19
  455               hist(i)=hist(i+1)
                hist(20)=rrnorm
            else
                hist(itnum)=rrnorm
            endif
            hist(21)=float(itnum)
            rlnorm=sqrt(rlnorm)*anorm
            reler0=rrnorm/brnorm
            reler1=rlnorm/blnorm
            relerr=amax1(reler0,reler1)
            if(relerr.le.eps) return
  500   continue
        if(relerr.gt.epsmin) iflag=12
        itnum=itmax
        return
c
        end
