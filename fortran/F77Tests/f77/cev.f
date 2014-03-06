        subroutine cev(ip,itri,ivert,ja,a,ul,ur,rln,
     1      x,y,z,bl,br,pr,apr,dur,pl,apl,dul,hist)
c
            integer itri(4,*),ivert(4,*),ja(*),ip(100)
            real a(*),ul(*),ur(*),rln(9,9),x(*),y(*),z(*),bl(*),
     1          br(*),dur(*),pr(*),apr(*),pl(*),dul(*),apl(*),
     2          hist(22,12)
c
c       compute approximate left and right singular vectors
c
        nv=itri(1,4)
        ispd=itri(4,8)
        jspd=1
        if(ispd.ne.1) jspd=-1
        eps=1.0e-4
        itmax=4
        if(ip(7).ge.8) itmax=2
c
c       compute norms, dot product
c
        cc=1.0e0
        uln=0.0e0
        urn=0.0e0
        dp=0.0e0
        do 15 i=1,nv
            dur(i)=0.0e0
            dul(i)=0.0e0
            if(idbc(i,ivert).eq.1) then
                ur(i)=0.0e0
                ul(i)=0.0e0
            endif
   10       uln=uln+ul(i)*ul(i)
            urn=urn+ur(i)*ur(i)
   15       dp=dp+ul(i)*ur(i)
c
c       fix things up if current vectors are null
c
        if(urn.gt.0.0e0.and.uln.gt.0.0e0) go to 35
        uln=0.0e0
        do 25 i=1,nv
            if(idbc(i,ivert).eq.1) then
                ur(i)=0.0e0
                ul(i)=0.0e0
            else
                ur(i)=1.0e0
                ul(i)=1.0e0
                uln=uln+1.0e0
            end if
   25   continue
        urn=uln
        dp=1.0e0
        if(uln.le.0.0e0) go to 120
c
c       normalize initial vectors
c
   35   urn=sqrt(urn)
        uln=sqrt(uln)
        if(dp.lt.0.0e0) uln=-uln
        do 40 i=1,nv
            q=ur(i)/urn
            ul(i)=ul(i)/uln
   40       ur(i)=q
        if(ispd.ne.1) then
            itri(4,8)=jspd
            call mtxmlt(ja,a,ul,apl,x,y,itri,ivert)
            itri(4,8)=ispd
        end if
        call mtxmlt(ja,a,ur,apr,x,y,itri,ivert)
c
c       inverse iteration loop
c
        hist(21,10)=0.0e0
        hist(22,10)=1.0e0
        do 100 iter=1,itmax
c
c       a ur = cc * ul
c       a(transpose)  ul = cc * ur
c
            cl=0.0e0
            cr=0.0e0
            do 55 i=1,nv
                cl=cl+ur(i)*apl(i)
   55           cr=cr+ul(i)*apr(i)
            do 60 i=1,nv
                bl(i)=cl*ur(i)-apl(i)
   60           br(i)=cr*ul(i)-apr(i)
c
            call mg(ip,itri,ivert,ja,a,dur,br,dul,bl,pr,
     1          apr,pl,apl,x,y,z,hist(1,9),1,iflag)
c
            if(ispd.ne.1) then
                itri(4,8)=jspd
                call csv(itri,ivert,ja,a,ul,apl,dul,br,
     1              cl,dl,x,y)
                itri(4,8)=ispd
            else
                dl=0.0e0
            end if
            call csv(itri,ivert,ja,a,ur,apr,dur,br,
     1          cr,dr,x,y)
c
c        convergence history
c
            if(iter.gt.20) then
                do 125 i=1,19
  125               hist(i,10)=hist(i+1,10)
                hist(20,10)=amax1(dl,dr)
            else
                hist(iter,10)=amax1(dl,dr)
            endif
            hist(21,10)=float(iter)
c
            if(amax1(dl,dr).lt.eps) go to 101
  100   continue
c
c       final computation of singular value
c       sign determined such that ul * ur is positive
c
  101   itri(4,8)=ispd
        call mtxmlt(ja,a,ur,z,x,y,itri,ivert)
        cc=0.0e0
        dp=0.0e0
        do 105 i=1,nv
            dp=dp+ur(i)*ul(i)
  105       cc=cc+ul(i)*z(i)
        if(dp.ge.0.0e0) go to 120
        cc=-cc
        do 110 i=1,nv
  110       ul(i)=-ul(i)
  120   rln(7,1)=cc
        return
        end
