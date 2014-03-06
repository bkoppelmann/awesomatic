        subroutine intrp(itri,ivert,vx,vy,rusr,
     1      u,u0,ur,ul,udot,u0dot,rln,nv0,iprob,
     2      a1xy,a2xy,fxy,gxy,p1xy,p2xy)
c
            integer itri(4,*),ivert(4,*),tag,vtag
            integer iv(3),ivf(2),nbr(2),iside(2)
            real vx(*),vy(*),u(*),udot(*),u0(*),u0dot(*),
     1          ur(*),ul(*),rusr(2,*),rln(9,9)
            real fa(6,6),fb(6),fd(6),fp(8)
            external a1xy,a2xy,fxy,gxy,p1xy,p2xy
c
c       compute an udate of u in the parts of the mesh which have
c       been refined
c
c       initialize
c
        nv=itri(4,4)
        if(nv0.ge.nv) return
c
        sh=rln(5,5)
        rl=rln(1,1)
        rldot=rln(3,1)
        if(rldot.eq.0.0e0) rldot=1.0e-6
        dl=0.0e0
        if(iprob.lt.8) dl=rln(5,7)
        call cvert(itri,ivert,vx,vy,rusr)
c
        ifirst=nv
        do 50 i=nv0+1,nv
            call vnbrs(i,ivf,nbr,iside,itri,ivert)
            ivf1=ivf(1)
            ivf2=ivf(2)
c
c       start with linear interpolation
c
            u(i)=(u(ivf1)+u(ivf2))/2.0e0
            ur(i)=(ur(ivf1)+ur(ivf2))/2.0e0
            ul(i)=(ul(ivf1)+ul(ivf2))/2.0e0
            u0(i)=(u0(ivf1)+u0(ivf2))/2.0e0
            udot(i)=(udot(ivf1)+udot(ivf2))/2.0e0
            u0dot(i)=(u0dot(ivf1)+u0dot(ivf2))/2.0e0
c           
c       dirichlet point
c
            if(idbc(i,ivert).eq.1) then
                ur(i)=0.0e0
                ul(i)=0.0e0
                jtag=vtag(i,itri,ivert)
                call eledbc(u(i),vx(i),vy(i),rl,jtag,4,gxy)
                if(iprob.lt.8) then
                    call eledbc(qd,vx(i),vy(i),rl,jtag,5,gxy)
                    udot(i)=qd/rldot
                endif
                go to 50
            endif
c
c       comput integrals
c
            it=nbr(1)
            j=3+iside(1)
            call knots(it,iv,itri,ivert)
            itag=tag(it,itri)
            call eleasm(vx,vy,u,rl,sh,iv,itag,fa,fb,
     1          fd,fp,1,ifirst,a1xy,a2xy,fxy,p1xy)
            ifirst=0
            if(iside(2).gt.0) then
                aa=fa(j,j)
                bb=-fb(j)
                it=nbr(2)
                j=3+iside(2)
                call knots(it,iv,itri,ivert)
                itag=tag(it,itri)
                call eleasm(vx,vy,u,rl,sh,iv,itag,fa,fb,
     1              fd,fp,1,ifirst,a1xy,a2xy,fxy,p1xy)
                aa=aa+fa(j,j)
                bb=bb-fb(j)
           else
                jj=iside(1)
                jtag=vtag(i,itri,ivert)
                call elenbc(vx,vy,u,iv,rl,jtag,jj,fa,fb,fd,1,gxy)
                aa=fa(j,j)
                bb=-fb(j)
            endif
c
c       update u
c
            u(i)=u(i)+bb/aa
   50   continue
        return
        end
