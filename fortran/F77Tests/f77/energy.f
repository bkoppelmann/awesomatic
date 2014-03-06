        subroutine energy(itri,ivert,vx,vy,rusr,u,udot,e,
     1      rln,iprob,nt0,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
c
            integer itri(4,*),ivert(4,*),son,tag
            integer iv(3),kv(3),iadj(3),mark(6)
            real vx(*),vy(*),u(*),udot(*),e(*),rusr(2,*),rln(9,9)
            real a(6,6),b(6),d(6),p(8),aa(3,3),bb(3,3)
            external a1xy,a2xy,fxy,gxy,p1xy,p2xy
c
c       compute error estimates for each element
c
        rl=rln(1,1)
        rldot=rln(3,1)
        if(iprob.ge.8) rldot=1.0e0
        if(rldot.eq.0.0e0) rldot=1.0e-6
        sh=0.0e0
        nv=itri(4,4)
        ifirst=nv
        im=itri(1,1)
        i1=itri(1,5)
        i2=itri(4,1)-1
        i3=itri(3,1)-1
        if(i1.ne.im) i2=i3
c
        dl=0.0e0
        if(iprob.lt.8) dl=rln(5,7)
c
        call cvert(itri,ivert,vx,vy,rusr)
c
c       the main loop
c
        enorm1=0.0e0
        unorm1=0.0e0
        enorm2=0.0e0
        unorm2=0.0e0
        efun=0.0e0
        fun=0.0e0
        do 90 i=i2,i1,-1
            ift=0
            e(i)=0.0e0
            if(istl(i,itri).eq.1) go to 40
            ift=1
            ison=son(i,itri)
            if(ison.gt.i3) go to 40
            do 20 j=0,3
   20           e(i)=e(i)+e(ison+j)
            go to 90
c
c        initialize for triangle i
c
   40       call knots(i,iv,itri,ivert)
            call ledges(i,iv,iadj,itri,ivert)
            itag=tag(i,itri)
            call grad(gx,gy,vx,vy,u,iv)
c
c       compute element stiffness matrix and right hand side
c
            call eleasm(vx,vy,u,rl,sh,iv,itag,a,b,
     1          d,p,1,ifirst,a1xy,a2xy,fxy,p1xy)
            ifirst=0
            do 70 j=1,3
                jp=j+3
                mark(jp)=0
                k=iadj(j)
                if(k.lt.0) go to 50
c
c       jump along internal edge
c
                call knots(k,kv,itri,ivert)
                ktag=tag(k,itri)
                call grad(zx,zy,vx,vy,u,kv)
                call eleun(vx,vy,u,iv,rl,gx,gy,itag,
     1              zx,zy,ktag,j,b,a1xy,a2xy)
                go to 70
c
c       a neumann boundary edge
c
   50           iedge=-k
                call cbound(iedge,ity,icen,k2,k3,jtag,itri)
                call elebdi(vx,vy,u,rl,iv,jtag,j,p,1,p2xy)
                if(ity.lt.0) go to 60
                call elenbc(vx,vy,u,iv,rl,jtag,j,a,b,d,1,gxy)
                go to 70
c
c       a dirichlet boundary edge
c
   60           call cmid(xm,ym,um,vx,vy,u,rusr,iv,j,icen)
                call cmid(xm,ym,gm,vx,vy,udot,rusr,iv,j,icen)
                call eledbc(qj,xm,ym,rl,jtag,4,gxy)
                call eledbc(sj,xm,ym,rl,jtag,5,gxy)
                b(jp)=um-qj
                d(jp)=gm/rldot-sj
                mark(jp)=1
   70       continue
c
c       solve 3 x 3 system
c
            do 80 j=1,3
                jp=j+3
                bb(j,1)=-(b(jp)+dl*d(jp))
                aa(j,1)=a(jp,4)
                aa(j,2)=a(jp,5)
                aa(j,3)=a(jp,6)
                if(mark(jp).eq.0) go to 80
                aa(j,1)=0.0e0
                aa(j,2)=0.0e0
                aa(j,3)=0.0e0
                aa(j,j)=1.0e0
   80       continue
            call c3x3(aa,bb,1)
c
c       compute errors
c
            iv1=iv(1)
            iv2=iv(2)
            iv3=iv(3)
            x2=vx(iv2)-vx(iv1)
            y2=vy(iv2)-vy(iv1)
            x3=vx(iv3)-vx(iv1)
            y3=vy(iv3)-vy(iv1)
            det=abs(x2*y3-x3*y2)
c
            r1=bb(1,1)
            r2=bb(2,1)
            r3=bb(3,1)
            dr1=r2-r3
            dr2=r3-r1
            dr3=r1-r2
c
c       l2 norm
c
            r4=r1+r2+r3
            rr=r1*r1+r2*r2+r3*r3
            e2=(r4*r4+rr)*det*2.0e0/45.0e0
            q1=(u(iv2)+u(iv3))/2.0e0
            q2=(u(iv3)+u(iv1))/2.0e0
            q3=(u(iv1)+u(iv2))/2.0e0
            u2=(q1*q1+q2*q2+q3*q3)*det/6.0e0
c
c       h1 norm
c
            d1=(x2*x3+y2*y3)/(2.0e0*det)
            d2=((x2-x3)*x2+(y2-y3)*y2)/(2.0e0*det)
            d3=((x3-x2)*x3+(y3-y2)*y3)/(2.0e0*det)
            e1=e2+(d1*(dr1*dr1+r1*r1)+d2*(dr2*dr2+r2*r2)
     1            +d3*(dr3*dr3+r3*r3))*8.0e0/3.0e0
            u1=u2+(gx*gx+gy*gy)*det/2.0e0
c
            e(i)=e1
            if(ift.ne.0) go to 90
            enorm1=enorm1+e1
            unorm1=unorm1+u1
            enorm2=enorm2+e2
            unorm2=unorm2+u2
            efun=efun+r1*p(4)+r2*p(5)+r3*p(6)+dl*p(7)
            fun=fun+p(8)
   90   continue
        if(im.ne.i1) return
        rln(1,7)=sqrt(enorm1)
        rln(2,7)=sqrt(unorm1)
        rln(3,7)=sqrt(enorm2)
        rln(4,7)=sqrt(unorm2)
        rln(1,8)=efun
        rln(2,8)=fun
        return
        end
