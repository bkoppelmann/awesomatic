        subroutine linsys(itri,ivert,vx,vy,rusr,b,d,p,ja,a,
     1      u,u0,u0dot,rln,del,usv,gm,a1xy,a2xy,fxy,gxy,p1xy,p2xy)
c
            integer itri(4,*),ivert(4,*),ja(*),tag,vtag
            integer iv(3),iadj(3)
            real vx(*),vy(*),u(*),u0(*),u0dot(*),usv(*),del(*),
     1          a(*),b(*),d(*),p(*),rusr(2,*),rln(9,9),gm(*)
            real fa(6,6),fb(6),fd(6),fp(8)
            external a1xy,a2xy,fxy,gxy,p1xy,p2xy
c
c       compute stiffness matrix, right hand side, and
c       the derivative of the rhs with respect to lamda
c
c       initialize
c
        nv=itri(1,4)
        ispd=itri(4,8)
        i1=itri(1,1)
        i2=itri(4,1)-1
c
        step=rln(5,4)
        sh=rln(5,5)
        rl0dot=rln(3,2)
        rl0=rln(1,2)
        r0=rln(2,2)
        r0dot=rln(4,2)
        theta=rln(3,6)
        theta2=2.0e0-theta
        ds=rln(4,6)
        rlcur=rln(1,1)
        delta=rln(3,5)
        iarc=iarcln(rln)
c
        ishift=0
        if(ispd.ne.1) ishift=ja(nv+1)-ja(1)
        qspd=1.0e0
        if(ispd.eq.1) qspd=1.0e0/2.0e0
c
c       initialize
c
        j=ja(nv+1)-1+ishift
        do 10 i=1,j
   10       a(i) = 0.0e0
        rl=rlcur+step*delta
        do 15 i=1,nv
            u(i)=usv(i)+step*del(i)
            gm(i)=0.0e0
            b(i)=0.0e0
            p(i)=0.0e0
            d(i)=0.0e0
   15   continue
        call cvert(itri,ivert,vx,vy,rusr)
        unorm=0.0e0
        enorm=0.0e0
        udu=0.0e0
        dpdl=0.0e0
        rr=0.0e0
        ifirst=nv
c
c       the main loop
c
        do 45 i=i1,i2
            if(istl(i,itri).eq.0) go to 45
            call knots(i,iv,itri,ivert)
            itag=tag(i,itri)
c
c       compute norms
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
c       compute integrals
c
            call eleasm(vx,vy,u,rl,sh,iv,itag,fa,fb,
     1          fd,fp,0,ifirst,a1xy,a2xy,fxy,p1xy)
            ifirst=0
c
c       check for boundary edges
c
            num=itbc(i,iadj,itri,ivert)
            if(num.eq.0) go to 25
            do 20 j=1,3
                if(iadj(j).eq.0) go to 20
                call cbound(iadj(j),ity,k1,k2,k3,jtag,itri)
                call elebdi(vx,vy,u,rl,iv,jtag,j,fp,0,p2xy)
                if(ity.lt.0) go to 20
                call elenbc(vx,vy,u,iv,rl,jtag,j,fa,fb,fd,0,gxy)
   20           continue
c
c       add elemental quantities to global quantities
c
   25       det=det/24.0e0
            uu=u(iv1)+u(iv2)+u(iv3)
            dd=del(iv1)+del(iv2)+del(iv3)
            ud=u0dot(iv1)+u0dot(iv2)+u0dot(iv3)
            dpdl=dpdl+fp(7)
            rr=rr+fp(8)
            do 40 k=1,3
                ivk=iv(k)
                qq=det*(ud+u0dot(ivk))
                udu=udu+qq*(u(ivk)-u0(ivk))
                unorm=unorm+det*(uu+u(ivk))*u(ivk)
                enorm=enorm+det*(dd+del(ivk))*del(ivk)
c
                gm(ivk)=gm(ivk)+det
                b(ivk)=b(ivk)-fb(k)
                p(ivk)=p(ivk)+fp(k)
                d(ivk)=d(ivk)-fd(k)
                a(ivk)=a(ivk)+fa(k,k)
                j=(5-k)/2
                l=6-j-k
                ivj=iv(j)
                ivl=iv(l)
                call elemap(ivl,ivj,lj,jl,ja,ishift)
                a(lj)=a(lj)+qspd*fa(l,j)
                a(jl)=a(jl)+qspd*fa(j,l)
   40       continue
   45   continue
c
c       scalar function
c
        ss=theta*r0dot*(rr-r0)+theta2*rl0dot*(rl-rl0)-ds
        if(iarc.eq.1) ss=theta*udu+theta2*rl0dot*(rl-rl0)-ds
        rln(1,6)=rl
        rln(2,6)=rr
        rln(4,5)=ss
        rln(3,4)=dpdl
c
c       norm of residual and diagonal of matrix
c
        anorm=0.0e0
        bmax=abs(ss)
        do 65 i=1,nv
            if(idbc(i,ivert).eq.0) go to 60
            itag=vtag(i,itri,ivert)
            call eledbc(q,vx(i),vy(i),rl,itag,5,gxy)
            d(i)=q
            call eledbc(q,vx(i),vy(i),rl,itag,4,gxy)
            b(i)=q-u(i)
   60       bmax=amax1(abs(b(i)),bmax)
            anorm=amax1(abs(a(i)),anorm)
   65   continue
        if(anorm.le.0.0e0) anorm=1.0e0
        if(bmax.le.0.0e0) bmax=1.0e0
        q=ss*rln(6,5)/bmax
        bnorm=q*q
        gl=0.0e0
        do 70 i=1,nv
            if(a(i).eq.0.0e0) a(i)=anorm
            q=b(i)/bmax
            bnorm=bnorm+q*q/gm(i)
            gl=gl+q*d(i)/gm(i)
   70   continue
        bnorm=sqrt(bnorm)*bmax
        rln(6,4)=bnorm
        a(nv+1)=anorm
        rln(6,6)=anorm
        rln(8,5)=bmax*gl
c
c       fix up right hand side for dirichlet boundary conditions
c
        do 90 i=1,nv
            j1=ja(i)
            j2=ja(i+1)-1
            if(j1.gt.j2) go to 90
            if(idbc(i,ivert).eq.0) go to 80
            do 75 jj=j1,j2
                j=ja(jj)
                if(idbc(j,ivert).eq.1) go to 75
                d(j)=d(j)-a(jj)*d(i)
                b(j)=b(j)-a(jj)*b(i)
   75       continue
            go to 90
   80       do 85 jj=j1,j2
                j=ja(jj)
                if(idbc(j,ivert).eq.0) go to 85
                js=jj+ishift
                d(i)=d(i)-a(js)*d(j)
                b(i)=b(i)-a(js)*b(j)
   85       continue
   90   continue
c
c       relative change in u in l2 norm
c
        unorm=unorm+rl*rl
        enorm=enorm+delta*delta
        rln(7,4)=1.0e0
        if(unorm.ne.0.0e0) rln(7,4)=sqrt(enorm/unorm)
        if(unorm+enorm.le.0.0e0) rln(7,4)=0.0e0
        return
        end
