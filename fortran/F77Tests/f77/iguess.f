        subroutine iguess(itri,ivert,rusr,u,u0,
     1      udot,u0dot,ur,ul,z,rln,rl0,gxy)
c
            integer itri(4,*),ivert(4,*),iv(3),vtag,tag
            real u(*),udot(*),u0(*),u0dot(*),ur(*),ul(*),z(*),
     1          rusr(2,*),rln(9,9)
            real x(3),y(3)
            external gxy
c
c       initialize
c
        nv=itri(4,4)
        lvl=itri(1,3)
        n1=itri(2,4)
        level1=itri(2,3)
        itri(1,3)=level1
        ivx=nv+1
        ivy=ivx+nv
c
c       initial guess, dirichlet boundary conditions
c
        do 10 i=1,9
        do 10 j=1,9
   10       rln(i,j)=0.0e0
        rln(1,3)=rl0
        rln(3,3)=1.0e0
        rln(1,6)=rl0
        rln(6,5)=1.0e0
        rln(4,4)=ceps(ibit)
c
c       compute initial guess
c
        call cvert(itri,ivert,z(ivx),z(ivy),rusr)
        do 20 i=1,n1
            udot(i)=0.0e0
            u0dot(i)=0.0e0
            ur(i)=0.0e0
            ul(i)=0.0e0
            u(i)=0.0e0
   20       z(i)=0.0e0
c
c       the main loop
c
        i1=itri(1,1)
        i2=itri(4,1)-1
        do 40 i=i1,i2
            if(istl(i,itri).eq.0) go to 40
            call knots(i,iv,itri,ivert)
            itag=tag(i,itri)
            do 25 j=1,3
                ivj=iv(j)
                x(j)=z(ivx+ivj-1)
   25           y(j)=z(ivy+ivj-1)
            do 30 j=1,3
               j2=(5-j)/2
               j3=6-j-j2
               ivj=iv(j)
c
c       evaluate function
c
                call eledbc(qq,x(j),y(j),rl0,itag,6,gxy)
c
c       compute angle at vertex ivj
c
                x2=x(j2)-x(j)
                y2=y(j2)-y(j)
                x3=x(j3)-x(j)
                y3=y(j3)-y(j)
                h2=sqrt((x2*x2+y2*y2)*(x3*x3+y3*y3))
                aa=(x2*x3+y2*y3)/h2
                aa=amin1(1.0e0,aa)
                aa=amax1(-1.0e0,aa)
                aa=acos(aa)
c
c       update z, u
c
                z(ivj)=z(ivj)+aa
                u(ivj)=u(ivj)+aa*qq
   30         continue
   40      continue
        itri(1,3)=lvl
c
c       dirichlet boundary conditions
c
        do 50 i=1,n1
            u(i)=u(i)/z(i)
            if(idbc(i,ivert).eq.0) go to 50
            jx=ivx+i-1
            jy=ivy+i-1
            itag=vtag(i,itri,ivert)
            call eledbc(u(i),z(jx),z(jy),rl0,itag,4,gxy)
   50       u0(i)=u(i)
c
        return
        end
