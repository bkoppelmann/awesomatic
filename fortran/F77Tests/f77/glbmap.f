        subroutine glbmap(ja,a,itri,ivert)
c
            integer itri(4,*),ivert(4,*),ja(*),vlev
            real a(*)
c
c       form course grid matrix elements from fine grid
c
        nv=itri(1,4)
        nn=itri(2,4)+1
        ispd=itri(4,8)
        ishift=0
        if(ispd.ne.1) ishift=ja(nv+1)-ja(1)
        qspd=1.0e0
        if(ispd.eq.1) qspd=1.0e0/2.0e0
        if(nn.ge.nv) go to 70
        do 60 ii=nn,nv
            i=nv+nn-ii
            jstrt=ja(i)+2
            jstop=ja(i+1)-1
c
c       first fixup vertex fathers
c
            ivf1=ja(jstrt-1)
            ivf2=ja(jstrt-2)
            call elemap(ivf1,ivf2,k12,k21,ja,ishift)
            dii=a(i)/4.0e0
            d1i=a(jstrt-1)/2.0e0
            d2i=a(jstrt-2)/2.0e0
            di1=a(jstrt-1+ishift)/2.0e0
            di2=a(jstrt-2+ishift)/2.0e0
            a(ivf1)=a(ivf1)+dii+d1i+di1
            a(ivf2)=a(ivf2)+dii+d2i+di2
            a(k12)=a(k12)+qspd*(dii+di2+d1i)
            a(k21)=a(k21)+qspd*(dii+d2i+di1)
c
c       now  process the remaining  elements
c
            ilev=vlev(i,itri,ivert)
            do 50 jj=jstrt,jstop
                j=ja(jj)
                jlev=vlev(j,itri,ivert)
                if(ilev.eq.jlev) go to 40
c
c       a green edge
c
                dji=qspd*a(jj)/2.0e0
                dij=qspd*a(jj+ishift)/2.0e0
                call elemap(ivf1,j,k1j,kj1,ja,ishift)
                a(k1j)=a(k1j)+dij
                a(kj1)=a(kj1)+dji
                call elemap(ivf2,j,k2j,kj2,ja,ishift)
                a(k2j)=a(k2j)+dij
                a(kj2)=a(kj2)+dji
                go to 50
c
c       a normal edge
c
   40           m=ja(j)
                icom=ivf2
                if(ja(m).eq.ivf1.or.ja(m+1).eq.ivf1) icom=ivf1
                ivi=ivf1+ivf2-icom
                ivj=ja(m)+ja(m+1)-icom
                dji=a(jj)/4.0e0
                dij=a(jj+ishift)/4.0e0
                a(icom)=a(icom)+dji+dij
                dij=qspd*dij
                dji=qspd*dji
                call elemap(ivi,ivj,kij,kji,ja,ishift)
                a(kij)=a(kij)+dij
                a(kji)=a(kji)+dji
                call elemap(icom,ivj,kcj,kjc,ja,ishift)
                a(kcj)=a(kcj)+dij
                a(kjc)=a(kjc)+dji
                call elemap(ivi,icom,kic,kci,ja,ishift)
                a(kic)=a(kic)+dij
                a(kci)=a(kci)+dji
   50       continue
   60   continue
c
c       dirichlet boundary conditions for matrix
c
   70   do 90 i=1,nv
            j1=ja(i)
            j2=ja(i+1)-1
            if(j1.gt.j2) go to 90
            if(idbc(i,ivert).eq.0) go to 80
            do 75 jj=j1,j2
                js=jj+ishift
                a(js)=0.0e0
                a(jj)=0.0e0
   75       continue
            go to 90
   80       do 85 jj=j1,j2
                j=ja(jj)
                if(idbc(j,ivert).eq.0) go to 85
                js=jj+ishift
                a(js)=0.0e0
                a(jj)=0.0e0
   85       continue
   90   continue
c
        return
        end
