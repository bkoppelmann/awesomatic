        subroutine ledges(i,iv,iadj,itri,ivert)
c
            integer iv(3),iadj(3),itri(4,*),ivert(4,*)
            integer jv(3),jadj(3),father,son
c
c       find edges in current level
c
        iadj(1)=0
        iadj(2)=0
        iadj(3)=0
        if(istl(i,itri).eq.0) go to 70
        if(i.lt.itri(3,1)) go to 30
c
c       green triangles
c
        iv3=iv(3)
        ifath=father(i,itri)
        call edges(ifath,jadj,itri,ivert)
        call knots(ifath,jv,itri,ivert)
c
c       sort knots
c
        ic=(i/2)*2
        j1=itri(3,ic)
        j3=(5-j1)/2
        if(jv(j3).ne.iv3) j3=6-j1-j3
        j2=6-j1-j3
c
c       set iadj using jadj
c
        iadj(3)=ic
        if(ic.eq.i) iadj(3)=i+1
        iadj(2)=jadj(j2)
c
        neigh=jadj(j1)
        nty=ielty(neigh,itri)
        nson=son(neigh,itri)
        call knots(neigh,jv,itri,ivert)
        do 10 k=1,3
            if(iv3.eq.jv(k)) go to 20
   10   continue
   20   iadj(1)=nson+k
        if(nty.ne.k) go to 40
        k2=iv(2)
        k2=ivert(3,k2)
        k2=iabs(ivert(1,k2))-nson
        kk=k-1
        if(kk.eq.0) kk=3
        if(kk.eq.k2) iadj(1)=nson
        go to 40
c
c       regular and marco triangles
c
   30   call edges(i,iadj,itri,ivert)
c
c       look for green sons on this level
c
   40   do 60 k=1,3
            j=iadj(k)
            if(j.le.0) go to 60
            if(istl(j,itri).eq.1) go to 60
            json=son(j,itri)
            call knots(json,jv,itri,ivert)
            k1=(5-k)/2
            k2=6-k-k1
            k1=iv(k1)
            k2=iv(k2)
            ic=0
            do 50 l=1,3
                if(jv(l).eq.k1.or.jv(l).eq.k2) ic=ic+1
   50       continue
            if(ic.le.1) json=json+1
            iadj(k)=json
   60   continue
        return
c
c       this code for elements not in the current level
c       form non-green elements, just make sure that all
c       adjacent elements are on the same level
c
   70   call edges(i,iadj,itri,ivert)
        ilevel=level(i,itri)
        do 90 k=1,3
            j=iadj(k)
            if(j.le.0) go to 90
            if(level(j,itri).eq.ilevel) go to 90
            json=son(j,itri)
            call knots(json,jv,itri,ivert)
            k1=(5-k)/2
            k2=6-k-k1
            k1=iv(k1)
            k2=iv(k2)
            ic=0
            do 80 l=1,3
                if(jv(l).eq.k1.or.jv(l).eq.k2) ic=ic+1
   80       continue
            if(ic.le.1) json=json+1
            iadj(k)=json
   90   continue
        return
        end
