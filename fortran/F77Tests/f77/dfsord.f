        subroutine dfsord(itri,ivert,ja,p,q,m,list)
c
            integer itri(4,*),ivert(4,*),ja(*),p(*),q(*)
            integer iv(3),iadj(3),m(*),list(*)
c
c       construct ja from triangle data
c
        nv=itri(2,4)
        lvl=itri(1,3)
        itri(1,3)=itri(2,3)
        lja=itri(3,7)
        i1=itri(1,1)
        i2=itri(4,1)-1
c
        do 5 i=1,nv
    5       q(p(i))=i
        do 10 i=1,lja
   10       ja(i)=0
c
c       count edges...   each edge except for boundary
c       edges will be counted twice as all the triangles
c       are processed, so count boundary edges twice
c
        do 30 i=i1,i2
            if(istl(i,itri).eq.0) go to 30
            call knots(i,iv,itri,ivert)
            num=itbc(i,iadj,itri,ivert)
            do 20 j=1,3
                j2=(5-j)/2
                j3=6-j-j2
                kmax=max0(q(iv(j2)),q(iv(j3)))+1
                ja(kmax)=ja(kmax)+1
                if(iadj(j).gt.0.and.num.gt.0) ja(kmax)=ja(kmax)+1
   20           continue
   30       continue
c
c       compute pointers in 1st n+1 locations of ja
c
        ja(1)=nv+2
        do 40 j=1,nv
   40       ja(j+1)=ja(j)+ja(j+1)/2
c
        do 80 i=i1,i2
            if(istl(i,itri).eq.0) go to 80
            call knots(i,iv,itri,ivert)
            do 70 j=1,3
                j2=(5-j)/2
                j3=6-j-j2
                kmin=min0(q(iv(j2)),q(iv(j3)))
                kmax=max0(q(iv(j2)),q(iv(j3)))
                jmin=ja(kmax)
                jmax=ja(kmax+1)-1
c
c       check if kmin is already on list for kmax
c
                do 50 jj=jmin,jmax
                    if(ja(jj).eq.0) go to 60
                    if(ja(jj).eq.kmin) go to 70
   50           continue
c
c        add a new entry to ja
c
   60           ja(jj)=kmin
   70       continue
   80   continue
        itri(1,3)=lvl
c
c       sort column indices in decreasing order
c
        do 120 i=1,nv
            j1=ja(i)+1
            j2=ja(i+1)-1
            if(j1.gt.j2) go to 120
            do 110 j=j1,j2
                jmax=j-1
                do 100 k=j,j2
                    if(ja(k).gt.ja(jmax)) jmax=k
  100           continue
                jtemp=ja(j-1)
                ja(j-1)=ja(jmax)
                ja(jmax)=jtemp
  110       continue
  120   continue
c
c       compute m
c
        do 130 i = 1, nv
           m(i) = 0
  130      list(i) = 0
c
        do 170 i = 1, nv
           if (ja(i) .ge. ja(i+1))  go to 170
c
c       loop over seed indices in decreasing order
c
           list(i) = i
           length=1
           do 150 iseed = ja(i), ja(i+1)-1
              k = ja(iseed)
c
c       add a new entry to list
c
  140         list(k) = list(i)
              list(i) = k
              length=length+1
              if (m(k) .eq. 0) m(k) = i
              k = m(k)
              if (list(k).eq.0) go to 140
  150         continue
c
c       clean up loop for list array
c
           k = i
           do 160 j = 1, length
              ksave = k
              k = list(k)
  160         list(ksave) = 0
c
  170   continue
c
       do 180 i=1,nv
           q(i)=0
           ja(i)=0
  180      list(i)=0
c
       do 190 i=nv-1,1,-1
            mi=m(i)
            list(i)=ja(mi)
  190       ja(mi)=i
c
        next=0
        i=nv
  200   if(ja(i).eq.0) then
            next=next+1
            q(next)=i
            if(next.eq.nv) go to 210
            j=i
            i=m(j)
            ja(i)=list(j)
        else
            i=ja(i)
        endif
        go to 200
c
  210   do 220 i=1,nv
  220       list(i)=p(q(i))
        do 230 i=1,nv
            p(i)=list(i)
  230       q(p(i))=i 
        return
        end
