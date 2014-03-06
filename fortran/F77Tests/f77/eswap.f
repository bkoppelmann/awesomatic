        subroutine eswap(irgn,ir,itnode,vx,vy,list,nghbr)
c
            integer irgn(6,*),list(*),itnode(4,*),nghbr(3,*)
            real vx(*),vy(*)
c
c       this routine swaps interior triangle edges in an attempt
c       to improve the overall quality of the triangulation
c
        itmax=3
        nv=irgn(2,ir+1)-1
        it1=irgn(1,ir)
        it2=irgn(1,ir+1)-1
c
c       count number of triangles at each vertex
c
        do 1 i=1,nv
    1       list(i)=0
        do 2 i=it1,it2
        do 2 j=1,3
            is=i-it1+1
            nghbr(j,is)=0
            k=itnode(j,i)
    2       list(k)=list(k)+1
c
c       set up pointers for adjancency lists
c
        list(1)=list(1)+2*nv
        list(nv+1)=2*nv
        do 5 i=2,nv
            list(i)=list(i)+list(i-1)
            nvi=nv+i
    5       list(nvi)=list(i-1)
c
c       make list of triangles as a function of vertex number
c
        do 6 i=it1,it2
        do 6 j=1,3
             k=itnode(j,i)+nv
             list(k)=list(k)+1
             k=list(k)
    6        list(k)=i
c
c       initialize adjacency lists
c
        list(nv+1)=2*nv+1
        do 7 i=2,nv
            nvi=nv+i
    7       list(nvi)=list(i-1)+1
        do 15 i=it1,it2
        do 14 j=1,3
            is=i-it1+1
            if(nghbr(j,is).ne.0) go to 14
            j1=(5-j)/2
            j2=6-j-j1
            j1=itnode(j1,i)
            j2=itnode(j2,i)
c
            k1=nv+j1
            k1=list(k1)+1
            m1=list(j1)
            if(k1.gt.m1) go to 14
            k2=nv+j2
            k2=list(k2)+1
            m2=list(j2)
            if(k2.gt.m2) go to 14
c
c       compare lists for knots j1 and j2
c
    9       if(list(k1)-list(k2)) 10,12,11
   10       k1=k1+1
            if(k1-m1)9,9,14
   11       k2=k2+1
            if(k2-m2) 9,9,14
c
c       we have found the adjacent triangle
c
   12       k1=list(k1)
            nghbr(j,is)=k1
c
c       triangle k1 is adjacent to triangle i
c       determine which edge....
c
            k2=1
            do 13 l1=2,3
                ll=itnode(l1,k1)
                if(ll.ne.j1.and.ll.ne.j2) k2= l1
   13       continue
            k1s=k1-it1+1
            nghbr(k2,k1s)=i
   14   continue
c
c       update list....we have finished triangle i
c
        do 15 j=1,3
            k=itnode(j,i)+nv
   15       list(k)=list(k)+1
c
c       mark boundary vertices
c
        do 33 i=1,nv
   33       list(i)=0
        do 35 i=it1,it2
            is=i-it1+1
            do 34 j=1,3
                if(nghbr(j,is).gt.0) go to 34
                j1=(5-j)/2
                j2=6-j-j1
                j1=itnode(j1,i)
                j2=itnode(j2,i)
                list(j1)=1
                list(j2)=1
   34       continue
   35   continue
c
c       thr main loop in which the edges are swapped
c
        do 30 itnum=1,itmax
            ichng=0
            do 29 i=it1,it2
                is=i-it1+1
                do 28 j=1,3
                    k=nghbr(j,is)
                    if(k.le.i) go to 28
                    ks=k-it1+1
                    ii=itnode(j,i)
                    j1=(5-j)/2
                    j2=6-j-j1
                    n1=itnode(j1,i)
                    n2=itnode(j2,i)
                    kk=itnode(1,k)-n1+itnode(2,k)-n2+itnode(3,k)
                    qi=geom(ii,n1,n2,vx,vy)
                    if(qi.gt.0.0e0) go to 16
                    nn=n1
                    n1=n2
                    n2=nn
                    qi=-qi
   16               qk=geom(kk,n2,n1,vx,vy)
                    q1=geom(n1,kk,ii,vx,vy)
                    q2=geom(n2,ii,kk,vx,vy)
                    q12=amin1(q1,q2)
                    qik=amin1(qi,qk)
                    l12=list(n1)+list(n2)
                    lik=list(ii)+list(kk)
c
c       test on geometry... if close try to avoid edges
c       connecting two boundary points
c
                    if(q12.le.0.9e0*qik.or.lik.eq.2) go to 28
                    if(q12.le.qik.and.l12.lt.2) go to 28
c
c       swap edges
c
                    ichng=ichng+1
                    kj=1
                    if(itnode(2,k).eq.kk) kj=2
                    if(itnode(3,k).eq.kk) kj=3
                    k1=(5-kj)/2
                    if(itnode(k1,k).ne.itnode(j1,i)) k1=6-kj-k1
                    k2=6-kj-k1
                    itnode(j1,i)=kk
                    itnode(k2,k)=ii
                    nghbr(j,is)=nghbr(k1,ks)
                    nghbr(kj,ks)=nghbr(j2,is)
                    nghbr(j2,is)=k
                    nghbr(k1,ks)=i
                    li=nghbr(j,is)-it1+1
                    if(li.le.0) go to 26
                    do 25 l=1,3
                        if(nghbr(l,li).eq.k) nghbr(l,li)=i
   25               continue
   26               lk=nghbr(kj,ks)-it1+1
                    if(lk.le.0) go to 28
                    do 27 l=1,3
                        if(nghbr(l,lk).eq.i) nghbr(l,lk)=k
   27               continue
   28           continue
   29       continue
            if(ichng.le.0) go to 32
   30   continue
   32   return
        end
