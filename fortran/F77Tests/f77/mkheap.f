        subroutine mkheap(i,llen,d,list)
            integer list(2,*)
            real d(*)
c
c       this routine makes a heap with root at vertex i, assuming its
c       sons are already roots of heaps
c
        k=i
   10   kson=2*k
        if(kson-llen) 20,30,40
   20   if(d(list(1,kson+1)).gt.d(list(1,kson))) kson=kson+1
   30   if(d(list(1,k)).ge.d(list(1,kson))) return
        ktemp=list(1,k)
        list(1,k)=list(1,kson)
        list(1,kson)=ktemp
        ktemp=list(2,k)
        list(2,k)=list(2,kson)
        list(2,kson)=ktemp
        k=kson
        go to 10
   40   return
        end
