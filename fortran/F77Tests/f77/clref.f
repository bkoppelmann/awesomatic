        subroutine clref(itri,ivert,lref,e,qminr,qmaxr,kref) 
            integer itri(4,*),ivert(4,*),level,lref(*),kref(*)
            real e(*)
c
c       conput lref array
c
        maxl=itri(4,3)
        i1=itri(1,1)
        i3=itri(3,1)-1
c
        lref(1)=maxl+2
        do 15 i=2,maxl+1
   15       lref(i)=0
        do 20 i=i1,i3
            if(itri(4,i).gt.0) go to 20
            itri(4,i)=0
            ilev=level(i,itri)
            lref(ilev+1)=lref(ilev+1)+1
   20   continue
        do 25 i=2,maxl+1
   25       lref(i)=lref(i-1)+lref(i)
        do 30 i=i1,i3
            if(itri(4,i).gt.0) go to 30
            ilev=level(i,itri)
            k=lref(ilev)
            lref(ilev)=k+1
            lref(k)=i
   30   continue
        do 35 i=maxl,2,-1
   35       lref(i)=lref(i-1)
        lref(1)=maxl+2
c
c       compute kref
c
        num=20
        do 40 i=1,num+1
   40       kref(i)=0
        h=(qmaxr-qminr)/float(num)
        if(h.eq.0.0e0) then
            kref(1)=lref(maxl+1)-lref(1)
            return
        endif
        do 50 it=lref(1),lref(maxl+1)-1
            i=lref(it)
            if(e(i).gt.0.0e0) then
                q=(alog(e(i))-qminr)/h
                iq=min0(int(q)+1,num)
                kref(iq)=kref(iq)+1
            else
                kref(1)=kref(1)+1
            endif
   50   continue
        do 60 i=num,1,-1
   60       kref(i)=kref(i)+kref(i+1)
        return
        end
