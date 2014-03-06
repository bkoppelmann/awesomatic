        subroutine clunrf(itri,ivert,lunref,e,qmin,qmax,kunref)
            integer itri(4,*),ivert(4,*),lunref(*),
     1          father,kunref(*)
            real e(*)
c
        maxl=itri(4,3)
        irefn=itri(2,8)
        i2=itri(2,1)
        i3=itri(3,1)-1
c
        lunref(1)=maxl+2
        lunref(2)=maxl+2
        if(i2.gt.i3) return
c
c       compute lunref array
c
        do 15 i=2,maxl+1
   15       lunref(i)=0
        do 20 i=i2,i3,4
            ilev=level(i,itri)
            if(ilev.gt.irefn) then
                itri(1,i+3)=1
                lunref(ilev+1)=lunref(ilev+1)+1
            else
                itri(1,i+3)=0
            endif
   20   continue
        do 25 i=2,maxl+1
   25       lunref(i)=lunref(i-1)+lunref(i)
        do 30 i=i2,i3,4
            ilev=level(i,itri)
            if(ilev.gt.irefn) then
                k=lunref(ilev)
                lunref(ilev)=k+1
                lunref(k)=i
            endif
   30   continue
        do 35 i=maxl,2,-1
   35       lunref(i)=lunref(i-1)
        lunref(1)=maxl+2
c
c       compute kunref
c
        num=20
        do 40 i=1,num+1
   40       kunref(i)=0
        h=(qmax-qmin)/float(num)
        if(h.eq.0.0e0) then
            kunref(1)=lunref(maxl+1)-lunref(1)
            return
        endif
        do 50 it=lunref(1),lunref(maxl+1)-1
            i=lunref(it)
            jf=father(i,itri)
            ee=amax1(e(jf),e(i),e(i+1),e(i+2),e(i+3))
            if(ee.gt.0.0e0) then
                q=(alog(ee)-qmin)/h
                iq=min0(int(q)+1,num)
                kunref(iq)=kunref(iq)+1
            else
                kunref(1)=kunref(1)+1
            endif
   50   continue
        do 60 i=num,1,-1
   60       kunref(i)=kunref(i)+kunref(i+1)
c
        return
        end
