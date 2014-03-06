        subroutine df(itri,ivert,ja,a,b,d,u,udot,
     1      au,x,y,rln)
c
            integer itri(4,*),ivert(4,*),ja(*)
            real a(*),u(*),udot(*),au(*),x(*),y(*),
     1          b(*),d(*),rln(9,9)
c
c       initialize
c
        nv=itri(1,4)
        rln(7,5)=0.0e0
        rln(7,6)=0.0e0
        rln(5,6)=0.0e0
c
c       compute a*udot
c
        call mtxmlt(ja,a,udot,au,x,y,itri,ivert)
c
c       inner products
c
        sd=0.0e0
        ss=0.0e0
        do 10 i=1,nv
            if(idbc(i,ivert).eq.1) then
                au(i)=udot(i)
            else
                ss=ss+au(i)*au(i)
                sd=sd+d(i)*au(i)
            end if
   10   continue
        if(ss.gt.0.0e0) ss=sd/ss
        rln(5,6)=ss
        do 15 i=1,nv
   15       d(i)=d(i)-ss*au(i)
c
c       compute a*u
c
        call mtxmlt(ja,a,u,au,x,y,itri,ivert)
c
c       inner products
c
        sb=0.0e0
        sd=0.0e0
        ss=0.0e0
        do 20 i=1,nv
            if(idbc(i,ivert).eq.1) then
                au(i)=u(i)
            else
                ss=ss+au(i)*au(i)
                sb=sb+b(i)*au(i)
                sd=sd+d(i)*au(i)
            end if
   20   continue
        if(ss.le.0.0e0) return
        sb=sb/ss
        sd=sd/ss
        rln(7,5)=sb
        rln(7,6)=sd
        do 30 i=1,nv
            b(i)=b(i)-sb*au(i)
            d(i)=d(i)-sd*au(i)
   30   continue
        return
        end
