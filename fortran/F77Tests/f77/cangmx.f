        function cangmx(kb,kv,ka,vx,vy)
c
            real vx(*),vy(*),h(3)
c
c       the function computes largest angle of the
c       triangle defined by the vertices kb,kv,ka
c
        h(1)=(vx(ka)-vx(kb))*(vx(ka)-vx(kb))+
     1       (vy(ka)-vy(kb))*(vy(ka)-vy(kb))
        h(2)=(vx(kb)-vx(kv))*(vx(kb)-vx(kv))+
     1       (vy(kb)-vy(kv))*(vy(kb)-vy(kv))
        h(3)=(vx(kv)-vx(ka))*(vx(kv)-vx(ka))+
     1       (vy(kv)-vy(ka))*(vy(kv)-vy(ka))
        j=1
        if(h(2).gt.h(1)) j=2
        if(h(3).gt.h(j)) j=3
        j2=(5-j)/2
        j3=6-j-j2
        h(j2)=h(j2)/h(j)
        h(j3)=h(j3)/h(j)
        q=(h(j2)+h(j3)-1.0e0)/(2.0e0*sqrt(h(j2)*h(j3)))
        q=amin1(1.0e0,q)
        q=amax1(-1.0e0,q)
        cangmx=acos(q)
        return
        end
