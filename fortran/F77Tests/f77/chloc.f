        function chloc(kb,kv,ka,vx,vy)
c
            real vx(*),vy(*)
c
c       this routine computes the local value of h
c
        x1=vx(ka)-vx(kv)
        y1=vy(ka)-vy(kv)
        x2=vx(kb)-vx(kv)
        y2=vy(kb)-vy(kv)
        chloc=((x1*x1+y1*y1)*(x2*x2+y2*y2))**0.25e0
        return
        end
