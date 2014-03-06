        function cang(kb,kv,ka,vx,vy)
c
            real vx(*),vy(*)
c
c       the function computes the interior angle
c       given by the segments (kb,kv) and (kv,ka)
c
        pi=3.141592653589793e0
        twopi=2.0e0*pi
        x1=vx(ka)-vx(kv)
        x2=vx(kb)-vx(kv)
        y1=vy(ka)-vy(kv)
        y2=vy(kb)-vy(kv)
        xx=x2*x1+y2*y1
        yy=x1*y2-y1*x2
        s=xx/sqrt(xx*xx+yy*yy)
        s=amin1(1.0e0,s)
        s=amax1(-1.0e0,s)
        cang=acos(s)
        if(yy.le.0.0e0) cang=twopi-cang
        return
        end
