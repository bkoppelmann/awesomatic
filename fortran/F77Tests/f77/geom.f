        function geom(kv,kb,ka,vx,vy)
c
            real vx(*),vy(*)
c
c       this function  computes a constant between
c       zero and one indicative of the quality of a triangle
c       (geom is neg if verts are given in clockwise order)
c
        x1=vx(ka)-vx(kv)
        y1=vy(ka)-vy(kv)
        x2=vx(kb)-vx(kv)
        y2=vy(kb)-vy(kv)
        det=x2*y1-x1*y2
        dd=x1*x1+y1*y1+x2*x2+y2*y2+(x1-x2)*(x1-x2)+
     1      (y1-y2)*(y1-y2)
        geom=det*3.464101616e0/dd
        return
        end
