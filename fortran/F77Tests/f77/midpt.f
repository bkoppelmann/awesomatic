        subroutine midpt(x1,y1,x2,y2,xc,yc,x,y)
c
c       compute the midpoint of the circle with center (xc,yc)
c       which passes through the points (x1,y1),(x2,y2).
c       the midpoint (x,y) is relative to the shorter
c       of the two arcs.
c
        x=(x1+x2)/2.0e0
        y=(y1+y2)/2.0e0
        c=(x+x1-2.0e0*xc)*(x1-x)+(y+y1-2.0e0*yc)*(y1-y)
        if(c.le.0.0e0) return
        dy=y1-y2
        dx=x1-x2
        b=(x-xc)*dy-(y-yc)*dx
        a=dx*dx+dy*dy
        a=c/(abs(b)+sqrt(b*b+a*c))
        if(b.lt.0.0e0)  a=-a
        x=x+a*dy
        y=y-a*dx
        return
        end
