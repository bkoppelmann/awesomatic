        subroutine inters(x1,x2,y1,y2,xc,yc,r)
c
c       compute the intersection of the line passing through
c       (x1,y1) and (x2,y2) and the circle of radius
c       sqrt(abs(r)) with center (xc,yc)
c
        dx=x1-x2
        dy=y1-y2
        a=dx*dx+dy*dy
        if(a.le.0.0e0) return
        c=(x2-xc)*(x2-xc)+(y2-yc)*(y2-yc)-abs(r)
        b=dx*(x2-xc)+dy*(y2-yc)
        q=sqrt(b*b-a*c)
        if(b) 1,4,2
    1   q=q-b
        alpha=c/q
        if(r.le.0.0e0) alpha=q/a
        go to 3
    2   q=q+b
        alpha=-q/a
        if(r.le.0.0e0) alpha=-c/q
    3   x2=x2+dx*alpha
        y2=y2+dy*alpha
    4   return
        end
