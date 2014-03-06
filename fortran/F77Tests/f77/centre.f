        subroutine centre(x1,y1,x2,y2,xm,ym,xc,yc)
c
c       compute the center of the circle which passes
c       through (x1,y1), (x2,y2), and (xm,ym)
c
        z1=x1-xm
        z2=x2-xm
        w1=y1-ym
        w2=y2-ym
        det=(z1*w2-z2*w1)*2.0e0
        r1=(z1*(x1+xm)+w1*(y1+ym))/det
        r2=(z2*(x2+xm)+w2*(y2+ym))/det
        xc=r1*w2-r2*w1
        yc=z1*r2-z2*r1
        return
        end
