        subroutine arc0(x1,y1,x2,y2,xm,ym,
     1      xc,yc,theta1,theta2,r)
c
c       compute are parameters choosing 
c       correct arc based on midpoint
c
        pi=3.141592653589793e0
        call centre(x1,y1,x2,y2,xm,ym,xc,yc)
        call arc(x1,y1,x2,y2,xc,yc,theta1,theta2,r)
        call arc(x1,y1,xm,ym,xc,yc,thetax,thetam,rr)
        if((theta2-thetam)*(thetam-theta1).lt.0.0e0) then
            if(theta2-theta1.lt.0.0e0) then
                theta2=theta2+2.0e0*pi
            else
                theta2=theta2-2.0e0*pi
            endif
        endif
        return
        end
