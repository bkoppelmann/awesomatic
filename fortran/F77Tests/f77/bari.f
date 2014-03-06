        subroutine bari(x,y,vx,vy,iv,c)
c
            integer iv(3)
            real vx(*),vy(*),c(3)
c
c       compute the barycentric coordinates of the point (x,y)
c
        iv1=iv(1)
        iv2=iv(2)
        iv3=iv(3)
        x2=vx(iv2)-vx(iv1)
        y2=vy(iv2)-vy(iv1)
        x3=vx(iv3)-vx(iv1)
        y3=vy(iv3)-vy(iv1)
        xr=x-vx(iv1)
        yr=y-vy(iv1)
        det=x2*y3-x3*y2
        c(2)=(xr*y3-x3*yr)/det
        c(3)=(x2*yr-xr*y2)/det
        c(1)=1.0e0-c(2)-c(3)
        return
        end
