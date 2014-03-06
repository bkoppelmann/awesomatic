        subroutine grad(ux,uy,vx,vy,u,iv)
c
            integer iv(3)
            real vx(*),vy(*),u(*)
c
c       compute the gradient of u in element defined by iv
c
        iv1=iv(1)
        iv2=iv(2)
        iv3=iv(3)
        x2=vx(iv2)-vx(iv1)
        x3=vx(iv3)-vx(iv1)
        y2=vy(iv2)-vy(iv1)
        y3=vy(iv3)-vy(iv1)
        u2=u(iv2)-u(iv1)
        u3=u(iv3)-u(iv1)
        det=x2*y3-x3*y2
        ux=(u2*y3-u3*y2)/det
        uy=(x2*u3-x3*u2)/det
        return
        end
