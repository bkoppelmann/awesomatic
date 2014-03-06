        subroutine cmid(xm,ym,um,vx,vy,u,rusr,iv,iside,icen)
c
            integer iv(3)
            real vx(*),vy(*),u(*),rusr(2,*)
            real c(3)
c
c       compute the coordinates of the midpoint of a boundary edge
c
        j1=(5-iside)/2
        j2=6-iside-j1
        ivj=iv(iside)
        iv1=iv(j1)
        iv2=iv(j2)
        xm=(vx(iv1)+vx(iv2))/2.0e0
        ym=(vy(iv1)+vy(iv2))/2.0e0
        um=(u(iv1)+u(iv2))/2.0e0
        if(icen.le.0) return
        call midpt(vx(iv1),vy(iv1),vx(iv2),vy(iv2),
     1      rusr(1,icen),rusr(2,icen),xm,ym)
        call bari(xm,ym,vx,vy,iv,c)
        um=c(iside)*u(ivj)+c(j1)*u(iv1)+c(j2)*u(iv2)
        return
        end
