        subroutine eledbc(u,x,y,rl,itag,itype,gxy)
c
            external gxy
c
c       this routine evaluates the initial guess and
c       dirichlet boundary conditions
c
c       itype = 4 dirichlet boundary point
c       itype = 5 d(u)/d(rl) at dirichlet boundary point
c       itype = 6 initial guess (iedge=0)
c
        u=gxy(x,y,0.0e0,rl,itag,itype)
        return
        end
