        subroutine lil(p,dp,q,dq,al,iflag)
c
            real p(2),dp(2),q(2),dq(2),al(2)
c
c       this routine find the intersection of two lines
c       if the lines are parallel iflag is set to 1
c
        iflag=0
        d1=p(1)-q(1)
        d2=p(2)-q(2)
        det=dp(2)*dq(1)-dp(1)*dq(2)
        if(det.eq.0.0e0) go to 10
        al(1)=(d1*dq(2)-d2*dq(1))/det
        al(2)=(dp(2)*d1-dp(1)*d2)/det
        return
   10   iflag=1
        al(1)=0.0e0
        al(2)=0.0e0
        return
        end
