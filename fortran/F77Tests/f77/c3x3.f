        subroutine c3x3(a,b,num)
c
            real a(3,3),b(3,3)
c
c       this routine solves 3 x 3 linear systems
c
c       scale rows so that largest element is one
c
        do 3 j=1,3
            rmax=amax1(abs(a(j,1)),abs(a(j,2)))
            rmax=amax1(abs(a(j,3)),rmax)
            if(rmax.ne.0.0e0) rmax=1.0e0/rmax
            do 1 k=1,3
    1           a(j,k)=a(j,k)*rmax
            do 2 k=1,num
    2           b(j,k)=b(j,k)*rmax
    3   continue
c
        j1=1
        if(abs(a(1,1)).lt.abs(a(2,1))) j1=2
        if(abs(a(j1,1)).lt.abs(a(3,1))) j1=3
        j2=(5-j1)/2
        j3=6-j1-j2
c
        if(a(j1,1).ne.0.0e0) a(j1,1)=1.0e0/a(j1,1)
        q2=a(j2,1)*a(j1,1)
        q3=a(j3,1)*a(j1,1)
        do 4 k=1,num
            b(j2,k)=b(j2,k)-b(j1,k)*q2
    4       b(j3,k)=b(j3,k)-b(j1,k)*q3
c
        a(j2,2)=a(j2,2)-a(j1,2)*q2
        a(j2,3)=a(j2,3)-a(j1,3)*q2
        a(j3,2)=a(j3,2)-a(j1,2)*q3
        a(j3,3)=a(j3,3)-a(j1,3)*q3
c
        if(abs(a(j2,2)).lt.abs(a(j3,2))) j2=j3
        j3=6-j1-j2
        if(a(j2,2).ne.0.0e0) a(j2,2)=1.0e0/a(j2,2)
        q3=a(j3,2)*a(j2,2)
        do 5 k=1,num
    5       b(j3,k)=b(j3,k)-b(j2,k)*q3
        a(j3,3)=a(j3,3)-a(j2,3)*q3
c
        if(a(j3,3).ne.0.0e0) a(j3,3)=1.0e0/a(j3,3)
        do 6 k=1,num
            x3=b(j3,k)*a(j3,3)
            x2=(b(j2,k)-a(j2,3)*x3)*a(j2,2)
            b(1,k)=(b(j1,k)-a(j1,2)*x2-a(j1,3)*x3)*a(j1,1)
            b(2,k)=x2
    6       b(3,k)=x3
        return
        end
