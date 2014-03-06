        subroutine jiggle(irgn,ir,list,llist,itnode,vx,vy,pram)
c
            integer  irgn(6,*),list(*),itnode(4,*)
            real vx(*),vy(*),pram(7)
c
c       this routine tries to optimize the placement of interior
c       knots for region ir
c
        itmax=4
        jtmax=0
        eps=pram(1)
        tol=1.0e-3
        s3=1.732050807568877e0/2.0e0
        n1=irgn(2,ir)
        n2=irgn(2,ir+1)-1
        if(n1.gt.n2) return
c
c       make list of triangles as a function of interior
c       vertex number
c
        i1=irgn(1,ir)
        i2=irgn(1,ir+1)-1
        do 10 i=1,n2
   10       list(i)=1
        do 20 i=i1,i2
        do 20 j=1,3
            iv=itnode(j,i)
   20       list(iv)=list(iv)+1
        l=n2+2
        do 30 i=n1,n2
            ii=list(i)
            list(i)=l
   30       l=l+ii
        list(n2+1)=l
        if(l-1.gt.llist) return
        j1=list(n1)
        j2=list(n2+1)-1
        do 35 i=j1,j2
   35       list(i)=0
        do 50 i=i1,i2
        do 50 j=1,3
            iv=itnode(j,i)
            if(iv.lt.n1.or.iv.gt.n2) go to 50
            k1=list(iv)+1
            k2=list(iv+1)-1
            do 40 k=k1,k2
   40           if(list(k).eq.0) go to 45
   45       list(k)=i
   50   continue
c
c       convert this list to a circular list of vertices
c       in counter clockwise order (first and last
c       vertices are the same)
c
        do 75 n=n1,n2
            i1=list(n)+2
            i2=list(n+1)-1
            i=list(i1-1)
            j=1
            if(itnode(2,i).eq.n) j=2
            if(itnode(3,i).eq.n) j=3
            j1=(5-j)/2
            j2=6-j-j1
            j1=itnode(j1,i)
            j2=itnode(j2,i)
            if(geom(n,j1,j2,vx,vy).gt.0.0e0) go to 55
            jj=j1
            j1=j2
            j2=jj
   55       list(i1-2)=j1
            list(i1-1)=j2
            do 70 i=i1,i2
                k=list(i-1)
                do 60 jj=i,i2
                    j=list(jj)
                    if(itnode(1,j).eq.k) go to 65
                    if(itnode(2,j).eq.k) go to 65
                    if(itnode(3,j).eq.k) go to 65
   60           continue
   65       list(jj)=list(i)
   70       list(i)=itnode(1,j)-n+itnode(2,j)-k+itnode(3,j)
   75   continue
c
c       thr main loop in which the knots positions are
c       optimized
c
        do 110 iii=1,itmax
        do 110 n=n1,n2
            i1=list(n)
            i2=list(n+1)-2
            qmin=1.0e0
            qmin2=1.0e0
            k=0
            l=0
            do 80 i=i1,i2
                kb=list(i)
                ka=list(i+1)
                q=geom(n,kb,ka,vx,vy)
                if(q.ge.qmin2) go to 80
                if(q.gt.qmin) go to 77
                qmin2=qmin
                qmin=q
                l=k
                k=i
                go to 80
   77           qmin2=q
                l=i
   80       continue
            xmin=vx(n)
            ymin=vy(n)
            kb=list(k)
            ka=list(k+1)
            dxk=(vx(ka)-vx(kb))*s3
            dyk=(vy(ka)-vy(kb))*s3
            xmk=(vx(kb)+vx(ka))/2.0e0
            ymk=(vy(kb)+vy(ka))/2.0e0
            xmax=xmk-dyk
            ymax=ymk+dxk
            if(iii.le.jtmax) go to 85
            rk=sqrt(dxk*dxk+dyk*dyk)
c
c       compute more sophisticated (xmax,ymax) using the point
c       where the level curves (circles) for the two worst triangles
c       are equal and tangent
c
            lb=list(l)
            la=list(l+1)
            dxl=(vx(la)-vx(lb))*s3
            dyl=(vy(la)-vy(lb))*s3
            xml=(vx(lb)+vx(la))/2.0e0
            yml=(vy(lb)+vy(la))/2.0e0
            rl=sqrt(dxl*dxl+dyl*dyl)
            xm=xmk-xml
            dx=dxk-dxl
            ym=ymk-yml
            dy=dyk-dyl
            r=rk+rl
            a=r*r-dx*dx-dy*dy
            b=ym*dx-xm*dy
            c=xm*xm+ym*ym+r*r
            beta=1.0e0
            if(a.gt.0.0e0) beta=(b+sqrt(b*b+a*c))/a
            xck=xmk-beta*dyk
            yck=ymk+beta*dxk
            xcl=xml-beta*dyl
            ycl=yml+beta*dxl
            xmax=(xck*rl+xcl*rk)/r
            ymax=(yck*rl+ycl*rk)/r
c
c       the bisection loop
c
   85       zx=abs(xmin-xmax)/(abs(xmin)+abs(xmax)+eps)
            zy=abs(ymin-ymax)/(abs(ymin)+abs(ymax)+eps)
            if(amax1(zx,zy).lt.tol) go to 100
            vx(n)=(xmin+xmax)/2.0e0
            vy(n)=(ymin+ymax)/2.0e0
            qq=1.0e0
            do 90 i=i1,i2
                kb=list(i)
                ka=list(i+1)
                q=geom(n,kb,ka,vx,vy)
                if(q.lt.qmin) go to 95
                qq=amin1(qq,q)
   90       continue
            xmin=vx(n)
            ymin=vy(n)
            qmin=qq
            go to 85
   95       xmax=vx(n)
            ymax=vy(n)
            go to 85
  100       vx(n)=xmin
            vy(n)=ymin
  110   continue
        return
        end
