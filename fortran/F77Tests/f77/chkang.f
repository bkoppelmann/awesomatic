        subroutine chkang(nv,vx,vy,cxy,jv,map,area,tol,itri)
c
            integer itri(4,*),jv(2,*),map(*),it(2),jt(2)
            real vx(*),vy(*),area(*),cxy(3,*)
c
c       look at all degree 2 vertices and eliminate
c       those with angle approximately equal to pi
c
        pi=3.141592653589793e0
        do 130 i=1,nv
            if(jv(2,i).le.1) go to 120
            if(jv(2,i).gt.2) go to 130
            i1=jv(1,i)+1
            iv1=iabs(jv(1,i1))
            iv2=iabs(jv(1,i1+1))
            if(jv(2,i1-1).gt.0) go to 10
c
c       boundary point
c
            k1=iabs(jv(2,i1-1))
            k2=iabs(jv(2,i1+1))
            if(k1.eq.k2) go to 20
            call cbound(k1,it1,icen1,mfi,mfe,itag1,itri)
            call cbound(k2,it2,icen2,mfi,mfe,itag2,itri)
            if(it1.ne.it2) go to 130
            if(itag1.ne.itag2) go to 130
            if(icen1.gt.0.or.icen2.gt.0) then
                if(icen1.le.0.or.icen2.le.0) go to 130
                qn=(cxy(1,icen1)-cxy(1,icen2))**2+
     1              (cxy(2,icen1)-cxy(2,icen2))**2
                qd=abs(cxy(3,icen1))+abs(cxy(3,icen2))
                if(qn.le.tol*qd) go to 20
                go to 130
            endif
c
c       test angle
c
   10       aa=cang(iv1,i,iv2,vx,vy)/pi
            if(abs(aa-1.0e0).ge.tol) go to 130
c
c       delete this point
c
   20       do 50 ll=1,2
                it(ll)=0
                jt(ll)=0
                i1=jv(1,iv1)+1
                i2=i1+jv(2,iv1)-1
                do 30 j=i1,i2
                    if(iabs(jv(1,j)).eq.i) it(ll)=j
                    if(iabs(jv(1,j)).eq.iv2) jt(ll)=j
   30           continue
                j=it(ll)
                if(j.eq.0) stop 101
                k=jv(1,j)
                jv(1,j)=iv2
                if(k.lt.0) jv(1,j)=-iv2
                if(jv(2,i1-1).le.0) go to 40
                jv(1,i1-1)=jv(1,i2)
                jv(1,i2+1)=jv(1,i1)
   40           ii=iv1
                iv1=iv2
                iv2=ii
   50       continue
c
c       ckeck for two sided region
c
            if(jt(1)+jt(2).eq.0) go to 120
c
c       fixup vertex iv1
c
            do 70 ll=1,2
                i1=jv(1,iv1)+1
                i2=i1+jv(2,iv1)-1
c
                k1=min0(jt(ll),it(ll))
                k2=jt(ll)+it(ll)-k1
                if(k1.eq.i1.and.k2.eq.i2) k1=i2
                irgn=jv(2,k1)
                ibef=jv(2,k1-1)
                iaft=jv(2,k1+1)
c
                jv(2,iv1)=jv(2,iv1)-1
                do 60 k=k1,i2
                    jv(1,k)=jv(1,k+1)
                    jv(2,k)=jv(2,k+1)
   60           continue
                if(jv(2,i1-1).le.0) go to 65
                jv(1,i1-1)=jv(1,i2-1)
                jv(2,i1-1)=jv(2,i2-1)
                jv(1,i2)=jv(1,i1)
                jv(2,i2)=jv(2,i1)
c
   65           ii=iv1
                iv1=iv2
                iv2=ii
   70       continue
 
c
c       fixup stack in map
c
            map(irgn)=map(1)
            map(1)=irgn
            if(iaft.lt.0) iaft=ibef
            if(ibef.lt.0) ibef=iaft
            area(ibef)=area(ibef)+area(irgn)/2.0e0
            area(iaft)=area(iaft)+area(irgn)/2.0e0
c
  120       jv(2,i)=0
  130   continue
        return
        end
