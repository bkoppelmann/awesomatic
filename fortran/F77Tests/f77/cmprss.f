        subroutine cmprss(nv,jv,list,map,vx,vy)
c
            integer jv(2,*),list(2,*),map(*)
            real vx(*),vy(*)
c
c       do garbage collection in jv array
c
        nv0=nv
        nn=0
        do 10 i=1,nv0
            if(jv(2,i).ge.2) nn=nn+1
   10   continue
c
c       reinitialize pointer arrays
c
        nv=0
        k=nn+2
        do 20 i=1,nv0
            list(2,i)=0
            if(jv(2,i).lt.2) go to 20
            nv=nv+1
            list(1,i)=jv(1,i)
            list(2,i)=nv
            jv(1,nv)=k
            jv(2,nv)=jv(2,i)
            vx(nv)=vx(i)
            vy(nv)=vy(i)
            k=k+2+jv(2,i)
   20   continue
        jv(1,nv+1)=k
c
c       shift and reset the balance of jv
c
        do 50 i=1,nv0
            if(list(2,i).eq.0) go to 50
            nn=list(2,i)
            l=list(1,i)
            length=jv(1,nn+1)-jv(1,nn)
            k=jv(1,nn)
            do 40 kc=1,length
                m=iabs(jv(1,l))
                jv(1,k)=0
                if(m.gt.0) jv(1,k)=list(2,m)
                jv(2,k)=jv(2,l)
                l=l+1
                k=k+1
   40       continue
   50   continue
c
c       mark interfaces specified by user
c
        do 80 n=1,nv
            i1=jv(1,n)+1
            i2=i1+jv(2,n)-1
            do 70 i=i1,i2
                irgn=jv(2,i-1)
                jrgn=jv(2,i)
                if(min0(irgn,jrgn).le.0) go to 60
                if(map(irgn).eq.map(jrgn))go to 70
   60           jv(1,i)=-jv(1,i)
   70       continue
   80   continue
        return
        end
