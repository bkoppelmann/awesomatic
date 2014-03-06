        subroutine green(itri,ivert,iflag)
c
            integer iv(3),ivf(2),itri(4,*),ivert(4,*)
c
c       scan knots and create geeen triangles as required
c
        iflag=0
        nv=itri(4,4)
        n1=itri(4,5)+1
        newt=itri(4,1)
        itri(3,1)=newt
        if(n1.gt.nv) return
        maxt=itri(1,6)
c
c       the main loop
c
        do 60 i=n1,nv
            if(ivert(1,i).ge.0) go to 60
            if(newt+1.gt.maxt) go to 100
            it=ivert(2,i)
            itri(4,it)=newt
            call knots(it,iv,itri,ivert)
            call vf(i,ivf,itri,ivert)
c
c       find shared knot
c
            j1=0
            do 10 j=1,3
                k=iv(j)
                iv(j)=ivert(3,k)
                if(iv(j).ne.ivf(1).and.iv(j).ne.ivf(2)) j1=j
   10       continue
            j2=(5-j1)/2
            j3=6-j1-j2
            itri(1,newt)=it
            itri(2,newt)=i
            itri(3,newt)=j1
            itri(4,newt)=0
            itri(2,newt+1)=iv(j1)
            itri(1,newt+1)=iv(j2)
            itri(3,newt+1)=iv(j3)
            itri(4,newt+1)=0
            newt=newt+2
            itri(4,1)=newt
   60   continue
        return
c
  100   iflag=41
        return
        end
