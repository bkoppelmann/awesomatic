        subroutine divide(i,iadj,iv,itri,ivert,iflag)
c
            integer iadj(3),iv(3),itri(4,*),ivert(4,*),son,tag
c
c       divide triangle i into four triangles
c
        iflag=0
        newt=itri(4,1)
        nv=itri(4,4)
c
c       branch if there is insufficient triangle space
c
        if(newt+4.gt.itri(1,6)) go to 100
        itri(4,1)=newt+4
        itri(3,1)=itri(4,1)
c
c       fill known info for this block of triangles
c
        itri(4,i)=newt
        itbc=0
        do 20 j=1,3
            newtj=newt+j
            itri(1,newtj)=0
            itri(2,newtj)=0
            k=iv(j)
            iv(j)=ivert(3,k)
            itri(3,newtj)=iv(j)
            itri(4,newtj)=0
            if(iadj(j).gt.0) go to 20
            if(itbc.eq.0) nn=j
            if(itbc.gt.0) nn=-(6-itbc-j)
            if(itbc.lt.0) nn=-4
            itbc=nn
   20   continue
        itri(1,newt)=ielty(i,itri)
        itri(1,newt+1)=tag(i,itri)
        itri(1,newt+2)=itbc
        itri(2,newt)=i
        ilevel=level(i,itri)
        itri(3,newt)=ilevel+1
        itri(4,newt)=0
c
c       fill in vertices of new center triangle
c
        do 90 iside=1,3
            j=iadj(iside)
            newtj=newt+iside
            if(j.le.0) go to 50
            jlevel=level(j,itri)
            if(jlevel.lt.ilevel) go to 50
            json=son(j,itri)
            if(json.le.0) go to 50
c
c       find the midpoint for this edge
c
            j1=(5-iside)/2
            j2=6-iside-j1
            j1=iv(j1)
            j2=iv(j2)
            do 30 k=1,3
                js=json+k
                jj=itri(3,js)
                if(jj.ne.j1.and.jj.ne.j2) go to 40
   30       continue
   40       nvert=itri(2,js)
            itri(2,newtj)=nvert
            ivert(2,nvert)=newtj
            ivert(1,nvert)=iabs(ivert(1,nvert))
            go to 90
c
c       create new vertex
c
   50       nv=nv+1
            if(nv.gt.itri(2,6)) go to 110
            ivert(1,nv)=newtj
            if(j.gt.0) ivert(1,nv)=-newtj
            ivert(2,nv)=j
            ivert(3,nv)=nv
            ivert(4,nv)=nv
            itri(2,newtj)=nv
   90   continue
c
        itri(4,4)=nv
        return
c
  100   iflag=41
        return
  110   iflag=42
        return
        end
