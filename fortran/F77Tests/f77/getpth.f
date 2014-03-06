        subroutine getpth(irgn,iseed,length,list,jv)
c
            integer list(2,*),jv(2,*)
c
c       compute path starting from vertex iseed
c
        length=0
        i=iseed
c
c       follow a region
c
        if(irgn.le.0) go to 50
   10   i1=jv(1,i)+1
        i2=i1+iabs(jv(2,i))-1
        do 20 j=i1,i2
            if(jv(2,j).eq.irgn) go to 30
   20   continue
        stop 115
   30   list(1,i)=iabs(jv(1,j))
        list(2,i)=0
        i=list(1,i)
        length=length+1
        if(iabs(jv(2,i)).le.2) go to 10
        go to 60
c
c       follow the boundary
c
   50   i1=jv(1,i)+1
        i2=i1+iabs(jv(2,i))-1
        list(1,i)=iabs(jv(1,i2))
        list(2,i)=0
        i=list(1,i)
        length=length+1
        if(iabs(jv(2,i)).le.2) go to 50
c
c       mark first and last vertices
c
   60   list(2,iseed)=1
        list(1,i)=0
        list(2,i)=1
        return
        end
