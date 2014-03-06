        subroutine getrgn(irgn,iseed,length,list,jv)
c
            integer list(2,*),jv(2,*)
c
c       compute boundary for region irgn
c
        length=0
        i=iseed
        ii=iseed
   10   i1=jv(1,i)+1
        i2=i1+iabs(jv(2,i))-1
c
        do 20 j=i1,i2
            if(jv(2,j).eq.irgn) go to 30
   20   continue
 
        stop 114
   30   k=iabs(jv(1,j))
        if(i.eq.ii) list(2,ii)=j
c
c       skip midpoint vertices for interior curved edges
c
        if(jv(2,k).eq.-2) go to 40
        list(1,ii)=k
        length=length+1
        ii=k
   40   i=k
        if(i.ne.iseed) go to 10
        return
        end
