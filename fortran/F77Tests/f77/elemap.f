        subroutine elemap(i,j,ij,ji,ja,ishift)
            integer ja(*)
c
c       compute location of a(i,j) and a(j,i)
c
        if(i.gt.j) go to 30
        j1=ja(j)
        j2=ja(j+1)-1
        do 10 ij=j1,j2
            if(ja(ij).eq.i) go to 20
   10   continue
   20   ji=ij+ishift
        return
c
   30   j1=ja(i)
        j2=ja(i+1)-1
        do 40 ji=j1,j2
            if(ja(ji).eq.j) go to 50
   40   continue
   50   ij=ji+ishift
        return
        end
