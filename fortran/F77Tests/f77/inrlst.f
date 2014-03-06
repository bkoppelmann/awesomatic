        subroutine inrlst(ir,irgn,jb,iusr,irptr,irlst,iflag)
c
            integer irgn(6,*),jb(*),iusr(6,*),irptr(*),irlst(2,*)
c
c       this routine initializes the irptr, and irlst arrays
c       for the region nr
c
        iflag=0
        irptr(1)=2
        ic=2
        i1=irgn(3,ir)
        i2=irgn(4,ir)
        j1=jb(i1)
        j2=jb(i2)
        kv=iusr(1,j1)
        if(kv.ne.iusr(1,j2).and.kv.ne.iusr(2,j2)) kv=iusr(2,j1)
c
c       the main loop
c
        do 20 ii=i1,i2
            i=jb(ii)
            kei=0
            if(iusr(4,i).ne.0) kei=i
            j1=iusr(6,i)
            j2=iusr(6,i+1)-1
            if(ic+j2-j1+2.gt.irlst(1,1)) go to 100
            irlst(1,ic)=kv
            irlst(2,ic)=kei
            ic=ic+1
            if(j1.gt.j2) go to 20
            jj=j1
            inc=1
            if(iusr(1,i).eq.kv) go to 4
            jj=j2
            inc=-1
    4       do 5 j=j1,j2
                irlst(1,ic)=jj
                jj=jj+inc
                irlst(2,ic)=kei
    5           ic=ic+1
   20       kv=iusr(1,i)+iusr(2,i)-kv
        irptr(2)=ic
        return
  100   iflag=20
        return
        end
