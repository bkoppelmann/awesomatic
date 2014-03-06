        subroutine clqupd(jx,jlen,jc,mark,list,equiv,iempty)
c
            integer jc(*),mark(*),equiv(*),list(*)
c
c       delete equivalent vertices from clique lists
c
        do 70 jj=1,jlen
            jnext=mark(jx)
            mark(jx)=jx
            list(jx)=0
            ilen=0
            jcj=jx
   10       j1=jc(jcj)
            j2=jc(jcj+1)-1
            if(jcj.ne.jx) then
                mark(jcj)=iempty
                iempty=jcj
            endif
            do 30 l=j1,j2
                jcj=iabs(jc(l))
                if(jc(l)) 10,40,20
   20           if(equiv(jcj).lt.0) jcj=-equiv(jcj)
                if(mark(jcj).eq.0) then
                    mark(jcj)=mark(jx)
                    mark(jx)=jcj
                    list(jx)=list(jx)+equiv(jcj)
                    ilen=ilen+1
                 endif
   30       continue
   40       call svcliq(jx,jc,mark,equiv,
     1          ilen,iempty)
            jx=jnext
   70   continue
        return
        end
