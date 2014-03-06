        subroutine mkcliq(imin,jc,list,mark,equiv,
     1      ilen,imndeg,iempty)
c
            integer jc(*),mark(*),list(*),equiv(*)
c
        mark(imin)=imin
        imndeg=0
        ilen=0
        i1=jc(imin)
        i2=jc(imin+1)-1
        do 80 j=i1,i2
            jcj=iabs(jc(j))
            if(jcj.eq.0) return  
            if(jc(j).gt.0) then
c
c       merge a normal vertex
c
                if(mark(jcj).eq.0) then
                    mark(jcj)=mark(imin)
                    mark(imin)=jcj
                    imndeg=imndeg+equiv(jcj)
                    ilen=ilen+1
                endif
c
c       merge a clique
c
            else
   50           j1=jc(jcj)
                j2=jc(jcj+1)-1
                list(jcj)=0
                mark(jcj)=iempty
                iempty=jcj
                do 70 l=j1,j2
                    jcj=iabs(jc(l))
                    if(jc(l)) 50,80,60
   60               if(mark(jcj).eq.0) then
                        mark(jcj)=mark(imin)
                        mark(imin)=jcj
                        imndeg=imndeg+equiv(jcj)
                        ilen=ilen+1
                    endif
   70           continue
            endif
   80   continue
        end
