        subroutine jcupdt(imin,i,jc,mark,equiv,list,
     1      befor,after)
c
            integer jc(*),mark(*),equiv(*),list(*),
     1          befor(*),after(*)
c
c       update jc for vertex i
c
        i1=jc(i)
        i2=jc(i+1)-1
        iptr=i1
        nvert=0
        ncliq=1
        do 180 j=i1,i2
            jcj=iabs(jc(j))
            if(jcj.eq.0) go to 190
            if(jc(j).gt.0) then
c
c       check a normal vertex
c
                if(mark(jcj).eq.0) then
                    jc(iptr)=jcj        
                    iptr=iptr+1
                    nvert=nvert+1
                endif
            else
c
c       this loop overestimates degrees for vertices
c       connected to three or more cliques
c       on the first encounter, compute the intersection
c
                if(list(jcj).le.0) go to 180
                if(befor(jcj).ne.-imin) then 
                    befor(jcj)=-imin
                    after(jcj)=0
                    jck=jcj
  120               j1=jc(jck)
                    j2=jc(jck+1)-1
                    do 140 k=j1,j2
                        jck=iabs(jc(k))
                        if(jc(k)) 120,150,130
  130                   if(mark(jck).le.0)
     1                      after(jcj)=after(jcj)+equiv(jck)
  140               continue
                endif
  150           if(after(jcj).gt.0) then
                    jc(iptr)=-jcj
                    ncliq=ncliq+1
                    iptr=iptr+1
                endif
            endif
  180   continue
  190   jc(iptr)=-imin
        if(iptr.lt.i2) jc(iptr+1)=0
        befor(i)=nvert
        after(i)=ncliq
        return
        end
