        subroutine md(n,jc,p,mark,lenu,list,
     1      equiv,befor,after)
c
            integer jc(*),p(*),mark(*),equiv(*),
     1          befor(*),after(*),list(*)
c
c       minimum degree algorithm
c
c       list        = linked list of equivalent vertices (v,e)
c                   = size of clique (c)
c       equiv       = number of equivalent vertices (v)
c                   = ptr to equivant vertex (e)
c                   = (temp) ptr to equiv vertex with clique imin (c)
c       befor/after = doubly linked list of verts. by degree (v)
c                     (temp) nvert/ncliq for verts in imin
c                     (temp) marker for outmtached verts in imin
c                   = (temp) switch/intersection size with imin (c)
c       mark        = temp linked list
c       p           = order (tail is head ptrs into befor/after)
c
        lenu=n+1
        mndeg=n+1
        imin=0
        iempty=0
        next=1
        do 10 i=1,n
            p(i)=0
            equiv(i)=1
            list(i)=i
            befor(i)=0
            after(i)=0
   10       mark(i)=0
        do 20 i=1,n
            ideg=jc(i+1)-jc(i)
            if(ideg.le.0) then
                p(next)=i
                next=next+1
            else
                id=n+1-ideg
                if(p(id).ne.0) befor(p(id))=i
                after(i)=p(id)
                p(id)=i
                befor(i)=-id
                mndeg=min0(mndeg,ideg)
            endif
   20   continue
        if(next.gt.n) go to 300
c
c       order vertex of min degree
c
   30   id=n+1-mndeg
        if(p(id).eq.0) then
            mndeg=mndeg+1
            go to 30
        endif
        imin=p(id)
        if(after(imin).gt.0) befor(after(imin))=-id
        p(id)=after(imin)
        befor(imin)=0
        after(imin)=0
c
c       build the current clique (imin)
c
        call mkcliq(imin,jc,list,mark,equiv,
     1      ilen,imndeg,iempty)
c
        numequ=equiv(imin)
        i=imin
        do 40 ii=1,numequ 
            p(next)=i
            next=next+1
            equiv(i)=0
            lenu=lenu+imndeg+numequ-ii
            i=list(i)
   40   continue
        if(next.gt.n) go to 300
c
c       eliminate redundant vertices from adjacency lists of clique
c       members...this allows simple elimination of equivalent vertices
c
        i=imin
        numequ=0
        jx=imin
        jlen=0
        do 100 ii=1,ilen
            i=mark(i)
            if(after(i).gt.0) befor(after(i))=befor(i)
            if(befor(i).lt.0) then
                id=-befor(i)
                if(id.ge.next)  p(id)=after(i)
            else
                after(befor(i))=after(i)
            endif
            befor(i)=0
            after(i)=0
            i1=jc(i)
c
c       update adjacency list
c
            call jcupdt(imin,i,jc,mark,equiv,list,
     1          befor,after)
            nvert=befor(i)
            ncliq=after(i)
c
c       test for equivalence
c
            if(nvert.eq.0.and.ncliq.eq.1) then
                inum=equiv(i)
                m=i
                do 50 mm=1,inum
                    p(next)=m
                    next=next+1
                    equiv(m)=0
                    numequ=numequ+1
                    lenu=lenu+imndeg-numequ
                    m=list(m)
   50           continue
            endif
c
c       look for equivalent vertices
c
            if(nvert.eq.0.and.ncliq.eq.2) then
                jcj=-jc(i1)
                if(mark(jcj).eq.0) then
                    mark(jcj)=jx
                    jx=jcj
                    jlen=jlen+1
                    equiv(jcj)=i
                else
                    ieq=equiv(jcj)
                    inum=equiv(i)
                    equiv(ieq)=equiv(ieq)+inum
                    m=list(i)
                    do 60 mm=1,inum
                        mnext=list(m)
                        list(m)=list(ieq)
                        list(ieq)=m
                        equiv(m)=-ieq
                        m=mnext
   60               continue
                endif
            endif
c
  100   continue
        if(next.gt.n) go to 300
c
c       clean up  mark, move clique to jc
c
        call svcliq(imin,jc,mark,equiv,
     1      ilen,iempty)
c
c     update cliques
c
        if(jlen.gt.0) 
     1      call clqupd(jx,jlen,jc,mark,list,equiv,iempty)
c 
c       degree updates
c
        list(imin)=imndeg-numequ
        mndeg=list(imin)
        i=imin
  110   j1=jc(i)
        j2=jc(i+1)-1
        do 140 j=j1,j2
            i=iabs(jc(j))
            if(jc(j)) 110,270,120
c
  120       nvert=befor(i)
            ncliq=after(i)
            k1=jc(i)+nvert
            k2=k1+ncliq-2
            ideg=nvert+list(imin)-1
            if(k1.le.k2) then
                do 130 kk=k1,k2
                    jck=-jc(kk)
                    ideg=ideg+after(jck)
  130           continue
            endif
c
c       overcounting with three cliques requires this
c
            ideg=min0(ideg,n-next)
            id=n+1-ideg
            if(p(id).ne.0) befor(p(id))=i
            after(i)=p(id)
            p(id)=i
            befor(i)=-id
  140   continue 
c
c       find the next vertex
c
  270   if(next.le.n)  go to 30
c
c       compute inverse permutation
c
  300   do 310 i=1,n
  310       mark(p(i))=i
        return
        end
