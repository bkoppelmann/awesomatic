        function cthrsh(kref,thrsh,qmin,qmax,isw)
            integer kref(*)
c
c       predict a new threshold
c
        fract=0.05e0
        num=20
        h=(qmax-qmin)/float(num)
        dr=amax1(fract*float(kref(1)),4.0e0)
        if(h.eq.0.0e0) then
            cthrsh=exp(qmin)
            return
        endif
        s=(alog(thrsh)-qmin)/h
        is=min0(int(s)+1,num)
        t=s-float(int(s))
        rn=(1.0e0-t)*float(kref(is))+t*float(kref(is+1))
        if(isw.eq.1) then
            rr=rn+dr
            if(rr.ge.float(kref(1))) then
                cthrsh=exp(qmin)*0.99e0
                return
            endif
        else
            rr=rn-dr
            if(rr.le.0.0e0) then
                cthrsh=exp(qmax)*1.01e0
                return
            endif
        endif
        do 10 i=num,1,-1
            if(float(kref(i)).ge.rr) go to 20
   10   continue
   20   t=(float(kref(i))-rr)/(float(kref(i))-float(kref(i+1)))
        q=qmin+(float(i-1)+t)*h
        cthrsh=exp(q)
        return
        end
