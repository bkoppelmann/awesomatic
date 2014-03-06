        function fscale(f,iscale,invrse)
c
c       set scaling function
c
        if(iscale.eq.0) then
c
c       linear scale
c
            fscale=f
        else if(iscale.eq.1) then
c
c       log scale
c
            if(invrse.eq.0) then
                fscale=alog(f)
            else
                fscale=exp(f)
            endif
        else
c
c       arcsinh scale
c
            if(invrse.eq.0) then
                af=abs(f)
                if(af.lt.1.0e0) then
                    q=sqrt(1.0e0+f*f)+af
                    fx=alog(q)
                    fscale=fx+(af-sinh(fx))/cosh(fx)
                else
                    q=1.0e0/f
                    q=sqrt(1.0e0+q*q)+1.0e0
                    fscale=alog(q)+alog(af)
                endif
                if(f.lt.0.0e0) fscale=-fscale
            else
                fscale=sinh(f)
            endif
        endif
        return
        end
