        function ceps(ibit)
            integer eptst
c
c       compute machine epsilon
c
        ibit=-4
        eps=1.0e0
    3   eps1=1.0e0+eps
        if(eptst(eps1).eq.1) go to 4
        eps=eps/2.0e0
        ibit=ibit+1
        go to 3
    4   ceps=2.0e0**(-ibit)
        return
        end
