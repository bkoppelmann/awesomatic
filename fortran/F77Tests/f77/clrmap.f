        subroutine clrmap(red,green,blue,jp)
c
            integer jp(25)
            real red(*),green(*),blue(*)
            real r(7),g(7),b(7)
            save r,g,b
            data r/1.0e0,1.0e0,1.0e0,0.0e0,0.0e0,0.0e0,1.0e0/
            data g/0.0e0,0.0e0,1.0e0,1.0e0,1.0e0,0.0e0,0.0e0/
            data b/1.0e0,0.0e0,0.0e0,0.0e0,1.0e0,1.0e0,1.0e0/
c
c       set up a color map
c
        ncolor=jp(5)
        ifun=jp(6)
        nshade=jp(16)
        mxcolr=jp(17)
        maplen=jp(18)
        gamma=0.7e0
        theta=1.0e0
c
c       background color (white)
c
        red(1)=1.0e0
        green(1)=1.0e0
        blue(1)=1.0e0
c
c       line-drawing color (black)
c
        red(2)=0.0e0
        green(2)=0.0e0
        blue(2)=0.0e0
c
        if(maplen.le.2) return
c
        if(ncolor.ge.mxcolr-2) then
            jcolor=mxcolr-2
        else
            jcolor=ncolor
        endif
c
c       the primary set of colors
c
        red(3)=r(7)
        green(3)=g(7)
        blue(3)=b(7)
        if(jcolor.eq.1) go to 20
        if(ifun.le.11) then
            h=5.0e0/float(jcolor-1)
        else
            h=6.0e0/float(jcolor)
        endif
        do 10 ii=2,jcolor
            i=ii+2
            x=6.0e0-h*float(ii-1)
            k=1+int(x)
            dl=float(k)-x
            dr=1.0e0-dl
            red(i)=dl*r(k)+dr*r(k+1)
            red(i)=amax1(0.0e0,red(i))**gamma
            red(i)=amin1(1.0e0,red(i))
            green(i)=dl*g(k)+dr*g(k+1)
            green(i)=amax1(0.0e0,green(i))**gamma
            green(i)=amin1(1.0e0,green(i))
            blue(i)=dl*b(k)+dr*b(k+1)
            blue(i)=amax1(0.0e0,blue(i))**gamma
            blue(i)=amin1(1.0e0,blue(i))
   10   continue
c
c       shading
c
   20   if(nshade.eq.0) return
        if(ifun.le.11) then
            bmax=0.5e0/float(nshade)
            wmax=0.5e0/float(nshade)
        else
            bmax=0.45e0/float(nshade)
            wmax=0.75e0/float(nshade)
        endif
        do 50 j=1,nshade
            jplus=j*ncolor+2
            jminus=jplus+nshade*ncolor
            fb=(1.0e0-float(j)*bmax)**theta
            fw=(1.0e0-float(j)*wmax)**theta
            w=1.0e0-fw
            do 40 i=1,ncolor
                k=i+jplus
                red(k)=red(i+2)*fw+w
                red(k)=amax1(red(k),0.0e0)
                red(k)=amin1(red(k),1.0e0)
                green(k)=green(i+2)*fw+w
                green(k)=amax1(green(k),0.0e0)
                green(k)=amin1(green(k),1.0e0)
                blue(k)=blue(i+2)*fw+w
                blue(k)=amax1(blue(k),0.0e0)
                blue(k)=amin1(blue(k),1.0e0)
                k=i+jminus
                red(k)=red(i+2)*fb
                red(k)=amax1(red(k),0.0e0)
                red(k)=amin1(red(k),1.0e0)
                green(k)=green(i+2)*fb
                green(k)=amax1(green(k),0.0e0)
                green(k)=amin1(green(k),1.0e0)
                blue(k)=blue(i+2)*fb
                blue(k)=amax1(blue(k),0.0e0)
                blue(k)=amin1(blue(k),1.0e0)
   40       continue
   50   continue
c
        return
        end
