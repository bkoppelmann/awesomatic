        subroutine  htextm(itype,nchr,ichr,slen,str,inum,
     1      ifmt,rnum)
c
            character*1 str(*)
            character*1 blank,cc
            integer map(128),ichr(*),inum,slen,temp(100)
            real rnum
            save map,blank
c
c       map maps ascii characters onto htext characters
c       ascii characters with no corresponding htext
c       character are mapped to blank
c
            data map/
     1       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     2       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     3       0,84,89,81,72,86,87,90,65,66,71,63,67,64,68,69,
     4      53,54,55,56,57,58,59,60,61,62,82,83,73,70,74,85,
     5      77, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
     6      16,17,18,19,20,21,22,23,24,25,26,79, 0,80,78, 0,
     7       0,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
     8      42,43,44,45,46,47,48,49,50,51,52,75, 0,76,88, 0/
c
            data blank/' '/
c
c       map ascii characters onto htext characaters
c
c       character string
c
        if(itype.eq.1) then
            do 5 istart=1,slen
                if(str(istart).ne.blank) go to 10
    5       continue
            nchr=1
            ichr(nchr)=map(1)
            return
   10       do 15 istop=slen,istart,-1
                if(str(istop).ne.blank) go to 20
   15       continue
   20       nchr=istop-istart+1
            do 25 i=istart,istop
                cc=str(i)
                ii=ichar(cc)
                ichr(i-istart+1)=map(ii+1)
   25       continue
            return
        endif
c
c       integer
c
        if(itype.eq.2) then
            if(inum.eq.0) then
                nchr=1
                ichr(nchr)=map(49)
                return
            endif
            n=iabs(inum)
            ndig=0
            do 30 i=1,100
                j=n/10
                ndig=ndig+1
                temp(ndig)=n-j*10
                if(j.eq.0) go to 35
                n=j
   30       continue
   35       if(inum.gt.0) then
                nchr=ndig
                ishift=0
            else
                nchr=ndig+1
                ishift=1
                ichr(1)=map(46)
            endif
            do 40 i=1,ndig
                k=temp(ndig+1-i)
                ichr(i+ishift)=map(49+k)
   40       continue
            return
        endif
c
c       floating point number
c
        if(itype.eq.3) then
            if(rnum.eq.0.0e0) then
                nchr=ifmt+2
                do 45 i=1,nchr
                    ichr(i)=map(49)
   45           continue
                ichr(2)=map(47)
                return
            endif
            tt=abs(rnum)*10.0e0**(ifmt)+0.5e0
            n=int(tt)
            if(n.eq.0) then
                do 50 i=1,ifmt+3
                    ichr(i)=map(49)
   50           continue
                if(rnum.gt.0) then
                    nchr=ifmt+2
                    ichr(2)=map(47)
                else
                    nchr=ifmt+3
                    ichr(1)=map(46)
                    ichr(3)=map(47)
                endif
                return
            endif
            ndig=0
            do 55 i=1,100
                j=n/10
                ndig=ndig+1
                temp(ndig)=n-j*10
                if(j.eq.0) go to 60
                n=j
   55       continue
   60       if(ndig.le.ifmt) then
                do 65 i=ndig+1,ifmt+1
                    temp(i)=0
   65           continue
                ndig=ifmt+1
            endif
            if(rnum.gt.0.0e0) then
                nchr=ndig+1
                ishift=0
            else
                nchr=ndig+2
                ishift=1
                ichr(1)=map(46)
            endif
            do 70 i=1,ndig-ifmt
                k=temp(ndig+1-i)
                ichr(i+ishift)=map(49+k)
   70       continue
            ishift=ishift+ndig-ifmt+1
            ichr(ishift)=map(47)
            do 75 i=1,ifmt
                k=temp(ifmt+1-i)
                ichr(i+ishift)=map(49)+k
   75       continue
            return
        endif
c
c       exponent format
c
        if(itype.eq.4) then
            zc=rnum
            if(zc.lt.0) then
                ishift=1
                ichr(1)=map(46)
            else
                ishift=0
            endif
            nchr=ifmt+5+ishift
            ichr(ishift+1)=map(47)
            ichr(nchr)=map(49)
            ichr(nchr-1)=map(49)
            ichr(nchr-2)=map(44)
            ichr(nchr-3)=map(102)
            if(zc.eq.0.0e0) then
                do 78 i=1,ifmt
                    ichr(i+1)=map(49)
   78           continue
                return
            endif
            zc=abs(zc)
            zz=alog10(zc)
            iex=int(zz)
            ratio=zc*(10.0e0**(-iex))
   80       if(ratio.lt.1.0e0.and.ratio.ge.0.1e0) go to 85
            if(ratio.ge.1.0e0) then
                ratio=amax1(ratio/10.0e0,0.1e0)
                iex=iex+1
            else
                ratio=amin1(ratio*10.0e0,1.0e0)
                iex=iex-1
            endif
            go to 80
c
c       exponent field (assume exponent lt 100)
c
   85       if(iex.lt.0) ichr(nchr-2)=map(46)
            iex=iabs(iex)
            if(iex.ge.100) then
                ichr(nchr)=map(43)
                ichr(nchr-1)=map(43)
            else
                ii=iex/10
                ichr(nchr-1)=map(49+ii)
                ii=iex-10*ii
                ichr(nchr)=map(49+ii)
            endif
c
c       mantissa field
c
            n=int(ratio*10.0e0**(ifmt)+0.5e0)
            n=min0(10**(ifmt)-1,n)
            n=max0(10**(ifmt-1),n)
            do 90 i=ifmt,1,-1
                j=n/10
                k=n-j*10
                ichr(i+ishift+1)=map(49+k)
                n=j
   90       continue
            return
        endif
        return
        end
