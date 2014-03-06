        subroutine htext(xl,yl,xr,yr,nchr,ichr,ijust,q,t)
c
            integer ichr(*),symbcd(631),istart(90)
            real width(90),x(2),y(2),t(25),q(3,3)
            save symbcd,istart,width
c
c       writes text given in ichr array in the rectangle defined by its
c       lower left corner of world coordinates xl,yl and its upper right
c       corner of world coordinates xr,yr.
c
c       ijust=-1  for justification on the left
c       ijust= 0  for centered text
c       ijust=+1  for justification on the right
c
c        the symbol numbers are
c        1-26   upper case roman simplex
c       27-52   lower case roman simplex
c       53-62   simplex numbers
c       63-76   symbols + - ( ) , . / = * $ < > { }
c       77-90   symbols @ ^ [ ] # : ; ! ? % & ~ " '
c
c
c       symbol parameters taken from n.m.wolcott, fortran iv enhanced
c       character graphics, nbs
c       ichr(j) contains the symbol number of the jth symbol
c       everything outside this range is considered a space
c
        data (symbcd(i),i=1,60)/
     +  443556555,443557579,432612882,        0,433070987,433071584,
     1  323987166,328083226,325854871,317404054,317400725,325723922,
     2  327657165,323364299,298156032,462268125,321889760,309339231,
     3  300852123,296493907,298329038,304489675,317040204,325527312,
     4          0,433070987,433071456,319792797,325953304,327788240,
     5  323429900,312845195,        0,433070987,433071840,432743830,
     6  432383691,        0,433070987,433071840,432743830,        0,
     7  462268125,321889760,309339231,300852123,296493907,298329038,
     8  304489675,317040204,325527312,327792083,327778304,433070987,
     9  462432011,432744214,        0,433070987,        0,449848720/
        data (symbcd(i),i=61,120)/
     +  312911116,306553867,298197837,294134546,        0,433070987,
     1  462431122,443262731,        0,433070987,432383627,        0,
     2  433070987,433071499,466625931,466626443,        0,433070987,
     3  433071883,462432011,        0,443556959,300852123,296493907,
     4  298329038,304489675,317040204,325527312,329885528,328050397,
     5  321889760,309329920,433070987,433071584,323987166,328083225,
     6  325822102,317367189,        0,443556959,300852123,296493907,
     7  298329038,304489675,317040204,325527312,329885528,328050397,
     8  321889760,309343631,327450624,433070987,433071584,323987166,
     9  328083226,325854871,317399958,447424267,        0,460236383/
        data (symbcd(i),i=121,180)/
     +  315630752,300917597,296592281,300688471,317367892,323593937,
     1  325527116,314942603,300294990,        0,441459851,426780256,
     2          0,433070993,300360780,310748555,321267406,327722784,
     3          0,426779851,460334283,        0,428876875,449848395,
     4  449849035,470820555,        0,430974667,460333899,        0,
     5  426779862,308655840,309002240,460333899,430974688,430286539,
     6          0,455910987,455812568,313304217,302785430,296330065,
     7  298263564,306554187,317072974,        0,433070987,432743448,
     8  307012953,317466198,323593873,321332684,312845451,302392206,
     9          0,455812568,313304217,302785430,296330065,298263564/
        data (symbcd(i),i=181,240)/
     +  306554187,317072974,        0,456140363,455812568,313304217,
     1  302785430,296330065,298263564,306554187,317072974,        0,
     2  430548563,321562135,317465945,307012632,298525523,296264590,
     3  302392459,312845772,321323008,445654176,303014876,300266265,
     4  309100544,455910985,318973381,312616068,302167638,317465945,
     5  307012632,298525523,296264590,302392459,312845772,321323008,
     6  433070987,432710744,309110169,319563349,321224704,430973855,
     7  300950433,296760217,298156032,435168287,305144865,300954649,
     8  302261189,295838404,        0,433070987,453813135,441034315,
     9          0,433070987,        0,432841611,432710744,309110169/
        data (symbcd(i),i=241,300)/
     +  319563349,321238613,327952281,338471128,344631563,        0,
     1  432841611,432710744,309110169,319563349,321224704,441230360,
     2  298525523,296264590,302392459,312845772,321332881,323593814,
     3  317465945,307003392,432841604,432743448,307012953,317466198,
     4  323593873,321332684,312845451,302392206,        0,455910980,
     5  455812568,313304217,302785430,296330065,298263564,306554187,
     6  317072974,        0,432841611,432645078,304882905,315392000,
     7  453715416,311207001,298591062,298460179,313075153,319268366,
     8  317072651,304456588,296157184,435168207,302392459,310752025,
     9  309100544,432841615,300295243,310748556,321369689,321224704/
        data (symbcd(i),i=301,360)/
     +  428647563,453813387,        0,430744651,447521867,447522379,
     1  464299595,        0,430745099,453813067,        0,428647563,
     2  453813387,302228357,293741252,        0,453813067,430745113,
     3  430286347,        0,443556895,298722135,296362895,302392523,
     4  312845836,323462868,325822108,319792480,309329920,437134493,
     5  313533771,        0,432907164,300885023,307242400,319792734,
     6  323888794,321660373,296068811,        0,435168928,311174616,
     7  321627798,325691089,323429900,312845451,300295053,296189952,
     8  451945298,327759328,317030400,456139744,298558424,307012953,
     9  319563414,325691089,323429900,312845451,300295053,296189952/
        data (symbcd(i),i=361,420)/
     +  458139231,315630880,305112028,298558354,300360780,310748491,
     1  319170190,325625554,323659287,313271576,304849877,298385408,
     2  460334155,430974688,        0,441459679,298754971,300721240,
     3  313239062,323626706,325559949,321267083,306553804,298230607,
     4  296297364,302720215,317466201,323856029,321889696,307232768,
     5  458008150,317334803,308913172,298525529,296559517,303015136,
     6  311436767,321824409,323626575,317072651,306553804,298254336,
     7  451847627,432678932,        0,432678932,        0,447882466,
     8  305112027,298525586,300328009,308487492,        0,431104994,
     9  305112283,311108882,308716617,300098372,        0,436609995/
        data (symbcd(i),i=421,480)/
     +  298197965,302392330,300163975,        0,434545548,300262412,
     1  300318720,466756356,        0,432777239,432580625,        0,
     2  441263246,430679505,451650385,        0,441590919,449979783,
     3  460236383,315630752,300917597,296592281,300688471,317367892,
     4  323593937,325527116,314942603,300294990,        0,466527124,
     5  331710464,432973716,298156032,443688035,303113184,300885020,
     6  304981145,306947093,439460897,303015005,307111130,309077142,
     7  298460306,308815054,306586699,302294023,304264211,306750607,
     8  304522252,300229576,302195781,308412416,435299427,307307744,
     9  309273756,304981017,302752917,439461025,307209309,302916570/
        data (symbcd(i),i=481,540)/
     +  300688406,311043090,300426190,302392395,306488455,304264339,
     1  302556175,304522380,308618440,306390085,300023808,462169818,
     2  321758619,311239897,306914451,308847952,319301265,325694875,
     3  311207126,308913425,313014043,325691089,329787344,338241685,
     4  340502618,336471966,328181344,315630815,305079260,298656599,
     5  296362897,300393549,308684171,321234700,331786190,464365331,
     6  327722832,        0,426321109,325661394,309012178,        0,
     7  433202052,435299268,433202532,432153924,        0,443688132,
     8  445785348,431105316,430056708,        0,447751044,460334340,
     9  432711445,430417615,        0,434938776,300655640,300725197/
        data (symbcd(i),i=541,600)/
     +  298197963,302392269,        0,434938776,300655640,300725195,
     1  298197965,302392330,300163975,        0,435168158,300491806,
     2  300954590,300692429,298197963,302392269,        0,432939995,
     3  298656603,296625054,300917856,311436767,319759964,321725976,
     4  317433045,308884768,315598302,319694362,317465942,442934412,
     5  308651276,308707328,468722507,441459998,311305434,304915417,
     6  296592221,298820640,307242271,317662878,330278880,459875921,
     7  319268365,323331851,331753422,333981522,325648384,468461463,
     8  334178327,336340953,332179288,327886481,319235468,310748235,
     9  298197838,296264595,311141785,317564381,315598112,307209309/
        data (symbcd(i),i=601,631)/
     +  304981144,311076430,325461899,333817868,335983691,300295054,
     1  298361811,304788571,307013262,327559051,        0,430482259,
     2  298525719,306947350,319399570,327755667,334148435,298492950,
     3  306914581,319366801,327722898,334145495,        0,435168153,
     4  437265305,451945881,454043033,        0,443557017,445654169,
     5          0/
c
        data istart/
     +     1,   5,  16,  26,  34,  39,  43,  54,  58,  60,  66,  70,
     1    73,  78,  82,  93, 100, 112, 120, 131, 134, 140, 143, 148,
     2   151, 154, 158, 167, 176, 184, 193, 202, 206, 217, 222, 226,
     3   232, 236, 238, 247, 252, 261, 270, 279, 283, 292, 296, 301,
     4   304, 309, 312, 317, 321, 330, 333, 341, 349, 352, 361, 373,
     5   376, 391, 403, 406, 408, 414, 420, 425, 428, 430, 433, 437,
     6   450, 452, 454, 473, 492, 519, 523, 528, 533, 538, 544, 551,
     7   558, 573, 588, 612, 624, 629/
c
        data width/
     +  18.,21.,21.,21.,19.,18.,21.,22., 8.,16.,21.,17.,
     1  24.,22.,22.,21.,22.,21.,20.,16.,22.,18.,24.,20.,
     2  18.,20.,19.,19.,18.,19.,18.,12.,19.,19., 8.,10.,
     3  17., 8.,30.,19.,19.,19.,19.,13.,17.,12.,19.,16.,
     4  22.,17.,16.,17.,20.,20.,20.,20.,20.,20.,20.,20.,
     5  20.,20.,26.,26.,14.,14.,10.,10.,22.,26.,16.,20.,
     6  24.,24.,14.,14.,27.,22.,14.,14.,21.,10.,10.,10.,
     7  18.,24.,25.,24.,16., 8./
c
c
c       ixtrct gets nbits from iword starting at the nstart
c       bit from the right
c
        ixtrct(nstart,nbits,iword)=mod(iword/(2**(nstart-nbits)),
     1      2**nbits)+((1-isign(1,iword))/2)*
     2      (2**nbits-min0(1,mod(-iword,2**(nstart-nbits))))
c
        if(nchr.le.0) return
        if(xl.ge.xr) return
        if(yl.ge.yr) return
        dx=xr-xl
        dy=yr-yl
c
c       find width of strings to be plotted
c
        wid=0.0e0
        do 10 i=1,nchr
            ic=ichr(i)
            if(ic.lt.1.or.ic.gt.90) then
                wid=wid+20.0e0
            else
                wid=wid+width(ic)
            endif
  10    continue
        wid=wid/21.0
c
        height=amin1(dx/wid,dy)
        if(height.lt.dy) then
            x0=xl
            y0=yl+(dy-height)/2.0e0
        else
c
c       justification
c
            y0=yl
            if(ijust.eq.-1) then
                x0=xl
            elseif(ijust.eq.0) then
                x0=xl+(dx-wid*height)/2.0e0
            elseif(ijust.eq.1) then
                x0=xr-wid*height
            endif
        endif
c
        scale=t(3)
        xshift=t(1)
        yshift=t(2)
c
        rscale=height/21.0e0
        xi=x0
        yi=y0
c
        do 100 i=1,nchr
            ic=ichr(i)
            if(ic.le.0.or.ic.gt.90)then
c
c        plot a space
c
                xi=xi+20.0e0*rscale
            else
c
c       plot a single symbol
c
                is=istart(ic)
                ib=30
   70           ipen=ixtrct(ib,3,symbcd(is))
                if(ipen.eq.0)then
                    xi=xi+rscale*width(ic)
                    goto 100
                endif
                ix=ixtrct(ib-3,6,symbcd(is))
                iy=ixtrct(ib-9,6,symbcd(is))
                xx=xi+(ix-10)*rscale
                yy=yi+(iy-11)*rscale
                xm=xx*q(1,1)+yy*q(2,1)
                ym=xx*q(1,2)+yy*q(2,2)
                xx=xm*scale+xshift
                yy=ym*scale+yshift
                if(ipen.eq.2) then
                    x(2)=xx
                    y(2)=yy
                    call lwindw(x,y,2,t)
                endif
                x(1)=xx
                y(1)=yy
                ib=45-ib
                if(ib.eq.30)is=is+1
                goto 70
            endif
  100   continue
        return
        end
