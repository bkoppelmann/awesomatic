*                                            1/6/92
*
*   This past year I have enhanced the LFK test program to automatically
*   increase sample run-timings in proportion to the cpu-clock resolution.
*   The poor resolution of ETIME in UNIX systems had required increasing
*   the run-time limit manually as the speed of workstations increased.
*   Now this LFK test will run dependably, hands-off.
*
*                    Frank McMahon
*
*
C     PROGRAM MFLOPS(TAPE6=OUTPUT)
C                    LATEST KERNEL MODIFICATION DATE: 22/DEC/86
C                    LATEST FILE   MODIFICATION DATE: 30/SEP/91 version mf523
C****************************************************************************
C MEASURES CPU PERFORMANCE RANGE OF THE COMPUTATION/COMPILER/COMPUTER COMPLEX
C****************************************************************************
C                                                                           *
C     L. L. N. L.   F O R T R A N   K E R N E L S  T E S T:   M F L O P S   *
C                                                                           *
C                                  Our little systems have their day;       *
C                                  They have their day and cease to be:     *
C                                  They are but broken parts of Thee,       *
C                                  And Thou, O Lord, are more than they.    *
C                                           Alfred, Lord Tennyson (1850)    *
C                                                                           *
C                                                                           *
C     These kernels measure  Fortran  numerical  computation rates for a    *
C     spectrum of  CPU-limited  computational  structures.  Mathematical    *
C     through-put is measured  in  units  of  millions of floating-point    *
C     operations executed per Second, called Mega-Flops/Sec.                *
C                                                                           *
C     The experimental  design  of some traditional  benchmark tests  is    *
C     defective when  applied  to computers employing vector or parallel    *
C     processing because the range of cpu performance is 10 to 100 times    *
C     the range  of conventional, serial processors.  In particular, the    *
C     effective Cpu performance  of supercomputers now ranges from a few    *
C     megaflops to a few thousand megaflops. Attempts by some marketeers    *
C     and decision makers to reduce this three orders of magnitude range    *
C     of cpu  performance  to  a  single  number is unscientific and has    *
C     produced much confusion.   The  LFK  test  also has been abused by    *
C     some analysts who quote only a single, average performance number.    *
C                                                                           *
C     The Livermore  Fortran  Kernels (LFK) test contains a broad sample    *
C     of generic Fortran computations which have been used to measure an    *
C     effective numerical  performance range, thus avoiding the peril of    *
C     a single performance  "rating".   A complete report of 72 LFK test    *
C     results must  quote  six performance range statistics(rates):  the    *
C     minimum, the  harmonic,   geometric,  and  arithmetic  means,  the    *
C     maximum and  the  standard deviation.  No single rate quotation is    *
C     sufficient or  honest.    These   measurements  show  a  realistic    *
C     variance in  Fortran  cpu  performance  that has stood the test of    *
C     time and that is vital data for circumspect computer evaluations.     *
C     Quote statistics from the SUMMARY table of 72 timings (DO Span= 167). *
C                                                                           *
C     This LFK test may be used as a standard performance test, as a test   *
C     of compiler accuracy (checksums), or as a hardware endurance test.    *
C     The LFK methodology is discussed in subroutine REPORT with references.*
C     The glossary and module hierarchy are documented in subroutine INDEX. *
C                                                                           *
C     Use of this program is granted with the request that a copy of the    *
C     results be sent to  the  author  at the address shown below, to be    *
C     added to  our studies of  computer performance.   Please send your    *
C     complete LFK test output file on 5" DOS floppy-disk, or by E-mail.    *
C     Your timing results  may be held as proprietary data, if so marked.   *
C     Otherwise your results will be quoted in published reports and will   *
C     be disseminated through a publicly accessable computer network.       *
C     Most computer vendors have run the LFK test(akas Livermore Loops test)*
C     and can provide LFK test results to prospective customers on request. *
C                                                                           *
C                                                                           *
C          F.H. McMahon     L-35                                            *
C          Lawrence Livermore National Laboratory                           *
C          P.0. Box 808                                                     *
C          Livermore, CA.   94550                                           *
C                                                                           *
C          (510) 422-1647                                                   *
C          mcmahon@ocfmail.ocf.llnl.gov                                     *
C          MCMAHON3@LLNL.GOV                                                *
C                                                                           *
C                                                                           *
C                    (C) Copyright 1983 the Regents of the                  *
C                University of California. All Rights Reserved.             *
C                                                                           *
C               This work was produced under the sponsorship of             *
C                the U.S. Department of Energy. The Government              *
C                       retains certain rights therein.                     *
C****************************************************************************
C
C
C                             DIRECTIONS
C
C  1. We REQUIRE one test-run of the Fortran kernels as is, that is, with
C     no reprogramming.  Standard product compiler directives may be used
C     for optimization as these do not constitute reprogramming. Use of
C     special compiler coding used only for specific LFK kernels is PROHIBITED.
C     We REQUIRE one mono-processed run (1 cpu) of this unaltered test.
C
C     The performance of the standard, "as is" LFK test (no modifications)
C     correlates well with the performance of the majority of cpu-bound,
C     Fortran applications and hence of diverse workloads.  These measured
C     correlations show the LFK to be a good sampling of the existing
C     inventory of Fortran coding practice in general.  The extrema in
C     the Fortran inventory are represented from serial recurrences on
C     small arrays to global-parallel computation on large arrays.
C
C  2. In addition, the vendor may, if so desired, reprogram the kernels to
C     demonstrate high performance hardware features.  Kernels 13,14,23
C     are partially vectorisable and kernels 15,16,24 are vectorisable if
C     re-written. Kernels 5,6,11,17,19,20,23 are implicit computations that
C     must NOT be explicitly vectorised using compiler directives to
C     ignore dependencies.  In any case, compiler listings of the codes
C     actually used should be returned along with the timing results.
C
C     We permit the LFK kernels to be reprogrammed ONLY as a partial
C     demonstration of the performance of innovative, high performance
C     architectures.  We may then infer from the reprogramming work
C     the kind and degree of optimisations which are necessary to achive
C     high performance as well as the cost in time and effort.
C     Only if it can be shown that this reprogramming can be automated
C     could we establish a correlation with the existing Fortran inventory.
C     These non-standard tests using the LFK samples are intended to explore
C     programming requirements and should not be correlated with standard
C     LFK test results (as in 1 above).
C
C  3. For vector processors, we REQUIRE an ALL-scalar compilation test-run
C     to measure the basic scalar performance range of the processor.
C
C  4. On computers where default single precision is REAL*4 we REQUIRE an
C     additional test-run with all mantissas.ge.47 .  Declare all REAL*8 using:
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
c
c     To change REAL*4 (MFLOPS) to REAL*8 Double Precision:
c
c      vi... :1,$s/cANSI/     /g
c      vi... :1,$s/      DOUBLE  PRE/Cout  DOUBLE  PRE/g
c     ( some redundance in IQRANF,REPORT,RESULT,SEQDIG,TALLY,TRIAL,VALUES)
c
c     To reverse REAL*8 (DPMFLOPS) to REAL*4 Single Precision:
c
c      vi... :1,$s/      IMPLICIT  DOUBLE PRE/cANSI IMPLICIT  DOUBLE PRE/g
c      vi... :1,$s/Cout  DOUBLE  PRE/      DOUBLE  PRE/g
C
C  5. Installation includes verifying or changing the following:
C
C      First :  the definition of function SECOND for CPU time only, and
C      Second:  the definition of function MOD2N in KERNEL
C      Third :  the system names Komput, Kontrl, and Kompil in MAIN.
C     During check-out run-time can be reduced by setting:    Nruns= 1 in SIZES.
C     For Standard LFK Benchmark Test verify:                 Nruns= 7 in SIZES.
C
C  6. Each kernel's computation is check-summed for easy validation.
C     Your checksums should compare to the precision used, within round-off.
C     The number of correct, significant digits in your check-sums is printed
C     in the OK column next to each check-sum.  Single precision should produce
C     6 to 8 OK digits and double precision should produce 11 to 16 OK digits.
C     Try REAL*16 in subr SIGNEL and SUMO to improve accuracy of DP checksums.
C
C  7. Verify CPU Time measurements from function SECOND by comparing the clock
C     calibration printout of total CPU time with system or real-time measures.
C     The accuracy of SECOND is also tested using subr VERIFY and CALIBR.
C     Each kernel's execution may be repeated arbitrarily many times
C     (MULTI >> 100) without overflow and produce verifiable checksums.
C
C     Default, uni-processor tests measure job  Cpu-time in SECOND (TSS mode).
C     Parallel processing tests should measure Real-time in stand-alone mode.
C
C  8. On computers with Virtual Storage Systems assure a working-set space
C     larger than the entire program so that page faults are negligible,
C     because we must measure the CPU-limited computation rates.
C     IT IS ALSO NECESSARY to run this test stand-alone, i.e. NO timesharing.
C     In VS Systems a series of runs are needed to show stable CPU timings.
C
C  9. On computers with Cache memories and high resolution CPU clocks we
C     need, if feasible, another ALL-scalar test-run setting Loop= 1
C     in SIZES to test un-primed cache (as well as encached) cpu rates.
C     Increase the size of array CACHE(in subr. VALUES) from 8192 to cache size.
C
C 10. On parallel computer systems which compile parallel Multi-tasking
C     at the Do-loop level (Micro-tasking) parallelisation of each
C     kernel is encouraged, but the number of processors used must be
C     reported.  Parallelisation of, or invarient code hoisting outside of
C     the outermost, repetition loop around each kernel (including TEST)
C     is PROHIBITED.  You may NOT declare NO-SIDE-EFFECTS function TEST.
C
C 11. A long endurance test can be set-up by redefining "laps" in SIZES.
C
C
C
C
C
C
C
C 12. Quote statistics from the SUMMARY table of 72 timings (DO Span= 167)
C     located near line 700+ in the output file and terminated with a banner>>>
C
C     ********************************************
C     THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *
C     ********************************************
C
C                  Computer : CRAY Y-MP1
C                  System   : UNICOS 5.1
C                  Compiler : CF77 4.0
C                  Date     : 06/03/90
C           .
C           .
C           .
C             MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
C             Mean DO Span   =   167
C             Code Samples   =    72
C
C             Maximum   Rate =    294.34   Mega-Flops/Sec.
C             Quartile  Q3   =    123.27   Mega-Flops/Sec.
C             Average   Rate =     82.71   Mega-Flops/Sec.
C             Geometric Mean =     43.42   Mega-Flops/Sec.
C             Median    Q2   =     31.14   Mega-Flops/Sec.
C             Harmonic  Mean =     23.20   Mega-Flops/Sec.
C             Quartile  Q1   =     17.16   Mega-Flops/Sec.
C             Minimum   Rate =      2.74   Mega-Flops/Sec.
C             <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
C             < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >
C             < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >
C             <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
C
C     Sadly some analysts quote only the long vector(DO span=471) LFK statistics
C     because they are the most impressive but they are not the best guide to
C     the performance of a large, diverse workload; the SUMMARY statistics are.
C
C     A complete LFK perform-range report must include the minimum, the Harmonic
C     Geometric, and Arithmetic means, the maximum and the standard deviation.
C     The best central measure is the Geometric Mean(GM) of 72 rates because the
C     GM is less biased by outliers than the Harmonic(HM) or Arithemetic(AM).
C     CRAY hardware monitors have demonstrated that net Mflop rates for the
C     LLNL and UCSD tuned workloads are closest to the 72 LFK test GM rate.
C
C
C        CORRELATION OF LFK TEST PERFORMANCE MEANS WITH LARGE WORKLOAD TUNING
C
C        -------      --------      ----------     -----------------------
C        Type of      CRAY-YMP1     Fraction       Tuning of Workload
C        Mean         (VL=167)      Flops in       Correlated with
C                     (MFlops)      Vector Ops     LFK Mean Performance
C        -------      --------      ----------     -----------------------
C
C         2*AM          165.0           .97        Best applications
C
C           AM           82.7           .89        Optimized applications
C
C           GM           43.4           .74        Tuned workload
C
C           HM           23.2           .45        Untuned workload
C
C           HM(scalar)   12.4           .0         All-scalar applications
C        -------      --------      ----------     -----------------------
C        (AM,GM,HM  stand for Arithmetic, Geometric, Harmonic Mean Rates)
C
C     Interpretation of LFK performance rates is discussed in Subr REPORT and:
C
C              F.H. McMahon,   The Livermore Fortran Kernels:
C              A Computer Test Of The Numerical Performance Range,
C              Lawrence Livermore National Laboratory,
C              Livermore, California, UCRL-53745, December 1986.
C
C****************************************************************************
C
C
C
C
C     DEVELOPMENT HISTORY OF THE LIVERMORE LOOPS TEST PROGRAM
C
C     The first version of the LFK Test (a.k.a. the Livermore Loops, circa
C     1970) consisting of 12 numerical Fortran kernels  was developed
C     and enhanced by F.H. McMahon unless noted otherwise below.
C     The author is grateful for the constructive criticism of colleagues:
C     J.Owens, H.Nelson, L.Berdahl, D.Fuss, L.Sloan, T.Rudy, M.Seager.
C     Since mainframe computers in that era all provided cpu-timers
C     with micro-second time resolution, each kernal was executed just
C     once and timed with negligible experimental timing errors.
C
C     In 1980 the number of Fortran samples was doubled to 24 kernels
C     to represent a broad range of computational structures that would
C     challenge a comiler's capability to generate optimal machine code.
C
C     In 1983 the LFK test driver was extended to execute all 24 kernels
C     three times using three sets of DO loop limits (Avg: 18, 89, 468)
C     since parallel computer performace depends on scale or granularity.
C     These 72 sample statistics are more robust and definitive.
C
C     In 1985 a repetition loop was placed around each kernel to execute
C     them long enough for accurate timing using the standard UNIX
C     timer ETIME which has a crude time resolution of 0.01 seconds.
C
C     In 1986 the LFK test driver was extended to run the entire test
C     seven times so that experimental timing errors for each of the
C     72 samples could be measured.  Reports of these timing errors
C     are necessary for honest scientific experiments. See App. B, C:
C
C           F.H.McMahon,   The Livermore Fortran Kernels:
C           A Computer Test Of The Numerical Performance Range,
C           Lawrence Livermore National Laboratory,
C           Livermore, California, UCRL-53745, December 1986.
C
C     In 1986 Greg Astfalk (AT&T) reprogrammed subroutine KERNEL containing
C     the 24 samples in the C language.  This C module can then be linked
C     with the standard Fortran LFK Test driver-program for testing under
C     identical benchmark conditions as the Fortran samples benchmark.
C     This C module was refined at LLNL by K.O'Hair, C.Rasbold, and M.Seager.
C  
C     In 1990 the repetition loops around each kernel were modified
C     following reports of some code-hoisting by global optimization.
C     These repetition loops were submerged into function TEST beyond
C     the scope of optimizers so the 72 samples are now bullet-proof.
C     New, highly accurate, convergent methods to measure overhead time
C     were implemented ( in VERIFY, SECOVT, TICK ).
C
C     In 1991 the LFK test runtime control MULTI was increased twenty fold
C     for accurate timing when crude UNIX timers having poor time resolution
C     (Tmin= 0.01 sec) were used on very fast computers.  This was only a
C     temporary fix since under UNIX each kernel must always be run
C     at least 1 sec for 1% accuracy despite ever increasing cpu speeds.
C     Thus new algorithms were implemented that automatically determine
C     appropriate values for MULTI which are sufficiently large for
C     accurate timing of the kernels in any system.  A new method
C     of repetition is used that allows MULTI to be increased indefinately
C     (MULTI >> 100) in future without causing overflow and still compute
C     verifiable checksums.  New checksums were generated using IEEE 754
C     standard floating-point hardware on SUN, SGI, and HP workstations.
C     Operational accuracy of the test program is assured in future.
C
C****************************************************************************
C
C
C
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      parameter( ntimes= 18 )
C
      CHARACTER  Komput*24, Kontrl*24, Kompil*24, Kalend*24, Identy*24 
      COMMON /SYSID/ Komput, Kontrl, Kompil, Kalend, Identy
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      DIMENSION  FLOPS(141), TR(141), RATES(141), ID(141)
      DIMENSION  LSPAN(141), WG(141), OSUM (141), TERR(141), TK(6)
      EXTERNAL VERIFY
CLOX  REAL*8 SECOND
CLLNL      CALL  DROPFILE (   '+MFLOPS' )
c                        Job start Cpu time
      cumtim(1)= 0.0d0
             ti= SECOND( cumtim(1))
C
c                                            Define your computer system:
       Komput  =  'Sun Sparc 2 (32 Mbyte)  '
       Kontrl  =  'Sun OS 4.1.1            '
       Kompil  =  'Various F90 comilers    '
       Kalend  =  'Late 1992               '
       Identy  =  'John K. Prentice, QCA   '
c
c                        Initialize variables and Open Files
           CALL  INDATA( TK, iou)
c                        Record name in active linkage chain in COMMON /DEBUG/
           CALL  TRACE (' MAIN.  ')
c
c                        Verify Sufficient Loop Size Versus Cpu Clock Accuracy
           CALL  VERIFY( iou )
             tj= SECOND( cumtim(1))
             nt= ntimes
c                        Define control limits:  Nruns(runs), Loop(time)
           CALL  SIZES(-1)
c
c                        Run test Mruns times Cpu-limited; I/O is deferred:
      DO 2    k= 1,Mruns
              i= k
             jr= MOD( i-1,7) + 1
           CALL  IQRAN0( 256)
c                        Run test using one of 3 sets of DO-Loop spans:
c                        Set iou Negative to supress all I/O during Cpu timing.
      DO 1    j= im,ml
             il= j
           tock= TICK( -iou, nt)
c
           CALL  KERNEL( TK)
    1 continue
           CALL  TRIAL( iou, i, ti, tj)
    2 continue
c
c                        Report timing errors, Mflops statistics:
      DO 3    j= im,ml
             il= j
           CALL  RESULT( iou,FLOPS,TR,RATES,LSPAN,WG,OSUM,TERR,ID)
c
c                Report  Mflops for Vector Cpus( short, medium, long vectors):
c
                 iovec= 0
        IF(      iovec.EQ.1 )  THEN
           CALL  REPORT( iou,   mk,mk,FLOPS,TR,RATES,LSPAN,WG,OSUM,ID)
        ENDIF
    3 continue
c                Report  Mflops SUMMARY Statistics: for Official Quotations
c
           CALL  REPORT( iou,3*mk,mk,FLOPS,TR,RATES,LSPAN,WG,OSUM,ID)
c
      cumtim(1)= 0.0d0
         totjob= SECOND( cumtim(1)) - ti - tsecov
          WRITE( iou,9) inseq, totjob, TK(1), TK(2)
          WRITE(   *,9) inseq, totjob, TK(1), TK(2)
    9    FORMAT( '1',//,' Version: 22/DEC/86  mf523 ',2X,I12,/,1P,
     .                  ' CHECK FOR CLOCK CALIBRATION ONLY: ',/,
     .                  ' Total Job    Cpu Time =  ',e14.5, ' Sec.',/,
     .                  ' Total 24 Kernels Time =  ',e14.5, ' Sec.',/,
     .                  ' Total 24 Kernels Flops=  ',e14.5, ' Flops')
C
C                        Optional Cpu Clock Calibration Test of SECOND:
c          CALL  CALIBR
      STOP
      END
c***********************************************
      BLOCK DATA
C***********************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  SUMS                                           REDUNDNT
C
C     l1 :=  param-dimension governs the size of most 1-d arrays
C     l2 :=  param-dimension governs the size of most 2-d arrays
C
C  ISPAN :=  Array of limits for DO loop control in the kernels
C  IPASS :=  Array of limits for multiple pass execution of each kernel
C  FLOPN :=  Array of floating-point operation counts for one pass thru kernel
C     WT :=  Array of weights to average kernel execution rates.
C  SKALE :=  Array of scale factors for SIGNEL data generator.
C   BIAS :=  Array of scale factors for SIGNEL data generator.
C
C    MUL :=  Array of multipliers * FLOPN  for each pass
C    WTP :=  Array of multipliers *    WT  for each pass
C     FR :=  Array of vectorisation fractions in REPORT
C   SUMW :=  Array of quartile weights in REPORT
C     IQ :=  Array of workload weights in REPORT
C   SUMS :=  Array of Verified Checksums of Kernels results: Nruns= 1 and 7.
C
C/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
C/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
C/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
C
C/      PARAMETER( l1=   27, l2=   15, l1d= 2*1001 )
C/      PARAMETER( l13=   8, l13h= 8/2, l213= 8+4, l813= 8*8 )
C/      PARAMETER( l14=  16, l16= 15, l416= 4*15 , l21= 15)
C
C
C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C/      PARAMETER( m1= 1001-1, m2= 101-1, m7= 1001-6 )
      parameter( nsys= 5, ns= nsys+1, nd= 11, nt= 4 )
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      CHARACTER  NAMES*8
      COMMON /TAGS/  NAMES(nd,nt)
      COMMON /RATS/  RATED(nd,nt)
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
C
      COMMON /PROOF/  SUMS(24,3,8)
C     ****************************************************************
C
      DATA  ( ISPAN(i,1), i= 1,47) /
     : 1001, 101, 1001, 1001, 1001, 64, 995, 100,
     : 101, 101, 1001, 1000, 64, 1001, 101, 75,
     : 101, 100, 101, 1000, 101, 101, 100, 1001, 23*0/
C
C*   : l1, l2, l1, l1, l1, l13, m7, m2,
C*   : l2, l2, l1, m1, l13, l1, l2, l16,
C*   : l2, m2, l2, m1, l21, l2, m2, l1, 23*0/
C
      DATA  ( ISPAN(i,2), i= 1,47) /
     : 101, 101, 101, 101, 101,  32, 101, 100,
     : 101, 101, 101, 100,  32, 101, 101,  40,
     : 101, 100, 101, 100,  50, 101, 100, 101,  23*0/
C
      DATA  ( ISPAN(i,3), i= 1,47) /
     : 27, 15, 27, 27, 27,  8, 21, 14,
     : 15, 15, 27, 26,  8, 27, 15, 15,
     : 15, 14, 15, 26, 20, 15, 14, 27,  23*0/
C
      DATA  ( IPASS(i,1), i= 1,47) /
     :   7, 67,  9, 14, 10,  3,  4, 10, 36, 34, 11, 12,
     :  36,  2,  1, 25, 35,  2, 39,  1,  1, 11,  8,  5,  23*0/
C
      DATA  ( IPASS(i,2), i= 1,47) /
     :   40, 40, 53, 70, 55,  7, 22,  6, 21, 19, 64, 68,
     :   41, 10,  1, 27, 20,  1, 23,  8,  1,  7,  5, 31,  23*0/
C
      DATA  ( IPASS(i,3), i= 1,47) /
     :   28, 46, 37, 38, 40, 21, 20,  9, 26, 25, 46, 48,
     :   31,  8,  1, 14, 26,  2, 28,  7,  1,  8,  7, 23,  23*0/
C
      DATA  (  MUL(i), i= 1,3) / 1, 2, 8 /
      DATA  (  WTP(i), i= 1,3) / 1.0, 2.0, 1.0 /
c
c     The following flop-counts (FLOPN) are required for scalar or serial
c     execution.  The scalar version defines the NECESSARY computation
c     generally, in the absence of proof to the contrary.  The vector
c     or parallel executions are only credited with executing the same
c     necessary computation.  If the parallel methods do more computation
c     than is necessary then the extra flops are not counted as through-put.
c
      DATA  ( FLOPN(i), i= 1,47)
     :     /5., 4., 2., 2., 2., 2., 16., 36., 17., 9., 1., 1.,
     :     7., 11., 33.,10., 9., 44., 6., 26., 2., 17., 11., 1., 23*0.0/
C
      DATA  ( WT(i), i= 1,47) /
     : 1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     : 1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     : 1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0, 23*0.0/
C
C
      DATA  ( SKALE(i), i= 1,47) /
     & 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0,
     & 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0,
     & 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0,
     & 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0,
     &       23*0.000D+0 /
C
c    : 0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,
c    : 0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,
c    : 0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  23*0.0/
C
      DATA  ( BIAS(i), i= 1,47) /
     : 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     : 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     : 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  23*0.0/
C
      DATA  ( FR(i), i= 1,9) /
     :  0.0, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 0.95, 1.0/
C
      DATA  ( SUMW(i), i= 1,7) /
     : 1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5/
C
      DATA  ( IQ(i), i= 1,7) /
     : 1, 2, 1, 2, 1, 2, 1/
C
C
C                                                 NEC SX-3/14
      DATA  ( NAMES(1,i), i= 1,3) /
     :        'NEC     ', 'SX-3/14 ', 'F77v.012' /
C
      DATA  ( RATED(1,i), i= 1,4) /
     :        311.82,  95.59,  38.73, 499.78 /
C                                                 CRAY-YMP/1
      DATA  ( NAMES(2,i), i= 1,3) /
     :        'CRAY    ', 'YMP/1   ', 'CFT771.2' /
C
      DATA  ( RATED(2,i), i= 1,4) /
     :         78.23,  36.63,  17.66,  86.75 /
C                                                 IBM 3090S180
c     DATA  ( NAMES(2,i), i= 1,3) /
c    :        'IBM     ', '3090s180', 'VSF2.2.0' /
C
c     DATA  ( RATED(2,i), i= 1,4) /
c    :         17.56,  12.23,   9.02,  16.32 /
C                                                 HP  9000/730
      DATA  ( NAMES(3,i), i= 1,3) /
     :        'HP      ', '9000/730', 'f77 8.05' /
C
      DATA  ( RATED(3,i), i= 1,4) /
     :         18.31,  15.72,  13.28,   9.68 /
C                                                 IBM 6000/540
      DATA  ( NAMES(4,i), i= 1,3) /
     :        'IBM     ', '6000/540', 'XL v0.90' /
C
      DATA  ( RATED(4,i), i= 1,4) /
     :         14.17,  10.73,   7.45,   9.59 /
C                                                 COMPAQ i486/25
      DATA  ( NAMES(5,i), i= 1,3) /
     :        'COMPAQ  ', 'i486/25 ', '        ' /
C
      DATA  ( RATED(5,i), i= 1,4) /
     :          1.15,   1.05,   0.92,   0.48 /
C
C
      DATA  START /0.0/, NPF/0/, ibuf/0/, match/0/, MULTI/200/, laps/1/
      DATA  npass/0/, nfail/0/, last/-1/
C
c MULTI= 200
c
      DATA  ( SUMS(i,1,5), i= 1,24 ) /
     &5.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     &5.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     &6.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     &1.202533961842803D+11,3.165553044000334D+09,3.943816690352042D+04,
     &5.650760000000000D+05,1.114641772902486D+03,1.015727037502300D+05,
     &5.421816960147207D+02,3.040644339351239D+07,1.597308280710199D+08,
     &2.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,5), i= 1,24 ) /
     &5.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     &5.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     &6.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     &9.816387810944345D+10,3.039983465145393D+07,3.943816690352042D+04,
     &6.480410000000000D+05,1.114641772902486D+03,1.015727037502300D+05,
     &5.421816960147207D+02,3.126205178815431D+04,7.824524877232093D+07,
     &2.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,5), i= 1,24 ) /
     &3.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     &5.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     &2.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     &1.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     &3.847124199949426D+10,2.923540598672011D+06,1.108997288134785D+03,
     &5.152160000000000D+05,2.947368618589360D+01,9.700646212337040D+02,
     &1.268230698051003D+01,5.987713249475302D+02,5.009945671204667D+07,
     &6.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
C
c MULTI= 100
c
      DATA  ( SUMS(i,1,4), i= 1,24 ) /
     &5.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     &5.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     &6.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     &4.958101723583047D+10,3.165278275112100D+09,3.943816690352042D+04,
     &2.825760000000000D+05,1.114641772902486D+03,7.507386432940455D+04,
     &5.421816960147207D+02,3.040644339351239D+07,8.002484742089500D+07,
     &2.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,4), i= 1,24 ) /
     &5.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     &5.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     &6.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     &3.542728632259964D+10,3.015943681556781D+07,3.943816690352042D+04,
     &3.240410000000000D+05,1.114641772902486D+03,7.507386432940455D+04,
     &5.421816960147207D+02,3.126205178815431D+04,3.916171317449981D+07,
     &2.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,4), i= 1,24 ) /
     &3.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     &5.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     &2.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     &1.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     &1.161063924078402D+10,2.609194549277411D+06,1.108997288134785D+03,
     &2.576160000000000D+05,2.947368618589360D+01,9.700646212337040D+02,
     &1.268230698051003D+01,5.987713249475302D+02,2.505599006414913D+07,
     &6.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
C
c MULTI= 50
c
      DATA  ( SUMS(i,1,3), i= 1,24 ) /
     &5.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     &5.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     &6.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     &2.217514090251080D+10,3.165140890667983D+09,3.943816690352042D+04,
     &1.413260000000000D+05,1.114641772902486D+03,6.203834985242972D+04,
     &5.421816960147207D+02,3.040644339351239D+07,4.017185709583275D+07,
     &2.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,3), i= 1,24 ) /
     &5.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     &5.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     &6.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     &1.430504282675192D+10,3.003923789762475D+07,3.943816690352042D+04,
     &1.620410000000000D+05,1.114641772902486D+03,6.203834985242972D+04,
     &5.421816960147207D+02,3.126205178815431D+04,1.961994537558922D+07,
     &2.938604376566697D+02,3.549900501563623D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,3), i= 1,24 ) /
     &3.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     &5.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     &2.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     &1.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     &3.899370197966012D+09,2.452021524580127D+06,1.108997288134785D+03,
     &1.288160000000000D+05,2.947368618589360D+01,9.700646212337040D+02,
     &1.268230698051003D+01,5.987713249475302D+02,1.253425674020030D+07,
     &6.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
C
c
c MULTI= 10     Old Checksums used before 1991 (longer run-times were needed)
c
      DATA  ( SUMS(i,1,2), i= 1,24 ) /
     &5.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     &5.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     &6.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     &4.057110454105199D+09,3.165030983112689D+09,3.943816690352042D+04,
     &2.832600000000000D+04,1.114641772902486D+03,5.165625410754861D+04,
     &5.421816960147207D+02,3.040644339351239D+07,8.289464835782872D+06,
     &2.938604376566697D+02,3.549834542443621D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,2), i= 1,24 ) /
     &5.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     &5.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     &6.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     &2.325318944820753D+09,2.994307876327030D+07,3.943816690352042D+04,
     &3.244100000000000D+04,1.114641772902486D+03,5.165625410754861D+04,
     &5.421816960147207D+02,3.126205178815431D+04,3.986531136460764D+06,
     &2.938604376566697D+02,3.549894609774404D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,2), i= 1,24 ) /
     &3.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     &5.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     &2.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     &1.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     &4.755211251524082D+08,2.326283104822299D+06,1.108997288134785D+03,
     &2.577600000000000D+04,2.947368618589360D+01,9.700646212337040D+02,
     &1.268230698051003D+01,5.987713249475302D+02,2.516870081041265D+06,
     &6.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
c
c MULTI=  1     Old Checksums used before 1986 (longer run-times were needed)
c
      DATA  ( SUMS(i,1,1), i= 1,24 ) /
     &5.114652693224671D+04,1.539721811668385D+03,1.000742883066363D+01,
     &5.999250595473891D-01,4.548871642387267D+03,4.375116344729986D+03,
     &6.104251075174761D+04,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.342910972650109D+07,2.907141294167248D-05,
     &4.468741170140841D+08,3.165006253912748D+09,3.943816690352042D+04,
     &2.901000000000000D+03,1.227055736845479D+03,4.932243865816480D+04,
     &5.421816960147207D+02,3.040644339351239D+07,1.115926577271652D+06,
     &2.938604376566697D+02,3.138872788135057D+04,5.000000000000000D+02/
c
      DATA  ( SUMS(i,2,1), i= 1,24 ) /
     &5.253344778937972D+02,1.539721811668385D+03,1.009741436578952D+00,
     &5.999250595473891D-01,4.589031939600982D+01,8.631675645333210D+01,
     &6.345586315784055D+02,1.501268005625798D+05,1.189443609974981D+05,
     &7.310369784325296D+04,3.433560407475758D+04,7.127569130821465D-06,
     &2.323352389500009D+08,2.992144295804055D+07,3.943816690352042D+04,
     &3.281000000000000D+03,1.114641772902486D+03,4.932243865816480D+04,
     &5.421816960147207D+02,3.126205178815431D+04,4.690129326568575D+05,
     &2.938604376566697D+02,3.228104575530876D+04,5.000000000000000D+01/
c
      DATA  ( SUMS(i,3,1), i= 1,24 ) /
     &3.855104502494961D+01,3.953296986903059D+01,2.699309089320672D-01,
     &5.999250595473891D-01,3.182615248447483D+00,1.120309393467088D+00,
     &2.845720217644024D+01,2.960543667875003D+03,2.623968460874250D+03,
     &1.651291227698265D+03,6.551161335845770D+02,1.943435981130448D-06,
     &4.143805389489125D+07,2.297991960376787D+06,1.108997288134785D+03,
     &2.592000000000000D+03,2.947368618589360D+01,9.700646212337040D+02,
     &1.268230698051003D+01,5.987713249475302D+02,2.629580827304779D+05,
     &6.109968728263972D+00,4.850340602749970D+02,1.300000000000000D+01/
C
C****************************************************************************
c
c     The following DP checksums are NOT used for the standard LFK
c     performance test but may be used to test Fortran compiler precision.
c
c     Checksums for Quadruple-Precision (IBM,DEC); or CRAY Double-Precision.
c     Quadruple precision checksums computed by Dr. D.S. Lindsay, HITACHI.
C     These Checksums were obtained with   MULTI= 10. (BLOCKDATA)
c     Change the numerical edit descriptor Q to D on CRAY systems.
CQc
CQ      DATA  ( SUMS(i,1,1), i= 1,24 )  /
CQ     a 0.5114652693224705102247326Q+05, 0.5150345372943066022569677Q+03,
CQ     b 0.1000742883066623145122027Q+02, 0.5999250595474070357564935Q+00,
CQ     c 0.4548871642388544199267412Q+04, 0.5229095383954675635496207Q+13,
CQ     d 0.6104251075163778121943921Q+05, 0.1501268005627157186827043Q+06,
CQ     e 0.1189443609975085966254160Q+06, 0.7310369784325972183233686Q+05,
CQ     f 0.3342910972650530676553892Q+08, 0.2907141428639174056565229Q-04,
CQ     g 0.4057110454105263471505061Q+10, 0.2982036205992255154832180Q+10,
CQ     h 0.3943816690352311804312052Q+05, 0.2832600000000000000000000Q+05,
CQ     i 0.1114641772903091760464680Q+04, 0.5165625410757306606559174Q+05,
CQ     j 0.5421816960150398899460410Q+03, 0.3040644339317275409518862Q+08,
CQ     k 0.8289464835786202431495974Q+07, 0.2938604376567099667790619Q+03,
CQ     l 0.3549834542446150511553453Q+05, 0.5000000000000000000000000Q+03/
CQc
CQ      DATA  ( SUMS(i,2,1), i= 1,24 )  /
CQ     a 0.5253344778938000681994399Q+03, 0.5150345372943066022569677Q+03,
CQ     b 0.1009741436579188086885138Q+01, 0.5999250595474070357564935Q+00,
CQ     c 0.4589031939602131581035992Q+02, 0.2693280957416549457193910Q+16,
CQ     d 0.6345586315772524401198340Q+03, 0.1501268005627157186827043Q+06,
CQ     e 0.1189443609975085966254160Q+06, 0.7310369784325972183233686Q+05,
CQ     f 0.3433560407476162346605343Q+05, 0.7127569144561925151361427Q-05,
CQ     g 0.2325318944820836005421577Q+10, 0.3045676741897511424188763Q+08,
CQ     h 0.3943816690352311804312052Q+05, 0.3244100000000000000000000Q+05,
CQ     i 0.1114641772903091760464680Q+04, 0.5165625410757306606559174Q+05,
CQ     j 0.5421816960150398899460410Q+03, 0.3126205178811007613028089Q+05,
CQ     k 0.3986531136462291709063170Q+07, 0.2938604376567099667790619Q+03,
CQ     l 0.3549894609776936556634240Q+05, 0.5000000000000000000000000Q+02/
CQc
CQ      DATA  ( SUMS(i,3,1), i= 1,24 )  /
CQ     a 0.3855104502494983491740258Q+02, 0.1199847611437483513040755Q+02,
CQ     b 0.2699309089321296439173090Q+00, 0.5999250595474070357564935Q+00,
CQ     c 0.3182615248448271678796560Q+01, 0.8303480073326955433087865Q+12,
CQ     d 0.2845720217638848365786224Q+02, 0.2960543667877649943946702Q+04,
CQ     e 0.2623968460874419268457298Q+04, 0.1651291227698377392796690Q+04,
CQ     f 0.6551161335846537217862474Q+03, 0.1943435981776804808483341Q-05,
CQ     g 0.4755211251524563699634913Q+09, 0.2547733008933910800455698Q+07,
CQ     h 0.1108997288135066584075059Q+04, 0.2577600000000000000000000Q+05,
CQ     i 0.2947368618590713935189324Q+02, 0.9700646212341513210532085Q+03,
CQ     j 0.1268230698051747067958265Q+02, 0.5987713249471801461035250Q+03,
CQ     k 0.2516870081042209239664473Q+07, 0.6109968728264795136407718Q+01,
CQ     l 0.4850340602751675804605762Q+03, 0.1300000000000000000000000Q+02/
CQc
      END
C
C
C***************************************
      SUBROUTINE CALIBR
C***********************************************************************
C                                                                      *
c     CALIBR - Cpu clock calibration tests accuracy of SECOND function.*
C                                                                      *
C     CALIBR tests function SECOND by using it to time a computation   *
C     repeatedly.  These SECOND timings are written to stdout(terminal)*
C     one at a time as the cpu-clock is read, so we can observe a real *
C     external clock time and thus check the accuracy of SECOND code.  *
C     Comparisons with an external clock require a stand-alone run.    *
C     Otherwise compare with system charge for total job cpu time.     *
C                                                                      *
C     Sample Output from CRAY-YMP1:                                    *
C                                                                      *
C                                                                      *
C CPU CLOCK CALIBRATION:  START STOPWATCH NOW !                        *
C           TESTS ACCURACY OF FUNCTION SECOND()                        *
C           Monoprocess this test, stand-alone, no TSS                 *
C           Verify  T or DT  observe external clock:                   *
C                                                                      *
C        -------     -------      ------      -----                    *
C        Total T ?   Delta T ?    Mflops ?    Flops                    *
C        -------     -------      ------      -----                    *
C  1        0.00        0.00        9.15    4.00000e+04    4.98000e-02 *
C  2        0.01        0.01       11.67    1.20000e+05    8.98000e-02 *
C  3        0.02        0.01       12.84    2.80000e+05    1.69800e-01 *
C  4        0.04        0.02       13.47    6.00000e+05    3.29800e-01 *
C  5        0.09        0.05       13.81    1.24000e+06    6.49800e-01 *
C  6        0.18        0.09       14.00    2.52000e+06    1.28980e+00 *
C  7        0.36        0.18       14.12    5.08000e+06    2.56980e+00 *
C  8        0.72        0.36       14.19    1.02000e+07    5.12980e+00 *
C  9        1.44        0.72       14.20    2.04400e+07    1.02498e+01 *
C 10        2.88        1.44       14.23    4.09200e+07    2.04898e+01 *
C 11        5.74        2.87       14.27    8.18800e+07    4.09698e+01 *
C 12       11.48        5.74       14.27    1.63800e+08    8.19298e+01 *
C 13       22.98       11.50       14.26    3.27640e+08    1.63850e+02 *
C 14       45.92       22.94       14.27    6.55320e+08    3.27690e+02 *
C 15       91.88       45.96       14.26    1.31068e+09    6.55369e+02 *
C***********************************************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      parameter( limitn= 101, ndim= limitn+10 )
      DIMENSION  X(ndim), Y(ndim), cumtim(10)
C
c     CALL TRACE ('CALIBR  ')
      cumtim(1)= 0.0d0
             t0= SECOND( cumtim(1))
C
          WRITE( *,111)
          WRITE( *,110)
          WRITE( *,112)
          WRITE( *,113)
          WRITE( *,114)
          WRITE( *,115)
          WRITE( *,114)
  111    FORMAT(//,' CPU CLOCK CALIBRATION:  START STOPWATCH NOW !')
  110    FORMAT('           TESTS ACCURACY OF FUNCTION SECOND()')
  112    FORMAT('           Monoprocess this test, stand-alone, no TSS')
  113    FORMAT('           Verify  T or DT  observe external clock:',/)
  114    FORMAT('           -------     -------      ------      -----')
  115    FORMAT('           Total T ?   Delta T ?    Mflops ?    Flops')
  119    FORMAT(4X,I2,3F12.2,2E15.5)
C
              l= 0
              n= 0
              m= 200
          nflop= 0
         totalt= 0.00d0
         deltat= 0.00d0
          flops= 0.00d0
             rn= 0.00d0
             t1= 0.00d0
             t2= 0.00d0
      cumtim(1)= 0.0d0
             t2= SECOND( cumtim(1))
             IF( t2.GT. 1.00d04 ) GO TO 911
             IF( t2.LT. 1.00d-8 ) GO TO 911
C
   10         l= l + 1
              m= m + m
C
           X(1)= 0.0098000d0
           Y(1)= 0.0000010d0
         DO 2 i= 2,limitn
           Y(i)= Y(1)
    2 continue
C                                  Compute LFK Kernel 11  m times
         DO 5 j= 1,m
         DO 4 k= 2,limitn
           X(k)= X(k-1) + Y(k)
    4 continue
           X(1)= X(limitn)
    5 continue
C
             t1= t2
      cumtim(1)= 0.0d0
             t2= SECOND( cumtim(1))
C                                  IF elapsed time can be observed, Print Mark.
         totalt= t2 - t0
         deltat= t2 - t1
          nflop= nflop + (limitn - 1) * m
             IF( deltat .GT. 2.00d0  .OR.  l.GT.12 )  THEN
                     n= n + 1
                    rn= REAL( nflop)
                 flops= 1.00d-6 *( REAL( nflop)/( totalt +1.00d-9))
                 WRITE( *,119)  l, totalt, deltat, flops, rn, X(limitn)
             ENDIF
             IF( deltat .LT. 200.0d0  .OR.  n.LT.3 )  GO TO 10
C
             IF( n.LE.0 )  THEN
                 WRITE( *,119)  l, totalt, deltat, flops, rn, X(limitn)
             ENDIF
      STOP
C
  911     WRITE( *,61)
          WRITE( *,62) totalt
      STOP
C
   61 FORMAT(1X,'FATAL(CALIBR): cant measure time using func SECOND()')
   62 FORMAT(/,13X,'using SECOND():  totalt=',1E20.8,' ?')
C
      END
C
C***********************************************
      SUBROUTINE INDEX
C***********************************************
C       MODULE     PURPOSE
C       ------     -----------------------------------------------
C
C       CALIBR     cpu clock calibration tests accuracy of SECOND function
C
C       INDATA     initialize variables
C
C       IQRANF     computes a vector of pseudo-random indices
C       IQRAN0     define seed for new IQRANF sequence
C
C       KERNEL     executes 24 samples of Fortran computation
C
C       PFM        optional call to system hardware performance monitor
C
c       RELERR     relative error between  u,v  (0.,1.)
C
C       REPORT     prints timing results
C
C       RESULT     computes execution rates  into pushdown store
C
C       SECOND     cumulative CPU time for task in seconds (M.K.S. units)
C
C       SECOVT     measures the Overhead time for calling   SECOND
C
C       SENSIT     sensitivity analysis of harmonic mean to 49 workloads
C
C       SEQDIG     computes nr significant, equal digits in pairs of numbers
C
C       SIGNEL     generates a set of floating-point numbers near 1.0
C
C       SIMD       sensitivity analysis of harmonic mean to SISD/SIMD model
C
C       SIZES      test and set the loop controls before each kernel test
C
C       SORDID     simple sort
C
C       SPACE      sets memory pointers for array variables.  optional.
C
C       SPEDUP     computes Speed-ups: A circumspect method of comparison.
C
C       STATS      calculates unweighted statistics
C
C       STATW      calculates   weighted statistics
C
C       SUMO       check-sum with ordinal dependency
C
C       SUPPLY     initializes common blocks containing type real arrays.
C
C       TALLY      computes average and minimum Cpu timings and variances.
C
C       TDIGIT     counts lead digits followed by trailing zeroes
C
C       TEST       Repeats and times the execution of each kernel
C
C       TESTS      Checksums and initializes the data for each kernel test
C
C       TICK       measures timing overhead of subroutine test
C
C       TILE       computes  m-tile value and corresponding index
C
C       TRACE ,TRACK    push/pop caller's name and serial nr. in /DEBUG/
C
C       TRAP       checks that index-list values are in valid domain
C
C       TRIAL      validates checksums of current run for endurance trial
C
C       VALID      compresses valid timing results
C
C       VALUES     initializes special values
C
C       VERIFY     verifies sufficient Loop size versus cpu clock accuracy
C
C       WATCH      can continually test COMMON variables and localize bugs
c
c  ------------ -------- -------- -------- -------- -------- --------
c  ENTRY LEVELS:   1        2        3        4        5        6
c  ------------ -------- -------- -------- -------- -------- --------
c               MAIN.    SECOND
c                        INDATA
c                        VERIFY   SECOND
c                                 SIZES    IQRAN0
c                                 STATS    SQRT
c                                 TDIGIT   LOG10
c                        SIZES    IQRAN0
c
c                        TICK     TEST     TESTS    SECOND
c                                                   SIZES
c                                                   SUMO
c                                                   VALUES   SUPPLY   SIGNEL
c                                                            IQRANF   MOD
c                                          SECOND
c                                 VALID    TRAP              TRAP
c                                 STATS    SQRT
c                                 IQRANF   MOD
c                                          TRAP
c                        KERNEL   SPACE
c                                 SQRT
c                                 EXP
c                                 TEST     TESTS    SECOND
c                                                   SIZES
c                                                   SUMO
c                                                   VALUES   SUPPLY   SIGNEL
c                                                            IQRANF   MOD
c                                          SECOND
c                        TRIAL    SEQDIG   LOG10    TDIGIT
c                                 IQRAN0
c
c                        RESULT   TALLY    SIZES    IQRAN0   TRAP
c                                          PAGE
c                                          STATS    SQRT
c
c                                 SEQDIG   LOG10    TDIGIT
c
c                        REPORT   VALID    TRAP
c                                 MOD
c                                 STATW    SORDID   TRAP
c                                          TILE
c                                          SQRT
c                                          LOG10
c                                 PAGE
c                                 TRAP
c                                 SENSIT   VALID    TRAP
c                                          SORDID   TRAP
c                                          PAGE
c                                          STATW    SORDID   TRAP
c                                                   TILE
c                                 SIMD     VALID    TRAP
c                                          STATW    SORDID   TRAP
c                                                   TILE
c                                 SPEDUP
C                        STOP
C
C
C
C
C    All subroutines also call TRACE , TRACK , and WATCH to assist debugging.
C
C
C
C
C
C
C
c    ------ ---- ------     -----   ------------------------------------
c    BASE   TYPE CLASS      NAME    GLOSSARY
c    ------ ---- ------     -----   ------------------------------------
c    SPACE0    R Array      BIAS  - scale factors for SIGNEL data generator
c    SPACE0    R Array      CSUM  - checksums of KERNEL result arrays
c    BETA      R Array      CSUMS - sets of CSUM for all test runs
c    BETA      R Array      DOS   - sets of TOTAL flops for all test runs
c    SPACE0    R Array      FLOPN - flop counts for one execution pass
c    BETA      R Array      FOPN  - sets of FLOPN for all test runs
c    SPACE0    R Array      FR    - vectorisation fractions; abscissa for REPORT
c    SPACES    I scalar     ibuf  - flag enables one call to SIGNEL
c    ALPHA     I scalar     ik    - current number of executing kernel
c    ALPHA     I scalar     il    - selects one of three sets of loop spans
c    SPACES    I scalar     ion   - logical I/O unit number for output
c    SPACEI    I Array      IPASS - Loop control limits for multiple-pass loops
c    SPACE0    I Array      IQ    - set of workload weights for REPORT
c    SPACEI    I Array      ISPAN - loop control limits for each kernel
c    SPACES    I scalar     j5    - datum in kernel 16
c    ALPHA     I scalar     jr    - current test run number (1 thru 7)
c    SPACES    I scalar     k2    - counter in kernel 16
c    SPACES    I scalar     k3    - counter in kernel 16
c    SPACES    I scalar     kr    - a copy of mk
c    SPACES    I scalar     laps  - multiplies Nruns for long Endurance test
c    SPACES    I scalar     Loop  - current multiple-pass loop limit in KERNEL
c    SPACES    I scalar     m     - temp integer datum
c    ALPHA     I scalar     mk    - number of kernels to evaluate .LE.24
c    ALPHA     I scalar     ml    - maximum value of il=  3
c    SPACES    I scalar     mpy   - repetiton counter of MULTI pass loop
c    SPACES    I scalar     Loops2- repetiton loop limit
c    ALPHA     I scalar     Mruns - number of complete test runs .GE.Nruns
c    SPACEI    I Array      MUL   - multipliers * IPASS defines Loop
c    SPACES    I scalar     MULTI - Multiplier used to compute Loop in SIZES
c    SPACES    I scalar     n     - current DO loop limit in KERNEL
c    SPACES    I scalar     n1    - dimension of most 1-D arrays
c    SPACES    I scalar     n13   - dimension used in kernel 13
c    SPACES    I scalar     n13h  - dimension used in kernel 13
c    SPACES    I scalar     n14   - dimension used in kernel 14
c    SPACES    I scalar     n16   - dimension used in kernel 16
c    SPACES    I scalar     n2    - dimension of most 2-D arrays
c    SPACES    I scalar     n21   - dimension used in kernel 21
c    SPACES    I scalar     n213  - dimension used in kernel 21
c    SPACES    I scalar     n416  - dimension used in kernel 16
c    SPACES    I scalar     n813  - dimension used in kernel 13
c    SPACE0    I scalar     npf   - temp integer datum
c    ALPHA     I Array      NPFS  - sets of NPFS1 for all test runs
c    SPACE0    I Array      NPFS1 - number of page-faults for each kernel
c    ALPHA     I scalar     Nruns - number of complete test runs .LE.7
c    SPACES    I scalar     nt1   - total size of common -SPACE1- words
c    SPACES    I scalar     nt2   - total size of common -SPACE2- words
c    BETA      R Array      SEE   - (i,1,jr,il) sets of TEST overhead times
c    BETA      R Array      SEE   - (i,2,jr,il) sets of csums of SPACE1
c    BETA      R Array      SEE   - (i,3,jr,il) sets of csums of SPACE2
c    SPACE0    R Array      SKALE - scale factors for SIGNEL data generator
c    SPACE0    R scalar     start - temp start time of each kernel
c    PROOF     R Array      SUMS  - sets of verified checksums for all test runs
c    SPACE0    R Array      SUMW  - set of quartile weights for REPORT
c    TAU       R scalar     tclock- minimum cpu clock time= resolution
c    SPACE0    R Array      TERR1 - overhead-time errors for each kernel
c    BETA      R Array      TERRS - sets of TERR1 for all runs
c    TAU       R scalar     testov- average overhead time in TEST linkage
c    BETA      R scalar     tic   - average overhead time in SECOND (copy)
c    SPACE0    R scalar     ticks - average overhead time in TEST linkage(copy)
c    SPACE0    R Array      TIME  - net execution times for all kernels
c    BETA      R Array      TIMES - sets of TIME for all test runs
c    SPACE0    R Array      TOTAL - total flops computed by each kernel
c    TAU       R scalar     tsecov- average overhead time in SECOND
c    SPACE0    R Array      WS    - unused
c    SPACE0    R Array      WT    - weights for each kernel sample
c    SPACEI    R Array      WTP   - weights for the 3 span-varying passes
c    SPACE0    R Array      WW    - unused
C
C
c  --------- -----------------------------------------------------------------
c   COMMON   Usage
c  --------- -----------------------------------------------------------------
C
C   /ALPHA /
C            VERIFY    TICK      TALLY     SIZES     RESULT    REPORT    KERNEL
C            MAIN.
C   /BASE1 /
C            SUPPLY
C   /BASE2 /
C            SUPPLY
C   /BASER /
C            SUPPLY
C   /BETA  /
C            TICK      TALLY     SIZES     RESULT    REPORT    KERNEL
C   /DEBUG /
C            TRACE     TRACK     TRAP
C   /ORDER /
C            TRACE     TRACK     TRAP
C   /PROOF /
C            RESULT    BLOCKDATA
C   /SPACE0/
C            VALUES    TICK      TEST      TALLY     SUPPLY    SIZES     RESULT
C            REPORT    KERNEL    BLOCKDATA
C   /SPACE1/
C            VERIFY    VALUES    TICK      TEST      SUPPLY    SPACE     KERNEL
C   /SPACE2/
C            VERIFY    VALUES    TICK      TEST      SUPPLY    SPACE     KERNEL
C   /SPACE3/
C            VALUES
C   /SPACEI/
C            VERIFY    VALUES    TICK      TEST      SIZES     RESULT    REPORT
C            KERNEL    BLOCKDATA
C   /SPACER/
C            VALUES    TICK      TEST      SUPPLY    SIZES     KERNEL
C   /SPACES/
C            VERIFY    VALUES    TICK      TEST      SUPPLY    SIZES     KERNEL
C            BLOCKDATA
c  --------- -----------------------------------------------------------------
c
c
c           SubrouTine Timing on CRAY-XMP1:
c
c           Subroutine   Time(%) All Scalar
c
c           KERNEL       52.24%
c           SUPPLY       17.85%
c           VERIFY        8.76%
c           VALUES        6.15%
c           STATS         5.44%
c           DMPY          1.97%
c           DADD          1.53%
c           EXP           1.02%
c           SQRT           .99%
c           SORDID         .81%
c           DDIV           .38%
c           IQRANF         .25%
c           SUMO           .22%
c           TRACE          .19%
c           SIGNEL         .16%
c           TRAP           .10%
c           TRACK          .10%
c           STATW          .08%
c           TILE           .04%
c           SIZES          .03%
c           ALOG10         .03%
c
c           Subroutine   Time(%)  Auto Vector
c
c           KERNEL       56.28%
c           VALUES       10.33%
c           STATS         8.57%
c           DADD          4.34%
c           DMPY          3.86%
c           VERIFY        2.61%
c           SUPPLY        2.28%
c           SQRT          2.10%
c           SORDID        1.84%
c           SUMO           .80%
c           DDIV           .78%
c           SDOT           .67%
c           TRACE          .53%
c           IQRANF         .50%
c           SIGNEL         .36%
c           EXP            .32%
c           TRACK          .23%
c           TRAP           .20%
c           ALOG10         .18%
c           STATW          .16%
c
c
      RETURN
      END
C
C***************************************
      SUBROUTINE INDATA( TK, iou)
C***************************************
C       INDATA     initialize variables
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      DIMENSION  TK(6)
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
       TK(1)= 0.00d0
       TK(2)= 0.00d0
      testov= 0.00d0
      ticks = 0.00d0
      tclock= 0.00d0
      tsecov= 0.00d0
      tic   = 0.00d0
C
      jr    = 1
      Nruns = 1
      il    = 1
      mk    = 1
      ik    = 1
C
      inseq = 0
      isave = 0
      iret  = 0
C
      Loops2= 1
      mpylim= Loops2
      mpy   = 1
      MULTI = 1
      mucho = 1
      L     = 1
      Loop  = 1
      LP    = Loop
      n     = 0
C
      iou   = 8
      ion   = iou
        CALL  INITIO( 8, 'output')
C       CALL  INITIO( 7, 'chksum')
C
      CALL TRACE ('INDATA  ')
CPFM       IF( INIPFM( ion, 0) .NE. 0 )  THEN
CPFM           CALL WHERE(20)
CPFM       ENDIF
C
CLLL.      call  Q8EBM
C
          WRITE (   *,7002)
          WRITE (   *,7003)
          WRITE (   *,7002)
          WRITE ( iou,7002)
          WRITE ( iou,7003)
          WRITE ( iou,7002)
 7002 FORMAT(  ' *********************************************' )
 7003 FORMAT(  ' THE LIVERMORE  FORTRAN KERNELS "MFLOPS" TEST:' )
      WRITE( iou, 797)
      WRITE( iou, 798)
  797 FORMAT(' >>> USE 72 SAMPLES LFK TEST RESULTS SUMMARY (line 330+)')
  798 FORMAT(' >>> USE ALL RANGE STATISTICS FOR OFFICIAL QUOTATIONS.  ')
      CALL TRACK ('INDATA  ')
      RETURN
      END
C
C*************************************************
      SUBROUTINE INITIO( iou, name )
C***********************************************************************
C                                                                      *
C       INITIO - Assign logdevice nr "iou" to disk file "name"         *
C                                                                      *
C          iou - logical i/o device number                             *
C         name - name to assign to disk file                           *
C                                                                      *
C***********************************************************************
      LOGICAL LIVING
      CHARACTER *(*) name
C     CALL TRACE ('INITIO  ')
C
           INQUIRE( FILE=name, EXIST= LIVING )
                IF( LIVING ) THEN
                    OPEN ( UNIT=iou, FILE=name, STATUS='OLD')
                    CLOSE( UNIT=iou, STATUS='DELETE')
               ENDIF
             OPEN  (UNIT=iou, FILE=name, STATUS='NEW')
C
C     CALL TRACK ('INITIO  ')
      RETURN
      END
C
C***************************************
      SUBROUTINE IQRAN0( newk)
C***************************************
C
c     IQRAN0  - define seed for new IQRANF sequence
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,K,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,K,O-Z)
C
      COMMON /IQRAND/ k0, k, k9
      CALL TRACE ('IQRAN0  ')
C
      IF( newk.LE.0 ) THEN
          CALL WHERE(1)
      ENDIF
      k = newk
C
      CALL TRACK ('IQRAN0  ')
      RETURN
      END
C
C***************************************
      SUBROUTINE IQRANF( M, Mmin,Mmax, n)
C***********************************************************************
C                                                                      *
c     IQRANF  - computes a vector of psuedo-random indices             *
c               in the domain (Mmin,Mmax)                              *
C                                                                      *
C     M     - result array ,  psuedo-random positive integers          *
C     Mmin  - input integer,  lower bound for random integers          *
C     Mmax  - input integer,  upper bound for random integers          *
C     n     - input integer,  number of results in M.                  *
C                                                                      *
C       M(i)= Mmin + INT( (Mmax-Mmin) * RANF(0))                       *
C                                                                      *
c        CALL IQRAN0( 256 )                                            *
c        CALL IQRANF( IX, 1,1001, 30)      should produce in IX:       *
c           3  674  435  415  389   54   44  790  900  282             *
c         177  971  728  851  687  604  815  971  155  112             *
c         877  814  779  192  619  894  544  404  496  505  ...        *
C                                                                      *
C     S.K.Park, K.W.Miller, Random Number Generators: Good Ones        *
C     Are Hard To Find, Commun ACM, 31(10), 1192-1201 (1988).          *
C***********************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,K,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,K,O-Z)
      DOUBLE  PRECISION  dq, dp, per, dk, spin, span                    REDUNDNT
C
      dimension  M(n)
      COMMON /IQRAND/ k0, k, k9
c     save k
      CALL TRACE ('IQRANF  ')
            IF( n.LE.0 )  GO TO 73
         inset= Mmin
          span= Mmax - Mmin
c         spin= 16807.00d0
c          per= 2147483647.00d0
          spin= 16807
           per= 2147483647
         realn= n
         scale= 1.0000100d0
             q= scale*(span/realn)
C
            dk= k
      DO  1  i= 1,n
            dp= dk*spin
c           dk=    DMOD( dp, per)
            dk= dp -INT( dp/per)*per
            dq= dk*span
          M(i)= inset + ( dq/ per)
            IF( M(i).LT.Mmin .OR. M(i).GT.Mmax )  M(i)= inset + i*q
    1 continue
             k= dk
C
C
ciC     double precision  k, ip, iq, id
ci         inset= Mmin
ci         ispan= Mmax - Mmin
ci         ispin= 16807
ci            id= 2147483647
ci             q= (REAL(ispan)/REAL(n))*1.00001
ciC
ci      DO  2  i= 1,n
ci            ip= k*ispin
ci             k=      MOD( ip, id)
ci            iq= k*ispan
ci          M(i)= inset + ( iq/ id)
ci            IF( M(i).LT.Mmin .OR. M(i).GT.Mmax )  M(i)= inset + i*q
ci    2 continue
C
            CALL TRAP( M, ' IQRANF  ', 1, Mmax, n)
C
   73 CONTINUE
      CALL TRACK ('IQRANF  ')
      RETURN
c     DATA  k /256/
c                        IQRANF TEST PROGRAM:
c      parameter( nrange= 10000, nmaps= 1001 )
c      DIMENSION  IX(nrange), IY(nmaps), IZ(nmaps), IR(nmaps)
c      COMMON /IQRAND/ k0, k, k9
cc
c        CALL  LINK( 'UNIT6=( output,create,text)//')
c         iou= 8
c      DO 7 j= 1,256,255
c      CALL IQRAN0( j )
c      CALL IQRANF( IX, 1, nmaps, nrange)
c      DO 1 i= 1,nmaps
c       IY(i)= 0
c    1  IZ(i)= 0
cc                     census for each index generated in (1:nmaps)
c      DO 2 i= 1,nrange
c    2 IY( IX(i))= IY( IX(i)) + 1
cc                     distribution of census tallies about nrange/nmaps
c      DO 3 i= 1,nmaps
c    3 IZ( IY(i))= IZ( IY(i)) + 1
c       IR(1)= IZ(1)
cc                     integral of distribution
c      DO 4 i= 1,nmaps
c    4  IR(i)= IR(i-1) + IZ(i)
c      WRITE( iou,112)   j, IR(nmaps), k
c      WRITE( iou,113) ( IX(i), i= 1,20 )
c      WRITE( iou,113) ( IY(i), i= 1,20 )
c      WRITE( iou,113) ( IZ(i), i= 1,20 )
c      WRITE( iou,113) ( IR(i), i= 1,20 )
c  112 FORMAT(/,1X,4I20)
c  113 FORMAT(20I4)
c    7 continue
c      STOP
c
c                   1                1000          1043618065
c  1 132 756 459 533 219  48 679 680 935 384 520 831  35  54 530 672   8 384  67
c 17  12   7  10  10  10  10  12   9   9   4  15  10   7   7   9   9   9  10  11
c  0   1   8  19  40  60  86 109 133 128 107 104  70  52  39  26   7   7   2   2
c  0   1   9  28  68 128 214 323 456 584 691 795 865 917 956 982 989 996 9981000
c
c                 256                1000           878252412
c  3 674 435 415 389  54  44 790 900 282 177 971 728 851 687 604 815 971 155 112
c 11  17  19   6  11  11   7   9  12   7  13   7   9  11  14   9   9  12   9   9
c  1   2  10  16  30  71  93 109 131 119 118 105  69  47  28  15  15   9   5   3
c  1   3  13  29  59 130 223 332 463 582 700 805 874 921 949 964 979 988 993 996
      END
C
C***********************************************
      SUBROUTINE KERNEL( TK)
C***********************************************************************
C                                                                      *
C            KERNEL     executes 24 samples of Fortran computation     *
c               TK(1) - total cpu time to execute only the 24 kernels. *
c               TK(2) - total Flops executed by the 24 Kernels         *
C***********************************************************************
C                                                                      *
C     L. L. N. L.   F O R T R A N   K E R N E L S:   M F L O P S       *
C                                                                      *
C   These kernels measure  Fortran  numerical  computation rates for a *
C   spectrum of  CPU-limited  computational  structures.  Mathematical *
C   through-put is measured  in  units  of  millions of floating-point *
C   operations executed per Second, called Mega-Flops/Sec.             *
C                                                                      *
C   This program  measures  a realistic  CPU performance range for the *
C   Fortran programming system  on  a  given day.  The CPU performance *
C   rates depend  strongly  on  the maturity of the Fortran compiler's *
C   ability to translate Fortran code into efficient machine code.     *
C   [ The CPU hardware  capability  apart  from  compiler maturity (or *
C   availability), could be measured (or simulated) by programming the *
C   kernels in assembly  or machine code directly.  These measurements *
C   can also  serve  as a framework for tracking the maturation of the *
C   Fortran compiler during system development.]                       *
C                                                                      *
C     Fonzi's Law: There is not now and there never will be a language *
C                  in which it is the least bit difficult to write     *
C                  bad programs.                                       *
C                                                    F.H.MCMAHON  1972 *
C***********************************************************************
C
C     l1 :=  param-dimension governs the size of most 1-d arrays
C     l2 :=  param-dimension governs the size of most 2-d arrays
C
C     Loop :=  multiple pass control to execute kernel long enough to time.
C     n  :=  DO loop control for each kernel.  Controls are set in subr. SIZES
C
C     ******************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
C/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
C/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
C/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C
C
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      INTEGER TEST, AND
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     2                AR,BR,C0,CR,DI,DK,
     3  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     4  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
C
CPFM  COMMON /KAPPA/ iflag1, ikern, statis(100,20), istats(100,20)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
C/      INTEGER    E,F,ZONE
C/      COMMON /ISPACE/ E(l213), F(l213),
C/     1  IX(l1), IR(l1), ZONE(l416)
C/C
C/      COMMON /SPACE1/ U(l1), V(l1), W(l1),
C/     1  X(l1), Y(l1), Z(l1), G(l1),
C/     2  DU1(l2), DU2(l2), DU3(l2), GRD(l1), DEX(l1),
C/     3  XI(l1), EX(l1), EX1(l1), DEX1(l1),
C/     4  VX(l14), XX(l14), RX(l14), RH(l14),
C/     5  VSP(l2), VSTP(l2), VXNE(l2), VXND(l2),
C/     6  VE3(l2), VLR(l2), VLIN(l2), B5(l2),
C/     7  PLAN(l416), D(l416), SA(l2), SB(l2)
C/C
C/      COMMON /SPACE2/ P(4,l813), PX(l21,l2), CX(l21,l2),
C/     1  VY(l2,l21), VH(l2,7), VF(l2,7), VG(l2,7), VS(l2,7),
C/     2  ZA(l2,7)  , ZP(l2,7), ZQ(l2,7), ZR(l2,7), ZM(l2,7),
C/     3  ZB(l2,7)  , ZU(l2,7), ZV(l2,7), ZZ(l2,7),
C/     4  B(l13,l13), C(l13,l13), H(l13,l13),
C/     5  U1(5,l2,2),  U2(5,l2,2),  U3(5,l2,2)
C
C     ******************************************************************
C
C
C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
C
C
care
C
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
C
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
C
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
C
C     ******************************************************************
C
      DIMENSION     ZX(1023), XZ(1500), TK(6)
      EQUIVALENCE ( ZX(1), Z(1)), ( XZ(1), X(1))
C
C
C//      DIMENSION       E(96), F(96), U(1001), V(1001), W(1001),
C//     1  X(1001), Y(1001), Z(1001), G(1001),
C//     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
C//     3  IX(1001), XI(1001), EX(1001), EX1(1001), DEX1(1001),
C//     4  VX(1001), XX(1001), IR(1001), RX(1001), RH(2048),
C//     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
C//     6  VE3(101), VLR(101), VLIN(101), B5(101),
C//     7  PLAN(300), ZONE(300), D(300), SA(101), SB(101)
C//C
C//      DIMENSION       P(4,512), PX(25,101), CX(25,101),
C//     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
C//     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
C//     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
C//     4  B(64,64), C(64,64), H(64,64),
C//     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
C//C
C//C     ******************************************************************
C//C
C//      COMMON /POINT/ ME,MF,MU,MV,MW,MX,MY,MZ,MG,MDU1,MDU2,MDU3,MGRD,
C//     1  MDEX,MIX,MXI,MEX,MEX1,MDEX1,MVX,MXX,MIR,MRX,MRH,MVSP,MVSTP,
C//     2  MVXNE,MVXND,MVE3,MVLR,MVLIN,MB5,MPLAN,MZONE,MD,MSA,MSB,
C//     3  MP,MPX,MCX,MVY,MVH,MVF,MVG,MVS,MZA,MZP,MZQ,MZR,MZM,MZB,MZU,
C//     4  MZV,MZZ,MB,MC,MH,MU1,MU2,MU3
C//C
C//      POINTER  (ME,E), (MF,F), (MU,U), (MV,V), (MW,W),
C//     1         (MX,X), (MY,Y), (MZ,Z), (MG,G),
C//     2         (MDU1,DU1),(MDU2,DU2),(MDU3,DU3),(MGRD,GRD),(MDEX,DEX),
C//     3         (MIX,IX), (MXI,XI), (MEX,EX), (MEX1,EX1), (MDEX1,DEX1),
C//     4         (MVX,VX), (MXX,XX), (MIR,IR), (MRX,RX), (MRH,RH),
C//     5         (MVSP,VSP), (MVSTP,VSTP), (MVXNE,VXNE), (MVXND,VXND),
C//     6         (MVE3,VE3), (MVLR,VLR), (MVLIN,VLIN), (MB5,B5),
C//     7         (MPLAN,PLAN), (MZONE,ZONE), (MD,D), (MSA,SA), (MSB,SB)
C//C
C//      POINTER  (MP,P), (MPX,PX), (MCX,CX),
C//     1         (MVY,VY), (MVH,VH), (MVF,VF), (MVG,VG), (MVS,VS),
C//     2         (MZA,ZA), (MZP,ZP), (MZQ,ZQ), (MZR,ZR), (MZM,ZM),
C//     3         (MZB,ZB), (MZU,ZU), (MZV,ZV), (MZZ,ZZ),
C//     4         (MB,B), (MC,C), (MH,H),
C//     5         (MU1,U1), (MU2,U2), (MU3,U3)
C..      COMMON DUMMY(2000)
C..      LOC(X)  =.LOC.X
C..      IQ8QDSP = 64*LOC(DUMMY)
C
C     ******************************************************************
C
C     STANDARD PRODUCT COMPILER DIRECTIVES MAY BE USED FOR OPTIMIZATION
C
CDIR$ VECTOR
CLLL. OPTIMIZE LEVEL i
CLLL. OPTION INTEGER (7)
CLLL. OPTION ASSERT (NO HAZARD)
CLLL. OPTION NODYNEQV
C
C     ******************************************************************
C       BINARY MACHINES MAY USE THE  AND(P,Q)  FUNCTION IF AVAILABLE
C       IN PLACE OF THE FOLLOWING CONGRUENCE FUNCTION (SEE KERNEL 13, 14)
C                                 IFF:   j= 2**N
c     IAND(j,k) = AND(j,k)
CLLL. IAND(j,k) = j.INT.k
c     MOD2N(i,j)= MOD(i,j)
      MOD2N(i,j)= IAND(i,j-1)
C                             i  is Congruent to  MOD2N(i,j)   mod(j)
C     ******************************************************************
C
C
C
C
C
      CALL TRACE ('KERNEL  ')
C
      CALL SPACE
C
CPFM       call  OUTPFM( 0, ion)
      mpy   = 1
      Loops2= 1
      mpylim= Loops2
      L     = 1
      Loop  = 1
      LP    = Loop
      it0   = TEST(0)
CPFM  iflag1= 13579
C
C*******************************************************************************
C***  KERNEL 1      HYDRO FRAGMENT
C*******************************************************************************
C
cdir$ ivdep
 1001    DO 1 k = 1,n
    1       X(k)= Q + Y(k) * (R * ZX(k+10) + T * ZX(k+11))
C
C...................
      IF( TEST(1) .GT. 0) GO TO 1001
C                   we must execute    DO k= 1,n  repeatedly for accurate timing
C
C*******************************************************************************
C***  KERNEL 2      ICCG EXCERPT (INCOMPLETE CHOLESKY - CONJUGATE GRADIENT)
C*******************************************************************************
C
C
 1002     II= n
       IPNTP= 0
  222   IPNT= IPNTP
       IPNTP= IPNTP+II
          II= II/2
           i= IPNTP+1
cdir$ ivdep
c:ibm_dir:ignore recrdeps (x)
C
      DO 2 k= IPNT+2,IPNTP,2
           i= i+1
    2   X(i)= X(k) - V(k) * X(k-1) - V(k+1) * X(k+1)
          IF( II.GT.1) GO TO 222
C
C...................
      IF( TEST(2) .GT. 0) GO TO 1002
C
C*******************************************************************************
C***  KERNEL 3      INNER PRODUCT
C*******************************************************************************
C
C
 1003      Q= 0.000d0
      DO 3 k= 1,n
    3      Q= Q + Z(k) * X(k)
C
C...................
      IF( TEST(3) .GT. 0) GO TO 1003
C
C*******************************************************************************
C***  KERNEL 4      BANDED LINEAR EQUATIONS
C*******************************************************************************
C
              m= (1001-7)/2
             fw= 1.000d-25
C
 1004 DO 404  k= 7,1001,m
             lw= k-6
           temp= XZ(k-1)
cdir$ ivdep
      DO   4  j= 5,n,5
         temp  = temp   - XZ(lw) * Y(j)
    4        lw= lw+1
        XZ(k-1)= Y(5) * temp
 404  CONTINUE
C
C...................
      IF( TEST(4) .GT. 0) GO TO 1004
C
C*******************************************************************************
C***  KERNEL 5      TRI-DIAGONAL ELIMINATION, BELOW DIAGONAL (NO VECTORS)
C*******************************************************************************
C
C
cdir$ novector
 1005 DO 5 i = 2,n
    5    X(i)= Z(i) * (Y(i) - X(i-1))
cdir$ vector
C
C...................
      IF( TEST(5) .GT. 0) GO TO 1005
C
C*******************************************************************************
C***  KERNEL 6      GENERAL LINEAR RECURRENCE EQUATIONS
C*******************************************************************************
C
C
 1006 DO  6  i= 2,n
          W(i)= 0.0100d0
      DO  6  k= 1,i-1
          W(i)= W(i)  + B(i,k) * W(i-k)
    6 CONTINUE
C
C...................
      IF( TEST(6) .GT. 0) GO TO 1006
C
C*******************************************************************************
C***  KERNEL 7      EQUATION OF STATE FRAGMENT
C*******************************************************************************
C
C
cdir$ ivdep
 1007 DO 7 k= 1,n
        X(k)=     U(k  ) + R*( Z(k  ) + R*Y(k  )) +
     .        T*( U(k+3) + R*( U(k+2) + R*U(k+1)) +
     .        T*( U(k+6) + Q*( U(k+5) + Q*U(k+4))))
    7 CONTINUE
C
C...................
      IF( TEST(7) .GT. 0) GO TO 1007
C
C
C*******************************************************************************
C***  KERNEL 8      A.D.I. INTEGRATION
C*******************************************************************************
C
C
 1008          nl1 = 1
               nl2 = 2
                fw= 2.000d0
      DO  8     kx = 2,3
cdir$ ivdep
      DO  8     ky = 2,n
            DU1(ky)=U1(kx,ky+1,nl1)  -  U1(kx,ky-1,nl1)
            DU2(ky)=U2(kx,ky+1,nl1)  -  U2(kx,ky-1,nl1)
            DU3(ky)=U3(kx,ky+1,nl1)  -  U3(kx,ky-1,nl1)
      U1(kx,ky,nl2)=U1(kx,ky,nl1) +A11*DU1(ky) +A12*DU2(ky) +A13*DU3(ky)
     .       + SIG*(U1(kx+1,ky,nl1) -fw*U1(kx,ky,nl1) +U1(kx-1,ky,nl1))
      U2(kx,ky,nl2)=U2(kx,ky,nl1) +A21*DU1(ky) +A22*DU2(ky) +A23*DU3(ky)
     .       + SIG*(U2(kx+1,ky,nl1) -fw*U2(kx,ky,nl1) +U2(kx-1,ky,nl1))
      U3(kx,ky,nl2)=U3(kx,ky,nl1) +A31*DU1(ky) +A32*DU2(ky) +A33*DU3(ky)
     .       + SIG*(U3(kx+1,ky,nl1) -fw*U3(kx,ky,nl1) +U3(kx-1,ky,nl1))
    8 CONTINUE
C
C...................
      IF( TEST(8) .GT. 0) GO TO 1008
C
C*******************************************************************************
C***  KERNEL 9      INTEGRATE PREDICTORS
C*******************************************************************************
C
C
 1009 DO 9  k = 1,n
      PX( 1,k)= DM28*PX(13,k) + DM27*PX(12,k) + DM26*PX(11,k) +
     .          DM25*PX(10,k) + DM24*PX( 9,k) + DM23*PX( 8,k) +
     .          DM22*PX( 7,k) +  C0*(PX( 5,k) +      PX( 6,k))+ PX( 3,k)
    9 CONTINUE
C
C...................
      IF( TEST(9) .GT. 0) GO TO 1009
C
C*******************************************************************************
C***  KERNEL 10     DIFFERENCE PREDICTORS
C*******************************************************************************
C
C
 1010 DO 10  k= 1,n
      AR      =      CX(5,k)
      BR      = AR - PX(5,k)
      PX(5,k) = AR
      CR      = BR - PX(6,k)
      PX(6,k) = BR
      AR      = CR - PX(7,k)
      PX(7,k) = CR
      BR      = AR - PX(8,k)
      PX(8,k) = AR
      CR      = BR - PX(9,k)
      PX(9,k) = BR
      AR      = CR - PX(10,k)
      PX(10,k)= CR
      BR      = AR - PX(11,k)
      PX(11,k)= AR
      CR      = BR - PX(12,k)
      PX(12,k)= BR
      PX(14,k)= CR - PX(13,k)
      PX(13,k)= CR
   10 CONTINUE
C
C...................
      IF( TEST(10) .GT. 0) GO TO 1010
C
C*******************************************************************************
C***  KERNEL 11     FIRST SUM.   PARTIAL SUMS.              (NO VECTORS)
C*******************************************************************************
C
C
 1011     X(1)= Y(1)
cdir$ novector
      DO 11 k = 2,n
   11     X(k)= X(k-1) + Y(k)
cdir$ vector
C
C...................
      IF( TEST(11) .GT. 0) GO TO 1011
C
C*******************************************************************************
C***  KERNEL 12     FIRST DIFF.
C*******************************************************************************
C
C
cdir$ ivdep
 1012 DO 12 k = 1,n
   12     X(k)= Y(k+1) - Y(k)
C
C...................
      IF( TEST(12) .GT. 0) GO TO 1012
C
C*******************************************************************************
C***  KERNEL 13      2-D PIC   Particle In Cell
C*******************************************************************************
C
                fw= 1.000d0
C
 1013 DO  13     k= 1,n
                i1= P(1,k)
                j1= P(2,k)
                i1=       1 + MOD2N(i1,64)
                j1=       1 + MOD2N(j1,64)
            P(3,k)= P(3,k)  + B(i1,j1)
            P(4,k)= P(4,k)  + C(i1,j1)
            P(1,k)= P(1,k)  + P(3,k)
            P(2,k)= P(2,k)  + P(4,k)
                i2= P(1,k)
                j2= P(2,k)
                i2=            MOD2N(i2,64)
                j2=            MOD2N(j2,64)
            P(1,k)= P(1,k)  + Y(i2+32)
            P(2,k)= P(2,k)  + Z(j2+32)
                i2= i2      + E(i2+32)
                j2= j2      + F(j2+32)
          H(i2,j2)= H(i2,j2) + fw
   13 CONTINUE
C
C...................
      IF( TEST(13) .GT. 0) GO TO 1013
C
C*******************************************************************************
C***  KERNEL 14      1-D PIC   Particle In Cell
C*******************************************************************************
C
C
               fw= 1.000d0
C
 1014 DO   141  k= 1,n
            VX(k)= 0.0d0
            XX(k)= 0.0d0
            IX(k)= INT(  GRD(k))
            XI(k)= REAL( IX(k))
           EX1(k)= EX   ( IX(k))
          DEX1(k)= DEX  ( IX(k))
 141  CONTINUE
C
      DO   142  k= 1,n
            VX(k)= VX(k) + EX1(k) + (XX(k) - XI(k))*DEX1(k)
            XX(k)= XX(k) + VX(k)  + FLX
            IR(k)= XX(k)
            RX(k)= XX(k) - IR(k)
            IR(k)= MOD2N(  IR(k),2048) + 1
            XX(k)= RX(k) + IR(k)
 142  CONTINUE
C
      DO  14    k= 1,n
      RH(IR(k)  )= RH(IR(k)  ) + fw - RX(k)
      RH(IR(k)+1)= RH(IR(k)+1) + RX(k)
  14  CONTINUE
C
C...................
      IF( TEST(14) .GT. 0) GO TO 1014
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C*******************************************************************************
C***  KERNEL 15     CASUAL FORTRAN.  DEVELOPMENT VERSION.
C*******************************************************************************
C
C
C       CASUAL ORDERING OF SCALAR OPERATIONS IS TYPICAL PRACTICE.
C       THIS EXAMPLE DEMONSTRATES THE NON-TRIVIAL TRANSFORMATION
C       REQUIRED TO MAP INTO AN EFFICIENT MACHINE IMPLEMENTATION.
C
C
 1015          NG= 7
               NZ= n
               AR= 0.05300d0
               BR= 0.07300d0
        DO 45  j = 2,NG
        DO 45  k = 2,NZ
               IF( j-NG) 31,30,30
   30     VY(k,j)= 0.0d0
                   GO TO 45
   31          IF( VH(k,j+1) -VH(k,j)) 33,33,32
   32           T= AR
                   GO TO 34
   33           T= BR
   34          IF( VF(k,j) -VF(k-1,j)) 35,36,36
   35           R= MAX( VH(k-1,j), VH(k-1,j+1))
                S= VF(k-1,j)
                   GO TO 37
   36           R= MAX( VH(k,j),   VH(k,j+1))
                S= VF(k,j)
   37     VY(k,j)= SQRT( VG(k,j)**2 +R*R)*T/S
               IF( k-NZ) 40,39,39
   39     VS(k,j)= 0.0d0
                   GO TO 45
   40          IF( VF(k,j) -VF(k,j-1)) 41,42,42
   41           R= MAX( VG(k,j-1), VG(k+1,j-1))
                S= VF(k,j-1)
                T= BR
                   GO TO 43
   42           R= MAX( VG(k,j),   VG(k+1,j))
                S= VF(k,j)
                T= AR
   43     VS(k,j)= SQRT( VH(k,j)**2 +R*R)*T/S
   45    CONTINUE
C
C...................
      IF( TEST(15) .GT. 0) GO TO 1015
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C*******************************************************************************
C***  KERNEL 16     MONTE CARLO SEARCH LOOP
C*******************************************************************************
C
            II= n/3
            LB= II+II
            k2= 0
            k3= 0
C
C
 1016        m= 1
            i1= m
  410       j2= (n+n)*(m-1)+1
      DO 470 k= 1,n
            k2= k2+1
            j4= j2+k+k
            j5= ZONE(j4)
            IF( j5-n      ) 420,475,450
  415       IF( j5-n+II   ) 430,425,425
  420       IF( j5-n+LB   ) 435,415,415
  425       IF( PLAN(j5)-R) 445,480,440
  430       IF( PLAN(j5)-S) 445,480,440
  435       IF( PLAN(j5)-T) 445,480,440
  440       IF( ZONE(j4-1)) 455,485,470
  445       IF( ZONE(j4-1)) 470,485,455
  450       k3= k3+1
            IF( D(j5)-(D(j5-1)*(T-D(j5-2))**2+(S-D(j5-3))**2
     .                        +(R-D(j5-4))**2)) 445,480,440
  455        m= m+1
            IF( m-ZONE(1) ) 465,465,460
  460        m= 1
  465       IF( i1-m) 410,480,410
  470 CONTINUE
  475 CONTINUE
  480 CONTINUE
  485 CONTINUE
C
C...................
      IF( TEST(16) .GT. 0) GO TO 1016
C
C*******************************************************************************
C***  KERNEL 17     IMPLICIT, CONDITIONAL COMPUTATION       (NO VECTORS)
C*******************************************************************************
C
C         RECURSIVE-DOUBLING VECTOR TECHNIQUES CAN NOT BE USED
C         BECAUSE CONDITIONAL OPERATIONS APPLY TO EACH ELEMENT.
C
                 dw= 5.0000d0/3.0000d0
                 fw= 1.0000d0/3.0000d0
                 tw= 1.0300d0/3.0700d0
cdir$ novector
C
 1017             k= n
                  j= 1
                ink= -1
              SCALE= dw
                XNM= fw
                 E6= tw
                     GO TO 61
C                                            STEP MODEL
  60             E6= XNM*VSP(k)+VSTP(k)
            VXNE(k)= E6
                XNM= E6
             VE3(k)= E6
                  k= k+ink
                 IF( k.EQ.j) GO TO  62
  61             E3= XNM*VLR(k) +VLIN(k)
               XNEI= VXNE(k)
            VXND(k)= E6
                XNC= SCALE*E3
C                                            SELECT MODEL
                 IF( XNM .GT.XNC) GO TO  60
                 IF( XNEI.GT.XNC) GO TO  60
C                                            LINEAR MODEL
             VE3(k)= E3
                 E6= E3+E3-XNM
            VXNE(k)= E3+E3-XNEI
                XNM= E6
                  k= k+ink
                 IF( k.NE.j) GO TO 61
   62 CONTINUE
cdir$ vector
C
C...................
      IF( TEST(17) .GT. 0) GO TO 1017
C
C*******************************************************************************
C***  KERNEL 18     2-D EXPLICIT HYDRODYNAMICS FRAGMENT
C*******************************************************************************
C
C
 1018           T= 0.003700d0
                S= 0.004100d0
               KN= 6
               JN= n
         DO 70  k= 2,KN
         DO 70  j= 2,JN
          ZA(j,k)= (ZP(j-1,k+1)+ZQ(j-1,k+1)-ZP(j-1,k)-ZQ(j-1,k))
     .            *(ZR(j,k)+ZR(j-1,k))/(ZM(j-1,k)+ZM(j-1,k+1))
          ZB(j,k)= (ZP(j-1,k)+ZQ(j-1,k)-ZP(j,k)-ZQ(j,k))
     .            *(ZR(j,k)+ZR(j,k-1))/(ZM(j,k)+ZM(j-1,k))
   70    CONTINUE
C
         DO 72  k= 2,KN
         DO 72  j= 2,JN
          ZU(j,k)= ZU(j,k)+S*(ZA(j,k)*(ZZ(j,k)-ZZ(j+1,k))
     .                    -ZA(j-1,k) *(ZZ(j,k)-ZZ(j-1,k))
     .                    -ZB(j,k)   *(ZZ(j,k)-ZZ(j,k-1))
     .                    +ZB(j,k+1) *(ZZ(j,k)-ZZ(j,k+1)))
          ZV(j,k)= ZV(j,k)+S*(ZA(j,k)*(ZR(j,k)-ZR(j+1,k))
     .                    -ZA(j-1,k) *(ZR(j,k)-ZR(j-1,k))
     .                    -ZB(j,k)   *(ZR(j,k)-ZR(j,k-1))
     .                    +ZB(j,k+1) *(ZR(j,k)-ZR(j,k+1)))
   72    CONTINUE
C
         DO 75  k= 2,KN
         DO 75  j= 2,JN
          ZR(j,k)= ZR(j,k)+T*ZU(j,k)
          ZZ(j,k)= ZZ(j,k)+T*ZV(j,k)
   75    CONTINUE
C
C...................
      IF( TEST(18) .GT. 0) GO TO 1018
C
C*******************************************************************************
C***  KERNEL 19      GENERAL LINEAR RECURRENCE EQUATIONS    (NO VECTORS)
C*******************************************************************************
C
 1019            KB5I= 0
C
C     IF( JR.LE.1 )  THEN
cdir$ novector
             DO 191 k= 1,n
           B5(k+KB5I)= SA(k) +STB5*SB(k)
                 STB5= B5(k+KB5I) -STB5
  191        CONTINUE
C     ELSE
C
             DO 193 i= 1,n
                    k= n-i+1
           B5(k+KB5I)= SA(k) +STB5*SB(k)
                 STB5= B5(k+KB5I) -STB5
  193        CONTINUE
C     ENDIF
cdir$ vector
C
C...................
      IF( TEST(19) .GT. 0) GO TO 1019
C
C*******************************************************************************
C***  KERNEL 20     DISCRETE ORDINATES TRANSPORT: RECURRENCE (NO VECTORS)
C*******************************************************************************
C
           dw= 0.200d0
cdir$ novector
C
 1020 DO 20 k= 1,n
           DI= Y(k)-G(k)/( XX(k)+DK)
           DN= dw
           IF( DI.NE.0.0) DN= MAX( S,MIN( Z(k)/DI, T))
         X(k)= ((W(k)+V(k)*DN)* XX(k)+U(k))/(VX(k)+V(k)*DN)
      XX(k+1)= (X(k)- XX(k))*DN+ XX(k)
   20 CONTINUE
cdir$ vector
C
C...................
      IF( TEST(20) .GT. 0) GO TO 1020
C
C*******************************************************************************
C***  KERNEL 21     MATRIX*MATRIX PRODUCT
C*******************************************************************************
C
C
 1021 DO 21 k= 1,25
      DO 21 i= 1,25
      DO 21 j= 1,n
      PX(i,j)= PX(i,j) +VY(i,k) * CX(k,j)
   21 CONTINUE
C
C...................
      IF( TEST(21) .GT. 0) GO TO 1021
C
C
C
C
C
C
C
C*******************************************************************************
C***  KERNEL 22     PLANCKIAN DISTRIBUTION
C*******************************************************************************
C
C
C      EXPMAX= 234.500d0
       EXPMAX= 20.0000d0
           fw= 1.00000d0
         U(n)= 0.99000d0*EXPMAX*V(n)
C
 1022 DO 22 k= 1,n
care       IF( U(k) .LT. EXPMAX*V(k))  THEN
                                            Y(k)= U(k)/V(k)
care                                   ELSE
care                                        Y(k)= EXPMAX
care    ENDIF
         W(k)= X(k)/( EXP( Y(k)) -fw)
   22 CONTINUE
C...................
      IF( TEST(22) .GT. 0) GO TO 1022
C
C*******************************************************************************
C***  KERNEL 23     2-D IMPLICIT HYDRODYNAMICS FRAGMENT
C*******************************************************************************
C
            fw= 0.17500d0
C
 1023 DO 23  j= 2,6
      DO 23  k= 2,n
            QA= ZA(k,j+1)*ZR(k,j) +ZA(k,j-1)*ZB(k,j) +
     .          ZA(k+1,j)*ZU(k,j) +ZA(k-1,j)*ZV(k,j) +ZZ(k,j)
   23  ZA(k,j)= ZA(k,j) +fw*(QA -ZA(k,j))
C
C...................
      IF( TEST(23) .GT. 0) GO TO 1023
C
C*******************************************************************************
C***  KERNEL 24     FIND LOCATION OF FIRST MINIMUM IN ARRAY
C*******************************************************************************
C
C      X( n/2)= -1.000d+50
       X( n/2)= -1.000d+10
C
 1024        m= 1
      DO 24  k= 2,n
            IF( X(k).LT.X(m))  m= k
   24 CONTINUE
C
C            m= imin1( n,x,1)        35 nanosec./element STACKLIBE/CRAY
C...................
      IF( TEST(24) .NE. 0) GO TO 1024
C
C*******************************************************************************
C
CPFM  iflag1= 0
           sum= 0.00d0
           som= 0.00d0
      DO 999 k= 1,mk
           sum= sum + TIME (k)
      TIMES(jr,il,k)= TIME (k)
      TERRS(jr,il,k)= TERR1(k)
      NPFS (jr,il,k)= NPFS1(k)
      CSUMS(jr,il,k)= CSUM (k)
      DOS  (jr,il,k)= TOTAL(k)
      FOPN (jr,il,k)= FLOPN(k)
           som= som + FLOPN(k) * TOTAL(k)
  999 continue
C
         TK(1)= TK(1) + sum
         TK(2)= TK(2) + som
C                                  Dumpout Checksums
c     WRITE ( 7,706) jr, il
c 706 FORMAT(1X,2I3)
c     WRITE ( 7,707) ( CSUM(k), k= 1,mk)
c 707 FORMAT(5X,'&',1PE21.15,',',1PE21.15,',',1PE21.15,',')
C
      CALL TRACK ('KERNEL  ')
      RETURN
      END
C***********************************************
      SUBROUTINE  PAGE( iou)
C***********************************************
      CALL TRACE ('PAGE    ')
      WRITE(iou,1)
    1 FORMAT('1')
c   1 FORMAT('')
      CALL TRACK ('PAGE    ')
      RETURN
      END
C
C********************************************
      FUNCTION  RELERR( U,V)
C********************************************
C
C       RELERR - RELATIVE ERROR BETWEEN  U,V  (0.,1.)
C            U - INPUT
C            V - INPUT
C********************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  x, y                                           REDUNDNT
C
      CALL TRACE ('RELERR  ')
                 w= 0.00d0
      IF( u .NE. v ) THEN
                 w= 1.00d0
                 o= 1.00d0
          IF( SIGN( o, u) .EQ. SIGN( o, v)) THEN
              a= ABS( u)
              b= ABS( v)
              x= MAX( a, b)
              y= MIN( a, b)
             IF( x .NE.  0.00d0) THEN
                 w= 1.00d0 - y/x
             ENDIF
          ENDIF
      ENDIF
C
      RELERR= w
      CALL TRACK ('RELERR  ')
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE REPORT( iou, ntk,nek,FLOPS,TR,RATES,LSPAN,WG,OSUM,ID)
C***********************************************************************
C                                                                      *
C     REPORT -  Prints Statistical Evaluation Of Fortran Kernel Timings*
C                                                                      *
C     iou    -  Logical Output Device Number                           *
C     ntk    -  Total number of Kernels to Edit in Report              *
C     nek    -  Number of Effective Kernels in each set to Edit        *
C     FLOPS  -  Array:  Number of Flops executed by each kernel        *
C     TR     -  Array:  Time of execution of each kernel(microsecs)    *
C     RATES  -  Array:  Rate of execution of each kernel(megaflops/sec)*
C     LSPAN  -  Array:  Span of inner DO loop in each kernel           *
C     WG     -  Array:  Weight assigned to each kernel for statistics  *
C     OSUM   -  Array:  Checksums of the results of each kernel        *
C***********************************************************************
c
c                                REFERENCES
c
c               F.H.McMahon,   The Livermore Fortran Kernels:
c               A Computer Test Of The Numerical Performance Range,
c               Lawrence Livermore National Laboratory,
c               Livermore, California, UCRL-53745, December 1986.
c
c        from:  National Technical Information Service
c               U.S. Department of Commerce
c               5285 Port Royal Road
c               Springfield, VA.  22161
c
c               J.T. Feo,  An Analysis Of The Computational And Parallel
c               Complexity Of The Livermore Loops, PARALLEL COMPUTING
c               (North Holland), Vol 7(2), 163-185, (1988).
c
c                                NOTICE
c
c               "This report was  prepared  as an account
c               of work  sponsored  by  the United States
c               Government.  Neither  the  United  States
c               nor the   United   States  Department  of
c               Energy, nor any  of  their employees, nor
c               any of their contractors, subcontractors,
c               or their employees,  makes  any warranty,
c               express or  implied, or assumes any legal
c               liability or   responsibility   for   the
c               accuracy, completeness  or  usefulness of
c               any information,  apparatus,  product  or
c               process disclosed, or represents that its
c               use would  not  infringe  privateiy-owned
c               rights."
c
c               Reference to  a  company  or product name
c               does not impiy approval or recommendation
c               of the   product  by  the  University  of
c               California or  the   U.S. Department   of
c               Energy to  the  exclusion  of others that
c               may be suitable.
c
c
c               Work performed under  the auspices of the
c               U.S. Department of Energy by the Lawrence
c               Livermore Laboratory    under    contract
c               number W-7405-ENG-48.
c
c***********************************************************************
c
c  Abstract
c
c  A computer performance  test that measures a realistic floating-point
c  performance range  for  Fortran applications is described.  A variety
c  of computer performance analyses may be easily carried out using this
c  small central  processing unit (cpu) test that would be infeasible or
c  too costly using complete applications as benchmarks, particularly in
c  the developmental  phase of an immature computer system.  The problem
c  of benchmarking numerical  applications  sufficiently,  especially on
c  new supercomputers,  is analyzed to identify several useful roles for
c  the Livermore Fortran  Kernal  (LFK) test.  The 24 LFK contain enough
c  samples of Fortran practice to expose many specific inefficiencies in
c  the formulation of the Fortran source, in the quality of compiled cpu
c  code, and   in   the  capability  of  the  instruction  architecture.
c  Examples show how the  LFK may be used to study compiled Fortran code
c  efficiency, to test the ability of compilers to vectorize Fortran, to
c  simulate mature coding  of  Fortran on new computers, and to estimate
c  the effective  subrange  of  supercomputer  performance  for  Fortran
c  applications.
c
c  Cpu performance  measurements   of  several  Fortran  benchmarks  and
c  numerical applications  that  correlate well with the cpu performance
c  range measured  by  the   LFK  test  are  presented.   The  numerical
c  performance metric  Mflops, first introduced in 1970 in this cpu test
c  to quantify the cpu  performance  range of numerical applications, is
c  discussed.  Analyses  of  the  LFK  performance results argue against
c  reducing the cpu performance  range  of  supercomputers  to  a single
c  number.  The  24  LFK  measured  rates  show  a realistic variance in
c  Fortran cpu  performance  that  is  essential  data  for  circumspect
c  computer evaluations.   Cpu performance data measured by the LFK test
c  on a number of recent computer systems are tabulated for reference.
c
c
c
c  I: FORTRAN CPU PERFORMANCE ANALYSIS
c
c
c     These kernels measure  Fortran numerical computation rates for a
c     spectrum of  CPU-limited computational structures or benchmarks.
c     The kernels benchmark  contains  extracts  or  kernels from more
c     than a score CPU-limited scientific application programs.  These
c     kernels are The most  important  CPU  time  components  from The
c     application programs.   This  benchmark  may  be easily extended
c     with important new kernels leaving performance statistics intact.
c
c     The time required  to  convert,  debug,  execute  and time many,
c     entire, large  programs  on  new  machines  each  having  a  new
c     implementation of  Fortran,   or   several   implementations  or
c     dialects rapidly  becomes  excessive.  Almost all The conversion
c     costs are in segments  of  The programs which are irrelevant for
c     evaluation of  The  CPU,  e.g.,  I/O, Fortran variations, memory
c     allocation, overlays,   job  control,   etc.    all   of   these
c     complexities are reduced to a single, small benchmark which uses
c     a minimum of I/O  and  a  single level of storage.  further, the
c     computation in  the  kernels  is  the  most  stable  part of the
c     Fortran language.
c
c     The kernels benchmark  is sufficient to determine a range of CPU
c     performance for  many  different  computational  structures in a
c     single computer run.   Since The range in performance is usually
c     large the  mean  has  a secondary significance.  To estimate the
c     performance of  a particular,  CPU-limited  application  program
c     select the  case(s) which are most similar to the application as
c     most relevent to the  estimate.   The  performance  ratio  of  a
c     kernel on  two  different  machines or compiled by two different
c     compilers on the same  machine  will  approximate  the  ratio of
c     through-puts for   an  application  which  is  very  similar  in
c     structure.
c
c     This set of kernels was chosen to measure lower and upper bounds
c     for scalar Fortran computation rates.  The upper bound on scalar
c     rates serves as a base  to  evaluate the effectiveness of vector
c     computation.  The  kind  of  Fortran  which  has the highest MIP
c     rates is pure arithmetic  in  DO-loops where complete local code
c     optimization by a Fortran compiler is possible.  All other kinds
c     of Fortran  operations  execute  at  much  lower  MIP  rates  on
c     multiple register machines (these ops may not be necessary).
c
c     Through-put is  measured  in  units of floating-point operations
c     executed per micro-second;  called  results  per micro-second or
c     mega-flops.  The  Mflop is a measure of the NECESSARY results in
c     a scientific application  program  regardless  of  the number or
c     kind of  operations  or processing.  The ratio of Mflops for two
c     different machines will approximate  the  ratio  of through-puts
c     for the  majority  of compute-limited scientific applications on
c     the two  machines.    The   kernels  measure  performance  scale
c     factors.
c
c
c II: FORTRAN PROGRAMMING SYSTEM MATURITY
c
c     Hardware performance   gains   depend   criticaly   on  compiler
c     maturity.  These  kernels   measure  the  joint  performance  of
c     hardware and  Fortran  compiler  software and may easily be used
c     for a comparative  analysis  of  all  the available compilers or
c     options on a given machine.  For a new or proposed machine where
c     no compiler is available  the  performance  may  be estimated by
c     simulating a  reasonable  compilation.  An example of simulation
c     rationale is given below.
c
c     Fortran compilers for new  types  of  machines require a lengthy
c     development cycle  to  achive  an  effective  level  of  machine
c     utilization.  A fully  mature  compiler  may not be completed in
c     the first  years  of  a  new machine.  Indeed, maturity is not a
c     stationary state   but  evolves   with   advances   in   program
c     optimization techniques.   Some  of  these  techniques depend on
c     special facilities in the  new  machines and serious development
c     and implementation cannot start much earlier than development of
c     the new machine.   Assumptions  on  the  maturity  of  available
c     Fortran compilers  are  crucial  to  the  evaluation  of Fortran
c     performance and  thus,   compiler   characteristics   should  be
c     explicit parameters of the performance analysis.
c
c
c -----------------------------------------------------------------------------
c III: A CPU Performance Metric For Computational Physics:    Mega-Flops/sec.
c -----------------------------------------------------------------------------
c
c
c A:  Floating-Point Instructions:  The Necessary Mathematics
c
c Computational physics applies  systems  of  PDEs from Mathematical physics to
c simulate the  evolution of physical systems.  The mathematical methods depend
c on real  valued  functions   and   the   algorithms  are  programmed,  almost
c exclusively, in  Fortran  Floating-point  computer operations (Flops).  These
c floating-point operations  are,  unquestionably,   the   NECESSARY   computer
c operations on  ANY  computer  and  the  total  number  is  INVARIANT.  Thus a
c meaningful computation rate can  always  be  measured  by  counting the total
c number of Flops and dividing by the total execution time of a program.
c
c B:   Procedural Machine  Instructions:   Artifices Of An Archetecture
c
c All of  the non-arithmetic instructions in a machine program are artifices of
c a particular hardware  architecture,  i.e.  machine dependant, as well as the
c result of  a  particular compiler's imperfect coding techniques.  How many of
c these procedural machine  instructions  are  strictly  necessary  can only be
c determined by further, tedious analysis which is ALWAYS machine dependant.  A
c famous example  of software  masking  hardware  capabilities  is  the  PASCAL
c compiler written  by  n.Wirth  which  used  only  50%  of  the command set to
c generate machine programs for the CDC-7600.
c
c Unless the next generation computer design is constrained for some reason, to
c closely resemble  its  obsolete  predecessor,  the  instruction  mix  used in
c current machines is not necessarily  relevent.   Furthermore, the instruction
c mix is  not  a  definitive  characterization  of the intrinsic physics or the
c mathematical algorithms.
c
c  1.  Primary Memory Access Instructions
c
c The number of memory  instructions  that  are necessary for a given algorithm
c depends strongly  on  the  number  and  kind of CPU registers and is a highly
c machine dependent number.   Operating registers, scratch-pad memories, vector
c buffers, short-stop  and  feed-back paths in the cpu are examples of hardware
c artifices which reduce the  number  of  primary memory operations.  Compilers
c and other  coders must make intelligent use of these particular cpu resources
c to minimize memory operations  and this is generally not the case, as is well
c known.
c
c  2.  Branching Instructions
c
c Branching instructions   are   the  slowest  and  most  expensive  procedural
c instructions and are very  often  unecessary.  Here the source programmer has
c primary responsibility  to  minimize  branching in the program by avoiding IF
c statements whenever possible  by  using MAX, MIN, or merge functions like
c CSMG.  Careful  logical  reduction  and  placement of IF tests is required to
c minimize the execution of branching operations.  Compilers can do very little
c to change or optimize the branch graph specified in the source program.
c
c On vector  computers ALL IF tests over mesh or array (state) variables can be
c eliminated.  Conditional computation can be vectorized by direct construction
c using explicit  sub-set mappings.  Vector relationals replace the IF clauses.
c Then sparse,  one-to-one   mappings  called  vector  Compress/Decompress  and
c one-to-many mappings   called   vector   Gather/Scatter   are  necessary  and
c sufficient to compose sub-vector operands for simple vector operations.
c
c
c
c
c
c IV: PERFORMANCE MEASUREMENTS
c
c
c     Through-put is measured in units of millions of floating-point
c     operations executed per second, called mflops.
c
c
c     Artificially long computer  runs do not have to be contrived for
c     timing on  machines  where  a cpu clock may be read in job mode.
c     Statistics on  the accuracy  of  the  timing  method  should  be
c     measured.
c
c     Net mflops is meaningful only if real run time of each kernel
c     is adjusted such that it weights the total time in proportion
c     to the actual usage of that catagory of computation in the
c     total workload.
c
c
c
c
c
c   1. Assignment Of Weights To Floating-Point Operations
c
c     Weights are assigned to different kinds of floating-point
c     operations to normalize their hardware execution time to
c     addition time so that the flop rates computed for various
c     Fortran Kernels will be commensurable.
c
c                           +,-,*   1
c                          /,SQRT   4
c                     EXP,SIN,ETC.  8
c                     IF(X.REL.Y)   1
c
c
c     Each Kernel flop-count is the weighted number of flops required for
c     serial execution.  The scalar version defines the NECESSARY computation
c     generally, in the absence of proof to the contrary.  The vector
c     or parallel executions are only credited with executing the same
c     necessary computation.  If the parallel methods do more computation
c     than is necessary then the extra flops are not counted as through-put.
c
c
c    2. SAMPLE OUTPUT:               CDC-7600/FTN-4.4
c
c                 KERNEL  FLOPS   TIME   MFLOPS
c                      1    500    94.4    5.30
c                      2    300    45.3    6.62
c                      3    100    21.9    4.57
c                      4    300   109.3    2.75
c                      5    100    25.6    3.91
c                      6    100    27.8    3.60
c                      7    640    88.2    7.25
c                      8   1440   249.0    5.78
c                      9    680   123.2    5.52
c                     10    360   102.8    3.50
c                     11     49    34.8    1.41
c                     12     49    18.3    2.68
c                     13    224   107.7    2.08
c                     14   3300   809.3    4.08
c                     15   3960  1769.5    2.24
c                     16    530   320.3    1.65
c                     17    405    92.2    4.39
c                     18   6600  1121.5    5.88
c                     19    540   105.8    5.11
c                     20   1300   266.0    4.89
c                     21   1250   370.9    3.37
c                     22   1700   601.9    2.82
c                     23   1650   362.4    4.55
c                     24    200   171.7    1.16
c
c                      AVERAGE  RATE =     3.96 MEGA-FLOPS/SEC.
c                      MEDIAN   RATE =     4.08 MEGA-FLOPS/SEC.
c                      HARMONIC MEAN =     3.15 MEGA-FLOPS/SEC.
c                      STANDARD DEV. =     1.61 MEGA-FLOPS/SEC.
c
c                                                    F.H.MCMAHON  1972
c
c
c
c
c
c
c    3. INTERPRETATION OF OUTPUT FILE FROM SUBROUTINE REPORT:
c
c
c
c  The highly instrumented LFK test program measures the effective cpu
c  performance range and has sufficient timed samples for many statisical
c  analyses thus avoiding the PERIL of a SINGLE performance "rating".
c  A COMPLETE REPORT OF LFK TEST RESULTS MUST QUOTE THE PERFORMANCE RANGE
c  STATISTICS BASED ON THE SUMMARY OF 72 TIMED SAMPLES:  the minium,
c  the equi-weighted harmonic, geometric, and arithmetic means and the maximum
c  rates.  The standard deviation must also be quoted to show the variance
c  in performance rates.  NO SINGLE RATE QUOTATION IS SUFFICIENT OR HONEST.
c
c  The LFK test (Livermore loops) outputs data for three benchmarking contexts
c  following print-outs of cpu clock checks and experimental timing errors:
c
c
c
c  1. Conventional "Balanced"  Cpus,  e.g. PCs, DEC-VAXs, IBM-370s.
c
c    1.1. [Refer to SUMMARY of 72 timings on pp.9-10 of LFK test OUTPUT file.
c         The bottom line is the set of nine performance range statistics
c         min thru max plus standard deviation listed after SUMMARY table.
c         These statistics may be used for computer comparisons as shown
c         in figure 11, p.24 of the LFK report UCRL-53745.  Ratios of the
c         range statistics from two computers show the range of speed-ups.]
c
c    1.2. An all-scalar coded LFK test (NOVECTOR) measures the basal scalar,
c         mono-processor computing capability.
c
c
c
c  2. Vector "Unbalanced"  Cpus,  e.g. CRAY, NEC, IBM-3090.
c
c    2.1. [Pages 2-8 of the LFK test OUTPUT file analyzes three different
c         runs of the 24 Livermore loops with short, medium, and long DO
c         loop spans (vector lengths).  The performance range statistics
c         for each of these three runs on vector computers should be compared
c         as shown in figure 12, p.25 of the LFK report UCRL-53745.]
c
c    2.2  The performance rates of most applications on vector computers are
c         observed in a sub-range from approximately the harmonic mean through
c         the mean rate of the 24 LFK samples (thru the two middle quartiles).
c
c         2.2.1  The equi-weighted arithmetic mean (AM) of 72 LFK rates
c                correlates with highly vectorised applications in the workload,
c                (80%-90% of flops) because the average is dominated by the high
c                vector rates.  Very highly vectorised applications (95%-99%+)
c                may run several times the average rate (figure 10, p21, ibid).
c
c         2.2.2  The equi-weighted harmonic mean (HM) of 72 LFK rates
c                correlates with poorly vectorised applications in the workload,
c                (30%-40% of flops) because the HM is dominated by the low
c                scalar rates.  An all-scalar coded LFK test (NOVECTOR)
c                measures the basal scalar, mono-processor computing capability.
c
c         2.2.3  The best central measure is the Geometric Mean(GM) of 72 rates,
c                because it is least biased by outliers.  CRAY hardware monitors
c                have demonstrated net Mflop rates for the LLNL and UCSD
c                workloads are closest to the 72 LFK test geometric mean rate.
c
c
c
c  3. Parallel "Unbalanced"  Cpus,  e.g. CRAY, NEC, IBM-3090.
c
c    3.1. The lower,   uni-processor bound of an MP system is given by 1.2.
c
c    3.2. The upper, multi-processor bound of an MP system is estimated by
c         multiplying the LFK performance statistics from 1.2 or from 2.2.
c         by N, the number of processors.
c
c
c
c
c      Comparision of two or more computers should make use of all the
c  performance range statistics in the tables below ( DO span= 167):
c  the extrema, the mean rates, and the standard deviation.
c  NO SINGLE MFLOPS RATE QUOTATION IS SUFFICIENT OR HONEST.
c  If the performance range is very large the causes and implications should
c  be fully explored.  Use of a single mean statistic is insufficient
c  but may be valid if the three mean rates are close in value and the
c  standard deviation is relatively small.  The geometric mean is a
c  better central measure than the median which depends on one value
c  in a small set.  The least biased central measure is the geometric
c  mean because it is less sensitive to outliers than either the average
c  or the harmonic mean.  When the computer performance range is very
c  large the net Mflops rates of many Fortran programs and workloads
c  have been observed to be in the sub-range between the equi-weighted
c  harmonic and arithmetic means depending on the degree of code
c  parallelism and optimization(Ref. 1).  Note that LFK mean Mflops rates
c  also imply the average efficiency of a computing system since
c  the peak rate is a well known constant.
c
c      The performance data shown for the computers below will be subject to
c  change with time.  Effective Cpu performance may improve as the programming
c  system software matures or effective performance may regress when the system
c  is oversubscribed.  We have observed degraded performance for the LFK test
c  in virtual storage systems when the working set size was too small, and in
c  multiprogramming or multiprocessing systems which were either immature or
c  very active.  In these active environments the LFK test measures a real
c  Cpu degradation in the effectiveness of caching data and data access
c  generally.  It is necessary to run the LFK test stand-alone to have
c  reproducable performance measurements.
c
c      The performance data sets tabulated below which have 72 sample
c  timings are a combination of three 24 sample sets produced by the
c  LFK test.  Statistics on the 72 sample data set are more significant
c  and these statistics should be quoted ( DO span= 167).
c
c
c
c
c
c
c                          REFERENCES
c
c         F.H.McMahon,   The Livermore Fortran Kernels:
c         A Computer Test Of The Numerical Performance Range,
c         Lawrence Livermore National Laboratory,
c         Livermore, California, UCRL-53745, December 1986.
c
c  from:  National Technical Information Service
c         U.S. Department of Commerce
c         5285 Port Royal Road
c         Springfield, VA.  22161
c
c
c         F.H.McMahon, "The Livermore Fortran Kernels Test of the Numerical
c         Performance Range", in Performance Evaluation of Supercomputers
c         (J.L.Martin, ed., North Holland, Amsterdam), 143-186(1988).
c
c
c         J.T. Feo,  An Analysis Of The Computational And Parallel
c         Complexity Of The Livermore Loops, PARALLEL COMPUTING
c         (North Holland), Vol 7(2), 163-185, (1988).
c
c
c         F.H.McMahon, "Measuring the Performance of Supercomputers",
c         in Energy and Technology Review (A.J.Poggio,ed.),
c         Lawrence Livermore National Laboratory, UCRL-52000-88-5, (1988).
c
c
c
c
c    The range of speed-ups shown below as ratios of the performance
c    statistics has a small variance compared to the enormous
c    performance ranges; the range of speed-ups are convergent estimates.
c    Report all nine performance range statistics on 72 samples, e.g.:
c
c
c
c
c D.117 LFK Test   117.1      117.2      117.3      117.4      117.5      117.6
c ------------- ---------- ---------- ---------- ---------- ---------- ---------
c   Vendor       CRAY RI    CRAY RI    CRAY RI    CDC        IBM        NEC
c   Model        XMP1 8.5   YMP1       2          ETA10-G    3090S180   SX-2
c   OSystem      COS 1.16   COS 1.16   UNICOS     EOS1.2J2   MVS2.2.0   SXOS1.21
c   Compiler     CFT771.2   CFT771.2   CFT771.3   F200 690   VSF2.3.0   F77/SX24
c   OptLevel     Vector     Vector     Vector     VAST2.25   Vector     Vector
c   NR.Procs          1          1          1          1          1          1
c   Samples          72         72         72         72         72         72
c   WordSize         64         64         64         64         64         64
c   DO Span         167        167        167        167        167        167
c   Year           1987       1988       1988       1988       1989       1986
c   Kernel/MFlops--------- ---------- ---------- ---------- ---------- ---------
c          1       183.57     258.64     160.17     405.57      56.03     800.05
c          2        42.49      67.09      21.61      12.55       8.88      49.94
c          3       173.19     236.67     111.93     233.09      53.66     528.67
c          4        65.68      95.05      47.45      59.48      40.72     164.18
c          5        15.89      18.69      13.01      11.86       8.83      11.26
c          6        12.91      20.58      13.07      13.13       8.57      29.30
c          7       207.28     295.48     228.00     488.07      62.08    1042.33
c          8       149.44     232.41     189.47     242.77      46.19     415.68
c          9       178.50     251.07     195.24     186.88      61.70     705.28
c         10        78.50     111.42      73.20      82.68       8.57     120.75
c         11        12.02      16.52      12.39       7.11       6.84       8.32
c         12        81.14     112.50      57.52     227.40      18.18     242.80
c         13         5.89       7.35       4.83       5.66       4.12      16.78
c         14        22.48      31.90      19.08      11.56      11.08      25.79
c         15         6.24       7.78       7.58      75.87       4.93       8.73
c         16         7.28       8.62       5.06       2.53       5.27       9.85
c         17        11.70      14.92      10.29       8.38      10.65      17.89
c         18       126.84     203.76     127.63     160.39      37.13     349.42
c         19        16.74      20.63      13.70       9.69      11.58      13.40
c         20        14.56      18.76      13.51       8.13       9.75      16.12
c         21       117.63     168.79      58.97     138.42      19.62     253.03
c         22        75.96     103.46      95.34      54.32      17.04     183.34
c         23        15.34      17.71      10.46      20.22      13.97      20.52
c         24         3.60       4.58       2.66      28.60       3.95       4.59
c -------------      ....       ....       ....       ....       ....       ....
c PM Correlation =   1.00       1.00       0.97       0.90       0.95       0.93
c Standard  Dev. =  59.92      86.75      61.18      89.09      16.32     219.72
c
c Maximum   Rate = 207.28     295.48     228.00     488.07      62.08    1042.33
c Quartile  Q3   =  78.59     111.42      73.20      78.61      19.20     156.56
c Average   Rate =  55.39      78.23      49.70      64.38      17.56     139.95
c Geometric Mean =  27.57      36.63      22.61      26.39      12.23      43.94
c Median    Q2   =  16.74      20.63      13.77      19.82      10.06      24.16
c Harmonic  Mean =  13.95      17.66      11.26      12.25       9.02      19.07
c Quartile  Q1   =  11.70      14.75       8.34       8.39       6.99      11.44
c Minimum   Rate =   2.20       2.85       2.01       2.25       2.43       4.47
c
c Maxima    Ratio=   1.00       1.43       1.10       2.35       0.30       5.03
c Average   Ratio=   1.00       1.41       0.90       1.16       0.32       2.53
c Geometric Ratio=   1.00       1.33       0.82       0.96       0.44       1.59
c Harmonic  Ratio=   1.00       1.27       0.81       0.88       0.65       1.37
c Minima    Ratio=   1.00       1.30       0.91       1.02       1.10       2.03
c
c           The range of speed-ups shown above as ratios of the performance
c           statistics has a small variance compared to the enormous
c           performance ranges; the range of speed-ups are convergent estimates.
c           More accurate projection of a cpu workload rate may be
c           computed by assigning appropriate weights for each kernel.
c
c           The upper bound for Fortran performance of a parallel
c           N-processor system is given by multiplying the seven range
c           statistics from a uni-processor LFK test (2.2) by N.
c
c D.118 LFK Test  118.1      118.2      118.3      118.4      118.5      118.6
c ------------- ---------- ---------- ---------- ---------- ---------- ---------
c   Vendor       CRAY RI    CRAY RI    CRAY RI    CRAY RI    CRAY RI    CRAY RI
c   Model        YMP1modY   YMP1modY   YMP/832    YMP/832    YMP/832    YMP/832
c   OSystem      NLTSS      NLTSS      UNICOS     UNICOS     UNICOS     UNICOS
c   Compiler     CFT77 3.   CFT77 3.   CF77 4.0   CF77 4.0   CF77 4.0   CF77 4.0
c   OptLevel     Scalar     Vector     vector     vector     vector     vector
c   NR.Procs            1          1          1          2          4          8
c   Samples            72         72         72         72         72         72
c   WordSize           64         64         64         64         64         64
c   DO Span           167        167        167        167        167        167
c   Year             1989       1989       1990       1990       1990       1990
c   Kernel/MFlops--------- ---------- ---------- ---------- ---------- ---------
c          1        23.33     258.08     188.23     364.86     535.99     581.75
c          2        14.26      68.12      64.45      64.86      65.59      64.08
c          3        25.05     232.20     236.81     236.93     233.45     236.86
c          4        22.92      92.14      89.72     110.24     160.77     156.70
c          5        19.44      19.59      19.30      19.64      19.59      19.65
c          6         9.24      21.15      20.76      21.07      20.93      20.86
c          7        32.83     291.31     274.07     521.69     896.68    1308.07
c          8        30.00     229.89     188.78     264.72     262.94     266.89
c          9        31.23     240.88     169.97     225.10     219.31     243.47
c         10        18.53     108.73     106.66     112.78     108.76     108.58
c         11        19.73      19.75      37.87      38.66      38.52      37.93
c         12        16.95     135.81     126.99     130.52     125.68     130.49
c         13         6.73       6.74      20.77      21.16      20.89      21.18
c         14         9.71      29.98      29.04      35.15      38.77      40.81
c         15         7.55       7.55      32.53      52.47      73.84     127.58
c         16         8.42       8.34       8.38       8.44       8.34       8.44
c         17        13.47      13.84      15.70      15.89      15.88      15.89
c         18        24.84     199.36     179.98     293.24     410.87     526.41
c         19        20.28      20.34      20.07      20.37      20.27      20.37
c         20        18.27      18.50      17.84      17.98      18.03      18.05
c         21        20.53     160.40     278.54     439.90     776.14    1268.94
c         22         8.74     106.25      86.52     132.58     131.62     129.79
c         23        19.53      20.16      36.35      36.65      36.75      36.79
c         24         3.85       3.93      38.04      38.82      38.58      38.80
c -------------      ....       ....       ....       ....       ....       ....
c Standard  Dev. =   7.93      85.18      78.37     113.05     171.34     246.63
c
c Maximum   Rate =  32.83     291.31     278.54     521.69     896.68    1308.07
c Quartile  Q3   =  20.53     109.15     116.42     120.27     125.70     128.95
c Average   Rate =  16.65      77.30      75.88      92.90     113.89     136.32
c Geometric Mean =  14.58      36.50      41.43      45.11      48.00      49.09
c Median    Q2   =  16.95      21.15      32.25      36.53      36.75      36.79
c Harmonic  Mean =  12.40      17.27      22.69      23.48      24.12      23.58
c Quartile  Q1   =   8.76      13.84      16.56      16.74      17.94      17.13
c Minimum   Rate =   3.73       2.90       2.82       2.87       2.86       2.83
c
c Maxima    Ratio=   1.00       8.87       8.48      15.89      27.31      39.84
c Average   Ratio=   1.00       4.64       4.56       5.58       6.84       8.19
c Geometric Ratio=   1.00       2.50       2.84       3.09       3.29       3.37
c Harmonic  Ratio=   1.00       1.39       1.83       1.89       1.95       1.90
c Minima    Ratio=   1.00       0.78       0.76       0.77       0.77       0.76
c
c
c
c
c           The parallel complexity and parallel techniques for the LFK
c           test are described in the following reference:
c
c           J.T.Feo,  An Analysis of the Computational and
c           Parallel Complexity of the Livermore Loops,
c           PARALLEL COMPUTING 7(2), 163-185(1988).
c
c
c
c
c
c
c
c    4. SAMPLE OUTPUT FILE FROM SUBROUTINe REPORT: (CRAY-YMP832/CFT77 Compiler)
c                                                                       aus
c
c         Output file for the Mono-processed Standard Benchmark Test:
c         The following output was uni-processed on CRAY-YMP832 in a
c         fully loaded, multi-processing, multi-programming system:
c
c
c  >>> USE 72 SAMPLES LFK TEST RESULTS SUMMARY (line 700+)
c  >>> USE ALL RANGE STATISTICS FOR OFFICIAL QUOTATIONS.
c SECOVT:     16000  5.0567e-05     1.0000
c SECOVT:     32000  5.0555e-05     0.0002
c VERIFY:       200  5.0885e-05 =  Time Resolution of Cpu-timer
c
c         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
c         -----     -------     -------    -------   --------
c         EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE
c         Loop      CPUTIME     CLOCK      CLOCK     TIMING
c         SIZE      SECONDS     ERROR      ERROR     ERROR
c         -----     -------     -------    -------   --------
c           256  4.0944e-04       0.00%      0.84%      0.66%
c           512  8.1144e-04       0.00%      0.42%      0.66%
c          1024  1.6184e-03       0.00%      0.22%      0.54%
c          2048  3.2344e-03       0.00%      0.13%      0.66%
c          4096  6.4544e-03       0.00%      0.56%      0.00%
c          6800       Repetition Count = MULTI * Loops2 =       50.000
c          8192  1.2917e-02       0.00%      0.53%      0.09%
c         16384  2.5677e-02       0.00%      0.25%      0.25%
c         32768  5.1674e-02       0.00%      0.56%      0.11%
c         65536  1.0320e-01       0.00%      0.28%      0.11%
c         -----     -------     -------    -------   --------
c
c
c  CLOCK CALIBRATION TEST OF INTERNAL CPU-TIMER: SECOND
c  MONOPROCESS THIS TEST, STANDALONE, NO TIMESHARING.
c  VERIFY TIMED INTERVALS SHOWN BELOW USING EXTERNAL CLOCK
c  START YOUR STOPWATCH NOW !
c
c            Verify  T or DT  observe external clock:
c
c            -------     -------      ------      -----
c            Total T ?   Delta T ?    Mflops ?    Flops
c            -------     -------      ------      -----
c      1        9.70        9.70       65.84    6.38411e+08
c      2       19.38        9.68       65.89    1.27682e+09
c      3       29.19        9.82       65.60    1.91523e+09
c      4       38.98        9.78       65.52    2.55364e+09
c            -------     -------      ------      -----
c  END CALIBRATION TEST.
c
c
c  ESTIMATED TOTAL JOB CPU-TIME:=    98.917 sec.  ( Nruns=       7 Trials)
c
c
c Trial=      1             ChkSum=  908    Pass=      0     Fail=      0
c Trial=      2             ChkSum=  908    Pass=      1     Fail=      0
c Trial=      3             ChkSum=  908    Pass=      2     Fail=      0
c Trial=      4             ChkSum=  908    Pass=      3     Fail=      0
c Trial=      5             ChkSum=  908    Pass=      4     Fail=      0
c Trial=      6             ChkSum=  908    Pass=      5     Fail=      0
c Trial=      7             ChkSum=  908    Pass=      6     Fail=      0
c1
c
c
c time TEST overhead (t err):
c
c      RUN        AVERAGE        STANDEV        MINIMUM        MAXIMUM
c TICK   1   9.856583e-07   7.917129e-10
c TICK   2   9.827836e-07   3.459393e-10
c TICK   3   9.845331e-07   4.918561e-10
c TICK   4   9.858521e-07   1.222940e-09
c TICK   5   9.843086e-07   3.900581e-12
c TICK   6   9.848147e-07   6.017975e-10
c TICK   7   9.839837e-07   1.912898e-10
c DATA   7   9.998664e-02   5.433200e-07   9.998564e-02   9.998765e-02
c DATA   7   9.998599e-02   9.174233e-07   9.998434e-02   9.998765e-02
c TICK   7   9.845620e-07   9.630494e-10   9.827836e-07   9.858521e-07
c
c
c THE EXPERIMENTAL TIMING ERRORS FOR ALL  7 RUNS
c --  ---------  ---------  --------- -----  -----   ---
c  k   T min      T avg      T max    T err   tick   P-F
c --  ---------  ---------  --------- -----  -----   ---
c  1 2.7669e-02 2.7708e-02 2.7761e-02  0.13%  0.00%     0
c  2 1.0227e-01 1.0274e-01 1.0327e-01  0.31%  0.00%     0
c  3 1.5476e-02 1.5608e-02 1.5690e-02  0.40%  0.00%     0
c  4 4.1177e-02 4.1403e-02 4.1745e-02  0.52%  0.00%     0
c  5 3.6697e-01 3.6710e-01 3.6722e-01  0.02%  0.00%     0
c  6 1.1576e-01 1.1602e-01 1.1629e-01  0.18%  0.00%     0
c  7 4.3866e-02 4.3928e-02 4.4012e-02  0.11%  0.00%     0
c  8 6.5278e-02 6.5553e-02 6.6188e-02  0.45%  0.00%     0
c  9 5.4051e-02 5.4393e-02 5.4880e-02  0.65%  0.00%     0
c 10 5.7777e-02 5.8703e-02 5.9561e-02  1.20%  0.00%     0
c 11 2.8525e-01 2.8528e-01 2.8531e-01  0.01%  0.00%     0
c 12 2.2396e-02 2.2675e-02 2.3112e-02  0.96%  0.00%     0
c 13 1.5454e-01 1.5542e-01 1.5671e-01  0.53%  0.00%     0
c 14 1.4662e-01 1.4694e-01 1.4733e-01  0.17%  0.00%     0
c 15 4.4328e-01 4.4352e-01 4.4394e-01  0.05%  0.00%     0
c 16 3.3733e-01 3.3750e-01 3.3766e-01  0.03%  0.00%     0
c 17 4.6434e-01 4.6448e-01 4.6475e-01  0.03%  0.00%     0
c 18 4.6521e-02 4.6617e-02 4.6883e-02  0.26%  0.00%     0
c 19 5.6179e-01 5.6192e-01 5.6215e-01  0.02%  0.00%     0
c 20 3.1377e-01 3.1387e-01 3.1395e-01  0.02%  0.00%     0
c 21 1.9713e-01 1.9862e-01 1.9933e-01  0.36%  0.00%     0
c 22 3.7298e-02 3.7309e-02 3.7319e-02  0.02%  0.00%     0
c 23 4.8072e-01 4.8090e-01 4.8128e-01  0.04%  0.00%     0
c 24 2.5737e-02 2.5783e-02 2.5863e-02  0.14%  0.00%     0
c --  ---------  ---------  --------- -----  -----   ---
c
c
c NET CPU TIMING VARIANCE (T err);  A few % is ok:
c
c                 AVERAGE        STANDEV        MINIMUM        MAXIMUM
c     Terr          0.28%          0.31%          0.01%          1.20%
c
c
c
c
c
c
c
c
c
c
c
c
c1
c ********************************************
c THE LIVERMORE  FORTRAN KERNELS:  M F L O P S
c ********************************************
c
c              Computer : CRAY-YMP (6.0ns)
c              System   : NLTSS   fully loaded
c              Compiler : CFT77 4.0.3.4
c              Date     : 91.07.14
c         Mean DO Span  =   471
c         When the computer performance range is very large
c         the net Mflops rate of many Fortran programs and
c         workloads will be in the sub-range between the equi-
c         weighted Harmonic and Arithmetic means depending
c         on the degree of code parallelism and optimization.
c         The least biased central measure is the Geometric
c         Mean of 72 rates,  quoted +- a standard deviation.
c         Mean Mflops rates imply the average efficiency of a
c         computing system since the peak rate is well known.
c         LFK test measures a lower bound for a Multi-processor
c         and N * LFK rates project an upper bound for N-procs.
c
c KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CHECK-SUMS             OK
c ------  -----   --------   --------- ---- ------  ---------------------- --
c  1 7.0070e+06 2.7708e+04    252.8850 1001   1.00  3.5802568852572702e+05 14
c  2 5.1992e+06 1.0274e+05     50.6078  101   1.00  1.0778052681679546e+04 13
c  3 3.6036e+06 1.5608e+04    230.8880 1001   1.00  7.0052001814661253e+01 13
c  4 3.3600e+06 4.1403e+04     81.1541 1001   1.00  4.1994754168318025e+00 14
c  5 4.0000e+06 3.6710e+05     10.8963 1001   1.00  3.1842101496719639e+04 13
c  6 2.3808e+06 1.1602e+05     20.5205   64   1.00  3.0625814413198270e+04 12
c  7 1.2736e+07 4.3928e+04    289.9310  995   1.00  4.2729757526224293e+05 14
c  8 1.4256e+07 6.5553e+04    217.4721  100   1.00  1.0508876039390042e+06 12
c  9 1.2362e+07 5.4393e+04    227.2807  101   1.00  8.3261052698255703e+05 13
c 10 6.1812e+06 5.8703e+04    105.2963  101   1.00  5.1172588490281440e+05 13
c 11 2.2000e+06 2.8528e+05      7.7117 1001   1.00  2.3400376808530903e+08 12
c 12 2.4000e+06 2.2675e+04    105.8436 1000   1.00  2.0349990127721540e-04  7
c 13 3.2256e+06 1.5542e+05     20.7546   64   1.00  8.4177377140212891e+11  9
c 14 4.4044e+06 1.4694e+05     29.9742 1001   1.00  2.2158871308010742e+10 12
c 15 3.3000e+06 4.4352e+05      7.4405  101   1.00  2.7606716832465865e+05 13
c 16 2.6500e+06 3.3750e+05      7.8518   75   1.00  3.9555320000000000e+06 16
c 17 6.3630e+06 4.6448e+05     13.6991  101   1.00  7.8024924103214871e+03 12
c 18 8.7120e+06 4.6617e+04    186.8842  100   1.00  7.1100892625153065e+05 13
c 19 4.7268e+06 5.6192e+05      8.4119  101   1.00  3.7952718721052370e+03 12
c 20 5.2000e+06 3.1387e+05     16.5676 1000   1.00  2.1284510375197506e+08 11
c 21 2.5250e+07 1.9862e+05    127.1262  101   1.00  1.1181157964903107e+09 11
c 22 3.7774e+06 3.7309e+04    101.2451  101   1.00  2.0570230635969638e+03 13
c 23 8.7120e+06 4.8090e+05     18.1160  100   1.00  2.4849303510962054e+05 12
c 24 1.0000e+06 2.5783e+04     38.7847 1001   1.00  3.5000000000000000e+03 16
c ------  -----   --------   --------- ---- ------  ---------------------- --
c 24 1.5301e+08 4.4140e+06     34.6643  471                               300
c
c         MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
c         Maximum   Rate =    289.9310 Mega-Flops/Sec.
c         Quartile  Q3   =    157.0052 Mega-Flops/Sec.
c         Average   Rate =     90.7226 Mega-Flops/Sec.
c         Geometric Mean =     45.9242 Mega-Flops/Sec.
c         Median    Q2   =     44.6962 Mega-Flops/Sec.
c         Harmonic  Mean =     23.0425 Mega-Flops/Sec.
c         Quartile  Q1   =     15.1334 Mega-Flops/Sec.
c         Minimum   Rate =      7.4405 Mega-Flops/Sec.
c
c
c         Standard  Dev. =     91.2693 Mega-Flops/Sec.
c         Avg Efficiency =     15.84%  Program & Processor
c         Mean Precision =     12.50   Decimal Digits
c1
c
c
c
c
c
c
c                    SENSITIVITY ANALYSIS
c
c
c         The sensitivity of the harmonic mean rate (Mflops)
c         to various weightings is shown in the table below.
c         Seven work distributions are generated by assigning
c         two distinct weights to ranked kernels by quartiles.
c         Forty nine possible cpu workloads are then evaluated
c         using seven sets of values for the total weights:
c
c
c             ------ ------ ------ ------ ------ ------ ------
c   1st QT:       O      O      O      O      O      X      X
c   2nd QT:       O      O      O      X      X      X      O
c   3rd QT:       O      X      X      X      O      O      O
c   4th QT:       X      X      O      O      O      O      O
c             ------ ------ ------ ------ ------ ------ ------
c   Total
c   Weights                    Net Mflops:
c    X    O
c  ---- ----
c
c  1.00 0.00    8.89  12.68  22.09  35.25  87.25 126.51 230.03
c
c  0.95 0.05    9.27  13.27  22.15  33.48  73.58  87.31 143.87
c
c  0.90 0.10    9.68  13.93  22.21  31.88  63.61  66.65 104.67
c
c  0.80 0.20   10.63  15.46  22.34  29.09  50.05  45.24  67.75
c
c  0.70 0.30   11.78  17.36  22.46  26.75  41.26  34.25  50.08
c
c  0.60 0.40   13.22  19.80  22.59  24.76  35.09  27.55  39.72
c
c  0.50 0.50   15.05  23.04  22.72  23.04  30.53  23.04  32.92
c  ---- ----
c             ------ ------ ------ ------ ------ ------ ------
c
c
c
c
c
c
c SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL)
c
c   12.68  15.63  20.38  29.27  37.44   51.93   84.73  123.85  230.03
c
c    0.00   0.20   0.40   0.60   0.70    0.80    0.90    0.95    1.00
c    Fraction Of Operations Run At Optimal Fortran Rates
c
c
c1
c
c
c time TEST overhead (t err):
c
c      RUN        AVERAGE        STANDEV        MINIMUM        MAXIMUM
c TICK   1   9.856583e-07   7.917129e-10
c TICK   2   9.827836e-07   3.459393e-10
c TICK   3   9.845331e-07   4.918561e-10
c TICK   4   9.858521e-07   1.222940e-09
c TICK   5   9.843086e-07   3.900581e-12
c TICK   6   9.848147e-07   6.017975e-10
c TICK   7   9.839837e-07   1.912898e-10
c DATA   7   9.998664e-02   5.433200e-07   9.998564e-02   9.998765e-02
c DATA   7   9.998599e-02   9.174233e-07   9.998434e-02   9.998765e-02
c TICK   7   9.845620e-07   9.630494e-10   9.827836e-07   9.858521e-07
c
c
c THE EXPERIMENTAL TIMING ERRORS FOR ALL  7 RUNS
c --  ---------  ---------  --------- -----  -----   ---
c  k   T min      T avg      T max    T err   tick   P-F
c --  ---------  ---------  --------- -----  -----   ---
c  1 4.0964e-02 4.1146e-02 4.1528e-02  0.43%  0.00%     0
c  2 1.2248e-01 1.2274e-01 1.2312e-01  0.16%  0.00%     0
c  3 6.0446e-02 6.0540e-02 6.0602e-02  0.10%  0.00%     0
c  4 2.5181e-01 2.5194e-01 2.5210e-01  0.04%  0.00%     0
c  5 4.1136e-01 4.1142e-01 4.1148e-01  0.01%  0.00%     0
c  6 2.2587e-01 2.2615e-01 2.2647e-01  0.08%  0.00%     0
c  7 5.2859e-02 5.3120e-02 5.3446e-02  0.43%  0.00%     0
c  8 7.8265e-02 7.8555e-02 7.9045e-02  0.35%  0.00%     0
c  9 6.2854e-02 6.3375e-02 6.3858e-02  0.55%  0.00%     0
c 10 6.3650e-02 6.4887e-02 6.6192e-02  1.29%  0.00%     0
c 11 3.4608e-01 3.4615e-01 3.4623e-01  0.01%  0.00%     0
c 12 3.3683e-02 3.3878e-02 3.4230e-02  0.47%  0.00%     0
c 13 2.0237e-01 2.0332e-01 2.0461e-01  0.35%  0.00%     0
c 14 1.5414e-01 1.5473e-01 1.5509e-01  0.20%  0.00%     0
c 15 8.8648e-01 8.8713e-01 8.8768e-01  0.04%  0.00%     0
c 16 3.9403e-01 3.9427e-01 3.9466e-01  0.05%  0.00%     0
c 17 5.3069e-01 5.3093e-01 5.3158e-01  0.05%  0.00%     0
c 18 4.6512e-02 4.6655e-02 4.7000e-02  0.33%  0.00%     0
c 19 6.6284e-01 6.6303e-01 6.6350e-01  0.03%  0.00%     0
c 20 5.0282e-01 5.0312e-01 5.0355e-01  0.04%  0.00%     0
c 21 2.6824e-01 2.7087e-01 2.7489e-01  0.97%  0.00%     0
c 22 4.7450e-02 4.7480e-02 4.7510e-02  0.04%  0.00%     0
c 23 6.0078e-01 6.0143e-01 6.0233e-01  0.10%  0.00%     0
c 24 8.3666e-02 8.3784e-02 8.4103e-02  0.18%  0.00%     0
c --  ---------  ---------  --------- -----  -----   ---
c
c
c NET CPU TIMING VARIANCE (T err);  A few % is ok:
c
c                 AVERAGE        STANDEV        MINIMUM        MAXIMUM
c     Terr          0.26%          0.31%          0.01%          1.29%
c
c
c
c
c
c
c
c
c
c
c
c
c1
c ********************************************
c THE LIVERMORE  FORTRAN KERNELS:  M F L O P S
c ********************************************
c
c              Computer : CRAY-YMP (6.0ns)
c              System   : NLTSS   fully loaded
c              Compiler : CFT77 4.0.3.4
c              Date     : 91.07.14
c         Mean DO Span  =    90
c         When the computer performance range is very large
c         the net Mflops rate of many Fortran programs and
c         workloads will be in the sub-range between the equi-
c         weighted Harmonic and Arithmetic means depending
c         on the degree of code parallelism and optimization.
c         The least biased central measure is the Geometric
c         Mean of 72 rates,  quoted +- a standard deviation.
c         Mean Mflops rates imply the average efficiency of a
c         computing system since the peak rate is well known.
c         LFK test measures a lower bound for a Multi-processor
c         and N * LFK rates project an upper bound for N-procs.
c
c KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CHECK-SUMS             OK
c ------  -----   --------   --------- ---- ------  ---------------------- --
c  1 8.0800e+06 4.1146e+04    196.3722  101   2.00  3.6773413452565728e+03 16
c  2 6.2080e+06 1.2274e+05     50.5788  101   2.00  1.0778052681679546e+04 13
c  3 4.2824e+06 6.0540e+04     70.7366  101   2.00  7.0681900560541635e+00 13
c  4 3.3600e+06 2.5194e+05     13.3367  101   2.00  4.1994754168318025e+00 14
c  5 4.4000e+06 4.1142e+05     10.6948  101   2.00  3.2123223577214776e+02 13
c  6 2.6880e+06 2.2615e+05     11.8860   32   2.00  6.0421729517419226e+02 12
c  7 1.4221e+07 5.3120e+04    267.7098  101   2.00  4.4419104210488440e+03 14
c  8 1.7107e+07 7.8555e+04    217.7736  100   2.00  1.0508876039390042e+06 12
c  9 1.4423e+07 6.3375e+04    227.5796  101   2.00  8.3261052698255703e+05 13
c 10 6.9084e+06 6.4887e+04    106.4682  101   2.00  5.1172588490281440e+05 13
c 11 2.5600e+06 3.4615e+05      7.3957  101   2.00  2.4034922852330748e+05 14
c 12 2.7200e+06 3.3878e+04     80.2871  100   2.00  4.9892984137045460e-05  8
c 13 3.6736e+06 2.0332e+05     18.0678   32   2.00  6.8714714682226172e+11 10
c 14 4.4440e+06 1.5473e+05     28.7211  101   2.00  2.1279884256031895e+08 12
c 15 6.6000e+06 8.8713e+05      7.4397  101   2.00  2.7606716832465865e+05 13
c 16 3.0240e+06 3.9427e+05      7.6699   40   2.00  4.5362870000000000e+06 16
c 17 7.2720e+06 5.3093e+05     13.6968  101   2.00  7.8024924103214871e+03 12
c 18 8.7120e+06 4.6655e+04    186.7309  100   2.00  7.1100892625153065e+05 13
c 19 5.5752e+06 6.6303e+05      8.4086  101   2.00  3.7952718721052370e+03 12
c 20 8.3200e+06 5.0312e+05     16.5369  100   2.00  2.1883436251674779e+05 12
c 21 2.5000e+07 2.7087e+05     92.2937   50   2.00  5.4771674139930725e+08 11
c 22 4.8076e+06 4.7480e+04    101.2550  101   2.00  2.0570230635969638e+03 13
c 23 1.0890e+07 6.0143e+05     18.1067  100   2.00  2.4849303510962054e+05 12
c 24 1.2400e+06 8.3784e+04     14.7999  101   2.00  3.5000000000000000e+02 16
c ------  -----   --------   --------- ---- ------  ---------------------- --
c 24 1.7652e+08 6.1406e+06     28.7455   90                               307
c
c         MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
c         Maximum   Rate =    267.7098 Mega-Flops/Sec.
c         Quartile  Q3   =    103.8616 Mega-Flops/Sec.
c         Average   Rate =     73.9394 Mega-Flops/Sec.
c         Geometric Mean =     36.2408 Mega-Flops/Sec.
c         Median    Q2   =     23.4139 Mega-Flops/Sec.
c         Harmonic  Mean =     19.7105 Mega-Flops/Sec.
c         Quartile  Q1   =     12.6114 Mega-Flops/Sec.
c         Minimum   Rate =      7.3957 Mega-Flops/Sec.
c
c
c         Standard  Dev. =     81.6635 Mega-Flops/Sec.
c         Avg Efficiency =     12.50%  Program & Processor
c         Mean Precision =     12.79   Decimal Digits
c1
c
c
c
c
c
c
c                    SENSITIVITY ANALYSIS
c
c
c         The sensitivity of the harmonic mean rate (Mflops)
c         to various weightings is shown in the table below.
c         Seven work distributions are generated by assigning
c         two distinct weights to ranked kernels by quartiles.
c         Forty nine possible cpu workloads are then evaluated
c         using seven sets of values for the total weights:
c
c
c             ------ ------ ------ ------ ------ ------ ------
c   1st QT:       O      O      O      O      O      X      X
c   2nd QT:       O      O      O      X      X      X      O
c   3rd QT:       O      X      X      X      O      O      O
c   4th QT:       X      X      O      O      O      O      O
c             ------ ------ ------ ------ ------ ------ ------
c   Total
c   Weights                    Net Mflops:
c    X    O
c  ---- ----
c
c  1.00 0.00    8.61  11.08  15.52  24.57  58.89  89.25 184.27
c
c  0.95 0.05    8.95  11.59  15.74  23.97  52.00  65.97 118.38
c
c  0.90 0.10    9.31  12.14  15.97  23.41  46.55  52.33  87.20
c
c  0.80 0.20   10.14  13.43  16.45  22.36  38.49  37.01  57.11
c
c  0.70 0.30   11.12  15.03  16.96  21.40  32.81  28.63  42.46
c
c  0.60 0.40   12.31  17.05  17.50  20.52  28.59  23.35  33.79
c
c  0.50 0.50   13.79  19.71  18.08  19.71  25.33  19.71  28.06
c  ---- ----
c             ------ ------ ------ ------ ------ ------ ------
c
c
c
c
c
c
c SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL)
c
c   11.08  13.64  17.75  25.41  32.39   44.65   71.89  103.43  184.27
c
c    0.00   0.20   0.40   0.60   0.70    0.80    0.90    0.95    1.00
c    Fraction Of Operations Run At Optimal Fortran Rates
c
c
c1
c
c
c time TEST overhead (t err):
c
c      RUN        AVERAGE        STANDEV        MINIMUM        MAXIMUM
c TICK   1   9.856583e-07   7.917129e-10
c TICK   2   9.827836e-07   3.459393e-10
c TICK   3   9.845331e-07   4.918561e-10
c TICK   4   9.858521e-07   1.222940e-09
c TICK   5   9.843086e-07   3.900581e-12
c TICK   6   9.848147e-07   6.017975e-10
c TICK   7   9.839837e-07   1.912898e-10
c DATA   7   9.998664e-02   5.433200e-07   9.998564e-02   9.998765e-02
c DATA   7   9.998599e-02   9.174233e-07   9.998434e-02   9.998765e-02
c TICK   7   9.845620e-07   9.630494e-10   9.827836e-07   9.858521e-07
c
c
c THE EXPERIMENTAL TIMING ERRORS FOR ALL  7 RUNS
c --  ---------  ---------  --------- -----  -----   ---
c  k   T min      T avg      T max    T err   tick   P-F
c --  ---------  ---------  --------- -----  -----   ---
c  1 4.6552e-02 4.6885e-02 4.7277e-02  0.44%  0.00%     0
c  2 2.3215e-01 2.3247e-01 2.3281e-01  0.10%  0.00%     0
c  3 1.3283e-01 1.3295e-01 1.3311e-01  0.07%  0.00%     0
c  4 5.4677e-01 5.4712e-01 5.4839e-01  0.10%  0.00%     0
c  5 3.2937e-01 3.2952e-01 3.2969e-01  0.03%  0.00%     0
c  6 5.7700e-01 5.7735e-01 5.7791e-01  0.06%  0.00%     0
c  7 5.7375e-02 5.7444e-02 5.7533e-02  0.10%  0.00%     0
c  8 1.1030e-01 1.1109e-01 1.1179e-01  0.55%  0.00%     0
c  9 8.8652e-02 8.9477e-02 9.0447e-02  0.64%  0.00%     0
c 10 8.1429e-02 8.1862e-02 8.2501e-02  0.42%  0.00%     0
c 11 2.7512e-01 2.7518e-01 2.7528e-01  0.02%  0.00%     0
c 12 4.5972e-02 4.6355e-02 4.6689e-02  0.47%  0.00%     0
c 13 2.9003e-01 2.9097e-01 2.9194e-01  0.19%  0.00%     0
c 14 1.5044e-01 1.5086e-01 1.5181e-01  0.29%  0.00%     0
c 15 4.9302e-01 4.9311e-01 4.9326e-01  0.02%  0.00%     0
c 16 3.2175e-01 3.2196e-01 3.2236e-01  0.06%  0.00%     0
c 17 3.9370e-01 3.9382e-01 3.9397e-01  0.03%  0.00%     0
c 18 1.0431e-01 1.0454e-01 1.0478e-01  0.17%  0.00%     0
c 19 4.9471e-01 4.9496e-01 4.9519e-01  0.03%  0.00%     0
c 20 4.5972e-01 4.5992e-01 4.6005e-01  0.03%  0.00%     0
c 21 8.4632e-01 8.5030e-01 8.5329e-01  0.29%  0.00%     0
c 22 5.1496e-02 5.1531e-02 5.1561e-02  0.05%  0.00%     0
c 23 4.6724e-01 4.6751e-01 4.6771e-01  0.04%  0.00%     0
c 24 1.4363e-01 1.4376e-01 1.4387e-01  0.05%  0.00%     0
c --  ---------  ---------  --------- -----  -----   ---
c
c
c NET CPU TIMING VARIANCE (T err);  A few % is ok:
c
c                 AVERAGE        STANDEV        MINIMUM        MAXIMUM
c     Terr          0.18%          0.19%          0.02%          0.64%
c
c
c
c
c
c
c
c
c
c
c
c
c1
c ********************************************
c THE LIVERMORE  FORTRAN KERNELS:  M F L O P S
c ********************************************
c
c              Computer : CRAY-YMP (6.0ns)
c              System   : NLTSS   fully loaded
c              Compiler : CFT77 4.0.3.4
c              Date     : 91.07.14
c         Mean DO Span  =    19
c         When the computer performance range is very large
c         the net Mflops rate of many Fortran programs and
c         workloads will be in the sub-range between the equi-
c         weighted Harmonic and Arithmetic means depending
c         on the degree of code parallelism and optimization.
c         The least biased central measure is the Geometric
c         Mean of 72 rates,  quoted +- a standard deviation.
c         Mean Mflops rates imply the average efficiency of a
c         computing system since the peak rate is well known.
c         LFK test measures a lower bound for a Multi-processor
c         and N * LFK rates project an upper bound for N-procs.
c
c KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CHECK-SUMS             OK
c ------  -----   --------   --------- ---- ------  ---------------------- --
c  1 6.0480e+06 4.6885e+04    128.9966   27   1.00  2.6985731517464592e+02 16
c  2 3.2384e+06 2.3247e+05     13.9304   15   1.00  2.7673078908323623e+02 13
c  3 3.1968e+06 1.3295e+05     24.0453   27   1.00  1.8895163625248799e+00 13
c  4 1.8240e+06 5.4712e+05      3.3338   27   1.00  4.1994754168318025e+00 14
c  5 3.3280e+06 3.2952e+05     10.0996   27   1.00  2.2278306739137747e+01 13
c  6 1.6128e+06 5.7735e+05      2.7934    8   1.00  7.8421657542730827e+00 12
c  7 1.0752e+07 5.7444e+04    187.1752   21   1.00  1.9920041523508189e+02 14
c  8 1.3478e+07 1.1109e+05    121.3250   14   1.00  2.0723805675143376e+04 12
c  9 1.0608e+07 8.9477e+04    118.5558   15   1.00  1.8367779226120794e+04 13
c 10 5.4000e+06 8.1862e+04     65.9647   15   1.00  1.1559038593888574e+04 13
c 11 1.9136e+06 2.7518e+05      6.9540   27   1.00  4.5858129350924282e+03 13
c 12 1.9968e+06 4.6355e+04     43.0764   26   1.00  1.3604051882776247e-05  9
c 13 2.7776e+06 2.9097e+05      9.5460    8   1.00  2.6929869385100195e+11  9
c 14 3.8016e+06 1.5086e+05     25.1997   27   1.00  2.0464784190878510e+07 11
c 15 3.6960e+06 4.9311e+05      7.4953   15   1.00  7.7629810169453849e+03 13
c 16 2.4640e+06 3.2196e+05      7.6531   15   1.00  3.6065120000000000e+06 16
c 17 5.6160e+06 3.9382e+05     14.2605   15   1.00  2.0631580330134784e+02 12
c 18 9.1520e+06 1.0454e+05     87.5496   14   1.00  6.7904523486389953e+03 12
c 19 4.0320e+06 4.9496e+05      8.1461   15   1.00  8.8776148863621074e+01 12
c 20 7.5712e+06 4.5992e+05     16.4620   26   1.00  4.1913992746301519e+03 12
c 21 4.0000e+07 8.5030e+05     47.0421   20   1.00  3.5069619696601486e+08 10
c 22 3.2640e+06 5.1531e+04     63.3400   15   1.00  4.2769781097853183e+01 13
c 23 8.0080e+06 4.6751e+05     17.1291   14   1.00  3.3952384219261585e+03 12
c 24 9.5680e+05 1.4376e+05      6.6557   27   1.00  9.1000000000000000e+01 16
c ------  -----   --------   --------- ---- ------  ---------------------- --
c 24 1.5474e+08 6.7509e+06     22.9207   19                               303
c
c         MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
c         Maximum   Rate =    187.1752 Mega-Flops/Sec.
c         Quartile  Q3   =     64.6524 Mega-Flops/Sec.
c         Average   Rate =     43.1971 Mega-Flops/Sec.
c         Geometric Mean =     22.0242 Mega-Flops/Sec.
c         Median    Q2   =     16.7955 Mega-Flops/Sec.
c         Harmonic  Mean =     11.9909 Mega-Flops/Sec.
c         Quartile  Q1   =      7.8996 Mega-Flops/Sec.
c         Minimum   Rate =      2.7934 Mega-Flops/Sec.
c
c
c         Standard  Dev. =     49.3389 Mega-Flops/Sec.
c         Avg Efficiency =      7.60%  Program & Processor
c         Mean Precision =     12.62   Decimal Digits
c1
c
c
c
c
c
c
c                    SENSITIVITY ANALYSIS
c
c
c         The sensitivity of the harmonic mean rate (Mflops)
c         to various weightings is shown in the table below.
c         Seven work distributions are generated by assigning
c         two distinct weights to ranked kernels by quartiles.
c         Forty nine possible cpu workloads are then evaluated
c         using seven sets of values for the total weights:
c
c
c             ------ ------ ------ ------ ------ ------ ------
c   1st QT:       O      O      O      O      O      X      X
c   2nd QT:       O      O      O      X      X      X      O
c   3rd QT:       O      X      X      X      O      O      O
c   4th QT:       X      X      O      O      O      O      O
c             ------ ------ ------ ------ ------ ------ ------
c   Total
c   Weights                    Net Mflops:
c    X    O
c  ---- ----
c
c  1.00 0.00    4.93   6.88  11.34  16.46  30.01  46.83 106.47
c
c  0.95 0.05    5.14   7.18  11.38  15.87  27.28  36.29  69.80
c
c  0.90 0.10    5.35   7.52  11.42  15.32  25.00  29.62  51.92
c
c  0.80 0.20    5.85   8.29  11.51  14.32  21.43  21.66  34.33
c
c  0.70 0.30    6.45   9.24  11.59  13.45  18.74  17.07  25.64
c
c  0.60 0.40    7.19  10.44  11.68  12.68  16.66  14.09  20.47
c
c  0.50 0.50    8.12  11.99  11.77  11.99  14.99  11.99  17.03
c  ---- ----
c             ------ ------ ------ ------ ------ ------ ------
c
c
c
c
c
c
c SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL)
c
c    6.88   8.46  10.99  15.67  19.92   27.32   43.48   61.75  106.47
c
c    0.00   0.20   0.40   0.60   0.70    0.80    0.90    0.95    1.00
c    Fraction Of Operations Run At Optimal Fortran Rates
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c1
c ********************************************
c THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *
c ********************************************
c
c              Computer : CRAY-YMP (6.0ns)
c              System   : NLTSS   fully loaded
c              Compiler : CFT77 4.0.3.4
c              Date     : 91.07.14
c         Mean DO Span  =   167
c         When the computer performance range is very large
c         the net Mflops rate of many Fortran programs and
c         workloads will be in the sub-range between the equi-
c         weighted Harmonic and Arithmetic means depending
c         on the degree of code parallelism and optimization.
c         The least biased central measure is the Geometric
c         Mean of 72 rates,  quoted +- a standard deviation.
c         Mean Mflops rates imply the average efficiency of a
c         computing system since the peak rate is well known.
c         LFK test measures a lower bound for a Multi-processor
c         and N * LFK rates project an upper bound for N-procs.
c
c KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CHECK-SUMS             OK
c ------  -----   --------   --------- ---- ------  ---------------------- --
c  1 6.0480e+06 4.6885e+04    128.9966   27   1.00  2.6985731517464592e+02 16
c  2 3.2384e+06 2.3247e+05     13.9304   15   1.00  2.7673078908323623e+02 13
c  3 3.1968e+06 1.3295e+05     24.0453   27   1.00  1.8895163625248799e+00 13
c  4 1.8240e+06 5.4712e+05      3.3338   27   1.00  4.1994754168318025e+00 14
c  5 3.3280e+06 3.2952e+05     10.0996   27   1.00  2.2278306739137747e+01 13
c  6 1.6128e+06 5.7735e+05      2.7934    8   1.00  7.8421657542730827e+00 12
c  7 1.0752e+07 5.7444e+04    187.1752   21   1.00  1.9920041523508189e+02 14
c  8 1.3478e+07 1.1109e+05    121.3250   14   1.00  2.0723805675143376e+04 12
c  9 1.0608e+07 8.9477e+04    118.5558   15   1.00  1.8367779226120794e+04 13
c 10 5.4000e+06 8.1862e+04     65.9647   15   1.00  1.1559038593888574e+04 13
c 11 1.9136e+06 2.7518e+05      6.9540   27   1.00  4.5858129350924282e+03 13
c 12 1.9968e+06 4.6355e+04     43.0764   26   1.00  1.3604051882776247e-05  9
c 13 2.7776e+06 2.9097e+05      9.5460    8   1.00  2.6929869385100195e+11  9
c 14 3.8016e+06 1.5086e+05     25.1997   27   1.00  2.0464784190878510e+07 11
c 15 3.6960e+06 4.9311e+05      7.4953   15   1.00  7.7629810169453849e+03 13
c 16 2.4640e+06 3.2196e+05      7.6531   15   1.00  3.6065120000000000e+06 16
c 17 5.6160e+06 3.9382e+05     14.2605   15   1.00  2.0631580330134784e+02 12
c 18 9.1520e+06 1.0454e+05     87.5496   14   1.00  6.7904523486389953e+03 12
c 19 4.0320e+06 4.9496e+05      8.1461   15   1.00  8.8776148863621074e+01 12
c 20 7.5712e+06 4.5992e+05     16.4620   26   1.00  4.1913992746301519e+03 12
c 21 4.0000e+07 8.5030e+05     47.0421   20   1.00  3.5069619696601486e+08 10
c 22 3.2640e+06 5.1531e+04     63.3400   15   1.00  4.2769781097853183e+01 13
c 23 8.0080e+06 4.6751e+05     17.1291   14   1.00  3.3952384219261585e+03 12
c 24 9.5680e+05 1.4376e+05      6.6557   27   1.00  9.1000000000000000e+01 16
c  1 8.0800e+06 4.1146e+04    196.3722  101   2.00  3.6773413452565728e+03 16
c  2 6.2080e+06 1.2274e+05     50.5788  101   2.00  1.0778052681679546e+04 13
c  3 4.2824e+06 6.0540e+04     70.7366  101   2.00  7.0681900560541635e+00 13
c  4 3.3600e+06 2.5194e+05     13.3367  101   2.00  4.1994754168318025e+00 14
c  5 4.4000e+06 4.1142e+05     10.6948  101   2.00  3.2123223577214776e+02 13
c  6 2.6880e+06 2.2615e+05     11.8860   32   2.00  6.0421729517419226e+02 12
c  7 1.4221e+07 5.3120e+04    267.7098  101   2.00  4.4419104210488440e+03 14
c  8 1.7107e+07 7.8555e+04    217.7736  100   2.00  1.0508876039390042e+06 12
c  9 1.4423e+07 6.3375e+04    227.5796  101   2.00  8.3261052698255703e+05 13
c 10 6.9084e+06 6.4887e+04    106.4682  101   2.00  5.1172588490281440e+05 13
c 11 2.5600e+06 3.4615e+05      7.3957  101   2.00  2.4034922852330748e+05 14
c 12 2.7200e+06 3.3878e+04     80.2871  100   2.00  4.9892984137045460e-05  8
c 13 3.6736e+06 2.0332e+05     18.0678   32   2.00  6.8714714682226172e+11 10
c 14 4.4440e+06 1.5473e+05     28.7211  101   2.00  2.1279884256031895e+08 12
c 15 6.6000e+06 8.8713e+05      7.4397  101   2.00  2.7606716832465865e+05 13
c 16 3.0240e+06 3.9427e+05      7.6699   40   2.00  4.5362870000000000e+06 16
c 17 7.2720e+06 5.3093e+05     13.6968  101   2.00  7.8024924103214871e+03 12
c 18 8.7120e+06 4.6655e+04    186.7309  100   2.00  7.1100892625153065e+05 13
c 19 5.5752e+06 6.6303e+05      8.4086  101   2.00  3.7952718721052370e+03 12
c 20 8.3200e+06 5.0312e+05     16.5369  100   2.00  2.1883436251674779e+05 12
c 21 2.5000e+07 2.7087e+05     92.2937   50   2.00  5.4771674139930725e+08 11
c 22 4.8076e+06 4.7480e+04    101.2550  101   2.00  2.0570230635969638e+03 13
c 23 1.0890e+07 6.0143e+05     18.1067  100   2.00  2.4849303510962054e+05 12
c 24 1.2400e+06 8.3784e+04     14.7999  101   2.00  3.5000000000000000e+02 16
c  1 7.0070e+06 2.7708e+04    252.8850 1001   1.00  3.5802568852572702e+05 14
c  2 5.1992e+06 1.0274e+05     50.6078  101   1.00  1.0778052681679546e+04 13
c  3 3.6036e+06 1.5608e+04    230.8880 1001   1.00  7.0052001814661253e+01 13
c  4 3.3600e+06 4.1403e+04     81.1541 1001   1.00  4.1994754168318025e+00 14
c  5 4.0000e+06 3.6710e+05     10.8963 1001   1.00  3.1842101496719639e+04 13
c  6 2.3808e+06 1.1602e+05     20.5205   64   1.00  3.0625814413198270e+04 12
c  7 1.2736e+07 4.3928e+04    289.9310  995   1.00  4.2729757526224293e+05 14
c  8 1.4256e+07 6.5553e+04    217.4721  100   1.00  1.0508876039390042e+06 12
c  9 1.2362e+07 5.4393e+04    227.2807  101   1.00  8.3261052698255703e+05 13
c 10 6.1812e+06 5.8703e+04    105.2963  101   1.00  5.1172588490281440e+05 13
c 11 2.2000e+06 2.8528e+05      7.7117 1001   1.00  2.3400376808530903e+08 12
c 12 2.4000e+06 2.2675e+04    105.8436 1000   1.00  2.0349990127721540e-04  7
c 13 3.2256e+06 1.5542e+05     20.7546   64   1.00  8.4177377140212891e+11  9
c 14 4.4044e+06 1.4694e+05     29.9742 1001   1.00  2.2158871308010742e+10 12
c 15 3.3000e+06 4.4352e+05      7.4405  101   1.00  2.7606716832465865e+05 13
c 16 2.6500e+06 3.3750e+05      7.8518   75   1.00  3.9555320000000000e+06 16
c 17 6.3630e+06 4.6448e+05     13.6991  101   1.00  7.8024924103214871e+03 12
c 18 8.7120e+06 4.6617e+04    186.8842  100   1.00  7.1100892625153065e+05 13
c 19 4.7268e+06 5.6192e+05      8.4119  101   1.00  3.7952718721052370e+03 12
c 20 5.2000e+06 3.1387e+05     16.5676 1000   1.00  2.1284510375197506e+08 11
c 21 2.5250e+07 1.9862e+05    127.1262  101   1.00  1.1181157964903107e+09 11
c 22 3.7774e+06 3.7309e+04    101.2451  101   1.00  2.0570230635969638e+03 13
c 23 8.7120e+06 4.8090e+05     18.1160  100   1.00  2.4849303510962054e+05 12
c 24 1.0000e+06 2.5783e+04     38.7847 1001   1.00  3.5000000000000000e+03 16
c ------  -----   --------   --------- ---- ------  ---------------------- --
c 72 4.8426e+08 1.7306e+07     27.9829  167                               910
c
c         MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
c         Maximum   Rate =    289.9310 Mega-Flops/Sec.
c         Quartile  Q3   =    105.2963 Mega-Flops/Sec.
c         Average   Rate =     70.4496 Mega-Flops/Sec.
c         Geometric Mean =     33.9496 Mega-Flops/Sec.
c         Median    Q2   =     24.0453 Mega-Flops/Sec.
c         Harmonic  Mean =     17.5236 Mega-Flops/Sec.
c         Quartile  Q1   =     10.8963 Mega-Flops/Sec.
c         Minimum   Rate =      2.7934 Mega-Flops/Sec.
c
c
c         Standard  Dev. =     79.4989 Mega-Flops/Sec.
c         Avg Efficiency =     11.71%  Program & Processor
c         Mean Precision =     12.64   Decimal Digits
c <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >
c < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >
c <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c1
c
c
c TOP QUARTILE: BEST ARCHITECTURE/APPLICATION MATCH
c
c
c
c KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT
c ------  -----   --------   --------- ---- ------
c  7 1.2736e+07 4.3928e+04    289.9310  995   1.00
c  7 1.4221e+07 5.3120e+04    267.7098  101   2.00
c  1 7.0070e+06 2.7708e+04    252.8850 1001   1.00
c  3 3.6036e+06 1.5608e+04    230.8880 1001   1.00
c  9 1.4423e+07 6.3375e+04    227.5796  101   2.00
c  9 1.2362e+07 5.4393e+04    227.2807  101   1.00
c  8 1.7107e+07 7.8555e+04    217.7736  100   2.00
c  8 1.4256e+07 6.5553e+04    217.4721  100   1.00
c  1 8.0800e+06 4.1146e+04    196.3722  101   2.00
c  7 1.0752e+07 5.7444e+04    187.1752   21   1.00
c 18 8.7120e+06 4.6617e+04    186.8842  100   1.00
c 18 8.7120e+06 4.6655e+04    186.7309  100   2.00
c  1 6.0480e+06 4.6885e+04    128.9966   27   1.00
c 21 2.5250e+07 1.9862e+05    127.1262  101   1.00
c  8 1.3478e+07 1.1109e+05    121.3250   14   1.00
c  9 1.0608e+07 8.9477e+04    118.5558   15   1.00
c 10 6.9084e+06 6.4887e+04    106.4682  101   2.00
c 12 2.4000e+06 2.2675e+04    105.8436 1000   1.00
c ------  -----   --------   --------- ---- ------
c
c         Frac.  Weights =      0.2500
c         Average   Rate =    191.6513 Mega-Flops/Sec.
c         Harmonic  Mean =    173.5450 Mega-Flops/Sec.
c         Standard  Dev. =     55.0716 Mega-Flops/Sec.
c
c
c
c KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT
c ------  -----   --------   --------- ---- ------
c 10 6.1812e+06 5.8703e+04    105.2963  101   1.00
c 22 4.8076e+06 4.7480e+04    101.2550  101   2.00
c 22 3.7774e+06 3.7309e+04    101.2451  101   1.00
c 21 2.5000e+07 2.7087e+05     92.2937   50   2.00
c 18 9.1520e+06 1.0454e+05     87.5496   14   1.00
c  4 3.3600e+06 4.1403e+04     81.1541 1001   1.00
c 12 2.7200e+06 3.3878e+04     80.2871  100   2.00
c  3 4.2824e+06 6.0540e+04     70.7366  101   2.00
c 10 5.4000e+06 8.1862e+04     65.9647   15   1.00
c 22 3.2640e+06 5.1531e+04     63.3400   15   1.00
c  2 5.1992e+06 1.0274e+05     50.6078  101   1.00
c  2 6.2080e+06 1.2274e+05     50.5788  101   2.00
c 21 4.0000e+07 8.5030e+05     47.0421   20   1.00
c 12 1.9968e+06 4.6355e+04     43.0764   26   1.00
c 24 1.0000e+06 2.5783e+04     38.7847 1001   1.00
c 14 4.4044e+06 1.4694e+05     29.9742 1001   1.00
c 14 4.4440e+06 1.5473e+05     28.7211  101   2.00
c 14 3.8016e+06 1.5086e+05     25.1997   27   1.00
c  3 3.1968e+06 1.3295e+05     24.0453   27   1.00
c 13 3.2256e+06 1.5542e+05     20.7546   64   1.00
c  6 2.3808e+06 1.1602e+05     20.5205   64   1.00
c 23 8.7120e+06 4.8090e+05     18.1160  100   1.00
c 23 1.0890e+07 6.0143e+05     18.1067  100   2.00
c 13 3.6736e+06 2.0332e+05     18.0678   32   2.00
c 23 8.0080e+06 4.6751e+05     17.1291   14   1.00
c 20 5.2000e+06 3.1387e+05     16.5676 1000   1.00
c 20 8.3200e+06 5.0312e+05     16.5369  100   2.00
c 20 7.5712e+06 4.5992e+05     16.4620   26   1.00
c 24 1.2400e+06 8.3784e+04     14.7999  101   2.00
c 17 5.6160e+06 3.9382e+05     14.2605   15   1.00
c  2 3.2384e+06 2.3247e+05     13.9304   15   1.00
c 17 6.3630e+06 4.6448e+05     13.6991  101   1.00
c 17 7.2720e+06 5.3093e+05     13.6968  101   2.00
c  4 3.3600e+06 2.5194e+05     13.3367  101   2.00
c  6 2.6880e+06 2.2615e+05     11.8860   32   2.00
c  5 4.0000e+06 3.6710e+05     10.8963 1001   1.00
c ------  -----   --------   --------- ---- ------
c
c         Frac.  Weights =      0.5104
c         Average   Rate =     40.5352 Mega-Flops/Sec.
c         Harmonic  Mean =     23.7604 Mega-Flops/Sec.
c         Standard  Dev. =     30.9452 Mega-Flops/Sec.
c
c
c
c KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT
c ------  -----   --------   --------- ---- ------
c  5 4.4000e+06 4.1142e+05     10.6948  101   2.00
c  5 3.3280e+06 3.2952e+05     10.0996   27   1.00
c 13 2.7776e+06 2.9097e+05      9.5460    8   1.00
c 19 4.7268e+06 5.6192e+05      8.4119  101   1.00
c 19 5.5752e+06 6.6303e+05      8.4086  101   2.00
c 19 4.0320e+06 4.9496e+05      8.1461   15   1.00
c 16 2.6500e+06 3.3750e+05      7.8518   75   1.00
c 11 2.2000e+06 2.8528e+05      7.7117 1001   1.00
c 16 3.0240e+06 3.9427e+05      7.6699   40   2.00
c 16 2.4640e+06 3.2196e+05      7.6531   15   1.00
c 15 3.6960e+06 4.9311e+05      7.4953   15   1.00
c 15 3.3000e+06 4.4352e+05      7.4405  101   1.00
c 15 6.6000e+06 8.8713e+05      7.4397  101   2.00
c 11 2.5600e+06 3.4615e+05      7.3957  101   2.00
c 11 1.9136e+06 2.7518e+05      6.9540   27   1.00
c 24 9.5680e+05 1.4376e+05      6.6557   27   1.00
c  4 1.8240e+06 5.4712e+05      3.3338   27   1.00
c  6 1.6128e+06 5.7735e+05      2.7934    8   1.00
c ------  -----   --------   --------- ---- ------
c
c         Frac.  Weights =      0.2396
c         Average   Rate =      7.7092 Mega-Flops/Sec.
c         Harmonic  Mean =      7.0169 Mega-Flops/Sec.
c         Standard  Dev. =      1.7944 Mega-Flops/Sec.
c1
c
c
c
c
c
c
c                    SENSITIVITY ANALYSIS
c
c
c         The sensitivity of the harmonic mean rate (Mflops)
c         to various weightings is shown in the table below.
c         Seven work distributions are generated by assigning
c         two distinct weights to ranked kernels by quartiles.
c         Forty nine possible cpu workloads are then evaluated
c         using seven sets of values for the total weights:
c
c
c             ------ ------ ------ ------ ------ ------ ------
c   1st QT:       O      O      O      O      O      X      X
c   2nd QT:       O      O      O      X      X      X      O
c   3rd QT:       O      X      X      X      O      O      O
c   4th QT:       X      X      O      O      O      O      O
c             ------ ------ ------ ------ ------ ------ ------
c   Total
c   Weights                    Net Mflops:
c    X    O
c  ---- ----
c
c  1.00 0.00    6.75   9.43  15.64  24.17  53.09  80.94 170.24
c
c  0.95 0.05    7.03   9.87  15.72  23.17  46.46  58.70 106.07
c
c  0.90 0.10    7.34  10.35  15.80  22.25  41.30  46.04  77.04
c
c  0.80 0.20    8.04  11.46  15.96  20.62  33.79  32.17  49.78
c
c  0.70 0.30    8.89  12.84  16.12  19.21  28.59  24.72  36.77
c
c  0.60 0.40    9.93  14.59  16.29  17.98  24.78  20.08  29.15
c
c  0.50 0.50   11.26  16.90  16.46  16.90  21.87  16.90  24.15
c  ---- ----
c             ------ ------ ------ ------ ------ ------ ------
c
c
c
c
c
c
c SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL)
c
c    9.80  12.08  15.74  22.58  28.86   39.96   64.97   94.54  173.54
c
c    0.00   0.20   0.40   0.60   0.70    0.80    0.90    0.95    1.00
c    Fraction Of Operations Run At Optimal Fortran Rates
c
c
c1
c
c
c  Cumulative Checksums:  RUN=    1
c
c  k    VL=  471                      90                      19
c  1  5.1146526932246750e+04  5.2533447789379716e+02  3.8551045024949644e+01
c  2  1.5397218116685108e+03  1.5397218116685108e+03  3.9532969869033877e+01
c  3  1.0007428830665901e+01  1.0097414365791693e+00  2.6993090893212646e-01
c  4  5.9992505954740238e-01  5.9992505954740238e-01  5.9992505954740238e-01
c  5  4.5488716423885198e+03  4.5890319396021141e+01  3.1826152484482577e+00
c  6  4.3751163447426225e+03  8.6316756453456492e+01  1.1203093934675863e+00
c  7  6.1042510751749156e+04  6.3455863157840940e+02  2.8457202176440319e+01
c  8  1.5012680056271516e+05  1.5012680056271516e+05  2.9605436678776314e+03
c  9  1.1894436099750828e+05  1.1894436099750828e+05  2.6239684608743992e+03
c 10  7.3103697843259200e+04  7.3103697843259200e+04  1.6512912276983698e+03
c 11  3.3429109726472735e+07  3.4335604074758245e+04  6.5511613358463364e+02
c 12  2.9071414468173629e-05  7.1275691624350657e-06  1.9434359832537496e-06
c 13  1.2025339591459033e+11  9.8163878117466309e+10  3.8471241978714600e+10
c 14  3.1655530440015411e+09  3.0399834651474237e+07  2.9235405986969322e+06
c 15  3.9438166903522797e+04  3.9438166903522797e+04  1.1089972881350550e+03
c 16  5.6507600000000000e+05  6.4804100000000000e+05  5.1521600000000000e+05
c 17  1.1146417729030727e+03  1.1146417729030727e+03  2.9473686185906899e+01
c 18  1.0157270375021873e+05  1.0157270375021873e+05  9.7006462123414531e+02
c 19  5.4218169601503541e+02  5.4218169601503541e+02  1.2682306980517353e+01
c 20  3.0406443393139362e+07  3.1262051788106910e+04  5.9877132494716716e+02
c 21  1.5973082807004452e+08  7.8245248771329880e+07  5.0099456709430933e+07
c 22  2.9386043765670911e+02  2.9386043765670911e+02  6.1099687282647608e+00
c 23  3.5499005015660077e+04  3.5499005015660077e+04  4.8503406027516576e+02
c 24  5.0000000000000000e+02  5.0000000000000000e+01  1.3000000000000000e+01
c
c
c  Cumulative Checksums:  RUN=    7
c
c  k    VL=  471                      90                      19
c  1  3.5802568852572702e+05  3.6773413452565728e+03  2.6985731517464592e+02
c  2  1.0778052681679546e+04  1.0778052681679546e+04  2.7673078908323623e+02
c  3  7.0052001814661253e+01  7.0681900560541635e+00  1.8895163625248799e+00
c  4  4.1994754168318025e+00  4.1994754168318025e+00  4.1994754168318025e+00
c  5  3.1842101496719639e+04  3.2123223577214776e+02  2.2278306739137747e+01
c  6  3.0625814413198270e+04  6.0421729517419226e+02  7.8421657542730827e+00
c  7  4.2729757526224293e+05  4.4419104210488440e+03  1.9920041523508189e+02
c  8  1.0508876039390042e+06  1.0508876039390042e+06  2.0723805675143376e+04
c  9  8.3261052698255703e+05  8.3261052698255703e+05  1.8367779226120794e+04
c 10  5.1172588490281440e+05  5.1172588490281440e+05  1.1559038593888574e+04
c 11  2.3400376808530903e+08  2.4034922852330748e+05  4.5858129350924282e+03
c 12  2.0349990127721540e-04  4.9892984137045460e-05  1.3604051882776247e-05
c 13  8.4177377140212891e+11  6.8714714682226172e+11  2.6929869385100195e+11
c 14  2.2158871308010742e+10  2.1279884256031895e+08  2.0464784190878510e+07
c 15  2.7606716832465865e+05  2.7606716832465865e+05  7.7629810169453849e+03
c 16  3.9555320000000000e+06  4.5362870000000000e+06  3.6065120000000000e+06
c 17  7.8024924103214871e+03  7.8024924103214871e+03  2.0631580330134784e+02
c 18  7.1100892625153065e+05  7.1100892625153065e+05  6.7904523486389953e+03
c 19  3.7952718721052370e+03  3.7952718721052370e+03  8.8776148863621074e+01
c 20  2.1284510375197506e+08  2.1883436251674779e+05  4.1913992746301519e+03
c 21  1.1181157964903107e+09  5.4771674139930725e+08  3.5069619696601486e+08
c 22  2.0570230635969638e+03  2.0570230635969638e+03  4.2769781097853183e+01
c 23  2.4849303510962054e+05  2.4849303510962054e+05  3.3952384219261585e+03
c 24  3.5000000000000000e+03  3.5000000000000000e+02  9.1000000000000000e+01
c1
c
c
c                          TABLE OF SPEED-UP RATIOS OF MEAN RATES (72 Samples)
c
c                          Arithmetic, Geometric, Harmonic Means (AM,GM,HM)
c                          The Geometric Mean is the least biased statistic.
c
c --------  ----  ------   -------- -------- -------- -------- -------- --------
c SYSTEM    MEAN  MFLOPS   SX-3/14  CRAY-YMP 3090s180 9000/720 6000/540 i486/25
c --------  ----  ------   -------- -------- -------- -------- -------- --------
c
c
c NEC       AM=  311.820 :    1.000    4.426   17.757   22.678   22.006  271.148
c SX-3/14   GM=   95.590 :    1.000    2.816    7.816    8.334    8.909   91.038
c F77v.012  HM=   38.730 :    1.000    2.210    4.294    4.242    5.199   42.098
c           SD=  499.780
c
c
c CRAY-YMP  AM=   70.450 :    0.226    1.000    4.012    5.124    4.972   61.261
c CRAY-YMP  GM=   33.950 :    0.355    1.000    2.776    2.960    3.164   32.333
c CFT77 4.  HM=   17.524 :    0.452    1.000    1.943    1.919    2.352   19.047
c           SD=   79.499
c
c
c IBM       AM=   17.560 :    0.056    0.249    1.000    1.277    1.239   15.270
c 3090s180  GM=   12.230 :    0.128    0.360    1.000    1.066    1.140   11.648
c VSF2.2.0  HM=    9.020 :    0.233    0.515    1.000    0.988    1.211    9.804
c           SD=   16.320
c
c
c HP        AM=   13.750 :    0.044    0.195    0.783    1.000    0.970   11.957
c 9000/720  GM=   11.470 :    0.120    0.338    0.938    1.000    1.069   10.924
c f77 8.05  HM=    9.130 :    0.236    0.521    1.012    1.000    1.226    9.924
c           SD=    7.510
c
c
c IBM       AM=   14.170 :    0.045    0.201    0.807    1.031    1.000   12.322
c 6000/540  GM=   10.730 :    0.112    0.316    0.877    0.935    1.000   10.219
c XL v0.90  HM=    7.450 :    0.192    0.425    0.826    0.816    1.000    8.098
c           SD=    9.590
c
c
c COMPAQ    AM=    1.150 :    0.004    0.016    0.065    0.084    0.081    1.000
c i486/25   GM=    1.050 :    0.011    0.031    0.086    0.092    0.098    1.000
c           HM=    0.920 :    0.024    0.053    0.102    0.101    0.123    1.000
c           SD=    0.480
c1
c
c Version: 22/DEC/86  MF508
c CHECK FOR CLOCK CALIBRATION ONLY:
c Total Job    Cpu Time =     1.57890e+02 Sec.
c Total 24 Kernels Time =     1.21139e+02 Sec.
c Total 24 Kernels Flops=     3.38982e+09 Flops
C                                                    F.H.MCMAHON  1991
C**********************************************************************
c
c
c
c    5. SAMPLE OUTPUT FILE FROM SUBROUTINe REPORT:  SUN SPARC station 1+
c
C
c >>> USE 72 SAMPLES LFK TEST RESULTS SUMMARY (line 330+)
c >>> USE ALL RANGE STATISTICS FOR OFFICIAL QUOTATIONS.
cSECOVT:     16000  0.2500E-04     1.0000
cSECOVT:     32000  0.1969E-04     0.2125
cSECOVT:     64000  0.1344E-04     0.3175
cSECOVT:    128000  0.1570E-04     0.1443
cSECOVT:    256000  0.1527E-04     0.0274
cSECOVT:    512000  0.1420E-04     0.0703
cSECOVT:   1024000  0.1488E-04     0.0459
c
c        VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
c        -----     -------     -------    -------   --------
c        EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE
c        Loop      CPUTIME     CLOCK      CLOCK     TIMING
c        SIZE      SECONDS     ERROR      ERROR     ERROR
c        -----     -------     -------    -------   --------
c            1 -0.1527E-04       0.00%    100.00%    115.28%
c            2 -0.1527E-04       0.00%    100.00%    107.64%
c            4 -0.1527E-04       0.00%    100.00%    103.82%
c            8  0.9985E-02       0.00%    201.54%    148.20%
c           16  0.9985E-02       0.00%    201.54%     24.10%
c           32  0.9985E-02       0.00%    201.54%     37.95%
c           64  0.9985E-02       0.00%     50.10%     24.81%
c          128  0.1999E-01       0.00%     33.38%      6.33%
c          256  0.2999E-01       0.00%     18.85%      1.54%
c          512  0.5998E-01       0.00%      7.69%      1.57%
c         1024  0.1100E+00       0.00%      3.92%      0.37%
c         2048  0.2100E+00       0.00%      2.38%      0.62%
c         4096  0.4100E+00       0.00%      0.00%      0.13%
c         8192  0.8200E+00       0.00%      0.49%      0.11%
c        13600              Current Run:   MULTI=  200.000
c        16384  0.1640E+01       0.00%      0.30%      0.11%
c        32768  0.3280E+01       0.00%      0.15%      0.02%
c        -----     -------     -------    -------   --------
c
c Estimated Total Job Cpu-Time=  2593.310 sec.  ( Nruns=       7 Trials)
c
c Trial=      1             ChkSum= 1146    Pass=      0     Fail=      0
c  Time=          452.71sec
c Trial=      2             ChkSum= 1146    Pass=      1     Fail=      0
c  Time=          782.49sec
c Trial=      3             ChkSum= 1146    Pass=      2     Fail=      0
c  Time=         1110.76sec
c Trial=      4             ChkSum= 1146    Pass=      3     Fail=      0
c  Time=         1437.19sec
c Trial=      5             ChkSum= 1146    Pass=      4     Fail=      0
c  Time=         1794.01sec
c Trial=      6             ChkSum= 1146    Pass=      5     Fail=      0
c  Time=         2150.69sec
c Trial=      7             ChkSum= 1146    Pass=      6     Fail=      0
c  Time=         2477.55sec
c1
c
c
c time TEST overhead (t err):
c
c      RUN        AVERAGE        STANDEV        MINIMUM        MAXIMUM
c TICK   1   0.249996E-05   0.451660E-10
c TICK   2   0.246863E-05   0.311374E-07
c TICK   3   0.248439E-05   0.154251E-07
c TICK   4   0.250008E-05   0.839114E-09
c TICK   5   0.250005E-05   0.868416E-09
c TICK   6   0.250005E-05   0.861573E-09
c TICK   7   0.250010E-05   0.818610E-09
c DATA   7   0.999866E-01   0.543320E-06   0.999856E-01   0.999877E-01
c DATA   7   0.999859E-01   0.862081E-06   0.999843E-01   0.999875E-01
c TICK   7   0.249332E-05   0.114356E-07   0.246863E-05   0.250010E-05
c
c
c THE EXPERIMENTAL TIMING ERRORS FOR ALL  7 RUNS
c --  ---------  ---------  --------- -----  -----   ---
c  k   T min      T avg      T max    T err   tick   P-F
c --  ---------  ---------  --------- -----  -----   ---
c  1 0.3606E+01 0.3609E+01 0.3627E+01  0.19%  0.00%     0
c  2 0.2726E+01 0.2735E+01 0.2737E+01  0.13%  0.00%     0
c  3 0.1135E+01 0.1141E+01 0.1146E+01  0.44%  0.00%     0
c  4 0.1063E+01 0.1069E+01 0.1073E+01  0.46%  0.00%     0
c  5 0.3065E+01 0.3066E+01 0.3075E+01  0.11%  0.00%     0
c  6 0.1768E+01 0.1769E+01 0.1769E+01  0.00%  0.00%     0
c  7 0.4608E+01 0.4619E+01 0.4628E+01  0.14%  0.00%     0
c  8 0.7105E+01 0.7109E+01 0.7115E+01  0.07%  0.00%     0
c  9 0.4332E+01 0.4338E+01 0.4342E+01  0.11%  0.00%     0
c 10 0.7303E+01 0.7307E+01 0.7313E+01  0.07%  0.00%     0
c 11 0.2404E+01 0.2409E+01 0.2415E+01  0.20%  0.00%     0
c 12 0.2404E+01 0.2408E+01 0.2414E+01  0.21%  0.00%     0
c 13 0.5062E+01 0.5066E+01 0.5072E+01  0.10%  0.00%     0
c 14 0.6379E+01 0.6382E+01 0.6389E+01  0.07%  0.00%     0
c 15 0.5790E+01 0.5797E+01 0.5800E+01  0.08%  0.00%     0
c 16 0.2017E+01 0.2020E+01 0.2028E+01  0.23%  0.00%     0
c 17 0.2712E+01 0.2718E+01 0.2722E+01  0.18%  0.00%     0
c 18 0.4549E+01 0.4553E+01 0.4559E+01  0.11%  0.00%     0
c 19 0.3850E+01 0.3856E+01 0.3861E+01  0.13%  0.00%     0
c 20 0.3530E+01 0.3535E+01 0.3540E+01  0.14%  0.00%     0
c 21 0.1274E+02 0.1276E+02 0.1281E+02  0.18%  0.00%     0
c 22 0.3884E+01 0.3892E+01 0.3895E+01  0.12%  0.00%     0
c 23 0.3886E+01 0.3889E+01 0.3896E+01  0.12%  0.00%     0
c 24 0.9975E+00 0.1003E+01 0.1008E+01  0.49%  0.00%     0
c --  ---------  ---------  --------- -----  -----   ---
c
c
c NET CPU TIMING VARIANCE (T err);  A few % is ok:
c
c                 AVERAGE        STANDEV        MINIMUM        MAXIMUM
c     Terr          0.17%          0.12%          0.00%          0.49%
c
c
c
c
c
c ********************************************
c THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *
c ********************************************
c
c              Computer : SUN SPARC 1+
c              System   : UNIX BSD 4
c              Compiler : f77 V1.4
c              Date     : 91.08.14
c         Mean DO Span  =   167
c         When the computer performance range is very large
c         the net Mflops rate of many Fortran programs and
c         workloads will be in the sub-range between the equi-
c         weighted Harmonic and Arithmetic means depending
c         on the degree of code parallelism and optimization.
c         The least biased central measure is the Geometric
c         Mean of 72 rates,  quoted +- a standard deviation.
c         Mean Mflops rates imply the average efficiency of a
c         computing system since the peak rate is well known.
c         LFK test measures a lower bound for a Multi-processor
c         and N * LFK rates project an upper bound for N-procs.
c
c KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CHECK-SUMS             OK
c ------  -----   --------   --------- ---- ------  ---------------------- --
c  1 6.0480E+06 3.1826E+06      1.9003   27   1.00  2.6985731517464728E+02 16
c  2 3.2384E+06 1.9908E+06      1.6267   15   1.00  2.7673078908321412E+02 16
c  3 3.1968E+06 1.0995E+06      2.9074   27   1.00  1.8895163625244704E+00 16
c  4 1.8240E+06 1.1913E+06      1.5311   27   1.00  4.1994754168317234E+00 16
c  5 3.3280E+06 2.5375E+06      1.3115   27   1.00  2.2278306739132379E+01 16
c  6 1.6128E+06 1.7233E+06      0.9359    8   1.00  7.8421657542696117E+00 15
c  7 1.0752E+07 3.8688E+06      2.7792   21   1.00  1.9920041523508166E+02 16
c  8 1.3478E+07 6.4283E+06      2.0967   14   1.00  2.0723805675125019E+04 16
c  9 1.0608E+07 3.6977E+06      2.8688   15   1.00  1.8367779226119754E+04 16
c 10 5.4000E+06 6.3503E+06      0.8504   15   1.00  1.1559038593887853E+04 16
c 11 1.9136E+06 2.2822E+06      0.8385   27   1.00  4.5858129350920399E+03 16
c 12 1.9968E+06 2.1599E+06      0.9245   26   1.00  1.3604051867913136E-05 16
c 13 2.7776E+06 4.4606E+06      0.6227    8   1.00  8.1274474685488098E+10 15
c 14 3.8016E+06 4.7952E+06      0.7928   27   1.00  1.8264361844941873E+07 16
c 15 3.6960E+06 6.3375E+06      0.5832   15   1.00  7.7629810169434950E+03 16
c 16 2.4640E+06 1.9713E+06      1.2499   15   1.00  1.8033120000000000E+06 16
c 17 5.6160E+06 2.3734E+06      2.3662   15   1.00  2.0631580330125522E+02 16
c 18 9.1520E+06 4.9492E+06      1.8492   14   1.00  6.7904523486359285E+03 16
c 19 4.0320E+06 3.4454E+06      1.1703   15   1.00  8.8776148863570242E+01 15
c 20 7.5712E+06 4.8563E+06      1.5590   26   1.00  4.1913992746327112E+03 16
c 21 4.0000E+07 2.0945E+07      1.9098   20   1.00  1.7539193044904393E+08 16
c 22 3.2640E+06 3.3795E+06      0.9658   15   1.00  4.2769781097847805E+01 16
c 23 8.0080E+06 3.6221E+06      2.2109   14   1.00  3.3952384219249789E+03 16
c 24 9.5680E+05 9.7110E+05      0.9853   27   1.00  9.1000000000000000E+01 16
c  1 8.0800E+06 4.0887E+06      1.9762  101   2.00  3.6773413452565801E+03 16
c  2 6.2080E+06 3.2629E+06      1.9026  101   2.00  1.0778052681678693E+04 16
c  3 4.2824E+06 1.7771E+06      2.4097  101   2.00  7.0681900560526625E+00 16
c  4 3.3600E+06 1.3273E+06      2.5314  101   2.00  4.1994754168317234E+00 16
c  5 4.4000E+06 3.2965E+06      1.3347  101   2.00  3.2123223577206875E+02 16
c  6 2.6880E+06 2.0630E+06      1.3030   32   2.00  6.0421729517332471E+02 16
c  7 1.4221E+07 5.0709E+06      2.8044  101   2.00  4.4419104210488395E+03 16
c  8 1.7107E+07 8.5283E+06      2.0059  100   2.00  1.0508876039380585E+06 16
c  9 1.4423E+07 5.0662E+06      2.8469  101   2.00  8.3261052698248636E+05 15
c 10 6.9084E+06 8.1696E+06      0.8456  101   2.00  5.1172588490277075E+05 16
c 11 2.5600E+06 2.8390E+06      0.9017  101   2.00  2.4034922852330303E+05 16
c 12 2.7200E+06 2.7422E+06      0.9919  100   2.00  4.9892983915750255E-05 16
c 13 3.6736E+06 5.8005E+06      0.6333   32   2.00  2.4799100425819748E+11 16
c 14 4.4440E+06 5.7100E+06      0.7783  101   2.00  2.1111605770897460E+08 15
c 15 6.6000E+06 1.1588E+07      0.5696  101   2.00  2.7606716832464293E+05 16
c 16 3.0240E+06 2.4488E+06      1.2349   40   2.00  2.2682870000000000E+06 16
c 17 7.2720E+06 3.1101E+06      2.3382  101   2.00  7.8024924103174017E+03 16
c 18 8.7120E+06 4.5518E+06      1.9140  100   2.00  5.2551705030583183E+05 16
c 19 5.5752E+06 4.5499E+06      1.2254  101   2.00  3.7952718721030451E+03 16
c 20 8.3200E+06 5.2934E+06      1.5718  100   2.00  2.1883436251708021E+05 16
c 21 2.5000E+07 1.2970E+07      1.9275   50   2.00  2.7413199222149867E+08 16
c 22 4.8076E+06 4.9544E+06      0.9704  101   2.00  2.0570230635966882E+03 16
c 23 1.0890E+07 4.8564E+06      2.2424  100   2.00  2.4849303510945366E+05 16
c 24 1.2400E+06 1.2491E+06      0.9927  101   2.00  3.5000000000000000E+02 16
c  1 7.0070E+06 3.6093E+06      1.9414 1001   1.00  3.5802568852572696E+05 16
c  2 5.1992E+06 2.7352E+06      1.9009  101   1.00  1.0778052681678693E+04 16
c  3 3.6036E+06 1.1412E+06      3.1576 1001   1.00  7.0052001814645450E+01 15
c  4 3.3600E+06 1.0687E+06      3.1439 1001   1.00  4.1994754168317234E+00 16
c  5 4.0000E+06 3.0664E+06      1.3045 1001   1.00  3.1842101496710871E+04 16
c  6 2.3808E+06 1.7685E+06      1.3462   64   1.00  3.0625814413109896E+04 16
c  7 1.2736E+07 4.6194E+06      2.7571  995   1.00  4.2729757526223321E+05 16
c  8 1.4256E+07 7.1093E+06      2.0053  100   1.00  1.0508876039380585E+06 16
c  9 1.2362E+07 4.3377E+06      2.8500  101   1.00  8.3261052698248636E+05 15
c 10 6.1812E+06 7.3073E+06      0.8459  101   1.00  5.1172588490277075E+05 16
c 11 2.2000E+06 2.4088E+06      0.9133 1001   1.00  2.3400376808550769E+08 16
c 12 2.4000E+06 2.4083E+06      0.9966 1000   1.00  2.0349989059170737E-04 16
c 13 3.2256E+06 5.0663E+06      0.6367   64   1.00  3.4706712065081335E+11 16
c 14 4.4044E+06 6.3818E+06      0.6901 1001   1.00  2.2156947925784698E+10 16
c 15 3.3000E+06 5.7966E+06      0.5693  101   1.00  2.7606716832464293E+05 16
c 16 2.6500E+06 2.0204E+06      1.3116   75   1.00  1.9780320000000000E+06 16
c 17 6.3630E+06 2.7183E+06      2.3408  101   1.00  7.8024924103174017E+03 16
c 18 8.7120E+06 4.5532E+06      1.9134  100   1.00  5.2551705030583183E+05 16
c 19 4.7268E+06 3.8562E+06      1.2258  101   1.00  3.7952718721030451E+03 16
c 20 5.2000E+06 3.5352E+06      1.4709 1000   1.00  2.1284510375458673E+08 16
c 21 2.5250E+07 1.2755E+07      1.9796  101   1.00  5.6017393194626498E+08 16
c 22 3.7774E+06 3.8916E+06      0.9706  101   1.00  2.0570230635966882E+03 16
c 23 8.7120E+06 3.8889E+06      2.2402  100   1.00  2.4849303510945366E+05 16
c 24 1.0000E+06 1.0032E+06      0.9968 1001   1.00  3.5000000000000000E+03 16
c ------  -----   --------   --------- ---- ------  ---------------------- --
c 72 0.4843E+09 0.3110E+09      1.5572  167                              1145
c
c         MFLOPS    RANGE:             REPORT ALL RANGE STATISTICS:
c         Maximum   Rate =      3.1576 Mega-Flops/Sec.
c         Quartile  Q3   =      2.0059 Mega-Flops/Sec.
c         Average   Rate =      1.5922 Mega-Flops/Sec.
c         Geometric Mean =      1.4275 Mega-Flops/Sec.
c         Median    Q2   =      1.3462 Mega-Flops/Sec.
c         Harmonic  Mean =      1.2712 Mega-Flops/Sec.
c         Quartile  Q1   =      0.9681 Mega-Flops/Sec.
c         Minimum   Rate =      0.5693 Mega-Flops/Sec.
c
c
c         Standard  Dev. =      0.7177 Mega-Flops/Sec.
c         Avg Efficiency =     45.21%  Program & Processor
c         Mean Precision =     15.90   Decimal Digits
c <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >
c < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >
c <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>
c1
c1
c
c
c
c
c
c
c                    SENSITIVITY ANALYSIS
c
c
c         The sensitivity of the harmonic mean rate (Mflops)
c         to various weightings is shown in the table below.
c         Seven work distributions are generated by assigning
c         two distinct weights to ranked kernels by quartiles.
c         Forty nine possible cpu workloads are then evaluated
c         using seven sets of values for the total weights:
c
c
c             ------ ------ ------ ------ ------ ------ ------
c   1st QT:       O      O      O      O      O      X      X
c   2nd QT:       O      O      O      X      X      X      O
c   3rd QT:       O      X      X      X      O      O      O
c   4th QT:       X      X      O      O      O      O      O
c             ------ ------ ------ ------ ------ ------ ------
c   Total
c   Weights                    Net Mflops:
c    X    O
c  ---- ----
c
c  1.00 0.00    0.75   0.90   1.14   1.40   1.81   2.12   2.56
c
c  0.95 0.05    0.77   0.93   1.15   1.39   1.76   1.99   2.40
c
c  0.90 0.10    0.79   0.96   1.16   1.37   1.71   1.87   2.26
c
c  0.80 0.20    0.84   1.02   1.17   1.34   1.62   1.67   2.01
c
c  0.70 0.30    0.89   1.09   1.19   1.32   1.54   1.51   1.82
c
c  0.60 0.40    0.96   1.17   1.21   1.29   1.47   1.38   1.66
c
c  0.50 0.50    1.03   1.27   1.22   1.27   1.41   1.27   1.52
c  ---- ----
c             ------ ------ ------ ------ ------ ------ ------
c
c
c
c
c
c
c SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL)
c
c    0.91   1.04   1.22   1.48   1.65    1.87    2.16    2.34    2.55
c
c    0.00   0.20   0.40   0.60   0.70    0.80    0.90    0.95    1.00
c    Fraction Of Operations Run At Optimal Fortran Rates
c
c
c1
c
c
c                          TABLE OF SPEED-UP RATIOS OF MEAN RATES (72 Samples)
c
c                          Arithmetic, Geometric, Harmonic Means (AM,GM,HM)
c                          The Geometric Mean is the least biased statistic.
c
c --------  ----  ------   -------- -------- -------- -------- -------- --------
c SYSTEM    MEAN  MFLOPS   SX-3/14  YMP/1    9000/720 6000/540 CRAY-YMP i486/25
c --------  ----  ------   -------- -------- -------- -------- -------- --------
c
c
c NEC       AM=  311.820 :    1.000    3.986   22.678   22.006  195.844  271.148
c SX-3/14   GM=   95.590 :    1.000    2.610    8.334    8.909   66.965   91.038
c F77v.012  HM=   38.730 :    1.000    2.193    4.242    5.199   30.466   42.098
c           SD=  499.780
c
c
c CRAY      AM=   78.230 :    0.251    1.000    5.689    5.521   49.134   68.026
c YMP/1     GM=   36.630 :    0.383    1.000    3.194    3.414   25.661   34.886
c CFT771.2  HM=   17.660 :    0.456    1.000    1.934    2.370   13.892   19.196
c           SD=   86.750
c
c
c HP        AM=   13.750 :    0.044    0.176    1.000    0.970    8.636   11.957
c 9000/720  GM=   11.470 :    0.120    0.313    1.000    1.069    8.035   10.924
c f77 8.05  HM=    9.130 :    0.236    0.517    1.000    1.226    7.182    9.924
c           SD=    7.510
c
c
c IBM       AM=   14.170 :    0.045    0.181    1.031    1.000    8.900   12.322
c 6000/540  GM=   10.730 :    0.112    0.293    0.935    1.000    7.517   10.219
c XL v0.90  HM=    7.450 :    0.192    0.422    0.816    1.000    5.860    8.098
c           SD=    9.590
c
c
c SUN       AM=    1.592 :    0.005    0.020    0.116    0.112    1.000    1.385
c SPARC 1+  GM=    1.427 :    0.015    0.039    0.124    0.133    1.000    1.359
c f77 V1.4  HM=    1.271 :    0.033    0.072    0.139    0.171    1.000    1.382
c           SD=    0.718
c
c
c COMPAQ    AM=    1.150 :    0.004    0.015    0.084    0.081    0.722    1.000
c i486/25   GM=    1.050 :    0.011    0.029    0.092    0.098    0.736    1.000
c           HM=    0.920 :    0.024    0.052    0.101    0.123    0.724    1.000
c           SD=    0.480
c
c
c Version: 22/DEC/86  MF508
c CHECK FOR CLOCK CALIBRATION ONLY:
c Total Job    Cpu Time =     2.47860E+03 Sec.
c Total 24 Kernels Time =     2.17686E+03 Sec.
c Total 24 Kernels Flops=     3.38982E+09 Flops
C
C                                                    F.H.MCMAHON  1991
C**********************************************************************
c
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  sum                                            REDUNDNT
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      parameter(  nt= 4 )
C
      CHARACTER  NAME*8
      CHARACTER  Komput*24, Kontrl*24, Kompil*24, Kalend*24, Identy*24  
C
      COMMON /SYSID/ Komput, Kontrl, Kompil, Kalend, Identy

      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
      DIMENSION  NAME(nt), RATE(nt)
      DIMENSION  FLOPS(141), TR(141), RATES(141)
      DIMENSION  LSPAN(141), WG(141), OSUM (141), ID(141)
      DIMENSION  HM(12), LVL(10)
      DIMENSION  LQ(5), STAT1(20), STAT2(20)
      DIMENSION  IN(141), CSUM1(141), TV4(141), TV5(141)
      DIMENSION  MAP1(141), MAP2(141), MAP3(141), IN2(141), VL1(141)
      DIMENSION  MAP(141), VL(141), TV(141), TV1(141), TV2(141)
      DIMENSION  FLOPS1(141), RT1(141), ISPAN1(141), WT1(141)
      DIMENSION  FLOPS2(141), RT2(141), ISPAN2(141), WT2(141)
      SAVE    kall,  LVL
C
       MODI(i,mm)= (MOD( ABS(i)-1, mm) + 1)
C
      DATA  kall/0/
C
      CALL TRACE ('REPORT  ')
C
          IF( iou.LT.0) GO TO 73
C
            meff= 0
            neff= 0
            fuzz= 1.0d-9
       DO 1000 k= 1,ntk
           VL(k)= LSPAN(k)
 1000  CONTINUE
C
              bl= 1.0d-5
              bu= 1.0d+5
            CALL  VALID( TV,MAP,neff,  bl, RATES, bu, ntk)
C
C      Compress valid data sets mapping on MAP.
C
              nd= 0
        DO  1  k= 1,neff
         MAP1(k)=  MODI( MAP(k),nek)
       FLOPS1(k)= FLOPS( MAP(k))
          RT1(k)=    TR( MAP(k))
          VL1(k)=    VL( MAP(k))
       ISPAN1(k)= LSPAN( MAP(k))
          WT1(k)=    WG( MAP(k))
          TV1(k)= RATES( MAP(k))
        CSUM1(k)=  OSUM( MAP(k))
              nd=    ID( MAP(k)) + nd
    1  continue
              IF( nd .LE. 8*neff )  nd= nd - 16*((neff-1+24)/24)
          precis= REAL(nd)/( REAL(neff) + fuzz)
C
             som= 0.00d0
             sum= 0.00d0
        DO 11  k= 1,neff
             som= som + FLOPS1(k)
             sum= sum + RT1(k)
   11  continue
           rneto= som/(sum + fuzz)
C
            CALL  STATW( STAT1,TV,IN, VL1,WT1,neff)
              lv= STAT1(1)
C
            CALL  STATW( STAT1,TV,IN, TV1,WT1,neff)
             twt= STAT1(6)
C                             compute average efficiency= GM/Max
            kall= kall +  1
            peak= 0.00d0
              if( kall.LE.1 .OR. il.EQ.im ) then
                  peak= STAT1(4)
              endif
          avgeff= (100.0d0* STAT1(10))/( peak + fuzz)
C
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
       CALL PAGE( iou)
          WRITE ( iou,7002)
C
      IF( ntk .EQ. nek )  THEN
          WRITE ( iou,7003)
      ELSE
          WRITE ( iou,7090)
      ENDIF
C
          WRITE ( iou,7002)
          WRITE ( iou,7007)  Komput
          WRITE ( iou,7057)  Kontrl
          WRITE ( iou,7008)  Kompil
          WRITE ( iou,7038)  Kalend
          WRITE ( iou,7039)  Identy
          WRITE ( iou,7061)
          WRITE ( iou,7062)
          WRITE ( iou,7063)
          WRITE ( iou,7064)
          WRITE ( iou,7065)
          WRITE ( iou,7066)
          WRITE ( iou,7067)
          WRITE ( iou,7071)
          WRITE ( iou,7072)
          WRITE ( iou,7068)
          WRITE ( iou,7069)
c         WRITE ( iou,7001)
          WRITE ( iou,7004)
          WRITE ( iou,7005)
          WRITE ( iou,7011) (MAP1(k),  FLOPS1(k), RT1(k), TV1(k),
     .                    ISPAN1(k), WT1(k), CSUM1(k), ID(k), k=1,neff)
          WRITE ( iou,7005)
C
          WRITE ( iou,7023)  neff, som, sum, rneto, lv, nd
          WRITE ( iou,7022)
          WRITE ( iou,7009)  lv
          WRITE ( iou,7010)  ntk
          WRITE ( iou,7041)  STAT1( 4)
          WRITE ( iou,7037)  STAT1(14)
          WRITE ( iou,7033)  STAT1( 1)
          WRITE ( iou,7043)  STAT1(10)
          WRITE ( iou,7030)  STAT1( 7)
          WRITE ( iou,7055)  STAT1( 5)
          WRITE ( iou,7036)  STAT1(13)
          WRITE ( iou,7042)  STAT1( 3)
          WRITE ( iou,7001)
          WRITE ( iou,7044)  STAT1( 2)
          WRITE ( iou,7091)  avgeff
          WRITE ( iou,7034)  precis
C
      IF( ntk .NE. nek )  THEN
          WRITE (   *,7001)
          WRITE (   *,7002)
          WRITE (   *,7090)
          WRITE (   *,7002)
          WRITE (   *,7007)  Komput
          WRITE (   *,7057)  Kontrl
          WRITE (   *,7008)  Kompil
          WRITE (   *,7038)  Kalend
          WRITE (   *,7039)  Identy
          WRITE (   *,7022)
          WRITE (   *,7009)  lv
          WRITE (   *,7010)  ntk
          WRITE (   *,7041)  STAT1( 4)
          WRITE (   *,7037)  STAT1(14)
          WRITE (   *,7033)  STAT1( 1)
          WRITE (   *,7043)  STAT1(10)
          WRITE (   *,7030)  STAT1( 7)
          WRITE (   *,7055)  STAT1( 5)
          WRITE (   *,7036)  STAT1(13)
          WRITE (   *,7042)  STAT1( 3)
          WRITE (   *,7001)
          WRITE (   *,7044)  STAT1( 2)
          WRITE (   *,7091)  avgeff
          WRITE (   *,7034)  precis
      ENDIF
C
C         WRITE ( iou,7031)  STAT1( 9)
C         WRITE ( iou,7032)  STAT1(15)
C
 7001 FORMAT(/)
 7002 FORMAT(  ' ******************************************** ')
 7003 FORMAT(  ' THE LIVERMORE  FORTRAN KERNELS:  M F L O P S  ')
 7090 FORMAT(  ' THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *  ')
 7004 FORMAT(/,' KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CH',
     X'ECK-SUMS             OK ')
 7005 FORMAT(  ' ------  -----   --------   --------- ---- ------  --',
     X'-------------------- -- ')
 7007 FORMAT(/,9X,'     Computer :  ',A )                                f77
 7057 FORMAT(  9X,'     System   :  ',A )                                f77
 7008 FORMAT(  9X,'     Compiler :  ',A )                                f77
 7038 FORMAT(  9X,'     Date     :  ',A )                                f77
 7039 FORMAT(  9X,'     Testor   :  ',A )                                f77
c7009 FORMAT(/,9X,'     Computer :  ',A8)                                f66
c7057 FORMAT(  9X,'     System   :  ',A8)                                f66
c7008 FORMAT(  9X,'     Compiler :  ',A8)                                f66
c7038 FORMAT(  9X,'     Date     :  ',A8)                                f66
 7009 FORMAT(  9X,'Mean DO Span   =  ',I5)
 7010 FORMAT(  9X,'Code Samples   =  ',I5)
 7011 FORMAT(1X,i2,1PE11.4,E11.4,0PF12.4,1X,I4,1X,F6.2,1PE24.16,1X,I2)
C7011 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,1X,F6.2,E35.25,1X,I2)
 7012 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,1X,F6.2)
 7023 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,30X,I4)
c7022 FORMAT(/,' MFLOPS  RANGE:,23X,28HREPORT ALL RANGE STATISTICS: ')  f66
 7022 FORMAT(/,9X,'MFLOPS    RANGE:',13X,'REPORT ALL RANGE STATISTICS:') f77
 7041 FORMAT(/,9X,'Maximum   Rate =  ',F12.4,' Mega-Flops/Sec. ')
 7037 FORMAT(  9X,'Quartile  Q3   =  ',F12.4,' Mega-Flops/Sec. ')
 7033 FORMAT(  9X,'Average   Rate =  ',F12.4,' Mega-Flops/Sec. ')
 7043 FORMAT(  9X,'Geometric Mean =  ',F12.4,' Mega-Flops/Sec. ')
 7030 FORMAT(  9X,'Median    Q2   =  ',F12.4,' Mega-Flops/Sec. ')
 7055 FORMAT(  9X,'Harmonic  Mean =  ',F12.4,' Mega-Flops/Sec. ')
 7036 FORMAT(  9X,'Quartile  Q1   =  ',F12.4,' Mega-Flops/Sec. ')
 7042 FORMAT(  9X,'Minimum   Rate =  ',F12.4,' Mega-Flops/Sec. ')
 7044 FORMAT(  9X,'Standard  Dev. =  ',F12.4,' Mega-Flops/Sec. ')
c7031 FORMAT(  9X,'Median    Dev. =  ',F12.4,' Mega-Flops/Sec. ')
c7032 FORMAT(  9X,'Geom.Mean Dev. =  ',F12.4,' Mega-Flops/Sec. ')
 7091 FORMAT(  9X,'Avg Efficiency =  ',F10.2,'%  Program & Processor')
 7034 FORMAT(  9X,'Mean Precision =  ',F10.2,'   Decimal Digits ')
 7053 FORMAT(/,9X,'Frac.  Weights =  ',F12.4)
 7104 FORMAT(/,' KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT   ')
 7105 FORMAT(  ' ------  -----   --------   --------- ---- ------   ')
C
 7061 FORMAT(/,9X,'When the computer performance range is very large ')
 7062 FORMAT(9X,'the net Mflops rate of many Fortran programs and    ')
 7063 FORMAT(9X,'workloads will be in the sub-range between the equi-')
 7064 FORMAT(9X,'weighted Harmonic and Arithmetic means depending    ')
 7065 FORMAT(9X,'on the degree of code parallelism and optimization. ')
c7066 FORMAT(9X,'More accurate estimates of cpu workload rates depend')
c7067 FORMAT(9X,'on assigning appropriate weights for each kernel.   ')
c7066 FORMAT(9X,'The best central measure is the Geometric Mean of 72')
c7067 FORMAT(9X,'rates which must be quoted +- a standard deviation. ')
 7066 FORMAT(9X,'The least biased central measure is the Geometric ')
 7067 FORMAT(9X,'Mean of 72 rates,  quoted +- a standard deviation.')
 7068 FORMAT(9X,'LFK test measures a lower bound for a Multi-processor')
 7069 FORMAT(9X,'and N * LFK rates project an upper bound for N-procs.')
 7071 FORMAT(9X,'Mean Mflops rates imply the average efficiency of a')
 7072 FORMAT(9X,'computing system since the peak rate is well known.')
C
      NAME(1)= Komput
      NAME(2)= Komput
      NAME(3)= Kompil
      RATE(1)= STAT1(1)
      RATE(2)= STAT1(10)
      RATE(3)= STAT1(5)
      RATE(4)= STAT1(2)
C
      IF( ntk .NE. nek )  THEN
      WRITE( iou,7099)
      WRITE( iou,7097)
      WRITE( iou,7098)
      WRITE( iou,7099)
 7097 FORMAT(' < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >')
 7098 FORMAT(' < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >')
 7099 FORMAT(' <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>')
       CALL PAGE( iou)
C
       IF( iovec.EQ.1 ) THEN
          WRITE ( iou,7070)
 7070 FORMAT(//,' TOP QUARTILE: BEST ARCHITECTURE/APPLICATION MATCH ')
C
C      Compute compression index-list MAP1:  Non-zero weights.
C
              bl= 1.0d-6
              bu= 1.0d+6
            CALL  VALID( TV,MAP1,meff,  bl, WT1, bu, neff)
C
C      Re-order data sets mapping on IN (descending order of MFlops).
C
        DO  2  k= 1,meff
         MAP3(k)=     IN( MAP1(k))
    2  continue
C
          IF( meff.GT.0 )  THEN
              CALL TRAP( MAP3, ' REPORT  ', 1, neff,meff)
          ENDIF
C
        DO  3  k= 1,meff
               i=   MAP3(k)
        FLOPS2(k)=  FLOPS1(i)
          RT2(k)=    RT1(i)
       ISPAN2(k)= ISPAN1(i)
          WT2(k)=    WT1(i)
          TV2(k)=    TV1(i)
         MAP2(k)=   MODI( MAP(i),nek)
    3  continue
C                             Sort kernels by performance into quartiles
              nq= meff/4
              lo= meff -4*nq
           LQ(1)= nq
           LQ(2)= nq + nq + lo
           LQ(3)= nq
              i2= 0
C
         DO 5  j= 1,3
              i1= i2 + 1
              i2= i2 + LQ(j)
              ll= i2 - i1 + 1
            CALL  STATW( STAT2,TV,IN2, TV2(i1),WT2(i1),ll)
            frac= STAT2(6)/( twt +fuzz)
C
          WRITE ( iou,7001)
          WRITE ( iou,7104)
          WRITE ( iou,7105)
          WRITE ( iou,7012) ( MAP2(k),  FLOPS2(k), RT2(k), TV2(k),
     .                         ISPAN2(k), WT2(k),  k=i1,i2 )
          WRITE ( iou,7105)
C
          WRITE ( iou,7053)  frac
          WRITE ( iou,7033)  STAT2(1)
          WRITE ( iou,7055)  STAT2(5)
          WRITE ( iou,7044)  STAT2(2)
    5 continue
C
       ENDIF
C
      ENDIF
C
C           Sensitivity analysis of harmonic mean rate to 49 workloads
C
      CALL  SENSIT(   iou,RATES,WG,IQ,SUMW, MAP,TV,TV4,TV2,TV5, ntk)
C
C
C           Sensitivity analysis of harmonic mean rate to SISD/SIMD model
C
      CALL  SIMD( HM, iou,RATES,WG,FR,9,    MAP,TV,TV4,TV2, ntk)
C
C
      IF( ntk .NE. nek )  THEN
        IF( iovec.EQ.1 )  THEN
               CALL  PAGE( iou)
               mrl= Nruns
                IF( Nruns.gt.8) mrl= 8
C
      DO  8      k= 1,mk
      DO  8      j= im,ml
               sum= 0.0d0
      DO  8      i= 1,mrl
               sum= sum + CSUMS(i,j,k)
      CSUMS(i,j,k)= sum
    8 continue
C
      DO  10     i= 1,mrl
                IF( (i.NE.1).AND.(i.NE.mrl))  GO TO 10
             WRITE( iou,76) i
             WRITE( iou,77)  ( LVL(j), j= 1,3 )
   76       FORMAT( //,'  Cumulative Checksums:  RUN=',i5)
   77       FORMAT( /,'  k    VL=',i5,3i24)
C
      DO  9      k= 1,mk
             WRITE( iou,78)  k, ( CSUMS(i,j,k), j= 1,3)
   78       FORMAT( 1X,I2,4E24.16)
    9 continue
   10 continue
        ENDIF
C
      CALL SPEDUP( iou, NAME, RATE )
      ENDIF
          LVL(il)= lv
   73 CONTINUE
      CALL TRACK ('REPORT  ')
      RETURN
C
      END
C**********************************************
      SUBROUTINE RESULT( iou,FLOPS,TR,RATES,LSPAN,WG,OSUM,TERR,ID)
C***********************************************************************
C                                                                      *
C     RESULT -  Computes timing Results into pushdown store.           *
C                                                                      *
C      iou   -  Input   IO unit number for print output                *
C     FLOPS  - Out.Ary  Number of Flops executed by each kernel        *
C     TR     - Out.Ary  Time of execution of each kernel(microsecs)    *
C     RATES  - Out.Ary  Rate of execution of each kernel(megaflops/sec)*
C     LSPAN  - Out.Ary  Span of inner DO loop in each kernel           *
C     WG     - Out.Ary  Weight assigned to each kernel for statistics  *
C     OSUM   - Out.Ary  Checksums of the results of each kernel        *
C     TERR   - Out.Ary  Experimental timing errors per kernel          *
C     ID     - Out.Ary  Number of valid digits in checksum.            *
C                                                                      *
C***********************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  SUMS, cs                                       REDUNDNT
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      DIMENSION  FLOPS(141), TR(141), RATES(141), ID(141)
      DIMENSION  LSPAN(141), WG(141), OSUM (141), TERR(141)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /PROOF/  SUMS(24,3,8)
C
C
      CALL TRACE ('RESULT  ')
C
           CALL  TALLY( iou, 1 )
C
C                             Push Result Arrays Down before entering new result
            isum= 0
           limit= 141 - mk
              j = 141
      DO 1001 k = limit,1,-1
        FLOPS(j)= FLOPS(k)
           TR(j)=    TR(k)
        RATES(j)= RATES(k)
        LSPAN(j)= LSPAN(k)
           WG(j)=    WG(k)
         OSUM(j)=  OSUM(k)
         TERR(j)=  TERR(k)
           ID(j)=    ID(k)
              j = j - 1
 1001 CONTINUE
C
C                             CALCULATE MFLOPS FOR EACH KERNEL
C                          setting RATES(k)= 0. deletes kernel k from REPORT.
            tmin= 1.0d0*tsecov
      DO 1010 k = 1,mk
        FLOPS(k)= FLOPN(k)*TOTAL(k)
           TR(k)=  TIME(k) * 1.0d+6
        RATES(k)= 0.0d0
              IF( TR(k).NE. 0.0d0)   RATES(k)= FLOPS(k)/TR(k)
              IF( WT(k).LE. 0.0d0)   RATES(k)= 0.0d0
              IF( TIME(k).LT.tmin)   RATES(k)= 0.0d0
              IF( TIME(k).LE. 0.0d0) RATES(k)= 0.0d0
        LSPAN(k)= ISPAN(k,il)
           WG(k)= WT(k)*WTP(il)
         OSUM(k)= CSUM(k)
         TERR(k)= TERR1(k)
c
c                 compute relative error and digits of precision in CSUM
c
c
                           ijk= 4
      IF( MULTI.LE.   1 )  ijk= 1
      IF( MULTI.EQ.  10 )  ijk= 2
      IF( MULTI.EQ.  50 )  ijk= 3
      IF( MULTI.GE. 100 )  ijk= 4
              cs= REAL( Nruns) * SUMS(k,il,ijk)
        TERR1(k)= cs
 1010 CONTINUE
C
      CALL SEQDIG( ID, isum, TERR1, CSUM, mk)
C
      CALL TRACK ('RESULT  ')
      RETURN
      END
C
C
C**********************************************
      FUNCTION SECOND( OLDSEC)
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
CLOX  REAL*8 SECOND
C
C     SECOND= Cumulative CPU time for job in seconds.  MKS unit is seconds.
C             Clock resolution should be less than 2% of Kernel 11 run-time.
C             ONLY CPU time should be measured, NO system or I/O time included.
C             In VM systems, page-fault time must be avoided (Direction 8).
C             SECOND accuracy may be tested by calling: CALIBR test.
C
C     IF your system provides a timing routine that satisfies
C     the definition above; THEN simply delete this function.
C
C     ELSE this function must be programmed using some
C     timing routine available in your system.
C     Timing routines with CPU-clock resolution are always  sufficient.
C     Timing routines with microsec. resolution are usually sufficient.
C
C     Timing routines with much less resolution have required the use
C     of multiple-pass loops around each kernel to make the run time
C     at least 50 times the tick-period of the timing routine.
C     Function SECOVT measures the overhead time for a call to SECOND.
C
C     If no CPU timer is available, then you can time each kernel by
C     the wall clock using the PAUSE statement at the end of func. TEST.
C
C     An independent calibration of the running time may be wise.
C     Compare the Total Job Cpu Time printout at end of the LFK output file
C     with the job Cpu time charged by your operating system.
C
C     Default, uni-processor tests measure job  Cpu-time in SECOND (TSS mode).
C     Parallel processing tests should measure Real-time in stand-alone mode.
C
C     The following statement is deliberately incomplete:
C
c      SECOND=                                                            sdef
C               USE THE HIGHEST RESOLUTION CPU-TIMER FUNCTION AVAILABLE
C*******************************************************************************
C
C     The following statements were used on  UNIX 4.2bsd systems, e.g.  SUN
C     Time Resolution of ETIME is poor= 0.01 Sec.
C
         REAL CPUTYM(4), ETIME                                            unix
         XT= ETIME( CPUTYM)                                               unix
         SECOND=    CPUTYM(1)                                             unix
C
C or
c        REAL*4 XTIME(4)                                                  unix
c        INTEGER    CLOCK                                                 unix
c        EXTERNAL   CLOCK                                                 unix
c        XT = REAL( CLOCK( XTIME)) * 1.00d-6                              unix
c        SECOND=  XT                                                      unix
C
C*******************************************************************************
C
C     The following statements were used on the DEC  VAX/780  VMS 3.0 .
C     Enable page-fault tallys in TEST by un-commenting LIB$STAT_TIMER calls.
C     Clock resolution is 0.01 Sec.
C
C       DATA  INITIA   /123/
C       IF(   INITIA.EQ.123 )  THEN
C             INITIA= 1
C             NSTAT = LIB$INIT_TIMER()
C       ELSE
C             NSTAT = LIB$STAT_TIMER(2,ISEC)
C             SECOND= REAL(ISEC)*0.01 - OLDSEC
C       ENDIF
C
C* OR less accurately:
C*        REAL    SECNDS
C*        SECOND= SECNDS( OLDSEC)
C
C*****************************************************************************
C
C     The following statements were used on the IBM RS/6000
C     Contrary to what the manual states, INTEGER FUNCTION MCLOCK()
C     returns the number of ticks with 100 ticks being one second.
C
CIBMRS          integer itemp, MCLOCK
CIBMRS          external MCLOCK
C
CIBMRS          itemp = MCLOCK()
CIBMRS          SECOND= REAL(itemp)/100.00d0
C
C*******************************************************************************
C     The following statements were used on the DEC PDP-11/23 RT-11 system.
C
C*       DIMENSION JT(2)
C*       CALL GTIM(JT)
C*       TIME1 = JT(1)
C*       TIME2 = JT(2)
C*       TIME = TIME1 * 65768. + TIME2
C*       SECOND=TIME/60. - OLDSEC
C*******************************************************************************
C
C     The following statements were used on the Hewlett-Packard HP 9000
C
C*       INTEGER*4 ITIME(4)
C*       CALL TIMES( ITIME(4))
C*       TIMEX= ITIME(1) + ITIME(2) + ITIME(3) + ITIME(4)
C*       SECOND= TIMEX/60. - OLDSEC
C
C*******************************************************************************
C
C     FOR THE GOULD 32/87 WITH MPX 3.2  (et seq. gratis D.Lindsay)
C
C     INTEGER*4 NSEC, NCLICK
C     REAL*8 CPUTIM
C
C      CALL M:CLOCK (NSEC, NCLICK)
C      CPUTIM = FLOAT(NSEC)
C      SECOND = CPUTIM + FLOAT(NCLICK)/60.
C
C*******************************************************************************
C
C  FOR THE HP 1000 RUNNING FORTRAN 77.
C  note that since the hp operating system has no facility for
C  returning cpu time, this routine only measures elapsed time.
C  therefore, the tests must be run stand-alone.
C
C     REAL*8 TOTIME
C     INTEGER*2 TIMEA(5)
C
C     CALL EXEC (11, TIMEA)
C     TOTIME = DBLE (TIMEA(1))/100.
C     TOTIME = TOTIME + DBLE (TIMEA(2))
C     TOTIME = TOTIME + DBLE (TIMEA(3)) * 60.
C     SECOND = TOTIME + DBLE (TIMEA(4)) * 3600.
C
C*******************************************************************************
C
C     FOR THE PR1ME SYSTEM UNDER PRIMOS
C
C     REAL*8 CPUTIM
C     INTEGER*2 TIMERS (28)
C
C     CALL TMDAT (TIMERS)
C     SECOND = DBLE (TIMERS(7))
C    .+ DBLE(TIMERS(8)) / DBLE(TIMERS(11))
C
C*******************************************************************************
C
C     The following statements were used on the Stellar
C
C      REAL DUMMY(8)
C      INTEGER*4 TIMES$
C      SAVE IOFSET
C      ITIME= TIMES$( DUMMY)
C      IF( IOFSET.EQ.0 )  IOFSET= ITIME
C      SECOND= (ITIME - IOFSET)/100.0  - OLDSEC
C*******************************************************************************
C
C     The following statements were used on the IBM 3090 VM system.
C     Clock resolution is 1 microsec.
C
C      SECOND= IOCPU(0.0d0)* 1.0d-6
C
C*******************************************************************************
C
C     The following statement was used on the IBM 3090  MVS
C
C**   CALL TODD( xtime)
c     TODD returns microsecs in REAL*8 form
c     TODD provides 1/16th of a microsecond precision
C**   xtime = xtime * 1.0D-6
C     SECOND= xtime - oldsec
C
C********************************
C     REAL*4 TIME(4)
C     xtime = 0.0D-6
C     CALL VCLOCK(time(1))
C     xtime = time(1)
C     SECOND= xtime - oldsec
C
C********************************
C     The following statement was used on the IBM 4381, 9370
C
C     real*8 elapsed(2),cpu(2)
C     call timer(elapsed,cpu)
C     second = cpu(1) - oldsec
C
C
C*******************************************************************************
C
C     The following statements were used on the IBM PC Professional Fortran.
C     Clock resolution is 0.01 Sec.
C
C      INTEGER*2 IHR,IMIN,ISEC,IS100
C      CALL GETTIM(IHR,IMIN,ISEC,IS100)
C      ISECT=(JFIX(IHR)*60+JFIX(IMIN))*60+JFIX(ISEC)
C      SECOND=FLOAT(ISECT)+FLOAT(IS100)/100.0
C
C*******************************************************************************
C
C     THE FOLLOWING STATEMENTS ARE USED ON IBM-PC WITH LAHEY COMPILER
C**   SECOND= REAL( MOD( ITICKS, 1000000)) * 1.0D-2
C
C**   INTEGER*4   ITICKS
C**   CALL TIMER( ITICKS)
C**   SECOND= REAL( ITICKS ) * 1.0D-2
C
C      INTEGER*4  I1, ITICK0, ITICKS
C      SAVE I1, ITICK0
C      DATA I1/-357/, ITICK0/0/
CC
C      IF(  I1.EQ.(-357)) THEN
C         CALL  TIMER( ITICK0)
C      ENDIF
C           I1 = 7
C         CALL  TIMER( ITICKS)
C       SECOND = REAL( ITICKS - ITICK0 ) * 1.0D-2
C
C
C*******************************************************************************
C
C  FOR THE IBM PC.
C  note that the pc's operating system has no facility for
C  returning cpu time; this routine only measures elapsed time.
C  also, the pc does not have real*8.  Remove all references to real*8
C
C      IMPLICIT INTEGER*4 (I-N)
C      LOGICAL FIRST
C      DATA FIRST /.TRUE./
C
C      CALL GETTIM (IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, IFRACT)
C
C  ifract is integer fractions of a second
C  in units of 1/32,768 seconds
C
C      IF (.NOT. FIRST) GO TO 10
C        FIRST = .FALSE.
C
C        LASTHR = IHOUR
C        BASETM = 0.
C10    CONTINUE
C
C  because of limited precision, do not include the time of day
C  in hours in the total time.  but correct for an hour change.
C
C      IF (LASTHR .EQ. IHOUR) GO TO 20
C        BASETM = BASETM + 3600.
C        LASTHR = IHOUR
C
C20    TOTIME = FLOAT(IMIN) * 60
C    . + FLOAT(ISEC)
C    . + FLOAT(IFRACT)/32768.
C      SECOND = TOTIME + BASETM
C
C
      RETURN
      END
c
c
c
C
C***********************************************************************
      FUNCTION  SECOVT( iou )
C***********************************************************************
C                                                                      *
C     SECOVT  - Measures the Overhead time for calling SECOND
C      toler  - tolerance for convergence= Relative error :  0.02
C        iou  - I/O unit number
C***********************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
CLOX  REAL*8 SECOND
C
      DIMENSION   TIM(20), TER(20), TMX(20), INX(20)
      COMMON /FAKE1/ t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11(20),t12(20)
      COMMON /FAKE2/ tcum(20)
C
      CALL TRACE ('SECOVT  ')
C
C***********************************************************************
C     Measure  tsecov:  Overhead time for calling SECOND
C***********************************************************************
C
         tseco= 0.000d0
           klm= 1600
            io= ABS(iou)
            jj= 0
C
      DO 820 j= 1,15
C
      DO 803 i= 1,10
        t12(i)= 0.000d0
  803 continue
       tcum(1)= 0.000d0
            t0= SECOND( tcum(1))
C                       assure that 10 calls to SECOND are NOT optimized
      DO 810 k= 1,klm
      DO 805 i= 1,10
       tcum(i)= t12(i)
  805 continue
            t1= SECOND( tcum(1))
            t2= SECOND( tcum(2))
            t3= SECOND( tcum(3))
            t4= SECOND( tcum(4))
            t5= SECOND( tcum(5))
            t6= SECOND( tcum(6))
            t7= SECOND( tcum(7))
            t8= SECOND( tcum(8))
            t9= SECOND( tcum(9))
           t10= SECOND( tcum(10))
  810 continue
        elapst= t10 - t0
         tseco= elapst/( REAL(10*klm) + 1.0e-9)
         toler= 0.020d0
          rerr= 1.000d0
C
C                                  Convergence test:  Rel.error .LT. 1%
            IF( elapst.GT. 1.00d04 ) GO TO 911
            IF( elapst.LT. 1.00d-10 .AND. j.GT.10 ) GO TO 911
            IF( elapst.GT. 1.00d-9 ) THEN
                     jj= jj + 1
                TIM(jj)= tseco
                     IF( jj.GT.1 ) THEN
                         rerr= RELERR( TIM(jj), TIM(jj-1))
                     ENDIF
                TER(jj)= rerr
            ENDIF
C
            IF( iou.GT.0 ) THEN
         WRITE( iou,64) 10*klm,  tseco, rerr
            ENDIF
            IF( rerr  .LT. toler   ) GO TO 825
            IF( elapst.GT. 10.00d0 ) GO TO 822
           klm= klm + klm
  820 continue
C                                  Poor accuracy on exit from loop
  822     IF( j .LE. 1 )  GO TO 911
          IF( jj.LT. 1 )  GO TO 911
         CALL SORDID( INX,TMX,  TER,jj,1)
C
           i= 0
  823      i= i + 1
       tseco= TIM( INX(i))
        rerr= TMX(i)
          IF( tseco.LE. 0.00d0 .AND. i.LT.jj ) GO TO 823
C
          IF(  rerr.GT. 0.050d0 ) THEN
               WRITE( io,63)  100.00d0 * rerr
          ENDIF
C                                  Good convergence, satifies 1% error tolerence
  825 SECOVT = tseco
C
      CALL TRACK ('SECOVT  ')
      RETURN
C
  911         WRITE( io,61)
              WRITE( io,62) elapst, j
              CALL WHERE(0)
C
   61 FORMAT(1X,'FATAL(SECOVT): cant measure overhead time subr SECOND')
   62 FORMAT(/,13X,'using SECOND:  elapst=',1E20.8,6X,'J=',I4)
   63 FORMAT(1X,'WARNING(SECOVT): SECOND overhead time relerr',f9.4,'%')
   64 FORMAT('SECOVT:',I10,E12.4,F11.4)
      END
C
C***********************************************************************
      SUBROUTINE  SENSIT( iou, RATES,WG,IQ,SUMW,  MAP,TV,TV1,TV2,TV3,n)
C***********************************************************************
C                                                                      *
C     SENSIT  - Sensitivity Of Harmonic Mean Rate(Mflops) 49 Workloads *
C                                                                      *
C     iou     - input scalar,  i/o unit number                         *
C     RATES   - input array ,  execution rates (Mflops)                *
C     WG      - input array ,  weights paired with RATES               *
C     IQ      - input array ,  1 or 2 quartiles specifier              *
C     SUMW    - input array ,  workload fractions.                     *
C                                                                      *
C     MAP,TV,TV1,TV2,TV3    -  output temporary arrays                 *
C     n       - input scalar,  number of rates, etc.                   *
C                                                                      *
C***********************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
c In
      DIMENSION  RATES(n), WG(n), IQ(7), SUMW(7)
c Temp
      DIMENSION  MAP(n), TV(n), TV1(n), TV2(n), TV3(n)
      DIMENSION  NR1(10), NR2(10), STAT2(20)

c     DIMENSION  TAG(4)                                                  f66
      CHARACTER*8  TAG(4)                                                f77
      SAVE  TAG
C
      DATA  ( TAG(i), i= 1,4)
     . /'1st QT: ',  '2nd QT: ',  '3rd QT: ',  '4th QT: '/               f77
c    ./8H1st QT:  , 8H2nd QT:  , 8H3rd QT:  , 8H4th QT:  /               f66
C
      CALL TRACE ('SENSIT  ')
C
C                 Compress valid data sets RATES,  mapping on MAP.

            meff= 0
            neff= 0
              bl= 1.0d-5
              bu= 1.0d+5
            CALL  VALID( TV1,MAP,neff,  bl, RATES, bu, n)

        DO  1  k= 1,neff
          TV3(k)=    WG( MAP(k))
    1  continue


C                 Compress valid data sets WG,  mapping on MAP.

            CALL  VALID( TV3,MAP,meff,  bl, TV3, bu, neff)

        DO  3  k= 1,meff
           TV(k)=TV1( MAP(k))
    3  continue
C
C                 Sort selected rates into descending order

            CALL  SORDID( MAP,TV2,   TV,meff,2)

C
C
       CALL PAGE( iou)
          WRITE ( iou,7001)
C
 7001 FORMAT(/)
 7301 FORMAT(9X,'           SENSITIVITY ANALYSIS ')
 7302 FORMAT(9X,'The sensitivity of the harmonic mean rate (Mflops)  ')
 7303 FORMAT(9X,'to various weightings is shown in the table below.  ')
 7304 FORMAT(9X,'Seven work distributions are generated by assigning ')
 7305 FORMAT(9X,'two distinct weights to ranked kernels by quartiles.')
 7306 FORMAT(9X,'Forty nine possible cpu workloads are then evaluated')
 7307 FORMAT(9X,'using seven sets of values for the total weights:   ')
 7341 FORMAT(3X,A ,6X,'O      O      O      O      O      X      X')      f77
 7342 FORMAT(3X,A ,6X,'O      O      O      X      X      X      O')      f77
 7343 FORMAT(3X,A ,6X,'O      X      X      X      O      O      O')      f77
 7344 FORMAT(3X,A ,6X,'X      X      O      O      O      O      O')      f77
c7341 FORMAT(3X,A7,6X,'O      O      O      O      O      X      X')      f66
c7342 FORMAT(3X,A7,6X,'O      O      O      X      X      X      O')      f66
c7343 FORMAT(3X,A7,6X,'O      X      X      X      O      O      O')      f66
c7344 FORMAT(3X,A7,6X,'X      X      O      O      O      O      O')      f66
 7346 FORMAT(13X,  '------ ------ ------ ------ ------ ------ ------')
 7348 FORMAT(3X,'Total',/,3X,'Weights',20X,'Net Mflops:',/,4X,'X    O')
 7349 FORMAT(2X,'---- ---- ')
 7220 FORMAT(/,1X,2F5.2,1X,7F7.2)
C
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7301)
          WRITE ( iou,7001)
          WRITE ( iou,7302)
          WRITE ( iou,7303)
          WRITE ( iou,7304)
          WRITE ( iou,7305)
          WRITE ( iou,7306)
          WRITE ( iou,7307)
          WRITE ( iou,7001)
          WRITE ( iou,7346)
          WRITE ( iou,7341)   TAG(1)
          WRITE ( iou,7342)   TAG(2)
          WRITE ( iou,7343)   TAG(3)
          WRITE ( iou,7344)   TAG(4)
          WRITE ( iou,7346)
          WRITE ( iou,7348)
          WRITE ( iou,7349)
C
            IF( meff .LE. 0 )  GO TO 73
          fuzz= 1.0d-9
             r= meff
            mq= (meff+3)/4
             q= mq
             j= 1
      DO 21  i= 8,2,-2
      NR1(i  )= j
      NR1(i+1)= j
      NR2(i  )= j + mq + mq - 1
      NR2(i+1)= j + mq - 1
             j= j + mq
   21  continue
C
       DO 29 j= 1,7
          sumo= 1.0d0 - SUMW(j)
       DO 27 i= 1,7
             p= IQ(i)*q
            xt= SUMW(j)/(p + fuzz)
            ot= sumo   /(r - p + fuzz)
       DO 23 k= 1,meff
        TV3(k)= ot
   23  continue
            k1= NR1(i+2)
            k2= NR2(i+2)
       DO 25 k= k1,k2
        TV3(k)= xt
   25  continue
          CALL  STATW( STAT2,TV,MAP, TV2,TV3,meff)
        TV1(i)= STAT2(5)
   27  continue
        WRITE ( iou,7220) SUMW(j), sumo, ( TV1(k), k=1,7)
   29  continue
C
           WRITE ( iou,7349)
           WRITE ( iou,7346)
C
C
   73 CONTINUE
      CALL TRACK ('SENSIT  ')
      RETURN
      END
C
C***************************************
      SUBROUTINE SEQDIG( ND, isum, A, B, nr)
C***********************************************************************
C                                                                      *
c     SEQDIG - compute relative error and significant digits of precisi*
C                                                                      *
C     ND     - Out.Ary  Number of Significant Equal Digits in A.eq.B   *
C     isum   - Result   Total number of Equal digits.                  *
C     A      -  In.Ary  Reference values for comparison.               *
C     B      -  In.Ary  New values of unknown precision.               *
C     nr     -  Input   number of results in ND                        *
C***********************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      parameter( maxsd= 16 )
c/    parameter( maxsd= 30 )
      DIMENSION  ND(nr), A(nr), B(nr)
C
      ISIGDG(reler)= INT( ABS( LOG10( ABS( reler))) + 0.500d0 )
      CALL TRACE ('SEQDIG  ')
c
c     Try to determine floating-point precision used: Max Sig Digits
c
        maxs= maxsd
         one= 1.00d0
          sd= 1073741824.00d0
         sum= sd + one
          IF( sum .EQ. sd )  maxs= 8
c
        isum= 0
      DO 1 k= 1,nr
          se= SIGN( one, A(k)) * SIGN( one, B(k))
          IF( se .LT. 0.0)  THEN
              ND(k)= 0
          ELSE
c
c             compute relative error and digits of precision in B.
c
                  re=  RELERR( A(k), B(k))
              IF((re.GT. 0.0d0 ) .AND. (re.LT. 1.0d0))  THEN
                       ND(k)= ISIGDG(re)
              ELSEIF( re .EQ. 0.0d0 )  THEN
                       ND(k)= maxs
              ELSEIF( re .GE. 1.0d0 )  THEN
                       ND(k)= 0
              ENDIF
              IF( ND(k).GT. maxs  )  ND(k)= maxs
          ENDIF
        isum= isum + ND(k)
    1 CONTINUE
C
      CALL TRACK ('SEQDIG  ')
      RETURN
      END
C
C
C***********************************************
      SUBROUTINE  SIGNEL( V, SCALE,BIAS, n)
C***********************************************
C
C    SIGNEL GENERATES VERY FRIENDLY FLOATING-POINT NUMBERS NEAR 1.0
C                     WHEN SCALE= 1.0 AND BIAS= 0.
C
C     V      - result array,  floating-point test data
C     SCALE  - input scalar,  scales magnitude of results
C     BIAS   - input scalar,  offsets magnitude of results
C     n      - input integer, number of results in V.
C
C***********************************************
cANSI           DOUBLE PRECISION  V, SCALE, BIAS
cIBM  REAL*8            V, SCALE, BIAS
c
c     Use the following Double Precision declaration to improve Real*4 tests.
c     Use the following Real*16          declaration to improve Real*8 tests.
c
         DOUBLE     PRECISION  SCALED,BIASED,FUZZ,BUZZ,FIZZ,ONE
cIBM  REAL*16           SCALED,BIASED,FUZZ,BUZZ,FIZZ,ONE
      DIMENSION  V(n)
C
      CALL TRACE ('SIGNEL  ')
C
        SCALED= SCALE
        BIASED= BIAS
C
        SCALED= 10.00d0
        SCALED=  1.00d0/SCALED
        BIASED=  0.00d0
C
C         FUZZ= 1.234500d-9
          FUZZ= 1.234500d-3
          BUZZ= 1.000d0  + FUZZ
          FIZZ= 1.100d0  * FUZZ
           ONE= 1.000d0
C
        DO 1 k= 1,n
          BUZZ= (ONE - FUZZ)*BUZZ +FUZZ
          FUZZ= -FUZZ
c         V(k)=((BUZZ- FIZZ) -BIASED)*SCALED
          V(k)= (BUZZ- FIZZ)*SCALED
    1 CONTINUE
C
      CALL TRACK ('SIGNEL  ')
      RETURN
      END
C
C
C***********************************************************************
      SUBROUTINE SIMD( HM,  iou,RATES,WG,FR,m,  MAP,TV1,TV2,TV3,n)
C***********************************************************************
C                                                                      *
C     SIMD  - Sensitivity Of Harmonic Mean Rate(Mflops) SISD/SIMD Model*
C                                                                      *
C     HM      - result array,  Harmonic Mean Rates(k)= f( FR(k))       *
C     iou     - input scalar,  i/o unit number                         *
C     RATES   - input array ,  execution rates (Mflops)                *
C     WG      - input array ,  weights paired with RATES               *
C     FR      - input array ,  fractions of flops executed SIMD        *
C     m       - input scalar,  number of fractions                     *
C                                                                      *
C     MAP,TV,TV1,TV2,TV3    -  output temporary arrays                 *
C     n       - input scalar,  number of rates, etc.                   *
C                                                                      *
C***********************************************************************
C
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
c SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN CODE(SISD/SIMD MODEL)
c Out
      DIMENSION  HM(m)
c In
      DIMENSION  FR(m), RATES(n), WG(n)
c Temp
      DIMENSION  MAP(n), TV1(n), TV2(n), TV3(n), STAT2(20)
C
      CALL TRACE ('SIMD    ')

C                 Compress valid data sets RATES,  mapping on MAP.

            meff= 0
            neff= 0
              bl= 1.0d-5
              bu= 1.0d+5
            CALL  VALID( TV1,MAP,neff,  bl, RATES, bu, n)

        DO  1  k= 1,neff
          TV3(k)=    WG( MAP(k))
    1  continue


C                 Compress valid data sets WG,  mapping on MAP.

            CALL  VALID( TV3,MAP,meff,  bl, TV3, bu, neff)

        DO  3  k= 1,meff
          TV2(k)= TV1( MAP(k))
    3  continue

C                 Sort RATES,WT into descending order.

            CALL  STATW( STAT2,TV1,MAP, TV2, TV3, meff)
             med= meff + 1 - INT(STAT2(8))
              lh= meff + 1 - med

        DO  5  k= 1,meff
          TV2(k)= TV3( MAP(k))
    5  continue


C                 Estimate vector rate= HMean of top LFK quartile.

              nq= meff/4
            CALL  STATW( STAT2,TV3,MAP, TV1,TV2,nq)
             vmf= STAT2(5)

C                 Estimate scalar rate= HMean of lowest two LFK quartiles.

            CALL  STATW( STAT2,TV3,MAP, TV1(med),TV2(med),lh)
             smf= STAT2(5)
            fuzz= 1.0d-9

               g= 1.0d0 -   smf/( vmf + fuzz)
           HM(1)= smf

          DO 7 k= 2,m
           HM(k)=   smf/( 1.0d0 - FR(k)*g + fuzz)
   7      continue
C
      IF( iou .GT. 0)  THEN
C
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7001)
          WRITE ( iou,7101)
          WRITE ( iou,7102) ( HM(k), k= 1,9)
          WRITE ( iou,7102) ( FR(k), k= 1,9)
          WRITE ( iou,7103)
          WRITE ( iou,7001)
 7001 FORMAT(/)
 7101 FORMAT(' SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN
     1CODE(SISD/SIMD MODEL)' )
 7102 FORMAT(/,1X,5F7.2,4F8.2)
 7103 FORMAT(3x,' Fraction Of Operations Run At Optimal Fortran Rates')
C
      ENDIF
C
      CALL TRACK ('SIMD    ')
      RETURN
C
      END
C
C
C***********************************************
      SUBROUTINE SIZES(i)
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
C            SIZES      test and set the loop controls before each kernel test
C
C     i    :=  kernel number
C
C     mk    :=  number of kernels to test
C     Nruns :=  number of timed runs of complete test.
C     tclock:=  cpu clock resolution or minimum time in seconds.
C     Loop  :=  multiple pass control to execute kernel long enough to time.
C     n     :=  DO loop control for each kernel.
C     ******************************************************************
C
C
C/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
C/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
C/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
C
C/      PARAMETER( l1=   27, l2=   15, l1d= 2*1001 )
C/      PARAMETER( l13= 8, l13h= 8/2, l213= 8+4, l813= 8*8 )
C/      PARAMETER( l14=  16, l16= 15, l416= 4*15 , l21= 15)
C
C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C
C/      PARAMETER( NNI=  2*l1 +2*l213 +l416 )
C/      PARAMETER( NN1= 16*l1 +13*l2 +2*l416 + l14 )
C/      PARAMETER( NN2= 4*l813 + 3*l21*l2 +121*l2 +3*l13*l13 )
C/      PARAMETER( Nl1= 19*l1, Nl2= 131*l2 +3*l21*l2 )
C/      PARAMETER( Nl13= 3*l13*l13 +34*l13 +32)
C
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     2                AR,BR,C0,CR,DI,DK,
     3  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     4  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
C     ******************************************************************
C
      CALL TRACE ('SIZES   ')
C
      nif= 0
c                        Set  mk .LE. 47  number of kernels to test.
             mk= 24
             im= 1
             ml= 3
c                        Set  Nruns .LT. 8  number of timed runs of KERNEL test
c                        Set  Nruns= 1   to REDUCE RUN TIME for debug runs.
          Nruns= 1
c                        Set  Nruns= 7   for Standard BENCHMARK Test. Maximum.
          Nruns= 7
             IF( Nruns.GT. 7) Nruns= 7
c
c                        Set  Mruns= 7   for Standard BENCHMARK Test.
          Mruns= Nruns
c
C****************************************************************************
c         OPTIONAL LONG ENDURANCE TEST FOR NEW HARDWARE ACCEPTANCE TESTING.
c         OPTIONAL       Set  Mruns=     for Hardware ENDURANCE TRIAL
c
c         Mruns= Nruns * ( Desired Trial Time(sec) / totjob Time(sec))
c                          where totjob-time is LFK Standard benchmark
c                          test Job-time printed at end of output file.
c
c   e.g.  12 Hour run on CRAY-XMP :   laps = 43200./ 17.5 = 2468
c         12 Hour run on VaxS3500 :   laps = 43200./478.4 =   90
c
c          laps= 1
C****************************************************************************
C
          Mruns= Nruns * laps
      IF( Mruns.LT.Nruns .OR. Mruns.GT.500000 ) Mruns= Nruns
C
      IF( i.EQ.-1)  GO TO 73
C
C****************************************************************************
C     Domain tests follow to detect overstoring of controls for array opns.
C****************************************************************************
C
      nif= 1
      iup= 999000
      IF( iup.LT.65000 ) iup= 65000
      IF( i.LT.1 .OR.  (i-1).GT.  24)      GO TO 911
      IF( n.LT.0 .OR.  n.GT.   1001)          GO TO 911
      IF(Loop.LT.0 .OR. Loop.GT.iup)        GO TO 911
C
      nif= 2
      IF(  il.LT.1 .OR. il.GT.3 )  GO TO 911
                 n= ISPAN(i,il)
      Loop        = IPASS(i,il) * MUL(il)
      Loop = MULTI * Loop
      LP   = Loop
c
c
c
c MULTI= 10
c        ------    ------    ------   -------   -------   ------------
c        kernel    L:Loop    n:loop   flops*1   flops*n   flops*n*Loop
c        ------    ------    ------   -------   -------   ------------
c   il= 1     1        70      1001         5      5005    350350
c             2       670        97         4       388    259960
c             3        90      1001         2      2002    180180
c             4       140       600         2      1200    168000
c             5       100      1000         2      2000    200000
c             6        30      1984         2      3968    119040
c             7        40       995        16     15920    636800
c             8       100       198        36      7128    712800
c             9       360       101        17      1717    618120
c            10       340       101         9       909    309060
c            11       110      1000         1      1000    110000
c            12       120      1000         1      1000    120000
c            13       360        64         7       448    161280
c            14        20      1001        11     11011    220220
c            15        10       500        33     16500    165000
c            16       250        53        10       530    132500
c            17       350       101         9       909    318150
c            18        20       495        44     21780    435600
c            19       390       101         6       606    236340
c            20        10      1000        26     26000    260000
c            21        10     63125         2    126250   1262500
c            22       110       101        17      1717    188870
c            23        80       495        11      5445    435600
c            24        50      1000         1      1000     50000
c   il= 2     1       800       101         5       505    404000
c             2       800        97         4       388    310400
c             3      1060       101         2       202    214120
c             4      1400        60         2       120    168000
c             5      1100       100         2       200    220000
c             6       140       480         2       960    134400
c             7       440       101        16      1616    711040
c             8       120       198        36      7128    855360
c             9       420       101        17      1717    721140
c            10       380       101         9       909    345420
c            11      1280       100         1       100    128000
c            12      1360       100         1       100    136000
c            13       820        32         7       224    183680
c            14       200       101        11      1111    222200
c            15        20       500        33     16500    330000
c            16       540        28        10       280    151200
c            17       400       101         9       909    363600
c            18        20       495        44     21780    435600
c            19       460       101         6       606    278760
c            20       160       100        26      2600    416000
c            21        20     31250         2     62500   1250000
c            22       140       101        17      1717    240380
c            23       100       495        11      5445    544500
c            24       620       100         1       100     62000
c   il= 3     1      2240        27         5       135    302400
c             2      3680        11         4        44    161920
c             3      2960        27         2        54    159840
c             4      3040        15         2        30     91200
c             5      3200        26         2        52    166400
c             6      1680        24         2        48     80640
c             7      1600        21        16       336    537600
c             8       720        26        36       936    673920
c             9      2080        15        17       255    530400
c            10      2000        15         9       135    270000
c            11      3680        26         1        26     95680
c            12      3840        26         1        26     99840
c            13      2480         8         7        56    138880
c            14       640        27        11       297    190080
c            15        80        70        33      2310    184800
c            16      1120        11        10       110    123200
c            17      2080        15         9       135    280800
c            18       160        65        44      2860    457600
c            19      2240        15         6        90    201600
c            20       560        26        26       676    378560
c            21        80     12500         2     25000   2000000
c            22       640        15        17       255    163200
c            23       560        65        11       715    400400
c            24      1840        26         1        26     47840
c
Computers with high resolution clocks tic= O(microsec.) should use Loop= 1
C     to show un-initialized as well as encached execution rates.
C
C     Loop= 1
C
      IF( Loop.LT. 1)   Loop= 1
      LP  = Loop
      L   = 1
      mpy = 1
      nif = 3
      IF( n.LT.0 .OR.  n.GT.   1001)  GO TO 911
      IF(Loop.LT.0 .OR. Loop.GT.iup)  GO TO 911
      n1  = 1001
      n2  = 101
      n13 = 64
      n13h= 32
      n213= 96
      n813= 512
      n14 = 2048
      n16 = 75
      n416= 300
      n21 = 25
C
      nt1= 16*1001 +13*101 +2*300 + 2048
      nt2= 4*512 + 3*25*101 +121*101 +3*64*64
C
   73 CONTINUE
      CALL TRACK ('SIZES   ')
      RETURN
C
C
  911 io= ABS( ion)
      IF( io.LE.0 .OR. io.GT.10 ) io=6
      WRITE( io,913) i, nif, n, Loop, il
  913 FORMAT('1',///,' FATAL OVERSTORE/ DATA LOSS.  TEST=  ',6I6)
      CALL WHERE(0)
C
      END
C***********************************************
      SUBROUTINE SORDID( I,W, V,n,KIND)
C***********************************************
C                    QUICK AND DIRTY PORTABLE SORT.
C
C                I - RESULT INDEX-LIST. MAPS V TO SORTED W.
C                W - RESULT ARRAY, SORTED V.
C
C                V - INPUT  ARRAY SORTED IN PLACE.
C                n - INPUT  NUMBER OF ELEMENTS IN V
C             KIND - SORT ORDER:   = 1  ASCENDING MAGNITUDE
C                                  = 2 DESCENDING MAGNITUDE
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      DIMENSION  I(n), W(n), V(n)
C
      CALL TRACE ('SORDID  ')
C
            IF( n.LE.0 )  GO TO 73
      DO  1  k= 1,n
          W(k)= V(k)
    1     I(k)= k
C
      IF( KIND.EQ.1)  THEN
C
          DO  3  j= 1,n-1
                 m= j
          DO  2  k= j+1,n
                IF( W(k).LT.W(m)) m= k
    2     CONTINUE
                 X= W(j)
                 k= I(j)
              W(j)= W(m)
              I(j)= I(m)
              W(m)= X
              I(m)= k
    3     CONTINUE
C
C
      ELSE
C
          DO  6  j= 1,n-1
                 m= j
          DO  5  k= j+1,n
                IF( W(k).GT.W(m)) m= k
    5     CONTINUE
                 X= W(j)
                 k= I(j)
              W(j)= W(m)
              I(j)= I(m)
              W(m)= X
              I(m)= k
    6     CONTINUE
      ENDIF
C
      IF( n.GT.0 )  THEN
          CALL TRAP( I, ' SORDID  ', 1, n,n)
      ENDIF
C
   73 CONTINUE
      CALL TRACK ('SORDID  ')
      RETURN
      END
C
C***********************************************
      SUBROUTINE  SPACE
C***********************************************
C
C            SPACE      sets memory pointers for array variables.  optional.
C
C     Subroutine Space dynamically allocates physical memory space
C     for the array variables in KERNEL by setting pointer values.
C     The POINTER declaration has been defined in the IBM PL1 language
C     and defined as a Fortran extension in Livermore and CRAY compilers.
C
C     In general, large FORTRAN simulation programs use a memory
C     manager to dynamically allocate arrays to conserve high speed
C     physical memory and thus avoid slow disk references (page faults).
C
C     It is sufficient for our purposes to trivially set the values
C     of pointers to the location of static arrays used in common.
C     The efficiency of pointered (indirect) computation should be measured
C     if available.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
C
C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
C
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
C
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
C
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
C
C     ******************************************************************
C
C//      COMMON /POINT/ ME,MF,MU,MV,MW,MX,MY,MZ,MG,MDU1,MDU2,MDU3,MGRD,
C//     1  MDEX,MIX,MXI,MEX,MEX1,MDEX1,MVX,MXX,MIR,MRX,MRH,MVSP,MVSTP,
C//     2  MVXNE,MVXND,MVE3,MVLR,MVLIN,MB5,MPLAN,MZONE,MD,MSA,MSB,
C//     3  MP,MPX,MCX,MVY,MVH,MVF,MVG,MVS,MZA,MZP,MZQ,MZR,MZM,MZB,MZU,
C//     4  MZV,MZZ,MB,MC,MH,MU1,MU2,MU3
C//C
C//CLLL. LOC(X) =.LOC.X
C//C
           CALL TRACE ('SPACE   ')
C//      ME     = LOC( E )
C//      MF     = LOC( F )
C//      MU     = LOC( U )
C//      MV     = LOC( V )
C//      MW     = LOC( W )
C//      MX     = LOC( X )
C//      MY     = LOC( Y )
C//      MZ     = LOC( Z )
C//      MG     = LOC( G )
C//      MDU1   = LOC( DU1 )
C//      MDU2   = LOC( DU2 )
C//      MDU3   = LOC( DU3 )
C//      MGRD   = LOC( GRD )
C//      MDEX   = LOC( DEX )
C//      MIX    = LOC( IX )
C//      MXI    = LOC( XI )
C//      MEX    = LOC( EX )
C//      MEX1   = LOC( EX1 )
C//      MDEX1  = LOC( DEX1 )
C//      MVX    = LOC( VX )
C//      MXX    = LOC( XX )
C//      MIR    = LOC( IR )
C//      MRX    = LOC( RX )
C//      MRH    = LOC( RH )
C//      MVSP   = LOC( VSP )
C//      MVSTP  = LOC( VSTP )
C//      MVXNE  = LOC( VXNE )
C//      MVXND  = LOC( VXND )
C//      MVE3   = LOC( VE3 )
C//      MVLR   = LOC( VLR )
C//      MVLIN  = LOC( VLIN )
C//      MB5    = LOC( B5 )
C//      MPLAN  = LOC( PLAN )
C//      MZONE  = LOC( ZONE )
C//      MD     = LOC( D )
C//      MSA    = LOC( SA )
C//      MSB    = LOC( SB )
C//      MP     = LOC( P )
C//      MPX    = LOC( PX )
C//      MCX    = LOC( CX )
C//      MVY    = LOC( VY )
C//      MVH    = LOC( VH )
C//      MVF    = LOC( VF )
C//      MVG    = LOC( VG )
C//      MVS    = LOC( VS )
C//      MZA    = LOC( ZA )
C//      MZP    = LOC( ZP )
C//      MZQ    = LOC( ZQ )
C//      MZR    = LOC( ZR )
C//      MZM    = LOC( ZM )
C//      MZB    = LOC( ZB )
C//      MZU    = LOC( ZU )
C//      MZV    = LOC( ZV )
C//      MZZ    = LOC( ZZ )
C//      MB     = LOC( B )
C//      MC     = LOC( C )
C//      MH     = LOC( H )
C//      MU1    = LOC( U1 )
C//      MU2    = LOC( U2 )
C//      MU3    = LOC( U3 )
C
      CALL TRACK ('SPACE   ')
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE  SPEDUP( iou, NAME, RATE )
C***********************************************************************
C                                                                      *
C     SPEDUP  - Computes Speed-ups: A circumspect method of comparison.*
C               Computers are ranked by their Geometric Mean Rates.    *
C                                                                      *
C     iou     - input scalar,  i/o unit number                         *
C     NAME    - input array ,  system name                             *
C     RATE    - input array ,  execution rates (Mflops)                *
C                                                                      *
C***********************************************************************
C
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      parameter( nsys= 5, ns= nsys+1, nd= 11, nt= 4 )
      CHARACTER  NAME*8, NAMES*8, ijk*8
      DIMENSION  RATE(nt), NAME(nt), RATIO(nd)
      CHARACTER*8  IT(nt)
      COMMON /TAGS/  NAMES(nd,nt)
      COMMON /RATS/  RATED(nd,nt)
C
      CALL TRACE ('SPEDUP  ')
C                            Rank computer NAME by its Geometric Mean.
      DO  2  k= 1,nsys
            IF( RATE(2) .GT. RATED(k,2))  GO TO 4
    2 continue
    4   insert= k
C                            Pushdown Tables to allow insertion.
      DO  8  i= nd, insert+1, -1
      DO  6  j= 1,nt
      NAMES(i,j)=  NAMES(i-1,j)
      RATED(i,j)=  RATED(i-1,j)
    6 continue
    8 continue
C                            Insert new computer NAME
      DO 10  j= 1,nt
      NAMES(insert,j)=  NAME(j)
      RATED(insert,j)=  RATE(j)
   10 continue
C                            Print Table of Speed-ups of Mean Rates.
      CALL PAGE( iou)
      IT(1)= 'AM='
      IT(2)= 'GM='
      IT(3)= 'HM='
      ijk  = '--------'
      fuzz = 1.0d-9
      WRITE( iou,111)
      WRITE( iou,104)
  104 FORMAT(26X,'TABLE OF SPEED-UP RATIOS OF MEAN RATES (72 Samples)')
      WRITE( iou,105)
  105 FORMAT(/,26X,'Arithmetic, Geometric, Harmonic Means (AM,GM,HM)')
      WRITE( iou,106)
  106 FORMAT(26X,'The Geometric Mean is the least biased statistic.',/)
      WRITE( iou,109) ( ijk, m= 1,ns)
  109 FORMAT(1X,'--------  ----  ------  ',11(1X,A ))
      WRITE( iou,110) ( NAMES(m,2), m= 1,ns)
  110 FORMAT(1X,'SYSTEM    MEAN  MFLOPS',2X,11(1X,A ))
      WRITE( iou,109) ( ijk, m= 1,ns)
C
      DO 40  i= 1,ns
      WRITE( iou,111)
  111 FORMAT(/)
C
      DO 26  j= 1,nt-1
C
      DO 22  m= 1,ns
      RATIO(m)= RATED(i,j) / (RATED(m,j) + fuzz)
   22 continue
C
      WRITE( iou,112) NAMES(i,j), IT(j), RATED(i,j), (RATIO(m), m=1,ns)
  112 FORMAT(1X,A ,2X,A3,F9.3,' :',11F9.3)
   26 continue
C
      WRITE( iou,114)  RATED(i,4)
  114 FORMAT(11X,'SD=',F9.3)
   40 continue
C
      CALL TRACK ('SPEDUP  ')
      RETURN
      END
C***********************************************
      SUBROUTINE STATS( STAT, X,n)
C***********************************************
C
C     UNWEIGHTED STATISTICS: MEAN, STADEV, MIN, MAX, HARMONIC MEAN.
C
C     STAT(1)= THE MEAN OF X.
C     STAT(2)= THE STANDARD DEVIATION OF THE MEAN OF X.
C     STAT(3)= THE MINIMUM OF X.
C     STAT(4)= THE MAXIMUM OF X.
C     STAT(5)= THE HARMONIC MEAN
C     X       IS THE ARRAY  OF INPUT VALUES.
C     n       IS THE NUMBER OF INPUT VALUES IN X.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      DIMENSION X(n), STAT(20)
cLLL. OPTIMIZE LEVEL G
C
      CALL TRACE ('STATS   ')
C
      DO 10   k= 1,9
   10 STAT(k)= 0.0
C
      IF(n.LE.0)  GO TO 73
C                             CALCULATE MEAN OF X.
      S= 0.0
      DO 1 k= 1,n
    1 S= S + X(k)
      A= S/n
      STAT(1)= A
C                             CALCULATE STANDARD DEVIATION OF X.
      D= 0.0
      DO 2 k= 1,n
    2 D= D + (X(k)-A)**2
      D= D/n
      STAT(2)= SQRT(D)
C                             CALCULATE MINIMUM OF X.
      U= X(1)
      DO 3 k= 2,n
    3 U= MIN(U,X(k))
      STAT(3)= U
C                             CALCULATE MAXIMUM OF X.
      V= X(1)
      DO 4 k= 2,n
    4 V= MAX(V,X(k))
      STAT(4)= V
C                             CALCULATE HARMONIC MEAN OF X.
      H= 0.0
      DO 5 k= 1,n
          IF( X(k).NE.0.0) H= H + 1.0/X(k)
    5 CONTINUE
          IF( H.NE.0.0) H= REAL(n)/H
      STAT(5)= H
C
   73 CONTINUE
      CALL TRACK ('STATS   ')
      RETURN
      END
C***********************************************
      SUBROUTINE STATW( STAT,OX,IX, X,W,n)
C***********************************************
C
C     WEIGHTED STATISTICS: MEAN, STADEV, MIN, MAX, HARMONIC MEAN, MEDIAN.
C
C     STAT( 1)=  THE MEAN OF X.
C     STAT( 2)=  THE STANDARD DEVIATION OF THE MEAN OF X.
C     STAT( 3)=  THE MINIMUM OF X.
C     STAT( 4)=  THE MAXIMUM OF X.
C     STAT( 5)=  THE HARMONIC MEAN
C     STAT( 6)=  THE TOTAL WEIGHT.
C     STAT( 7)=  THE MEDIAN.
C     STAT( 8)=  THE MEDIAN INDEX, ASCENDING.
C     STAT( 9)=  THE ROBUST MEDIAN ABSOLUTE DEVIATION.
C     STAT(10)=  THE GEOMETRIC MEAN
C     STAT(11)=  THE MOMENTAL SKEWNESS
C     STAT(12)=  THE KURTOSIS
C     STAT(13)=  THE LOWER QUARTILE BOUND Q1/Q2 VALUE
C     STAT(14)=  THE UPPER QUARTILE BOUND Q3/Q4 VALUE
C     STAT(15)=  THE DEVIATION OF THE GEOMETRIC MEAN OF X
C
C     OX      IS THE ARRAY  OF ORDERED (DECENDING) Xs.
C     IX      IS THE ARRAY  OF INDEX LIST MAPS X TO OX.
C
C     X       IS THE ARRAY  OF INPUT VALUES.
C     W       IS THE ARRAY  OF INPUT WEIGHTS.
C     n       IS THE NUMBER OF INPUT VALUES IN X.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      DIMENSION STAT(20), OX(n), IX(n), X(n), W(n)
cLLL. OPTIMIZE LEVEL G
C
      CALL TRACE ('STATW   ')
         stin09= 0.00d0
         stin13= 0.00d0
         stin14= 0.00d0
C
      DO 50   k= 1,15
   50 STAT(k)= 0.0d0
C
      IF( n.LE.0 )  GO TO 73
C
      IF( n.EQ.1 )  THEN
          STAT( 1)= X(1)
          STAT( 3)= X(1)
          STAT( 4)= X(1)
          STAT( 5)= X(1)
          STAT( 6)= W(1)
          STAT( 7)= X(1)
          STAT( 8)= 1.0d0
          STAT(10)= X(1)
          GO TO 73
      ENDIF
C
C
C                             CALCULATE MEAN OF X.
      A= 0.0d0
      S= 0.0d0
      T= 0.0d0
C
      DO 1 k= 1,n
      S= S + W(k)*X(k)
    1 T= T + W(k)
          IF( T.NE.0.0d0) A= S/T
      STAT(1)= A
C                             CALCULATE STANDARD DEVIATION OF X.
      D= 0.0d0
      E= 0.0d0
      F= 0.0d0
      Q= 0.0d0
      U= 0.0d0
C
      DO 2 k= 1,n
      B= W(k) *( X(k) -A)**2
      D= D + B
      E= E + B*( X(k) -A)
    2 F= F + B*( X(k) -A)**2
          IF( T.NE.0.0d0) Q= 1.0d0/T
                          D= D*Q
                          E= E*Q
                          F= F*Q
          IF( D.GE.0.0d0) U= SQRT(D)
      STAT(2)= U
C                             CALCULATE MINIMUM OF X.
      U= X(1)
      DO 3 k= 2,n
    3 U= MIN(U,X(k))
      STAT(3)= U
C                             CALCULATE MAXIMUM OF X.
      V= X(1)
      DO 4 k= 2,n
    4 V= MAX(V,X(k))
      STAT(4)= V
C                             CALCULATE HARMONIC MEAN OF X.
      H= 0.0d0
      DO 5 k= 1,n
          IF( X(k).NE.0.0d0) H= H + W(k)/X(k)
    5 CONTINUE
          IF( H.NE.0.0d0) H= T/H
      STAT(5)= H
      STAT(6)= T
C                             CALCULATE WEIGHTED MEDIAN
      CALL SORDID( IX, OX, X, n, 1)
C
           ew= 0.0d0
      DO 7  k= 2,n
           IF( W(1) .NE. W(k))  GO TO 75
    7 continue
           ew= 1.0d0
   75 continue
C
        qt= 0.500d0
      CALL  TILE( STAT( 7), STAT(8), OX,IX,W,ew,T, qt,n)
C
        qt= 0.250d0
      CALL  TILE( STAT(13),  stin13, OX,IX,W,ew,T, qt,n)
C
        qt= 0.750d0
      CALL  TILE( STAT(14),  stin14, OX,IX,W,ew,T, qt,n)
C
C
C                           CALCULATE ROBUST MEDIAN ABSOLUTE DEVIATION (MAD)
      DO 90 k= 1,n
   90   OX(k)= ABS( X(k) - STAT(7))
C
      CALL SORDID( IX, OX, OX, n, 1)
C
        qt= 0.700d0
      CALL  TILE( STAT( 9),  stin09, OX,IX,W,ew,T, qt,n)
C
C                             CALCULATE GEOMETRIC MEAN
            R= 0.0d0
      DO 10 k= 1,n
           IF( X(k).LE. 0.0d0)  GO TO 10
            R= R + W(k) *LOG10( X(k))
   10 CONTINUE
             U= R*Q
             G= 10.0d0
            IF( U.LT. 0.0d0)  G= 0.1D0
        POWTEN= 50.0d0
            IF( ABS(U) .GT. POWTEN)  U= SIGN( POWTEN, U)
      STAT(10)=  G** ABS(U)
C
C                             CALCULATE MOMENTAL SKEWNESS
             G= 0.0d0
           DXD= D*D
            IF( DXD.NE.0.0d0) G= 1.0d0/(DXD)
      STAT(11)= 0.50d0*E*G*STAT(2)
C
C                             CALCULATE KURTOSIS
      STAT(12)= 0.50d0*( F*G -3.0d0)
C
C                             CALCULATE DEVIATION OF GEOMETRIC MEAN
      D= 0.0d0
      Q= 0.0d0
      U= 0.0d0
      GM= STAT(10)
C
      DO 15 k= 1,n
      B= W(k) *( X(k) -GM)**2
   15 D= D + B
          IF( T.NE.0.0d0) Q= 1.0d0/T
                          D= D*Q
          IF( D.GE.0.0d0) U= SQRT(D)
      STAT(15)= U
C
C                             CALCULATE DESCENDING ORDERED X.
      CALL SORDID( IX, OX, X, n, 2)
C
   73 CONTINUE
      CALL TRACK ('STATW   ')
      RETURN
      END
C
C***********************************************
      FUNCTION SUMO( V,n)
C***********************************************
C
C     CHECK-SUM WITH ORDINAL DEPENDENCY.
C
C     V   - input array,   floating-point numbers
C     n   - input integer, number of elements in V.
C
C***********************************************
cANSI           DOUBLE PRECISION  SUMO, V
cIBM  REAL*8            SUMO, V
c
c     Use the following Double Precision declaration to improve Real*4 tests.
c     Use the following Real*16          declaration to improve Real*8 tests.
c
         DOUBLE     PRECISION  S
cIBM  REAL*16           S
C
      DIMENSION  V(n)
C
      CALL TRACE ('SUMO    ')
           S= 0.00d0
C
      DO 1 k= 1,n
    1      S= S + REAL(k)*V(k)
       SUMO = S
      CALL TRACK ('SUMO    ')
      RETURN
      END
C
C***********************************************
      SUBROUTINE  SUPPLY(i)
C***********************************************
C
C            SUPPLY     initializes common blocks containing type real arrays.
C
C     i    :=  kernel number
C
C****************************************************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  DS, DW                                         REDUNDNT
C
C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C/C
C/C/      PARAMETER( NN0= 39 )
C/C/      PARAMETER( NNI=  2*l1 +2*l213 +l416 )
C/C/      PARAMETER( NN1= 16*l1 +13*l2 +2*l416 + l14 )
C/C/      PARAMETER( NN2= 4*512 + 3*25*101 +121*101 +3*64*64 )
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
      COMMON /CKSUMS/ cksumu,ckoldu, cksump,ckoldp, cksuma,ckolda
C
C/      COMMON /SPACE1/ U(NN1)
C/      COMMON /SPACE2/ P(NN2)
C/      COMMON /SPACER/ A11(NN0)
C/C
        COMMON /SPACE1/ U(19977)
        COMMON /SPACE2/ P(34132)
        COMMON /SPACER/ A11(39)
C/
C
C***********************************************************************
C           Method 1:  Least space and most cpu time (D.P. SIGNEL arith)
C***********************************************************************
C
Csmall      CALL TRACE ('SUPPLY  ')
Csmall      IP1= i+1
Csmall      nt0= 39
CsmallC
Csmall      CALL SIGNEL(  U, SKALE(IP1), BIAS(IP1), nt1)
Csmall      CALL SIGNEL(  P, SKALE(IP1), BIAS(IP1), nt2)
Csmall      CALL SIGNEL(A11, SKALE(IP1), BIAS(IP1), nt0)
Csmall      CALL TRACK ('SUPPLY  ')
Csmall      RETURN
C
C***********************************************************************
C           Method 2:  Double space and least cpu time
C***********************************************************************
C
        COMMON /BASE1/ BUFU(19977)
        COMMON /BASE2/ BUFP(34132)
        COMMON /BASER/ BUFA(39)
      DIMENSION P0(4,512)
      EQUIVALENCE(BUFP,P0)
C
C/C kleiner
C/      COMMON /BASE1/ BUFU( 2136)
C/      COMMON /BASE2/ BUFP( 2938)
C
      CALL TRACE ('SUPPLY  ')
C
      IP1= i
      nt0= 39
C               Execute SIGNEL calls only once; re-use generated data.
          ibuf= ibuf+1
      IF( ibuf.EQ. 1) THEN
          CALL SIGNEL(  BUFU, SKALE(IP1), BIAS(IP1), nt1)
          CALL SIGNEL(  BUFP, SKALE(IP1), BIAS(IP1), nt2)
          CALL SIGNEL(  BUFA, SKALE(IP1), BIAS(IP1), nt0)
                   DS= 1.000d0
                   DW= 0.500d0
             DO 205 j= 1,4
             DO 205 k= 1,512
             P0(j,k) = DS
                   DS= DS + DW
  205        CONTINUE
      ENDIF
C
C                                       Test for Trashing Data in BUF
               idebug=   0
      IF(      idebug.EQ.1
     .    .OR. ibuf  .EQ.1
     .    .OR. i     .EQ.(24-1))  THEN
C
           cksumu= SUMO( BUFU, nt1)
           cksump= SUMO( BUFP, nt2)
           cksuma= SUMO( BUFA, nt0)
C
           IF( ibuf.EQ. 1) THEN
                ckoldu= cksumu
                ckoldp= cksump
                ckolda= cksuma
           ELSEIF(      cksumu.NE.ckoldu
     .             .OR. cksump.NE.ckoldp
     .             .OR. cksuma.NE.ckolda )  THEN
                iou= ABS(ion)
                WRITE( iou,111) jr, il, ik
                WRITE( iou,112) ckoldu, ckoldp, ckolda
                WRITE( iou,113) cksumu, cksump, cksuma
  111 FORMAT(' SUPPLY: OVERSTORED! Trial=',I2,' Pass=',I2,' Kernel=',I3)
  112 FORMAT(' ckold:',3E24.15)
  113 FORMAT(' cksum:',3E24.15)
           ENDIF
      ENDIF
C                             Refill Work-Space from copies in Buffers
      DO 1 k= 1,nt0
    1 A11(k)= BUFA(k)
      DO 2 k= 1,nt1
    2   U(k)= BUFU(k)
      DO 3 k= 1,nt2
    3   P(k)= BUFP(k)
C
      CALL TRACK ('SUPPLY  ')
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE  TALLY( iou, mode )
C***********************************************************************
C                                                                      *
C    TALLY      computes average and minimum Cpu timings and variances.*
C                                                                      *
C               iou -  i/o unit number                                 *
C                                                                      *
C              mode -  = 1 selects average run time: Preferred mode.   *
C                      = 2 selects minimum run time: Less accurate mode*
C                                                                      *
C***********************************************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  cs                                             REDUNDNT
C
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      DIMENSION  S1(20), S2(20), S3(20), S4(20)
      DIMENSION  T1(47), T4(47)
C
      CALL TRACE ('TALLY   ')
C
           CALL  SIZES(-1)
C
      m= 1
      IF( mode .EQ. 2 )  m= 3
      CALL  PAGE(iou)
      WRITE( iou, 99)
      WRITE( iou,100)
C                        Checks valid domain for min and max of data sets
      DO 2 j= 1,Nruns
      WRITE( iou,102)  j, ( SEE(k,1,j,il), k= 1,2)
      T1(j)= SEE(1,1,j,il)
      i= 0
      IF( (SEE(3,2,j,il).LT. 0.01) .OR. (SEE(4,2,j,il).GT. 1.0))  i= i+1
      IF( (SEE(3,3,j,il).LT. 0.01) .OR. (SEE(4,3,j,il).GT. 1.0))  i= i+1
      IF( i.GT.0 )  THEN
      WRITE( iou,131)  j, il
      ENDIF
      IF( ( j.EQ.Nruns ) .OR. ( i.GT.0 ))  THEN
      WRITE( iou,104)  j, ( SEE(k,2,j,il), k= 1,4)
      WRITE( iou,104)  j, ( SEE(k,3,j,il), k= 1,4)
      ENDIF
    2 continue
C
      CALL STATS( S1, T1, Nruns)
      WRITE( iou,102)  Nruns, ( S1(k), k= 1,4)
C
C
C
      WRITE( iou,120) Nruns
      WRITE( iou,122)
      WRITE( iou,121)
      WRITE( iou,122)
C                        Computes and Checks experimental timing errors
      DO 8 k= 1,mk
        npft= 0
          cs= 0.0d0
C
      DO 4 j= 1,Nruns
        npft= npft +  NPFS(j,il,k)
          cs= cs   + CSUMS(j,il,k)
    4 continue
C
      CALL  STATS( S2, TIMES(1,il,k), Nruns)
      TIME(k)= S2(m)
      CSUM(k)= cs
      TERR1(k)= 100.0d0*( S2(2)/( S2(1) + 1.0d-9))
      T4(k)= TERR1(k)
C
C
C     If this clock resolution test fails, you must increase Loop (Subr. SIZES)
C
      CALL  STATS( S3, TERRS(1,il,k), Nruns)
         IF( S3(1) .GT. 15.0)  THEN
            WRITE( iou,113) k
         ENDIF
C
      WRITE( iou,123) k, S2(3), S2(1), S2(4), TERR1(k), S3(1), npft
      TERR1(k)= MAX( TERR1(k), S3(1))
      CALL  STATS( S1, DOS(1,il,k), Nruns)
      TOTAL(k)= S1(1)
           IF( (S1(1).LE.0.0d0) .OR. (ABS(S1(3)-S1(4)).GT.1.0d-5)) THEN
           WRITE( iou,131) il, k, ( S1(k4), k4= 1,4)
           ENDIF
      CALL  STATS( S4, FOPN(1,il,k), Nruns)
      FLOPN(k)= S4(1)
           IF( (S4(1).LE.0.0d0) .OR. (ABS(S4(3)-S4(4)).GT.1.0d-5)) THEN
           WRITE( iou,131) il, k, ( S4(k4), k4= 1,4)
           ENDIF
    8 continue
C
      WRITE( iou,122)
      CALL  STATS( S4, T4, mk)
      WRITE(   *,124)
      WRITE(   *,133)
      WRITE(   *,125)  ( S4(k), k= 1,4)
      WRITE( iou,124)
      WRITE( iou,133)
      WRITE( iou,125)  ( S4(k), k= 1,4)
C
      CALL TRACK ('TALLY   ')
      RETURN
C
   99 FORMAT(//,' time TEST overhead (t err):  ')
  100 FORMAT(/,6X,'RUN',8X,'AVERAGE',8X,'STANDEV',8X,'MINIMUM',8X,
     1 'MAXIMUM')
  102 FORMAT(1X,'TICK ',I3,4E15.6)
  104 FORMAT(1X,'DATA ',I3,4E15.6)
  113 FORMAT(/,1X,I2,' POOR CPU CLOCK RESOLUTION; NEED LONGER RUN. ')
  120 FORMAT(//,' THE EXPERIMENTAL TIMING ERRORS FOR ALL',I3,' RUNS')
  121 FORMAT('  k   T min      T avg      T max    T err   tick   P-F')
  122 FORMAT(' --  ---------  ---------  --------- -----  -----   ---')
  123 FORMAT(1X,I2,3E11.4,F6.2,'%',F6.2,'%',1X,I5)
  124 FORMAT(//,' NET CPU TIMING VARIANCE (T err);  A few % is ok: ')
  125 FORMAT(4X,' Terr',4(F14.2,'%'))
  131 FORMAT(1X,'**  TALLY: ERROR INVALID DATA** ',2I6,4E14.6)
  133 FORMAT(/,17X,'AVERAGE',8X,'STANDEV',8X,'MINIMUM',8X,'MAXIMUM' )
      END
C
C***********************************************
      SUBROUTINE TDIGIT( derr, nzd, s )
C***********************************************************************
C                                                                      *
C     TDIGIT  -  Count Lead Digits Followed By Trailing Zeroes.        *
C                                                                      *
C       derr  -  Result,  Digital Error in percent.                    *
C        nzd  -  Result,  Number Of Lead Digits                        *
C          s  -  Input ,  A Floated Integer                            *
C                                                                      *
C***********************************************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  frac, fuzz, x, y, v, z                         REDUNDNT
C
c     frac(z)= (SIGN((ABS(z) - AINT(ABS(z))),z))
      frac(z)= ( ABS( ABS(z) - AINT(ABS(z))))
C
      CALL TRACE ('TDIGIT  ')
C
            x= 0.00d0
            n= 14
            x= ABS(s)
         fuzz= 1.0d-6
         derr= 100.0d0
          nzd= 0
           IF( x.EQ. 0.0d0)  GO TO 73
C                                  Normalize x
            y= LOG10(x)
            v= REAL( 10**( ABS( INT(y)) + 1 ))
C
           IF( (y.GE. 0.0d0) .AND. (v.NE. 0.0d0))  THEN
            x= (x/v) * 10.0d0
           ELSE
            x= x*v
           ENDIF
C                                  Multiply x Until Trailing Digits= Fuzz
       DO 1 k= 1,n
           IF( ((1.0d0-frac(x)).LE.fuzz) .OR. (frac(x).LE.fuzz)) GO TO 2
            x= 10.0d0*x
    1 continue
C
    2      IF( x.NE. 0.0d0)  THEN
               derr= 50.0d0/x
                nzd= INT( LOG10( ABS( 9.999999990d0*x )))
           ENDIF
C
   73 CONTINUE
      CALL TRACK ('TDIGIT  ')
      RETURN
      END
C
C*************************************************
      INTEGER FUNCTION  TEST( i )
C***********************************************************************
C                                                                      *
C              REPEAT AND TIME THE EXECUTION OF KERNEL i               *
C                                                                      *
C                    i  - Input integer;   Test Kernel Serial Number   *
C                 TEST  - Repetition Loop Counter, decremented to 0    *
C                                                                      *
C***********************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
CLOX  REAL*8 SECOND
C
C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
C
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     2                AR,BR,C0,CR,DI,DK,
     3  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     4  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
C
      DIMENSION     ZX(1023), XZ(1500)
      EQUIVALENCE ( ZX(1), Z(1)), ( XZ(1), X(1))
C
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
C
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
C
C
      COMMON /BASER/ A110,A120,A130,A210,A220,A230,A310,A320,A330,
     2                AR0,BR0,C00,CR0,DI0,DK0,
     3  DM220,DM230,DM240,DM250,DM260,DM270,DM280,DN0,E30,E60,EXPMAX0,
     4  FLX0,Q0,QA0,R0,RI0,S0,SCALE0,SIG0,STB50,T0,XNC0,XNEI0,XNM0
C
      COMMON /BASE1/ U0(1001), V0(1001), W0(1001),
     1  X0(1001), Y0(1001), Z0(1001), G0(1001),
     2  DU10(101), DU20(101), DU30(101), GRD0(1001), DEX0(1001),
     3  XI0(1001), EX0(1001), EX10(1001), DEX10(1001),
     4  VX0(1001), XX0(1001), RX0(1001), RH0(2048),
     5  VSP0(101), VSTP0(101), VXNE0(101), VXND0(101),
     6  VE30(101), VLR0(101), VLIN0(101), B50(101),
     7  PLAN0(300), D0(300), SA0(101), SB0(101)
C
      COMMON /BASE2/ P0(4,512), PX0(25,101), CX0(25,101),
     1  VY0(101,25), VH0(101,7), VF0(101,7), VG0(101,7), VS0(101,7),
     2  ZA0(101,7)  , ZP0(101,7), ZQ0(101,7), ZR0(101,7), ZM0(101,7),
     3  ZB0(101,7)  , ZU0(101,7), ZV0(101,7), ZZ0(101,7),
     4  B0(64,64), CC0(64,64), H0(64,64),
     5  U10(5,101,2),  U20(5,101,2),  U30(5,101,2)
C
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
C
C*******************************************************************************
C         Repeat execution of each Kernel(i) :     DO 1 L= 1,Loop   etc.
C*******************************************************************************
C
C    From the beginning in 1970 each sample kernel was executed just
C    once since supercomputers had high resolution, microsecond clocks.
C    In 1982 a repetition Loop was placed around each of the 24 LFK
C    kernels in order to run each kernel long enough for accurate
C    timing on mini-computer systems with poor cpu-clock resolution since
C    the majority of systems could only measure cpu-time to 0.01 seconds.
C    By 1990 however, several compilers' optimizers were factoring or
C    hoisting invariant computation outside some repetition Loops thus
C    distorting those Fortran samples.  The effect was usually absurd
C    Mflop rates which had to be corrected with compiler directives.
C    Therefore, in April 1990 these repetition Loops were removed from
C    subroutine KERNEL and submerged in subroutine TEST beyond the scope
C    of compiler optimizations.   Thus the 24 samples are now foolproof
C    and it will no longer be necessary to double check the machine code.
C
C    Very accurate, convergent methods have been developed to measure the
C    overhead time used for subroutines SECOND and TEST in subroutines
C    SECOVT and TICK respectively.  Thus, the LFK test may use substantially
C    more cpu time on systems with poor cpu-clock resolution.
C    The 24 C verison tests in CERNEL have also been revised to correspond with
C    the Fortran KERNEL. The 24 computation samples have NOT been changed.
C
C*******************************************************************************
C
cbug  IF( (LP.NE.Loop).OR.(L.LT.1).OR.(L.GT.Loop)) THEN
cbug      CALL TRACE('TEST    ')
cbug      CALL WHERE(0)
cbug  ENDIF
C                                    Repeat kernel test:   Loop times.
      IF( L .LT. Loop )  THEN
          L    = L + 1
          TEST = L
          RETURN
      ENDIF
C                                    Repeat kernel test:   Loop*Loops2
          ik   = i
      IF( mpy .LT. Loops2 )  THEN
          mpy  = mpy + 1
          nn   = n
C
           IF( i.EQ.0 ) GO TO 100
           IF( i.LT.0 .OR. i.GT.24 )  THEN
               CALL TRACE('TEST    ')
               CALL WHERE(0)
           ENDIF
C                   RE-INITIALIZE OVER-STORED INPUTS:
C
        GO TO( 100,   2, 100,   4,   5,   6, 100, 100,
     .         100,  10, 100, 100,  13,  14, 100,  16,
     .          17,  18,  19,  20,  21, 100,  23, 100, 100  ),  i
C
C     When MULTI.GE.100 each kernel is executed over a million times
C     and the time used to re-intialize overstored input variables
C     is negligible.  Thus each kernel may be run arbitrarily many times
C     (MULTI >> 100) without overflow and produce verifiable checksums.
C
C***********************************************************************
C
    2 DO 200 k= 1,nn
  200 X(k)= X0(k)
      GO TO 100
C***************************************
C
    4        m= (1001-7)/2
      DO 400 k= 7,1001,m
  400 XZ(k)= X0(k)
      GO TO 100
C***************************************
C
    5 DO 500 k= 1,nn
  500 X(k)= X0(k)
      GO TO 100
C***************************************
C
    6 DO 600 k= 1,nn
  600 W(k)= W0(k)
      GO TO 100
C***************************************
C
   10 DO 1000 k= 1,nn
      DO 1000 j= 5,13
 1000   PX(j,k)= PX0(j,k)
      GO TO 100
C***************************************
C
   13 DO 1300 k= 1,nn
         P(1,k)= P0(1,k)
         P(2,k)= P0(2,k)
         P(3,k)= P0(3,k)
 1300    P(4,k)= P0(4,k)
c
      DO 1301 k= 1,64
      DO 1301 j= 1,64
 1301    H(j,k)= H0(j,k)
      GO TO 100
C***************************************
C
   14 DO 1400   k= 1,nn
      RH(IR(k)  )= RH0(IR(k)  )
 1400 RH(IR(k)+1)= RH0(IR(k)+1)
      GO TO 100
C***************************************
C
   16 k2= 0
      k3= 0
      GO TO 100
C***************************************
C
   17 DO 1700 k= 1,nn
 1700     VXNE(k)= VXNE0(k)
      GO TO 100
C***************************************
C
   18 DO 1800 k= 2,6
      DO 1800 j= 2,nn
        ZU(j,k)= ZU0(j,k)
        ZV(j,k)= ZV0(j,k)
        ZR(j,k)= ZR0(j,k)
 1800   ZZ(j,k)= ZZ0(j,k)
      GO TO 100
C***************************************
C
   19 STB5= STB50
      GO TO 100
C***************************************
C
   20 XX(1)= XX0(1)
      GO TO 100
C***************************************
C
   21 DO 2100 k= 1,nn
      DO 2100 j= 1,25
 2100   PX(j,k)= PX0(j,k)
      GO TO 100
C***************************************
C
   23 DO 2300 k= 2,6
      DO 2300 j= 2,nn
 2300   ZA(j,k)= ZA0(j,k)
C***********************************************************************
C
  100 CONTINUE
C
          L    = 1
          TEST = 1
          RETURN
      ENDIF
C
          mpy  = 1
          L    = 1
          TEST = 0
C                                   switchback to TICK to measure testov
           IF( i.EQ. (-73))  RETURN
C
C***********************************************************************
C           t= second(0)  := cumulative cpu time for task in seconds.
C***********************************************************************
C
      cumtim(1)= 0.0d0
         TEMPUS= SECOND( cumtim(1)) - START
C
      CALL TRACE ('TEST    ')
CPFM      ikern= i
CPFM      call ENDPFM(ion)
C$C                           5 get number of page faults (optional)
C$      KSTAT= LIB$STAT_TIMER(5,KPF)
C$      NPF  = KPF - IPF
C
C
C                             Checksum results; re-initialize all inputs
      CALL TESTS ( i, TEMPUS )
C
C
C$C                           5 get number of page faults (optional) VAX
C$      NSTAT= LIB$STAT_TIMER(5,IPF)
C
CPFM       IF( INIPFM( ion, 0) .NE. 0 )  THEN
CPFM           CALL WHERE(20)
CPFM       ENDIF
      CALL TRACK ('TEST    ')
C
C      The following pause can be used for stop-watch timing of each kernel.
C      You may have to increase the iteration count MULTI in Subr. VERIFY.
C
C/           PAUSE
C
      mpy   = 1
      mpylim= Loops2
      L     = 1
      LP    = Loop
      ik    = i+1
      TEST  = 0
      cumtim(1)= 0.0d0
      START= SECOND( cumtim(1))
      RETURN
C
C$      DATA  IPF/0/, KPF/0/
      END
C
C***********************************************
      SUBROUTINE  TESTS( i, TEMPUS )
C***********************************************************************
C                                                                      *
C               CHECKSUM AND INITIALIZE THE EXECUTION OF KERNEL i      *
C                                                                      *
C                    i  - Input integer;  Test Kernel Serial Number    *
C               TEMPUS  - Input float  ;  Elapsed Cpu-time Test(i) used*
C                                                                      *
C***********************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
C
C/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
C/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
C/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
C
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     2                AR,BR,C0,CR,DI,DK,
     3  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     4  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
CPFM  COMMON /KAPPA/ iflag1, ikern, statis(100,20), istats(100,20)
C
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
C
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
C
C
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
C
          ik   = i
      CALL TRACE ('TESTS   ')
C
          NP   = Loop * Loops2
          Loop = 1
          LP   = Loop
          NN   = n
           IF( i.LT.0 .OR. i.GT.24 )  THEN
               CALL WHERE(0)
           ENDIF
C
           IF( i.EQ.0 )  GO TO 120
         CALL  SIZES(i)
C
C     Net Time=  Timing - Overhead Time
C
      TIME(i)= TEMPUS - REAL( NP) *testov - tsecov
C
C
        GO TO(  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     .         11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     .         21, 22, 23, 24, 25                      ), i
C
C
C
C***********************************************************************
C
    1 CSUM (1) =  SUMO ( X, n)
      TOTAL(1) =  NP*NN
      GO TO 100
C***********************************************************************
C
    2 CSUM (2) =  SUMO ( X, 2*n)
      TOTAL(2) =  NP*(NN-4)
      GO TO 100
C***********************************************************************
C
    3 CSUM (3) =  Q
      TOTAL(3) =  NP*NN
      GO TO 100
C***********************************************************************
C
    4        MM= (1001-7)/2
      DO 400 k = 7,1001,MM
  400      V(k)= X(k)
      CSUM (4) = SUMO ( V, 3)
      TOTAL(4) =  NP*(((NN-5)/5)+1)*3
      GO TO 100
C***********************************************************************
C
    5 CSUM (5) =  SUMO ( X(2), n-1)
      TOTAL(5) =  NP*(NN-1)
      GO TO 100
C***********************************************************************
C
    6 CSUM (6) =  SUMO ( W, n)
      TOTAL(6) =  NP*NN*((NN-1)/2)
      GO TO 100
C***********************************************************************
C
    7 CSUM (7) =  SUMO ( X, n)
      TOTAL(7) =  NP*NN
      GO TO 100
C***********************************************************************
C
    8 CSUM (8) = SUMO ( U1,5*n*2) + SUMO ( U2,5*n*2) + SUMO ( U3,5*n*2)
      TOTAL(8) =  NP*(NN-1)*2
      GO TO 100
C***********************************************************************
C
    9 CSUM (9) =  SUMO ( PX, 15*n)
      TOTAL(9) =  NP*NN
      GO TO 100
C***********************************************************************
C
   10 CSUM (10) =  SUMO ( PX, 15*n)
      TOTAL(10) =  NP*NN
      GO TO 100
C***********************************************************************
C
   11 CSUM (11) =  SUMO ( X(2), n-1)
      TOTAL(11) =  NP*(NN-1)
      GO TO 100
C***********************************************************************
C
   12 CSUM (12) =  SUMO ( X, n-1)
      TOTAL(12) =  NP*NN
      GO TO 100
C***********************************************************************
C
   13 CSUM (13) =  SUMO ( P, 8*n) + SUMO ( H, 8*n)
      TOTAL(13) =  NP*NN
      GO TO 100
C***********************************************************************
C
   14 CSUM (14) =  SUMO ( VX,n) + SUMO ( XX,n) + SUMO ( RH,67)
      TOTAL(14) =  NP*NN
      GO TO 100
C***********************************************************************
C
   15 CSUM (15) =  SUMO ( VY, n*7) + SUMO ( VS, n*7)
      TOTAL(15) =  NP*(NN-1)*5
      GO TO 100
C***********************************************************************
C
   16 CSUM (16) =  REAL( k3+k2+j5+m)
      FLOPN(16) =  ( k2+k2+10*k3 ) * Loops2
      TOTAL(16) =  1.0d0
      GO TO 100
C***********************************************************************
C
   17 CSUM (17) =  SUMO ( VXNE, n) + SUMO ( VXND, n) + XNM
      TOTAL(17) =  NP*NN
      GO TO 100
C***********************************************************************
C
   18 CSUM (18) =  SUMO ( ZR, n*7) + SUMO ( ZZ, n*7)
      TOTAL(18) =  NP*(NN-1)*5
      GO TO 100
C***********************************************************************
C
   19 CSUM (19) =  SUMO ( B5, n) + STB5
      TOTAL(19) =  NP*NN
      GO TO 100
C***********************************************************************
C
   20 CSUM (20) =  SUMO ( XX(2), n)
      TOTAL(20) =  NP*NN
      GO TO 100
C***********************************************************************
C
   21 CSUM (21) =  SUMO ( PX, 25*n)
      TOTAL(21) =  NP*25*25*NN
      GO TO 100
C***********************************************************************
C
   22 CSUM (22) =  SUMO ( W, n)
      TOTAL(22) =  NP*NN
      GO TO 100
C***********************************************************************
C
   23 CSUM (23) =  SUMO ( ZA, n*7)
      TOTAL(23) =  NP*(NN-1)*5
      GO TO 100
C***********************************************************************
C
   24 CSUM (24) =  REAL(m)
      TOTAL(24) =  NP*(NN-1)
      GO TO 100
C***********************************************************************
C
   25 CONTINUE
      GO TO 100
C***********************************************************************
C
  100 CONTINUE
C
C     delta( testov)= relerr * testov
            overr= SEE(2,1,jr,il)*REAL(NP)* testov
         TERR1(i)= 100.0
               IF( TIME(i).NE. 0.0d0) TERR1(i)= TERR1(i)*(overr/TIME(i))
         NPFS1(i)= NPF
               IF( ion .LE. 0 )  GO TO 120
C
C     If this clock resolution test fails, you must increase Loop (Subr. SIZES)
C
               IF( TERR1(i) .LT. 15.0)  GO TO 114
            WRITE( ion,113) I
  113 FORMAT(/,1X,I2,' TESTS:  POOR TIMING OR ERROR. NEED LONGER RUN')
C
  114      WRITE ( ion,115) i, TIME(i), TERR1(i), NPF
  115      FORMAT( 2X,i2,' Done  T= ',E11.4,'  T err= ',F8.2,'%' ,
     1             I8,'  Page-Faults ')
C
  120      IF( i.GE.0 .AND. i.LT.24 )  THEN
               CALL VALUES(i+1)
               CALL SIZES (i+1)
           ENDIF
C
      CALL TRACK ('TESTS   ')
      RETURN
      END
C
C***********************************************
      FUNCTION TICK( iou, ntimes)
C***********************************************
C
C            TICK       measures timing overhead of subroutine test
C
C     iou    -  Logical Output Device Number                           *
C
C***********************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
C
C/      PARAMETER( l1= 1001, l2=  101, l1d= 2*1001 )
C/      PARAMETER( l13=  64, l13h= l13/2, l213= l13+l13h, l813= 8*l13 )
C/      PARAMETER( l14=2048, l16=  75, l416= 4*l16 , l21= 25 )
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      parameter( l4813= 4*512, l4813p= l4813 + 1 )
      INTEGER TEST
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     2                AR,BR,C0,CR,DI,DK,
     3  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     4  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
C
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
C
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
C
      DIMENSION  TIM(20), TER(20), TMX(20), INX(20), P1(l4813p)
      EQUIVALENCE( P,P1)
      SAVE retest
C
C
      CALL TRACE ('TICK    ')
C
      ion= iou
      kr = mk
      n  = 0
      k2 = 0
      k3 = 0
      m  = 0
      neff= 0
      IF( il .EQ. 1 )  THEN
C
C***********************************************************************
C     Measure tsecov:  Overhead time for calling SECOND
C***********************************************************************
C
      tsecov = SECOVT( iou)
         tic = tsecov
C
C***********************************************************************
C     Measure testov:  Overhead time for calling TEST
C***********************************************************************
C
         testo= 0.00d0
           klm= 8000
            io= ABS(iou)
            jj= 0
            nt= ntimes - 6
             j= nt
            IF( nt.LT.8 .OR. nt.GT.30 )  GO TO 911
C
      DO 820 j= 1,nt
             L= 1
           mpy= 1
        Loops2= 1
        mpylim= Loops2
          Loop= klm
            LP= Loop
C                                  Measure overhead time for empty loop
      cumtim(1)= 0.0d0
             t0= SECOND( cumtim(1))
  801        IF( TEST(-73) .GT. 0 )  GO TO 801
  802        IF( TEST(-73) .GT. 0 )  GO TO 802
  803        IF( TEST(-73) .GT. 0 )  GO TO 803
  804        IF( TEST(-73) .GT. 0 )  GO TO 804
  805        IF( TEST(-73) .GT. 0 )  GO TO 805
  806        IF( TEST(-73) .GT. 0 )  GO TO 806
  807        IF( TEST(-73) .GT. 0 )  GO TO 807
  808        IF( TEST(-73) .GT. 0 )  GO TO 808
  809        IF( TEST(-73) .GT. 0 )  GO TO 809
  810        IF( TEST(-73) .GT. 0 )  GO TO 810
      cumtim(1)= 0.0d0
             t1= SECOND( cumtim(1)) - tsecov
         elapst= t1 - t0
          testo= elapst/( REAL(10*klm) + 1.0e-9)
          toler= 0.020d0
           rerr= 1.00d0
C
C                                  Convergence test:  Rel.error .LT. 1%
            IF( elapst.GT. 1.00d04 ) GO TO 911
            IF( elapst.LT. 1.00d-9 .AND. j.GT.8 ) GO TO 911
            IF( elapst.GT. 1.00d-9 ) THEN
                     jj= jj + 1
                TIM(jj)= testo
                     IF( jj.GT.1 ) THEN
                         rerr= RELERR( TIM(jj), TIM(jj-1))
                     ENDIF
                TER(jj)= rerr
            ENDIF
C
            IF( iou.GT.0 ) THEN
         WRITE( iou,64) 10*klm,  testo, rerr
            ENDIF
            IF( rerr  .LT. toler   ) GO TO 825
            IF( elapst.GT. 10.00d0 ) GO TO 822
           klm= klm + klm
  820 continue
C                                  Poor accuracy on exit from loop
  822     IF( j .LE. 1 )  GO TO 911
          IF( jj.LT. 1 )  GO TO 911
         CALL SORDID( INX,TMX,  TER,jj,1)
       testo= TIM( INX(1))
        rerr= TMX(1)
       WRITE( io,63)  100.00d0*rerr
C                                  Good convergence, satifies 1% error tolerence
  825 continue
      testov        = testo
      retest        = rerr * testov
      ENDIF
C
C***********************************************************************
C                                  Generate data sets
      SEE(1,1,jr,il)= testov
      SEE(2,1,jr,il)= retest
      ticks         = testov
      TICK          = testov
      L      = 1
      Loop   = 1
      LP     = Loop
      j      = TEST(0)
C
      DO 20 k= 1,47
      TIME(k)= 0.0d0
      CSUM(k)= 0.0d0
20    CONTINUE
C
      IF( il .EQ. 1 )  THEN
          CALL  STATS( SEE(1,2,jr,il), U, nt1)
c         CALL  STATS( SEE(1,3,jr,il), P, nt2)
          CALL  STATS( SEE(1,3,jr,il), P1(l4813+1), nt2-l4813)
      ELSE
          DO 45 k= 1,5
              SEE( k,2,jr,il)= SEE( k,2,jr,1)
              SEE( k,3,jr,il)= SEE( k,3,jr,1)
   45     continue
      ENDIF
C
      IF( iou.GT.0 ) THEN
      WRITE( iou, 99)
      WRITE( iou,100)
      WRITE( iou,102)  ( SEE(k,1,jr,il), k= 1,2)
      WRITE( iou,104)  ( SEE(k,2,jr,il), k= 1,4)
      WRITE( iou,104)  ( SEE(k,3,jr,il), k= 1,4)
      ENDIF
C
      CALL TRACK ('TICK    ')
      RETURN
C
  911         WRITE( io,61)
              WRITE( io,62) elapst, j
              CALL WHERE(0)
C
   61 FORMAT(1X,'FATAL(TICK): cant measure overhead time of subr. TEST')
   62 FORMAT(/,13X,'using SECOND:  elapst=',1E20.8,6X,'J=',I4)
   63 FORMAT(1X,'WARNING(TICK):  TEST overhead time relerr',f9.4,'%')
   64 FORMAT(1X,'testov(TICK)',I12,E12.4,F11.4)
   99 FORMAT(//,' CLOCK OVERHEAD:  ')
  100 FORMAT(/,14X,'AVERAGE',8X,'STANDEV',8X,'MINIMUM',8X,'MAXIMUM')
  102 FORMAT(/,1X,' TICK',4E15.6)
  104 FORMAT(/,1X,' DATA',4E15.6)
      END
C
C***********************************************
      SUBROUTINE TILE( sm, si, OX,IX,W,ew,T,tiles,n)
C***********************************************
C
C     TILE       computes  m-tile value and corresponding index
C
C     sm      -  RESULT VALUE  IS m-TILE VALUE
C     si      -  RESULT VALUE  IS CORRESPONDING INDEX.r IN W
C
C     OX      -  INPUT  ARRAY  OF ORDERED (DECENDING) Xs.
C     IX      -  INPUT  ARRAY  OF INDEX LIST MAPS X TO OX.
C     W       -  INPUT  ARRAY  OF INPUT  WEIGHTS.
C     ew      -  INPUT  VALUE  FLAGS EQUAL WEIGHTS= 1.0; ELSE 0.0d0
C     T       -  INPUT  VALUE  IS SUM OF WEIGHTS
C     tiles   -  INPUT  VALUE  IS FRACTION OF RANGE, E.G. 0.25
C     n       -  INPUT  NUMBER OF INPUT  VALUES IN X.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      DIMENSION  OX(n), IX(n), W(n)
C
      CALL TRACE ('TILE    ')
C
       thresh= tiles*T + 0.50d0*ew*W(1)
            R= 0.0d0
            S= R
      DO 70 k= 1,n
            S= R
            R= R + W( IX(k))
           IF( R .GT. thresh )  GO TO 7
   70 CONTINUE
            k= n
    7       z= 0.0d0
            y= 0.0d0
           IF( k.GT.1 )    y =   OX(k-1)
           IF( R.NE.S )    z = ( thresh - S)/( R - S)
           sm= y         + z * ( OX(k)  - y)
           si= REAL(k-1) + z
C
      CALL TRACK ('TILE    ')
      RETURN
      END
C
C***********************************************
      SUBROUTINE TRACE ( name )
C***********************************************
C
C      Records /DEBUG/ info: sequence of called subroutine names
C      Enters current subroutine name on top of /DEBUG/ stack
C      NOT NECESSARY FOR PERFORMANCE TEST, MAY BE DUMMIED.
C
C      name   -  Input;  Callers name
C      ISTACK -  Contains names of subroutines in active linkage chain.
C
C                Interupt shows active chain of subr. names in ISTACK:
C      bkp kernel
C      run
C      BREAKPOINT REACHED AT 00417457PB = KERNEL:KERNEL+201PB
C      bkp trap
C      run
C      BREAKPOINT REACHED AT 00450122PB = TRAP:TRAP+45PB
C      sub= tracks  bcd  istack,10  dec  nstack,10
C
C      ISTACK(1) = "IQRANF  VALUES  TEST    KERNEL   MAIN.  "
C      NSTACK(1) =  164  162  157  65  1  0  0  0  0  0
C
C                Setting TRACE  call 5317 causes CALL to STOPS:
C      sub= tracks
C      set match= 5317
C      rel all.
C      bkp trace
C      run
C      BREAKPOINT REACHED AT 00440575PB = STOPS:STOPS+6PB
C      sub= tracks  bcd  istack,10  dec  nstack,10
C
C      ISTACK(1) = "SORDID  STATW   SENSIT  REPORT   MAIN.  "
C      NSTACK(1) =  5317  5316  5308  5282  1  0  0  0  0  0
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      CHARACTER  name*8, ISTACK*8
      COMMON /DEBUG/     ISTACK(20)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
C
C                              pushdown stack of subroutine names and call nrs.
          DO  1  k = 10,2,-1
          NSTACK(k)= NSTACK(k-1)
          ISTACK(k)= ISTACK(k-1)
    1     continue
C
          inseq= inseq + 1
      NSTACK(1)= inseq
      ISTACK(1)= name
          isave= inseq
c     WRITE( 6,111) inseq, ( ISTACK(k), k= 1,7 )
c 111 FORMAT(2X,I6,4X,10A8)
C
      IF( inseq.EQ.match ) THEN
          CALL STOPS
      ENDIF
C
          CALL WATCH(1)
C
      RETURN
      END
C
C***********************************************
      SUBROUTINE STOPS
C***********************************************
C
C     This routine is a convenient program break-point which is
C     selected by pre-setting:  match in COMMON /ORDER/  or by data
C     loading in BLOCK DATA  to equal the serial index of a
C     particular call to TRACE , as previously recorded in NSTACK.
C     The call to STOPS is selected in subroutine TRACE .
C
C     PAUSE 1
      RETURN
      END
C
C***********************************************
      SUBROUTINE TRACK( name )
C***********************************************
C
C      Releases current subroutine name from top of /DEBUG/ stack
C      NOT NECESSARY FOR PERFORMANCE TEST, MAY BE DUMMIED.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      CHARACTER  name*8, ISTACK*8
      COMMON /DEBUG/     ISTACK(20)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
C
           iret= iret + 1
           CALL  WATCH(2)
C                             pop stack of subroutine names
      IF( name.EQ. ISTACK(1))  THEN
           DO  1  k = 1,9
           NSTACK(k)= NSTACK(k+1)
           ISTACK(k)= ISTACK(k+1)
    1      continue
      ELSE
           ISTACK(20)= name
           CALL  WHERE(12)
      ENDIF
C
      RETURN
      END
C
C***********************************************
      SUBROUTINE TRAP( I, name, mini, maxi,meff)
C***********************************************
C
C      Checks that Index List values are in valid domain
C
C     I     - ARRAY  OF INPUT INDEX-LIST
C     name  -           INPUT CALLERS name
C     mini  - INPUT SMALLEST INDEX VALUE
C     maxi  - INPUT LARGEST  INDEX VALUE
C     meff  - NUMBER OF INPUT VALUES IN I.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
      DIMENSION  I(meff)
      CHARACTER NAME*(*)
C
      CALL TRACE ('TRAP    ')
C
           LX= 0
      DO 1 k= 1,meff
          IF( I(k).LT.mini .OR. I(k).GT.maxi )  LX= k
    1 CONTINUE
C
          IF( LX.NE.0 )   THEN
              io= ABS( ion)
              IF( io.LE.0 .OR. io.GT.10 ) io=6
              WRITE( io,110)  LX, name
  110   FORMAT(////,' TRAP: ERROR IN INDEX-LIST(',i4,')  IN SUBR:  ',A )
              WRITE( io,113) I
  113         FORMAT(1X,10I6)
C
              CALL WHERE(0)
          ENDIF
C
      CALL TRACK ('TRAP    ')
      RETURN
      END
C
C***********************************************
      SUBROUTINE TRIAL( iou, i, t0, tj )
C***********************************************
C
C     TRIAL - validates checksums of current run for endurance trial
C
C***********************************************************************
C
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  SUMS                                           REDUNDNT
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C/      PARAMETER( nk= 47, nl= 3, nr= 8 )
      parameter( mall= 24 * 3 )
c
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
C
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
C
      COMMON /PROOF/  SUMS(24,3,8)
      DIMENSION  ID(mall), LD(mall), CS1(mall), CS2(mall)
      SAVE isum
      MODI(ii,mm)= (MOD( ABS(ii)-1, mm) + 1)
      NPER(ii,mm)= ((ABS(ii)-1+mm)/(mm))
C
      CALL TRACE ('TRIAL   ')
C
      IF( i.EQ.1 ) THEN
           estime= (tj-t0) + REAL( Mruns) *( SECOND(0.0) - tj)
      WRITE( iou,70) estime, Nruns
      WRITE(   *,70) estime, Nruns
   70 FORMAT(/,' ESTIMATED TOTAL JOB CPU-TIME:=' ,F10.3,' sec.',
     1 '  ( Nruns=',I8,' Trials)',/)
      ENDIF
C
                           ijk= 4
      IF( MULTI.LE.   1 )  ijk= 1
      IF( MULTI.EQ.  10 )  ijk= 2
      IF( MULTI.EQ.  50 )  ijk= 3
      IF( MULTI.GE. 100 )  ijk= 4
c
           lx= 0
      DO  1 j= im,ml
      DO  1 k= 1,24
           lx= lx + 1
      CS1(lx)= CSUMS(jr,j,k)
      CS2(lx)= SUMS(k,j,ijk)
    1 continue
c
      CALL  SEQDIG( ID, isum, CS1, CS2, mall)
c
      IF( i.EQ.1 ) THEN
c
          DO 2 k= 1,mall
          LD(k)= ID(k)
    2     continue
      ELSE
          IF( isum.EQ.last .AND. isum.GT.200 ) THEN
              npass= npass + 1
          ELSE
              nfail= nfail + 1
c
              DO 4 k= 1,mall
              IF( ID(k) .NE. LD(k))  THEN
              WRITE( iou,333) i, MODI(k,24), NPER(k,24), ID(k), LD(k)
              ENDIF
    4         continue
          ENDIF
      ENDIF
c
c
      IF( i.LE.7 .OR. MODI(i,7).EQ.1 )  THEN
      WRITE( iou,111) i, isum, npass, nfail
      WRITE(   *,111) i, isum, npass, nfail
  111 FORMAT(' Trial=',I7,13X,'ChkSum=',I5,4X,'Pass=',I7,5X,'Fail=',I7)
c
c     cumtim(1)= 0.0d0
c          tjob= SECOND( cumtim(1)) - t0
c     WRITE( iou,123)  tjob
c     WRITE(   *,123)  tjob
c 123 FORMAT(2X,'Tcpu=',4X,F10.2,' sec')
c
c     WRITE( iou,222) ( MODI(k,24), ID(k), CS1(k), CS2(k), k= 1,mall )
c 222 FORMAT(2X,2I6,3X,2E24.16)
  333 FORMAT(1X,'TRIAL:',I7,6X,'Kernel=',I5,6X,'j= ',I7,6X,'ERROR',2I7)
      ENDIF
        last= isum
        ibuf= 0
C
      CALL TRACK ('TRIAL   ')
      RETURN
      END
C
C***********************************************
      SUBROUTINE VALID( VX,MAP,LX,  BL,X,BU,n )
C***********************************************
C
C      Compress valid data sets;  form compression list.
C
C
C     VX    - ARRAY  OF RESULT COMPRESSED Xs.
C     MAP   - ARRAY  OF RESULT COMPRESSION INDICES
C     LX     -           RESULT COMPRESSED LENGTH OF VX, MAP
C           -
C     BL    -           INPUT LOWER BOUND FOR VX
C     X     - ARRAY  OF INPUT VALUES.
C     BU    -           INPUT UPPER BOUND FOR VX
C     n     - NUMBER OF INPUT VALUES IN X.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      DIMENSION  VX(n), MAP(n), X(n)
CLLL. OPTIMIZE LEVEL G
C
      CALL TRACE ('VALID   ')
C
           m= 0
           LX= 0
          IF( n.LE.0 )  GO TO 73
      DO 1 k= 1,n
              IF( X(k).LE. BL .OR. X(k).GE. BU )  GO TO 1
                     m= m + 1
                MAP(m)= k
                 VX(m)= X(k)
    1 CONTINUE
C
      LX= m
      IF( m.GT.0 )  THEN
          CALL TRAP( MAP, ' VALID  ' , 1, n,m)
      ENDIF
   73 CONTINUE
      CALL TRACK ('VALID   ')
      RETURN
      END
C
C***********************************************
      SUBROUTINE VALUES(i)
C***********************************************
C
C            VALUES     initializes special values
C
C     i    :=  kernel number
C
C****************************************************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
      DOUBLE  PRECISION  DS, DW                                         REDUNDNT
C
C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
C
C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /SPACER/ A11,A12,A13,A21,A22,A23,A31,A32,A33,
     2                AR,BR,C0,CR,DI,DK,
     3  DM22,DM23,DM24,DM25,DM26,DM27,DM28,DN,E3,E6,EXPMAX,FLX,
     4  Q,QA,R,RI,S,SCALE,SIG,STB5,T,XNC,XNEI,XNM
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
      INTEGER    E,F,ZONE
      COMMON /ISPACE/ E(96), F(96),
     1  IX(1001), IR(1001), ZONE(300)
C
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
C
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
C
      COMMON /BASE2/ P0(4,512), PX0(25,101), CX0(25,101),
     1  VY0(101,25), VH0(101,7), VF0(101,7), VG0(101,7), VS0(101,7),
     2  ZA0(101,7)  , ZP0(101,7), ZQ0(101,7), ZR0(101,7), ZM0(101,7),
     3  ZB0(101,7)  , ZU0(101,7), ZV0(101,7), ZZ0(101,7),
     4  B0(64,64), CC0(64,64), H0(64,64),
     5  U10(5,101,2),  U20(5,101,2),  U30(5,101,2)
C
      COMMON /SPACE3/ CACHE(8192)
C
C     ******************************************************************
      CALL TRACE ('VALUES  ')
C
      CALL SIZES (i)
      IP1= i
c              Initialize the dummy  Cache-memory with never used data-set.
      DO 666 k= 1,8192
      CACHE(k)= 0.10
  666 CONTINUE
C
      CALL  SUPPLY( i)
C
      IF( IP1.NE.13 ) GO TO 14
            DS= 1.000d0
            DW= 0.500d0
      DO 205 j= 1,4
      DO 205 k= 1,512
      P(j,k)  = DS
      P0(j,k) = DS
            DS= DS + DW
  205 CONTINUE
C
      DO 210 j= 1,96
      E(j) = 1
      F(j) = 1
  210 CONTINUE
C
   14 IF( IP1.NE.14) GO TO 16
C
      mmin= 1
      mmax= 1001
      CALL IQRANF( IX, mmin, mmax, 1001)
c
            DW= -100.000d0
      DO 215 J= 1,1001
      DEX(J) =  DW*DEX(J)
      GRD(J) = IX(J)
  215 CONTINUE
      FLX= 0.00100d0
C
   16 IF( IP1.NE.16 ) GO TO 50
CONDITIONS:
            MC= 2
            lr= n
            II= lr/3
            FW= 1.000d-4
          D(1)= 1.0198048642876400d0
      DO 400 k= 2,300
  400     D(k)= D(k-1) + FW/D(k-1)
             R= D(lr)
            FW= 1.000d0
      DO 403 LX= 1,MC
             m= (lr+lr)*(LX-1)
      DO 401 j= 1,2
      DO 401 k= 1,lr
             m= m+1
             S= REAL(k)
       PLAN(m)= R*((S + FW)/S)
  401  ZONE(m)= k+k
  403 CONTINUE
             k= lr+lr+1
       ZONE(k)= lr
             S= D(lr-1)
             T= D(lr-2)
C
   50 CONTINUE
c               Clear the scalar Cache-memory with never used data-set.
c     fw= 1.000d0
c     CALL SIGNEL( CACHE, fw, 0.0d0, 8192)
c
             j= 0
            sc= 0.0d0
      DO 777 k= 1,8192
            IF( CACHE(k).EQ. 0.0)  THEN
             j= j + k
            sc= sc + REAL(j*k)
            ENDIF
  777 CONTINUE
C
      CALL TRACK ('VALUES  ')
      RETURN
      END
C
C***********************************************
      SUBROUTINE VERIFY( iou )
C***********************************************************************
C                                                                      *
C      VERIFY     auxiliary test routine to check-out function SECOND  *
C                 and to verify that sufficiently long Loop sizes are  *
C                 defined in Subr. SIZES for accurate CPU timing.      *
C                                                                      *
C       iou    -  Logical Output Device Number                         *
C                                                                      *
C***********************************************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
CLOX  REAL*8 SECOND
C
C/C/      PARAMETER( l1=   1001, l2=   101, l1d= 2*1001 )
C/C/      PARAMETER( l13= 64, l13h= 64/2, l213= 64+32, l813= 8*64 )
C/C/      PARAMETER( l14= 2048, l16= 75, l416= 4*75 , l21= 25)
C/C/      PARAMETER( kn= 47, kn2= 95, np= 3, ls= 3*47, krs= 24)
C
      parameter( ntmp= 100 )
C
      COMMON /SPACE1/ U(1001), V(1001), W(1001),
     1  X(1001), Y(1001), Z(1001), G(1001),
     2  DU1(101), DU2(101), DU3(101), GRD(1001), DEX(1001),
     3  XI(1001), EX(1001), EX1(1001), DEX1(1001),
     4  VX(1001), XX(1001), RX(1001), RH(2048),
     5  VSP(101), VSTP(101), VXNE(101), VXND(101),
     6  VE3(101), VLR(101), VLIN(101), B5(101),
     7  PLAN(300), D(300), SA(101), SB(101)
C
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101),
     1  VY(101,25), VH(101,7), VF(101,7), VG(101,7), VS(101,7),
     2  ZA(101,7)  , ZP(101,7), ZQ(101,7), ZR(101,7), ZM(101,7),
     3  ZB(101,7)  , ZU(101,7), ZV(101,7), ZZ(101,7),
     4  B(64,64), C(64,64), H(64,64),
     5  U1(5,101,2),  U2(5,101,2),  U3(5,101,2)
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
C
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
C
C
      DIMENSION  TIM(ntmp), TUM(ntmp), TAV(ntmp), TER(ntmp)
      DIMENSION  TMX(ntmp), SIG(ntmp), LEN(ntmp)
C
C
C     CALL TRACE ('VERIFY  ')
c
      DO  1 k = 1,101
          X(k)= 0.0d0
          Y(k)= 0.0d0
    1  CX(1,k)= 0.0d0
           nzd= 0
C
C***********************************************************************
C     Measure tsecov:  Overhead time for calling SECOND
C***********************************************************************
C
      tsecov = SECOVT( iou)
         tic = tsecov
C
C***********************************************************************
C     Measure time resolution of cpu-timer;  tclock= MIN t
C***********************************************************************
C
        fuzz= 1.00d-12
      nticks= INT( 1.00d0/( tsecov + fuzz ))
          IF( nticks.LT.1000 ) nticks= 1000
          dt= 0.00d0
          t1= SECOND( cum)
           m= 0
C
      DO 2 k= 1,nticks
          t2= SECOND( cum)
          IF( t2 .NE. t1 ) THEN
                  m= m + 1
                 dt= dt + ( t2 - t1 )
                 t1= t2
                 IF( m .GE. 200 ) GO TO 3
          ENDIF
    2 continue
C
    3     IF( m.LE.2 ) THEN
              tclock= 1.00d0
              WRITE(   *,163)
              WRITE( iou,163)
          ELSE
              tclock= dt/( REAL(m) + fuzz )
          ENDIF
C
       WRITE(   *,164) m, tclock
       WRITE( iou,164) m, tclock
  163 FORMAT(1X,'WARNING(VERIFY): POOR Cpu-timer resolution; REPLACE?')
  164 FORMAT('VERIFY:',I10,E12.4,' =  Time Resolution of Cpu-timer')
C
C****************************************************************************
C         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
C****************************************************************************
C
C         VERIFY produced the following output on CRAY-XMP4 in a
C         fully loaded, multi-processing, multi-programming system:
C
C
C         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
C         -----     -------     -------    -------   --------
C         EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE
C         Loop      CPUTIME     CLOCK      CLOCK     TIMING
C         SIZE      SECONDS     ERROR      ERROR     ERROR
C         -----     -------     -------    -------   --------
C             1  5.0000e-06      10.00%     17.63%     14.26%
C             2  7.0000e-06       7.14%      6.93%      4.79%
C             4  1.6000e-05       3.12%      6.56%      7.59%
C             8  2.8000e-05       1.79%      2.90%      2.35%
C            16  6.1000e-05       0.82%      6.72%      4.50%
C            32  1.1700e-04       0.43%      4.21%      4.62%
C            64  2.2700e-04       0.22%      3.13%      2.41%
C           128  4.4900e-04       0.11%      3.14%      0.96%
C           256  8.8900e-04       0.06%      2.06%      2.50%
C           512  1.7740e-03       0.03%      1.92%      1.59%
C          1024  3.4780e-03       0.01%      0.70%      1.63%
C          1360              Current Run:    MULTI=   10.000
C          2048  7.0050e-03       0.01%      0.74%      1.28%
C          4096  1.3823e-02       0.00%      1.35%      0.78%
C         -----     -------     -------    -------   --------
C
C          Approximate Serial Job Time=   2.5e+01 Sec.    ( Nruns= 7 RUNS)
C
C****************************************************************************
C
                WRITE( iou,45)
                WRITE( iou,49)
                WRITE( iou,46)
                WRITE( iou,47)
                WRITE( iou,48)
                WRITE( iou,49)
   45 FORMAT(/,8X,'VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY')
   46 FORMAT(8X,'EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE')
   47 FORMAT(8X,'Loop      CPUTIME     CLOCK      CLOCK     TIMING  ')
   48 FORMAT(8X,'SIZE      SECONDS     ERROR      ERROR     ERROR   ')
   49 FORMAT(8X,'-----     -------     -------    -------   --------')
C
C
C****************************************************************************
C     Measure Cpu Clock Timing Errors As A Function Of Loop Size(lo)
C****************************************************************************
C
         ttest= 100.00d0 * tclock
        ilimit= 30
            nj= 5
            lo= 128
             i= 0
C
   10        i= i + 1
            lo= lo + lo
      DO 53  j= 1,nj
             n= 100
      cumtim(1)= 0.0d0
             t0= SECOND( cumtim(1))
c                                    Time Kernel 12
      DO 12 m = 1,lo
      DO 12 k = 1,n
   12     X(k)= X(k+1) - X(k)
c
      cumtim(1)= 0.0d0
         TIM(j)= SECOND( cumtim(1)) - t0 - tsecov
   53 continue
c                                    Compute Dynamic Clock Error
c
          CALL  STATS( TUM, TIM, nj)
         rterr= 100.0*( TUM(2)/( TUM(1) + fuzz ))
            IF( TUM(1).LE. 0.00d0)  rterr= 100.00d0
c
c                                    Compute Digital Clock Error
c
          CALL  TDIGIT( SIG(i), nzd, TUM(4))
C
        TAV(i)= TUM(1)
        TMX(i)= TUM(4)
        TER(i)= rterr
        LEN(i)= lo
      IF( i.GT.ilimit .AND. ( TUM(1).LT.fuzz )) THEN
      WRITE(  *,146)  lo, TUM(1)
  146 FORMAT('VERIFY:',I12,' Repetitions.  Bad Timer=',E14.5,' sec.')
      ENDIF
      IF( i.LE.8 .OR.  ( TUM(1).LT.ttest .AND. i.LT.ntmp )) GO TO 10
            nn= i
C
C****************************************************************************
C     Compute Multiple-Pass Loop Counters MULTI and Loops2
C     Such that:  each Kernel is run at least 100 ticks of Cpu-timer.
C****************************************************************************
C
          i2= 2
       MULTI= 1
       mucho= 1
        CALL  SIZES(12)
      loop12= IPASS(12,2) * MUL(2)
C
      MULTI= INT( (REAL(lo)/(REAL(loop12)+fuzz))*(ttest/(TUM(1)+fuzz)))
      mucho= MULTI
C
C     If timing errors are too large, you must increase MULTI...
C     When MULTI.GE.100 each kernel is executed over a million times
C     and the time used to re-intialize overstored input variables
C     is negligible.  Thus each kernel may be run arbitrarily many times
C     (MULTI >> 100) without overflow and produce verifiable checksums.
c
C     Each kernel's results are automatically checksummed for  MULTI :=
c
C     MULTI=   1      clock resolution << 0.01 SEC,  or Cpu << 1 Mflops
C     MULTI=  10      clock resolution << 0.01 SEC,  or Cpu <  2 Mflops
C     MULTI=  50      clock resolution <= 0.01 SEC,  or Cpu <  2 Mflops
C     MULTI= 100      clock resolution <= 0.01 SEC,  or Cpu <  5 Mflops
C     MULTI= 200      clock resolution <= 0.01 SEC,  or Cpu < 10 Mflops
C
c     MULTI=   1
c     MULTI=  10
c     MULTI=  50
c     MULTI= 100
c     MULTI= 200
C
                 mpy= 1
              Loops2= 1
              mpylim= Loops2
          IF( MULTI.LE.  1 ) THEN
              MULTI =    1
      ELSEIF( MULTI.LE. 10 ) THEN
              MULTI =   10
      ELSEIF( MULTI.LE. 50 ) THEN
              MULTI =   50
      ELSEIF( MULTI.LE.100 ) THEN
              MULTI =  100
      ELSE
              Loops2= (MULTI + 50)/100
              mpylim= Loops2
              MULTI =  100
      ENDIF
C
C
         mucho= MULTI
        loops0= loop12 * MULTI * Loops2
        repeat= REAL(    MULTI * Loops2 )
            IF( Loop.EQ.1 ) repeat= 1.00d0/( REAL( loop12) + fuzz)
C
C****************************************************************************
C     Estimate Timing Error By Comparing Time Of Each Run With Longest Run
C****************************************************************************
C
             m= 0
           tnn= ( TAV(nn) + 2.00d0* TAV(nn-1))* 0.500d0
          fuzz= 1.0d-12
            IF( tnn.LT.fuzz)  tnn= fuzz
      DO 69  i= 1,nn
         rterr= TER(i)
            lo= LEN(i)
c                                    Compute Relative Clock Error
c
            rt= 0.0d0
            IF( LEN(i).GE. 0)     rt= LEN(nn)/LEN(i)
         rperr= 100.00d0
            IF( tnn.GT.fuzz) rperr= 100.00d0*(ABS( tnn - rt*TAV(i))/tnn)
         WRITE( iou,64) lo, TMX(i), SIG(i),rterr, rperr
   64   FORMAT(6X,I7,E12.4,F11.2,'%',F10.2,'%',F10.2,'%')
c
c                                    Find loops0 Size Used
c
            IF( (loops0.GE.lo) .AND. (loops0.LE.2*lo))  THEN
                     m= lo
                WRITE( iou,66)  loops0, repeat
                WRITE(   *,66)  loops0, repeat
                IF( rterr .GT. 10.00d0)  THEN
                  WRITE( iou, 67)
                  WRITE( iou, 68)
                  WRITE(   *, 67)
                  WRITE(   *, 68)
                ENDIF
   66 FORMAT(7X,i6,7X,'Repetition Count = MULTI * Loops2 = ',F12.3)
   67 FORMAT(34X,'VERIFY: POOR TIMING OR ERROR. NEED LONGER RUN  ')
   68 FORMAT(34X,'INCREASE:   MULTI  IN SUBR. VERIFY     ')
            ENDIF
C
   69 continue
            IF( m.LE.0 )  THEN
                WRITE( iou,66)  loops0, repeat
                WRITE(   *,66)  loops0, repeat
            ENDIF
                WRITE( iou,49)
C
C
      WRITE(   *,991)
      WRITE(   *,992)
  991 FORMAT(/,16X,'    (C) Copyright 1983 the Regents of the     ')
  992 FORMAT(  16X,'University of California. All Rights Reserved.',/)
C
C****************************************************************************
C     Clock Calibration Test of Internal Cpu-timer SECOND;
C           Verify 10 Internal SECOND Intervals using External Stopwatch
C****************************************************************************
C
C
  106 FORMAT(//,' CLOCK CALIBRATION TEST OF INTERNAL CPU-TIMER: SECOND')
  107 FORMAT(' MONOPROCESS THIS TEST, STANDALONE, NO TIMESHARING.')
  108 FORMAT(' VERIFY TIMED INTERVALS SHOWN BELOW USING EXTERNAL CLOCK')
  109 FORMAT(' START YOUR STOPWATCH NOW !')
  113 FORMAT(/,11X,'Verify  T or DT  observe external clock(sec):',/)
  114 FORMAT('           -------     -------      ------      -----')
  115 FORMAT('           Total T ?   Delta T ?    Mflops ?    Flops')
  119 FORMAT(4X,I2,3F12.2,2E15.5)
  120 FORMAT(' END CALIBRATION TEST.',/)
          WRITE( iou,106)
          WRITE( iou,107)
          WRITE( iou,108)
          WRITE( iou,109)
          WRITE( iou,113)
          WRITE( iou,114)
          WRITE( iou,115)
          WRITE( iou,114)
          WRITE(   *,106)
          WRITE(   *,107)
          WRITE(   *,108)
          WRITE(   *,109)
          WRITE(   *,113)
          WRITE(   *,114)
          WRITE(   *,115)
          WRITE(   *,114)
C
           task= 10.00d0
         passes= REAL(lo) * ( task/( tnn + fuzz))
         loiter= INT( passes )
          flops= 0.00d0
      cumtim(1)= 0.0d0
             t1= SECOND( cumtim(1))
             t2= 0.00d0
C
      DO 86   j= 1,4
              n= 100
             t0= t1
c                                    Time Kernel 12
      DO 82  m = 1,loiter
      DO 82  k = 1,n
   82      X(k)= X(k+1) - X(k)
c
      cumtim(1)= 0.0d0
             t1= SECOND( cumtim(1))
             td= t1 - t0 -tsecov
             t2= t2 + td
          flops= flops + passes*REAL(n)
         ratemf= ( 1.00d-6 * flops )/( t2 + fuzz )
          WRITE(   *,119)  j, t2, td, ratemf, flops
          WRITE( iou,119)  j, t2, td, ratemf, flops
   86 continue
          WRITE( iou,114)
          WRITE( iou,120)
          WRITE(   *,114)
          WRITE(   *,120)
C
C     CALL TRACK ('VERIFY  ')
      RETURN
      END
C
C***********************************************
      SUBROUTINE WATCH( mode)
C***********************************************
C
C  WATCH is called at every subroutine entry and exit point by TRACE .
C  COMMON variables may be tested continually during execution(watched)
C  for known error conditions so the occurance of the error is localized.
C  WATCH may be used for programmable data-breakpoints to aid debugging.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
c     parameter( ntests=  1, krs1= 24 + 1 )
      parameter( ntests= 14, krs1= 24 + 1 )
C
      CHARACTER  name*8, ISTACK*8
      COMMON /DEBUG/     ISTACK(20)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
C
      COMMON /ALPHA/ mk,ik,im,ml,il,Mruns,Nruns,jr,iovec,NPFS(8,3,47)
      COMMON /TAU/   tclock, tsecov, testov, cumtim(4)
      COMMON /BETA / tic, TIMES(8,3,47), SEE(5,3,8,3),
     1              TERRS(8,3,47), CSUMS(8,3,47),
     2              FOPN(8,3,47), DOS(8,3,47)
C
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), ticks,
     1                FR(9), TERR1(47), SUMW(7), START,
     2              SKALE(47), BIAS(47), WS(95), TOTAL(47), FLOPN(47),
     3                IQ(7), NPF, NPFS1(47)
C
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
      DIMENSION  IE(20)
c     LOGICAL BOUNDS
c     BOUNDS(A,X,B,E)= ((((A)*(1.-E)).LE.(X)).AND.((X).LE.((B)*(1.+E))))
C
C                                       Debug Trace Info
                       name= 'watch'
c     IF( made.EQ.1 )  name= ' ENTRY  '
c     IF( made.EQ.2 )  name= ' RETURN '
c     WRITE(*,101) inseq, name, ISTACK(1)
c 101 FORMAT(1X,I6,5X,A ,1X,A )
C
C                                       Domain Tests of Critical Variables
      DO 1 k= 1,ntests
    1  IE(k)= 0
      IF(    testov  .NE. ticks      ) IE(1)= 1
      IF(    tsecov  .NE. tic        ) IE(2)= 2
      IF( inseq.LE.0 .OR. inseq.NE.isave .OR. inseq.GT.99999) IE(3)= 3
      IF( Nruns.LT.1 .OR. Nruns.GT.8 ) IE(4)= 4
      IF(    il.LT.1 .OR. il.GT.3    ) IE(5)= 5
      IF(    mk.LT.1 .OR. mk.GT.24   ) IE(6)= 6
      IF(    ik.LT.0 .OR. ik.GT.krs1  ) IE(7)= 7
      IF(    jr.LT.1 .OR. jr.GT.8    ) IE(8)= 8
      IF(    Loops2  .LT. 1          ) IE(9)= 9
      IF(    Loops2  .NE. mpylim     ) IE(10)= 10
      IF(    MULTI   .LT. 1          ) IE(11)= 11
      IF(    MULTI   .NE. mucho      ) IE(12)= 12
      IF(    Loop    .LT. 1          ) IE(13)= 13
      IF(    Loop    .NE. LP         ) IE(14)= 14
C
C                        Insert your debug data tests here
c     IF( BOUNDS( 1.7669e+5,CSUMS(jr,1,8),1.7669e+5,1.0e-3)) IE(15)= 15
C
      ierr= 0
      DO 2 k= 1,ntests
    2 ierr= ierr + IE(k)
          IF( ierr.NE.0 )   THEN
              io= ABS( ion)
              IF( io.LE.0 .OR. io.GT.10 ) io=6
                   k1=0
                   k2=0
              WRITE(  *,111)
              WRITE(  *,112) (    k , k= 1,ntests )
              WRITE(  *,112) ( IE(k), k= 1,ntests )
              WRITE(  *,112) k1,k2,inseq,Nruns,il,mk,ik,jr,
     .                       Loops2,mpylim,MULTI,mucho,Loop,LP
              WRITE( io,111)
              WRITE( io,112) (    k , k= 1,ntests )
              WRITE( io,112) ( IE(k), k= 1,ntests )
              WRITE( io,112) k1,k2,inseq,Nruns,il,mk,ik,jr,
     .                       Loops2,mpylim,MULTI,mucho,Loop,LP
  111         FORMAT(/,' WATCH: STORAGE FAULT DETECTED.  IE=')
  112         FORMAT(1X,15I5)
              CALL WHERE( mode)
          ENDIF
      RETURN
      END
C
C***********************************************
      SUBROUTINE WHERE( mode)
C***********************************************
C
C  Prints Subroutine names in the active linkage chain for debugging.
C
C***********************************************
cANSI IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
cIBM  IMPLICIT  REAL*8           (A-H,O-Z)
C
      parameter( insert= 2 )
      COMMON /SPACES/ ion,j5,k2,k3,MULTI,laps,Loop,m,kr,LP,n13h,ibuf,nx,
     1 L,npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,
     2 last,idebug,mpy,Loops2,mucho,mpylim, intbuf(16)
C
      CHARACTER  name*8, ISTACK*8
      COMMON /DEBUG/     ISTACK(20)
      COMMON /ORDER/ inseq, match, NSTACK(20), isave, iret
C
      made= MOD( mode,10)
                       name= 'internal'
      IF( made.EQ.1 )  name= ' ENTRY  '
      IF( made.EQ.2 )  name= ' RETURN '
      io= ABS( ion)
      IF( io.LE.0 .OR. io.GT.10 ) io=6
C
      IF( mode.EQ.12 ) THEN
           WRITE(  *,112)  ISTACK(20), ISTACK(1)
           WRITE( io,112)  ISTACK(20), ISTACK(1)
  112      FORMAT(2X,'WHERE: SEQ.ERROR.  RETURN ',A  ,'.NE. CALL ',A  )
      ENDIF
C
CPFM  IF( mode.EQ.20 ) THEN
CPFM       WRITE( io,9)
CPFM9      FORMAT(2X,'WHERE: INIPFM FAILED.' )
CPFM  ENDIF
      WRITE(  *,110)  name, ISTACK(1)
      WRITE( io,110)  name, ISTACK(1)
  110 FORMAT(/,' WHERE:  ERROR detected at ',A  ,' point in: ',A  )
C
      IF( made.EQ.1 .OR. made.EQ.2 )  THEN
C                    Pushdown stack of subroutine names and call nrs.
          DO  1  k = 12,insert+1,-1
          NSTACK(k)= NSTACK(k-insert)
          ISTACK(k)= ISTACK(k-insert)
    1     continue
C
          NSTACK(1)= inseq
          ISTACK(1)= 'WATCH   '
          NSTACK(2)= inseq
          ISTACK(2)= 'TRACE   '
          IF( made.EQ.2 )  ISTACK(2)= 'TRACK   '
      ENDIF
      WRITE(  *,111)
      WRITE(  *,114)
      WRITE(  *,113)
      WRITE(  *,114)
      WRITE(  *,118) ( ISTACK(k), NSTACK(k), k= 1,12 )
C
      WRITE( io,111)
      WRITE( io,114)
      WRITE( io,113)
      WRITE( io,114)
      WRITE( io,118) ( ISTACK(k), NSTACK(k), k= 1,12 )
  111 FORMAT(/,' ACTIVE SUBROUTINE LINKAGE CHAIN:')
  114 FORMAT('          ----           -----------')
  113 FORMAT('          name           call number')
  118 FORMAT(10X,A  ,4X,I8)
C
      DO 222 k= 1,200
      WRITE( io,221)
  221 FORMAT(/,' ********* TERMINAL ERROR; FLUSH I/O BUFFER **********')
  222 continue
C      PAUSE
      STOP
c     RETURN
      END
