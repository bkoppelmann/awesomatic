      
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /SYSID/
      COMMON /SYSID/ KOMPUT, KONTRL, KOMPIL, KALEND, IDENTY
      CHARACTER   KOMPUT*24, KONTRL*24, KOMPIL*24, KALEND*24, IDENTY*24
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /ORDER/
      COMMON /ORDER/ INSEQ, MATCH, NSTACK(20), ISAVE, IRET
      INTEGER   INSEQ, MATCH, NSTACK, ISAVE, IRET
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: NTIMES = 18
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(141) :: ID, LSPAN
      INTEGER :: IOU, NT, K, I, J
      REAL, DIMENSION(141) :: FLOPS, TR, RATES, WG, OSUM, TERR
      REAL, DIMENSION(6) :: TK
      REAL :: TI, TJ, TOCK, TOTJOB
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: SECOND, TICK
      EXTERNAL VERIFY
!-----------------------------------------------
!LOX  REAL*8 SECOND
!LLNL      CALL  DROPFILE (   '+MFLOPS' )
!                        Job start Cpu time
      CUMTIM(1) = 0.0D0
      TI = SECOND(CUMTIM(1))
!
!                                            Define your computer system:
      KOMPUT = 'Sun Sparc 2 (32 Mbyte)  '
      KONTRL = 'Sun OS 4.1.1            '
      KOMPIL = 'Various F90 comilers    '
      KALEND = 'Late 1992               '
      IDENTY = 'John K. Prentice, QCA   '
!
!                        Initialize variables and Open Files
      CALL INDATA (TK, IOU)
!                        Record name in active linkage chain in COMMON /DEBUG/
      CALL TRACE (' MAIN.  ')
!
!                        Verify Sufficient Loop Size Versus Cpu Clock Accuracy
      CALL VERIFY (IOU)
      TJ = SECOND(CUMTIM(1))
      NT = NTIMES
!                        Define control limits:  Nruns(runs), Loop(time)
      CALL SIZES (-1)
!
!                        Run test Mruns times Cpu-limited; I/O is deferred:
      DO K = 1, MRUNS
           I = K
           JR = MOD(I - 1,7) + 1
           CALL IQRAN0 (256)
!                        Run test using one of 3 sets of DO-Loop spans:
!                        Set iou Negative to supress all I/O during Cpu
           DO J = IM, ML
                IL = J
                TOCK = TICK((-IOU),NT)
!
                CALL KERNEL (TK)
           END DO
           CALL TRIAL (IOU, I, TI, TJ)
      END DO
!
!                        Report timing errors, Mflops statistics:
      DO J = IM, ML
           IL = J
           CALL RESULT(IOU,FLOPS,TR,RATES,LSPAN,WG,OSUM,TERR,ID)
!
!                Report  Mflops for Vector Cpus( short, medium, long vectors):
!
           IOVEC = 0
           IF (IOVEC == 1) CALL REPORT (IOU, MK, MK, FLOPS, TR, RATES,  &
     &          LSPAN, WG, OSUM, ID)
      END DO
!                Report  Mflops SUMMARY Statistics: for Official Quotations
!
      CALL REPORT (IOU, 3*MK, MK, FLOPS, TR, RATES, LSPAN, WG, OSUM, ID)
!
      CUMTIM(1) = 0.0D0
      TOTJOB = SECOND(CUMTIM(1)) - TI - TSECOV
      WRITE (IOU, 9) INSEQ, TOTJOB, TK(1), TK(2)
      WRITE (*, 9) INSEQ, TOTJOB, TK(1), TK(2)
    9    FORMAT( '1',//,' Version: 22/DEC/86  mf523 ',2X,I12,/,1P,      &
     &                  ' CHECK FOR CLOCK CALIBRATION ONLY: ',/,        &
     &                  ' Total Job    Cpu Time =  ',e14.5, ' Sec.',/,  &
     &                  ' Total 24 Kernels Time =  ',e14.5, ' Sec.',/,  &
     &                  ' Total 24 Kernels Flops=  ',e14.5, ' Flops')
!
!                        Optional Cpu Clock Calibration Test of SECOND:
!          CALL  CALIBR
      STOP 
      END 
      BLOCK DATA
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: ND = 11
      INTEGER, PARAMETER :: NT = 4
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /TAGS/
      COMMON /TAGS/ NAMES(ND,NT)
      CHARACTER   NAMES*8
!...  /RATS/
      COMMON /RATS/ RATED(ND,NT)
      REAL   RATED
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!...  /ORDER/
      COMMON /ORDER/ INSEQ, MATCH, NSTACK(20), ISAVE, IRET
      INTEGER   INSEQ, MATCH, NSTACK, ISAVE, IRET
!...  /PROOF/
      COMMON /PROOF/ SUMS(24,3,8)
      DOUBLE PRECISION ::SUMS
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: NSYS = 5
      INTEGER, PARAMETER :: NS = NSYS + 1
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: J1, J2, J3, J4, J6, J7, J8, J9, J10, J11, J12, J13, J14&
     &     , J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, J26 &
     &     , J27, J28, J29, J30, J31, J32, J33, J34, J35, J36, J37, J38 &
     &     , J39, J40, J41
!-----------------------------------------------
      DATA (ISPAN(J1,1),J1=1,47)/1001, 101, 1001, 1001, 1001, 64, 995,  &
     &     100, 101, 101, 1001, 1000, 64, 1001, 101, 75, 101, 100, 101, &
     &     1000, 101, 101, 100, 1001, 23*0/
      DATA (ISPAN(J2,2),J2=1,47)/101, 101, 101, 101, 101, 32, 101, 100, &
     &     101, 101, 101, 100, 32, 101, 101, 40, 101, 100, 101, 100, 50 &
     &     , 101, 100, 101, 23*0/
      DATA (ISPAN(J3,3),J3=1,47)/27, 15, 27, 27, 27, 8, 21, 14, 15, 15, &
     &     27, 26, 8, 27, 15, 15, 15, 14, 15, 26, 20, 15, 14, 27, 23*0/
      DATA (IPASS(J4,1),J4=1,47)/7, 67, 9, 14, 10, 3, 4, 10, 36, 34, 11 &
     &     , 12, 36, 2, 1, 25, 35, 2, 39, 1, 1, 11, 8, 5, 23*0/
      DATA (IPASS(J6,2),J6=1,47)/40, 40, 53, 70, 55, 7, 22, 6, 21, 19,  &
     &     64, 68, 41, 10, 1, 27, 20, 1, 23, 8, 1, 7, 5, 31, 23*0/
      DATA (IPASS(J7,3),J7=1,47)/28, 46, 37, 38, 40, 21, 20, 9, 26, 25, &
     &     46, 48, 31, 8, 1, 14, 26, 2, 28, 7, 1, 8, 7, 23, 23*0/
      DATA (MUL(J8),J8=1,3)/1, 2, 8/
      DATA (WTP(J9),J9=1,3)/1.0, 2.0, 1.0/
      DATA (FLOPN(J10),J10=1,47)/5., 4., 2., 2., 2., 2., 16., 36., 17., &
     &     9., 1., 1., 7., 11., 33., 10., 9., 44., 6., 26., 2., 17., 11.&
     &     , 1., 23*0.0/
      DATA (WT(J11),J11=1,47)/1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   &
     &     1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  &
     &     1.0, 1.0, 1.0, 1.0, 23*0.0/
      DATA (SKALE(J12),J12=1,47)/0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0 &
     &     , 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0 &
     &     , 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0 &
     &     , 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0, 0.100D+0 &
     &     , 0.100D+0, 0.100D+0, 23*0.000D+0/
      DATA (BIAS(J13),J13=1,47)/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
     &     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  &
     &     0.0, 0.0, 0.0, 0.0, 23*0.0/
      DATA(FR(J14),J14=1,9)/0.0,0.2,0.4,0.6,0.7,0.8,0.9,0.95,1.0/
      DATA (SUMW(J15),J15=1,7)/1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5/
      DATA (IQ(J16),J16=1,7)/1, 2, 1, 2, 1, 2, 1/
      DATA (NAMES(1,J17),J17=1,3)/'NEC     ', 'SX-3/14 ', 'F77v.012'/
      DATA (RATED(1,J18),J18=1,4)/311.82, 95.59, 38.73, 499.78/
      DATA (NAMES(2,J19),J19=1,3)/'CRAY    ', 'YMP/1   ', 'CFT771.2'/
      DATA (RATED(2,J20),J20=1,4)/78.23, 36.63, 17.66, 86.75/
      DATA (NAMES(3,J21),J21=1,3)/'HP      ', '9000/730', 'f77 8.05'/
      DATA (RATED(3,J22),J22=1,4)/18.31, 15.72, 13.28, 9.68/
      DATA (NAMES(4,J23),J23=1,3)/'IBM     ', '6000/540', 'XL v0.90'/
      DATA (RATED(4,J24),J24=1,4)/14.17, 10.73, 7.45, 9.59/
      DATA (NAMES(5,J25),J25=1,3)/'COMPAQ  ', 'i486/25 ', '        '/
      DATA (RATED(5,J26),J26=1,4)/1.15, 1.05, 0.92, 0.48/
      DATA START/0.0/
      DATA NPF/0/
      DATA IBUF/0/
      DATA MATCH/0/
      DATA MULTI/200/
      DATA LAPS/1/
      DATA NPASS/0/
      DATA NFAIL/0/
      DATA LAST/-1/
      DATA (SUMS(J27,1,5),J27=1,24)/5.114652693224671D+04,              &
     &     1.539721811668385D+03, 1.000742883066363D+01,                &
     &     5.999250595473891D-01, 4.548871642387267D+03,                &
     &     4.375116344729986D+03, 6.104251075174761D+04,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.342910972650109D+07,                &
     &     2.907141294167248D-05, 1.202533961842803D+11,                &
     &     3.165553044000334D+09, 3.943816690352042D+04,                &
     &     5.650760000000000D+05, 1.114641772902486D+03,                &
     &     1.015727037502300D+05, 5.421816960147207D+02,                &
     &     3.040644339351239D+07, 1.597308280710199D+08,                &
     &     2.938604376566697D+02, 3.549900501563623D+04,                &
     &     5.000000000000000D+02/
      DATA (SUMS(J28,2,5),J28=1,24)/5.253344778937972D+02,              &
     &     1.539721811668385D+03, 1.009741436578952D+00,                &
     &     5.999250595473891D-01, 4.589031939600982D+01,                &
     &     8.631675645333210D+01, 6.345586315784055D+02,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.433560407475758D+04,                &
     &     7.127569130821465D-06, 9.816387810944345D+10,                &
     &     3.039983465145393D+07, 3.943816690352042D+04,                &
     &     6.480410000000000D+05, 1.114641772902486D+03,                &
     &     1.015727037502300D+05, 5.421816960147207D+02,                &
     &     3.126205178815431D+04, 7.824524877232093D+07,                &
     &     2.938604376566697D+02, 3.549900501563623D+04,                &
     &     5.000000000000000D+01/
      DATA (SUMS(J29,3,5),J29=1,24)/3.855104502494961D+01,              &
     &     3.953296986903059D+01, 2.699309089320672D-01,                &
     &     5.999250595473891D-01, 3.182615248447483D+00,                &
     &     1.120309393467088D+00, 2.845720217644024D+01,                &
     &     2.960543667875003D+03, 2.623968460874250D+03,                &
     &     1.651291227698265D+03, 6.551161335845770D+02,                &
     &     1.943435981130448D-06, 3.847124199949426D+10,                &
     &     2.923540598672011D+06, 1.108997288134785D+03,                &
     &     5.152160000000000D+05, 2.947368618589360D+01,                &
     &     9.700646212337040D+02, 1.268230698051003D+01,                &
     &     5.987713249475302D+02, 5.009945671204667D+07,                &
     &     6.109968728263972D+00, 4.850340602749970D+02,                &
     &     1.300000000000000D+01/
      DATA (SUMS(J30,1,4),J30=1,24)/5.114652693224671D+04,              &
     &     1.539721811668385D+03, 1.000742883066363D+01,                &
     &     5.999250595473891D-01, 4.548871642387267D+03,                &
     &     4.375116344729986D+03, 6.104251075174761D+04,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.342910972650109D+07,                &
     &     2.907141294167248D-05, 4.958101723583047D+10,                &
     &     3.165278275112100D+09, 3.943816690352042D+04,                &
     &     2.825760000000000D+05, 1.114641772902486D+03,                &
     &     7.507386432940455D+04, 5.421816960147207D+02,                &
     &     3.040644339351239D+07, 8.002484742089500D+07,                &
     &     2.938604376566697D+02, 3.549900501563623D+04,                &
     &     5.000000000000000D+02/
      DATA (SUMS(J31,2,4),J31=1,24)/5.253344778937972D+02,              &
     &     1.539721811668385D+03, 1.009741436578952D+00,                &
     &     5.999250595473891D-01, 4.589031939600982D+01,                &
     &     8.631675645333210D+01, 6.345586315784055D+02,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.433560407475758D+04,                &
     &     7.127569130821465D-06, 3.542728632259964D+10,                &
     &     3.015943681556781D+07, 3.943816690352042D+04,                &
     &     3.240410000000000D+05, 1.114641772902486D+03,                &
     &     7.507386432940455D+04, 5.421816960147207D+02,                &
     &     3.126205178815431D+04, 3.916171317449981D+07,                &
     &     2.938604376566697D+02, 3.549900501563623D+04,                &
     &     5.000000000000000D+01/
      DATA (SUMS(J32,3,4),J32=1,24)/3.855104502494961D+01,              &
     &     3.953296986903059D+01, 2.699309089320672D-01,                &
     &     5.999250595473891D-01, 3.182615248447483D+00,                &
     &     1.120309393467088D+00, 2.845720217644024D+01,                &
     &     2.960543667875003D+03, 2.623968460874250D+03,                &
     &     1.651291227698265D+03, 6.551161335845770D+02,                &
     &     1.943435981130448D-06, 1.161063924078402D+10,                &
     &     2.609194549277411D+06, 1.108997288134785D+03,                &
     &     2.576160000000000D+05, 2.947368618589360D+01,                &
     &     9.700646212337040D+02, 1.268230698051003D+01,                &
     &     5.987713249475302D+02, 2.505599006414913D+07,                &
     &     6.109968728263972D+00, 4.850340602749970D+02,                &
     &     1.300000000000000D+01/
      DATA (SUMS(J33,1,3),J33=1,24)/5.114652693224671D+04,              &
     &     1.539721811668385D+03, 1.000742883066363D+01,                &
     &     5.999250595473891D-01, 4.548871642387267D+03,                &
     &     4.375116344729986D+03, 6.104251075174761D+04,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.342910972650109D+07,                &
     &     2.907141294167248D-05, 2.217514090251080D+10,                &
     &     3.165140890667983D+09, 3.943816690352042D+04,                &
     &     1.413260000000000D+05, 1.114641772902486D+03,                &
     &     6.203834985242972D+04, 5.421816960147207D+02,                &
     &     3.040644339351239D+07, 4.017185709583275D+07,                &
     &     2.938604376566697D+02, 3.549900501563623D+04,                &
     &     5.000000000000000D+02/
      DATA (SUMS(J34,2,3),J34=1,24)/5.253344778937972D+02,              &
     &     1.539721811668385D+03, 1.009741436578952D+00,                &
     &     5.999250595473891D-01, 4.589031939600982D+01,                &
     &     8.631675645333210D+01, 6.345586315784055D+02,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.433560407475758D+04,                &
     &     7.127569130821465D-06, 1.430504282675192D+10,                &
     &     3.003923789762475D+07, 3.943816690352042D+04,                &
     &     1.620410000000000D+05, 1.114641772902486D+03,                &
     &     6.203834985242972D+04, 5.421816960147207D+02,                &
     &     3.126205178815431D+04, 1.961994537558922D+07,                &
     &     2.938604376566697D+02, 3.549900501563623D+04,                &
     &     5.000000000000000D+01/
      DATA (SUMS(J35,3,3),J35=1,24)/3.855104502494961D+01,              &
     &     3.953296986903059D+01, 2.699309089320672D-01,                &
     &     5.999250595473891D-01, 3.182615248447483D+00,                &
     &     1.120309393467088D+00, 2.845720217644024D+01,                &
     &     2.960543667875003D+03, 2.623968460874250D+03,                &
     &     1.651291227698265D+03, 6.551161335845770D+02,                &
     &     1.943435981130448D-06, 3.899370197966012D+09,                &
     &     2.452021524580127D+06, 1.108997288134785D+03,                &
     &     1.288160000000000D+05, 2.947368618589360D+01,                &
     &     9.700646212337040D+02, 1.268230698051003D+01,                &
     &     5.987713249475302D+02, 1.253425674020030D+07,                &
     &     6.109968728263972D+00, 4.850340602749970D+02,                &
     &     1.300000000000000D+01/
      DATA (SUMS(J36,1,2),J36=1,24)/5.114652693224671D+04,              &
     &     1.539721811668385D+03, 1.000742883066363D+01,                &
     &     5.999250595473891D-01, 4.548871642387267D+03,                &
     &     4.375116344729986D+03, 6.104251075174761D+04,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.342910972650109D+07,                &
     &     2.907141294167248D-05, 4.057110454105199D+09,                &
     &     3.165030983112689D+09, 3.943816690352042D+04,                &
     &     2.832600000000000D+04, 1.114641772902486D+03,                &
     &     5.165625410754861D+04, 5.421816960147207D+02,                &
     &     3.040644339351239D+07, 8.289464835782872D+06,                &
     &     2.938604376566697D+02, 3.549834542443621D+04,                &
     &     5.000000000000000D+02/
      DATA (SUMS(J37,2,2),J37=1,24)/5.253344778937972D+02,              &
     &     1.539721811668385D+03, 1.009741436578952D+00,                &
     &     5.999250595473891D-01, 4.589031939600982D+01,                &
     &     8.631675645333210D+01, 6.345586315784055D+02,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.433560407475758D+04,                &
     &     7.127569130821465D-06, 2.325318944820753D+09,                &
     &     2.994307876327030D+07, 3.943816690352042D+04,                &
     &     3.244100000000000D+04, 1.114641772902486D+03,                &
     &     5.165625410754861D+04, 5.421816960147207D+02,                &
     &     3.126205178815431D+04, 3.986531136460764D+06,                &
     &     2.938604376566697D+02, 3.549894609774404D+04,                &
     &     5.000000000000000D+01/
      DATA (SUMS(J38,3,2),J38=1,24)/3.855104502494961D+01,              &
     &     3.953296986903059D+01, 2.699309089320672D-01,                &
     &     5.999250595473891D-01, 3.182615248447483D+00,                &
     &     1.120309393467088D+00, 2.845720217644024D+01,                &
     &     2.960543667875003D+03, 2.623968460874250D+03,                &
     &     1.651291227698265D+03, 6.551161335845770D+02,                &
     &     1.943435981130448D-06, 4.755211251524082D+08,                &
     &     2.326283104822299D+06, 1.108997288134785D+03,                &
     &     2.577600000000000D+04, 2.947368618589360D+01,                &
     &     9.700646212337040D+02, 1.268230698051003D+01,                &
     &     5.987713249475302D+02, 2.516870081041265D+06,                &
     &     6.109968728263972D+00, 4.850340602749970D+02,                &
     &     1.300000000000000D+01/
      DATA (SUMS(J39,1,1),J39=1,24)/5.114652693224671D+04,              &
     &     1.539721811668385D+03, 1.000742883066363D+01,                &
     &     5.999250595473891D-01, 4.548871642387267D+03,                &
     &     4.375116344729986D+03, 6.104251075174761D+04,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.342910972650109D+07,                &
     &     2.907141294167248D-05, 4.468741170140841D+08,                &
     &     3.165006253912748D+09, 3.943816690352042D+04,                &
     &     2.901000000000000D+03, 1.227055736845479D+03,                &
     &     4.932243865816480D+04, 5.421816960147207D+02,                &
     &     3.040644339351239D+07, 1.115926577271652D+06,                &
     &     2.938604376566697D+02, 3.138872788135057D+04,                &
     &     5.000000000000000D+02/
      DATA (SUMS(J40,2,1),J40=1,24)/5.253344778937972D+02,              &
     &     1.539721811668385D+03, 1.009741436578952D+00,                &
     &     5.999250595473891D-01, 4.589031939600982D+01,                &
     &     8.631675645333210D+01, 6.345586315784055D+02,                &
     &     1.501268005625798D+05, 1.189443609974981D+05,                &
     &     7.310369784325296D+04, 3.433560407475758D+04,                &
     &     7.127569130821465D-06, 2.323352389500009D+08,                &
     &     2.992144295804055D+07, 3.943816690352042D+04,                &
     &     3.281000000000000D+03, 1.114641772902486D+03,                &
     &     4.932243865816480D+04, 5.421816960147207D+02,                &
     &     3.126205178815431D+04, 4.690129326568575D+05,                &
     &     2.938604376566697D+02, 3.228104575530876D+04,                &
     &     5.000000000000000D+01/
      DATA (SUMS(J41,3,1),J41=1,24)/3.855104502494961D+01,              &
     &     3.953296986903059D+01, 2.699309089320672D-01,                &
     &     5.999250595473891D-01, 3.182615248447483D+00,                &
     &     1.120309393467088D+00, 2.845720217644024D+01,                &
     &     2.960543667875003D+03, 2.623968460874250D+03,                &
     &     1.651291227698265D+03, 6.551161335845770D+02,                &
     &     1.943435981130448D-06, 4.143805389489125D+07,                &
     &     2.297991960376787D+06, 1.108997288134785D+03,                &
     &     2.592000000000000D+03, 2.947368618589360D+01,                &
     &     9.700646212337040D+02, 1.268230698051003D+01,                &
     &     5.987713249475302D+02, 2.629580827304779D+05,                &
     &     6.109968728263972D+00, 4.850340602749970D+02,                &
     &     1.300000000000000D+01/
!
!****************************************************************************
!
!     The following DP checksums are NOT used for the standard LFK
!     performance test but may be used to test Fortran compiler precision.
!
!     Checksums for Quadruple-Precision (IBM,DEC); or CRAY Double-Precision.
!     Quadruple precision checksums computed by Dr. D.S. Lindsay, HITACHI.
!     These Checksums were obtained with   MULTI= 10. (BLOCKDATA)
!     Change the numerical edit descriptor Q to D on CRAY systems.
!Qc
!Q      DATA  ( SUMS(i,1,1), i= 1,24 )  /
!Q     a 0.5114652693224705102247326Q+05, 0.5150345372943066022569677Q+03,
!Q     b 0.1000742883066623145122027Q+02, 0.5999250595474070357564935Q+00,
!Q     c 0.4548871642388544199267412Q+04, 0.5229095383954675635496207Q+13,
!Q     d 0.6104251075163778121943921Q+05, 0.1501268005627157186827043Q+06,
!Q     e 0.1189443609975085966254160Q+06, 0.7310369784325972183233686Q+05,
!Q     f 0.3342910972650530676553892Q+08, 0.2907141428639174056565229Q-04,
!Q     g 0.4057110454105263471505061Q+10, 0.2982036205992255154832180Q+10,
!Q     h 0.3943816690352311804312052Q+05, 0.2832600000000000000000000Q+05,
!Q     i 0.1114641772903091760464680Q+04, 0.5165625410757306606559174Q+05,
!Q     j 0.5421816960150398899460410Q+03, 0.3040644339317275409518862Q+08,
!Q     k 0.8289464835786202431495974Q+07, 0.2938604376567099667790619Q+03,
!Q     l 0.3549834542446150511553453Q+05, 0.5000000000000000000000000Q+03/
!Qc
!Q      DATA  ( SUMS(i,2,1), i= 1,24 )  /
!Q     a 0.5253344778938000681994399Q+03, 0.5150345372943066022569677Q+03,
!Q     b 0.1009741436579188086885138Q+01, 0.5999250595474070357564935Q+00,
!Q     c 0.4589031939602131581035992Q+02, 0.2693280957416549457193910Q+16,
!Q     d 0.6345586315772524401198340Q+03, 0.1501268005627157186827043Q+06,
!Q     e 0.1189443609975085966254160Q+06, 0.7310369784325972183233686Q+05,
!Q     f 0.3433560407476162346605343Q+05, 0.7127569144561925151361427Q-05,
!Q     g 0.2325318944820836005421577Q+10, 0.3045676741897511424188763Q+08,
!Q     h 0.3943816690352311804312052Q+05, 0.3244100000000000000000000Q+05,
!Q     i 0.1114641772903091760464680Q+04, 0.5165625410757306606559174Q+05,
!Q     j 0.5421816960150398899460410Q+03, 0.3126205178811007613028089Q+05,
!Q     k 0.3986531136462291709063170Q+07, 0.2938604376567099667790619Q+03,
!Q     l 0.3549894609776936556634240Q+05, 0.5000000000000000000000000Q+02/
!Qc
!Q      DATA  ( SUMS(i,3,1), i= 1,24 )  /
!Q     a 0.3855104502494983491740258Q+02, 0.1199847611437483513040755Q+02,
!Q     b 0.2699309089321296439173090Q+00, 0.5999250595474070357564935Q+00,
!Q     c 0.3182615248448271678796560Q+01, 0.8303480073326955433087865Q+12,
!Q     d 0.2845720217638848365786224Q+02, 0.2960543667877649943946702Q+04,
!Q     e 0.2623968460874419268457298Q+04, 0.1651291227698377392796690Q+04,
!Q     f 0.6551161335846537217862474Q+03, 0.1943435981776804808483341Q-05,
!Q     g 0.4755211251524563699634913Q+09, 0.2547733008933910800455698Q+07,
!Q     h 0.1108997288135066584075059Q+04, 0.2577600000000000000000000Q+05,
!Q     i 0.2947368618590713935189324Q+02, 0.9700646212341513210532085Q+03,
!Q     j 0.1268230698051747067958265Q+02, 0.5987713249471801461035250Q+03,
!Q     k 0.2516870081042209239664473Q+07, 0.6109968728264795136407718Q+01,
!Q     l 0.4850340602751675804605762Q+03, 0.1300000000000000000000000Q+02/
!Qc
      END 
!
!
!***************************************
      SUBROUTINE CALIBR
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: LIMITN = 101
      INTEGER, PARAMETER :: NDIM = LIMITN + 10
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: L, N, M, NFLOP, I, J, K
      REAL, DIMENSION(NDIM) :: X, Y
      REAL, DIMENSION(10) :: CUMTIM
      REAL :: T0, TOTALT, DELTAT, FLOPS, RN, T1, T2
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: SECOND
!-----------------------------------------------
!
!     CALL TRACE ('CALIBR  ')
      CUMTIM(1) = 0.0D0
      T0 = SECOND(CUMTIM(1))
!
      WRITE (*, 111)
       WRITE (*, 110)
       WRITE (*, 112)
       WRITE (*, 113)
       WRITE (*, 114)
       WRITE (*, 115)
       WRITE (*, 114)
  111    FORMAT(//,' CPU CLOCK CALIBRATION:  START STOPWATCH NOW !')
  110    FORMAT('           TESTS ACCURACY OF FUNCTION SECOND()')
  112    FORMAT('           Monoprocess this test, stand-alone, no TSS')
  113    FORMAT('           Verify  T or DT  observe external clock:',/)
  114    FORMAT('           -------     -------      ------      -----')
  115    FORMAT('           Total T ?   Delta T ?    Mflops ?    Flops')
  119    FORMAT(4X,I2,3F12.2,2E15.5)
!
       L = 0
      N = 0
      M = 200
      NFLOP = 0
      TOTALT = 0.00D0
      DELTAT = 0.00D0
      FLOPS = 0.00D0
      RN = 0.00D0
      T1 = 0.00D0
      T2 = 0.00D0
      CUMTIM(1) = 0.0D0
      T2 = SECOND(CUMTIM(1))
      IF (T2 > 1.00D04) GO TO 911
      IF (T2 >= 1.00D-8) THEN
!
   10      CONTINUE
           L = L + 1
           M = M + M
!
           X(1) = 0.0098000D0
           Y(1) = 0.0000010D0
           Y(2:LIMITN) = Y(1)
!                                  Compute LFK Kernel 11  m times
           DO J = 1, M
                DO K = 2, LIMITN
                     X(K) = X(K-1) + Y(K)
                END DO
                X(1) = X(LIMITN)
           END DO
!
           T1 = T2
           CUMTIM(1) = 0.0D0
           T2 = SECOND(CUMTIM(1))
!                                  IF elapsed time can be observed, Print Mark.
           TOTALT = T2 - T0
           DELTAT = T2 - T1
           NFLOP = NFLOP + (LIMITN - 1)*M
           IF (DELTAT>2.00D0 .OR. L>12) THEN
                N = N + 1
                RN = REAL(NFLOP)
                FLOPS = 1.00D-6*(REAL(NFLOP)/(TOTALT + 1.00D-9))
                WRITE (*, 119) L, TOTALT, DELTAT, FLOPS, RN, X(LIMITN)
           ENDIF
           IF (DELTAT<200.0D0 .OR. N<3) GO TO 10
           IF(N<=0)WRITE(*,119)L,TOTALT,DELTAT,FLOPS,RN,X(LIMITN)
           STOP 
      ENDIF
!
  911 CONTINUE
      WRITE (*, 61)
       WRITE (*, 62) TOTALT
      STOP 
!
   61 FORMAT(1X,'FATAL(CALIBR): cant measure time using func SECOND()')
   62 FORMAT(/,13X,'using SECOND():  totalt=',1E20.8,' ?')
!
      END SUBROUTINE CALIBR
!
!***********************************************
      SUBROUTINE INDEX
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!***********************************************
!       MODULE     PURPOSE
!       ------     -----------------------------------------------
!
!       CALIBR     cpu clock calibration tests accuracy of SECOND function
!
!       INDATA     initialize variables
!
!       IQRANF     computes a vector of pseudo-random indices
!       IQRAN0     define seed for new IQRANF sequence
!
!       KERNEL     executes 24 samples of Fortran computation
!
!       PFM        optional call to system hardware performance monitor
!
!       RELERR     relative error between  u,v  (0.,1.)
!
!       REPORT     prints timing results
!
!       RESULT     computes execution rates  into pushdown store
!
!       SECOND     cumulative CPU time for task in seconds (M.K.S. units)
!
!       SECOVT     measures the Overhead time for calling   SECOND
!
!       SENSIT     sensitivity analysis of harmonic mean to 49 workloads
!
!       SEQDIG     computes nr significant, equal digits in pairs of numbers
!
!       SIGNEL     generates a set of floating-point numbers near 1.0
!
!       SIMD       sensitivity analysis of harmonic mean to SISD/SIMD model
!
!       SIZES      test and set the loop controls before each kernel test
!
!       SORDID     simple sort
!
!       SPACE      sets memory pointers for array variables.  optional.
!
!       SPEDUP     computes Speed-ups: A circumspect method of comparison.
!
!       STATS      calculates unweighted statistics
!
!       STATW      calculates   weighted statistics
!
!       SUMO       check-sum with ordinal dependency
!
!       SUPPLY     initializes common blocks containing type real arrays.
!
!       TALLY      computes average and minimum Cpu timings and variances.
!
!       TDIGIT     counts lead digits followed by trailing zeroes
!
!       TEST       Repeats and times the execution of each kernel
!
!       TESTS      Checksums and initializes the data for each kernel test
!
!       TICK       measures timing overhead of subroutine test
!
!       TILE       computes  m-tile value and corresponding index
!
!       TRACE ,TRACK    push/pop caller's name and serial nr. in /DEBUG/
!
!       TRAP       checks that index-list values are in valid domain
!
!       TRIAL      validates checksums of current run for endurance trial
!
!       VALID      compresses valid timing results
!
!       VALUES     initializes special values
!
!       VERIFY     verifies sufficient Loop size versus cpu clock accuracy
!
!       WATCH      can continually test COMMON variables and localize bugs
!
!  ------------ -------- -------- -------- -------- -------- --------
!  ENTRY LEVELS:   1        2        3        4        5        6
!  ------------ -------- -------- -------- -------- -------- --------
!               MAIN.    SECOND
!                        INDATA
!                        VERIFY   SECOND
!                                 SIZES    IQRAN0
!                                 STATS    SQRT
!                                 TDIGIT   LOG10
!                        SIZES    IQRAN0
!
!                        TICK     TEST     TESTS    SECOND
!                                                   SIZES
!                                                   SUMO
!                                                   VALUES   SUPPLY   SIGNEL
!                                                            IQRANF   MOD
!                                          SECOND
!                                 VALID    TRAP              TRAP
!                                 STATS    SQRT
!                                 IQRANF   MOD
!                                          TRAP
!                        KERNEL   SPACE
!                                 SQRT
!                                 EXP
!                                 TEST     TESTS    SECOND
!                                                   SIZES
!                                                   SUMO
!                                                   VALUES   SUPPLY   SIGNEL
!                                                            IQRANF   MOD
!                                          SECOND
!                        TRIAL    SEQDIG   LOG10    TDIGIT
!                                 IQRAN0
!
!                        RESULT   TALLY    SIZES    IQRAN0   TRAP
!                                          PAGE
!                                          STATS    SQRT
!
!                                 SEQDIG   LOG10    TDIGIT
!
!                        REPORT   VALID    TRAP
!                                 MOD
!                                 STATW    SORDID   TRAP
!                                          TILE
!                                          SQRT
!                                          LOG10
!                                 PAGE
!                                 TRAP
!                                 SENSIT   VALID    TRAP
!                                          SORDID   TRAP
!                                          PAGE
!                                          STATW    SORDID   TRAP
!                                                   TILE
!                                 SIMD     VALID    TRAP
!                                          STATW    SORDID   TRAP
!                                                   TILE
!                                 SPEDUP
!                        STOP
!
!
!
!
!    All subroutines also call TRACE , TRACK , and WATCH to assist debugging.
!
!
!
!
!
!
!
!    ------ ---- ------     -----   ------------------------------------
!    BASE   TYPE CLASS      NAME    GLOSSARY
!    ------ ---- ------     -----   ------------------------------------
!    SPACE0    R Array      BIAS  - scale factors for SIGNEL data generator
!    SPACE0    R Array      CSUM  - checksums of KERNEL result arrays
!    BETA      R Array      CSUMS - sets of CSUM for all test runs
!    BETA      R Array      DOS   - sets of TOTAL flops for all test runs
!    SPACE0    R Array      FLOPN - flop counts for one execution pass
!    BETA      R Array      FOPN  - sets of FLOPN for all test runs
!    SPACE0    R Array      FR    - vectorisation fractions; abscissa for REPORT
!    SPACES    I scalar     ibuf  - flag enables one call to SIGNEL
!    ALPHA     I scalar     ik    - current number of executing kernel
!    ALPHA     I scalar     il    - selects one of three sets of loop spans
!    SPACES    I scalar     ion   - logical I/O unit number for output
!    SPACEI    I Array      IPASS - Loop control limits for multiple-pass loops
!    SPACE0    I Array      IQ    - set of workload weights for REPORT
!    SPACEI    I Array      ISPAN - loop control limits for each kernel
!    SPACES    I scalar     j5    - datum in kernel 16
!    ALPHA     I scalar     jr    - current test run number (1 thru 7)
!    SPACES    I scalar     k2    - counter in kernel 16
!    SPACES    I scalar     k3    - counter in kernel 16
!    SPACES    I scalar     kr    - a copy of mk
!    SPACES    I scalar     laps  - multiplies Nruns for long Endurance
!    SPACES    I scalar     Loop  - current multiple-pass loop limit in
!    SPACES    I scalar     m     - temp integer datum
!    ALPHA     I scalar     mk    - number of kernels to evaluate .LE.24
!    ALPHA     I scalar     ml    - maximum value of il=  3
!    SPACES    I scalar     mpy   - repetiton counter of MULTI pass loop
!    SPACES    I scalar     Loops2- repetiton loop limit
!    ALPHA     I scalar     Mruns - number of complete test runs .GE.Nruns
!    SPACEI    I Array      MUL   - multipliers * IPASS defines Loop
!    SPACES    I scalar     MULTI - Multiplier used to compute Loop in SIZES
!    SPACES    I scalar     n     - current DO loop limit in KERNEL
!    SPACES    I scalar     n1    - dimension of most 1-D arrays
!    SPACES    I scalar     n13   - dimension used in kernel 13
!    SPACES    I scalar     n13h  - dimension used in kernel 13
!    SPACES    I scalar     n14   - dimension used in kernel 14
!    SPACES    I scalar     n16   - dimension used in kernel 16
!    SPACES    I scalar     n2    - dimension of most 2-D arrays
!    SPACES    I scalar     n21   - dimension used in kernel 21
!    SPACES    I scalar     n213  - dimension used in kernel 21
!    SPACES    I scalar     n416  - dimension used in kernel 16
!    SPACES    I scalar     n813  - dimension used in kernel 13
!    SPACE0    I scalar     npf   - temp integer datum
!    ALPHA     I Array      NPFS  - sets of NPFS1 for all test runs
!    SPACE0    I Array      NPFS1 - number of page-faults for each kernel
!    ALPHA     I scalar     Nruns - number of complete test runs .LE.7
!    SPACES    I scalar     nt1   - total size of common -SPACE1- words
!    SPACES    I scalar     nt2   - total size of common -SPACE2- words
!    BETA      R Array      SEE   - (i,1,jr,il) sets of TEST overhead times
!    BETA      R Array      SEE   - (i,2,jr,il) sets of csums of SPACE1
!    BETA      R Array      SEE   - (i,3,jr,il) sets of csums of SPACE2
!    SPACE0    R Array      SKALE - scale factors for SIGNEL data generator
!    SPACE0    R scalar     start - temp start time of each kernel
!    PROOF     R Array      SUMS  - sets of verified checksums for all test runs
!    SPACE0    R Array      SUMW  - set of quartile weights for REPORT
!    TAU       R scalar     tclock- minimum cpu clock time= resolution
!    SPACE0    R Array      TERR1 - overhead-time errors for each kernel
!    BETA      R Array      TERRS - sets of TERR1 for all runs
!    TAU       R scalar     testov- average overhead time in TEST linkage
!    BETA      R scalar     tic   - average overhead time in SECOND (copy)
!    SPACE0    R scalar     ticks - average overhead time in TEST linkage(copy)
!    SPACE0    R Array      TIME  - net execution times for all kernels
!    BETA      R Array      TIMES - sets of TIME for all test runs
!    SPACE0    R Array      TOTAL - total flops computed by each kernel
!    TAU       R scalar     tsecov- average overhead time in SECOND
!    SPACE0    R Array      WS    - unused
!    SPACE0    R Array      WT    - weights for each kernel sample
!    SPACEI    R Array      WTP   - weights for the 3 span-varying passes
!    SPACE0    R Array      WW    - unused
!
!
!  --------- -----------------------------------------------------------------
!   COMMON   Usage
!  --------- -----------------------------------------------------------------
!
!   /ALPHA /
!            VERIFY    TICK      TALLY     SIZES     RESULT    REPORT
!            MAIN.
!   /BASE1 /
!            SUPPLY
!   /BASE2 /
!            SUPPLY
!   /BASER /
!            SUPPLY
!   /BETA  /
!            TICK      TALLY     SIZES     RESULT    REPORT    KERNEL
!   /DEBUG /
!            TRACE     TRACK     TRAP
!   /ORDER /
!            TRACE     TRACK     TRAP
!   /PROOF /
!            RESULT    BLOCKDATA
!   /SPACE0/
!            VALUES    TICK      TEST      TALLY     SUPPLY    SIZES
!            REPORT    KERNEL    BLOCKDATA
!   /SPACE1/
!            VERIFY    VALUES    TICK      TEST      SUPPLY    SPACE
!   /SPACE2/
!            VERIFY    VALUES    TICK      TEST      SUPPLY    SPACE
!   /SPACE3/
!            VALUES
!   /SPACEI/
!            VERIFY    VALUES    TICK      TEST      SIZES     RESULT
!            KERNEL    BLOCKDATA
!   /SPACER/
!            VALUES    TICK      TEST      SUPPLY    SIZES     KERNEL
!   /SPACES/
!            VERIFY    VALUES    TICK      TEST      SUPPLY    SIZES
!            BLOCKDATA
!  --------- -----------------------------------------------------------------
!
!
!           SubrouTine Timing on CRAY-XMP1:
!
!           Subroutine   Time(%) All Scalar
!
!           KERNEL       52.24%
!           SUPPLY       17.85%
!           VERIFY        8.76%
!           VALUES        6.15%
!           STATS         5.44%
!           DMPY          1.97%
!           DADD          1.53%
!           EXP           1.02%
!           SQRT           .99%
!           SORDID         .81%
!           DDIV           .38%
!           IQRANF         .25%
!           SUMO           .22%
!           TRACE          .19%
!           SIGNEL         .16%
!           TRAP           .10%
!           TRACK          .10%
!           STATW          .08%
!           TILE           .04%
!           SIZES          .03%
!           ALOG10         .03%
!
!           Subroutine   Time(%)  Auto Vector
!
!           KERNEL       56.28%
!           VALUES       10.33%
!           STATS         8.57%
!           DADD          4.34%
!           DMPY          3.86%
!           VERIFY        2.61%
!           SUPPLY        2.28%
!           SQRT          2.10%
!           SORDID        1.84%
!           SUMO           .80%
!           DDIV           .78%
!           SDOT           .67%
!           TRACE          .53%
!           IQRANF         .50%
!           SIGNEL         .36%
!           EXP            .32%
!           TRACK          .23%
!           TRAP           .20%
!           ALOG10         .18%
!           STATW          .16%
!
!
      RETURN 
      END SUBROUTINE INDEX
!
!***************************************
      SUBROUTINE INDATA(TK, IOU)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU
      REAL, DIMENSION(6) :: TK
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /ORDER/
      COMMON /ORDER/ INSEQ, MATCH, NSTACK(20), ISAVE, IRET
      INTEGER   INSEQ, MATCH, NSTACK, ISAVE, IRET
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
!
      TK(1) = 0.00D0
      TK(2) = 0.00D0
      TESTOV = 0.00D0
      TICKS = 0.00D0
      TCLOCK = 0.00D0
      TSECOV = 0.00D0
      TIC = 0.00D0
!
      JR = 1
      NRUNS = 1
      IL = 1
      MK = 1
      IK = 1
!
      INSEQ = 0
      ISAVE = 0
      IRET = 0
!
      LOOPS2 = 1
      MPYLIM = LOOPS2
      MPY = 1
      MULTI = 1
      MUCHO = 1
      L = 1
      LOOP = 1
      LP = LOOP
      N = 0
!
      IOU = 8
      ION = IOU
      CALL INITIO (8, 'output')
!       CALL  INITIO( 7, 'chksum')
!
      CALL TRACE ('INDATA  ')
!PFM       IF( INIPFM( ion, 0) .NE. 0 )  THEN
!PFM           CALL WHERE(20)
!PFM       ENDIF
!
!LLL.      call  Q8EBM
!
      WRITE (*, 7002)
       WRITE (*, 7003)
       WRITE (*, 7002)
       WRITE (IOU, 7002)
       WRITE (IOU, 7003)
       WRITE (IOU, 7002)
 7002 FORMAT(  ' *********************************************' )
 7003 FORMAT(  ' THE LIVERMORE  FORTRAN KERNELS "MFLOPS" TEST:' )
       WRITE (IOU, 797)
       WRITE (IOU, 798)
  797 FORMAT(' >>> USE 72 SAMPLES LFK TEST RESULTS SUMMARY (line 330+)')
  798 FORMAT(' >>> USE ALL RANGE STATISTICS FOR OFFICIAL QUOTATIONS.  ')
       CALL TRACK ('INDATA  ')
      RETURN 
      END SUBROUTINE INDATA
!
!*************************************************
      SUBROUTINE INITIO(IOU, NAME)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU
      CHARACTER NAME*(*)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL :: FILE, EXIST, UNIT, STATUS
      LOGICAL :: LIVING
!-----------------------------------------------
!     CALL TRACE ('INITIO  ')
!
      INQUIRE(FILE=NAME, EXIST=LIVING)
      IF (LIVING) THEN
           OPEN(UNIT=IOU, FILE=NAME, STATUS='OLD')
           CLOSE(UNIT=IOU, STATUS='DELETE')
      ENDIF
      OPEN(UNIT=IOU, FILE=NAME, STATUS='NEW')
!
!     CALL TRACK ('INITIO  ')
      RETURN 
      END SUBROUTINE INITIO
!
!***************************************
      SUBROUTINE IQRAN0(NEWK)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NEWK
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /IQRAND/
      COMMON /IQRAND/ K0, K, K9
      INTEGER   K0, K, K9
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
      CALL TRACE ('IQRAN0  ')
      IF (NEWK <= 0) CALL WHERE (1)
      K = NEWK
!
      CALL TRACK ('IQRAN0  ')
      RETURN 
      END SUBROUTINE IQRAN0
!
!***************************************
      SUBROUTINE IQRANF(M, MMIN, MMAX, N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER MMIN, MMAX, N
      INTEGER, DIMENSION(N) :: M
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /IQRAND/
      COMMON /IQRAND/ K0, K, K9
      INTEGER   K0, K, K9
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: INSET, I
      REAL :: REALN, SCALE, Q
      DOUBLE PRECISION :: DQ, DP, PER, DK, SPIN, SPAN
!-----------------------------------------------
!     save k
      CALL TRACE ('IQRANF  ')
      IF (N > 0) THEN
           INSET = MMIN
           SPAN = MMAX - MMIN
!         spin= 16807.00d0
!          per= 2147483647.00d0
           SPIN = 16807
           PER = 2147483647
           REALN = N
           SCALE = 1.0000100D0
           Q = SCALE*(SPAN/REALN)
!
           DK = K
           DO I = 1, N
                DP = DK*SPIN
!           dk=    DMOD( dp, per)
                DK = DP - INT(DP/PER)*PER
                DQ = DK*SPAN
                M(I) = INSET + DQ/PER
                IF (M(I)<MMIN .OR. M(I)>MMAX) M(I) = INSET + I*Q
           END DO
           K = DK
!
!
!iC     double precision  k, ip, iq, id
!i         inset= Mmin
!i         ispan= Mmax - Mmin
!i         ispin= 16807
!i            id= 2147483647
!i             q= (REAL(ispan)/REAL(n))*1.00001
!iC
!i      DO  2  i= 1,n
!i            ip= k*ispin
!i             k=      MOD( ip, id)
!i            iq= k*ispan
!i          M(i)= inset + ( iq/ id)
!i            IF( M(i).LT.Mmin .OR. M(i).GT.Mmax )  M(i)= inset + i*q
!i    2 continue
!
           CALL TRAP (M, ' IQRANF  ', 1, MMAX, N)
!
      ENDIF
      CALL TRACK ('IQRANF  ')
      RETURN 
!     DATA  k /256/
!                        IQRANF TEST PROGRAM:
!      parameter( nrange= 10000, nmaps= 1001 )
!      DIMENSION  IX(nrange), IY(nmaps), IZ(nmaps), IR(nmaps)
!      COMMON /IQRAND/ k0, k, k9
!c
!        CALL  LINK( 'UNIT6=( output,create,text)//')
!         iou= 8
!      DO 7 j= 1,256,255
!      CALL IQRAN0( j )
!      CALL IQRANF( IX, 1, nmaps, nrange)
!      DO 1 i= 1,nmaps
!       IY(i)= 0
!    1  IZ(i)= 0
!c                     census for each index generated in (1:nmaps)
!      DO 2 i= 1,nrange
!    2 IY( IX(i))= IY( IX(i)) + 1
!c                     distribution of census tallies about nrange/nmaps
!      DO 3 i= 1,nmaps
!    3 IZ( IY(i))= IZ( IY(i)) + 1
!       IR(1)= IZ(1)
!c                     integral of distribution
!      DO 4 i= 1,nmaps
!    4  IR(i)= IR(i-1) + IZ(i)
!      WRITE( iou,112)   j, IR(nmaps), k
!      WRITE( iou,113) ( IX(i), i= 1,20 )
!      WRITE( iou,113) ( IY(i), i= 1,20 )
!      WRITE( iou,113) ( IZ(i), i= 1,20 )
!      WRITE( iou,113) ( IR(i), i= 1,20 )
!  112 FORMAT(/,1X,4I20)
!  113 FORMAT(20I4)
!    7 continue
!      STOP
!
!                   1                1000          1043618065
!  1 132 756 459 533 219  48 679 680 935 384 520 831  35  54 530 672   8 384  67
! 17  12   7  10  10  10  10  12   9   9   4  15  10   7   7   9   9   9  10  11
!  0   1   8  19  40  60  86 109 133 128 107 104  70  52  39  26   7   7   2   2
!  0   1   9  28  68 128 214 323 456 584 691 795 865 917 956 982 989 996 9981000
!
!                 256                1000           878252412
!  3 674 435 415 389  54  44 790 900 282 177 971 728 851 687 604 815 971 155 112
! 11  17  19   6  11  11   7   9  12   7  13   7   9  11  14   9   9  12   9   9
!  1   2  10  16  30  71  93 109 131 119 118 105  69  47  28  15  15   9   5   3
!  1   3  13  29  59 130 223 332 463 582 700 805 874 921 949 964 979 988 993 996
      END SUBROUTINE IQRANF
!
!***********************************************
      SUBROUTINE KERNEL(TK)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL, DIMENSION(6) :: TK
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /SPACER/
      COMMON /SPACER/ A11, A12, A13, A21, A22, A23, A31, A32, A33, AR,  &
     &     BR, C0, CR, DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28 &
     &     , DN, E3, E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5 &
     &     , T, XNC, XNEI, XNM
      REAL   A11, A12, A13, A21, A22, A23, A31, A32, A33, AR, BR, C0, CR&
     &     , DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28, DN, E3,  &
     &     E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5, T, XNC,  &
     &     XNEI, XNM
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!...  /ISPACE/
      COMMON /ISPACE/ E(96), F(96), IX(1001), IR(1001), ZONE(300)
      INTEGER   E, F, IX, IR, ZONE
!...  /SPACE1/
      COMMON /SPACE1/ U(1001), V(1001), W(1001), X(1001), Y(1001), Z(   &
     &     1001), G(1001), DU1(101), DU2(101), DU3(101), GRD(1001), DEX(&
     &     1001), XI(1001), EX(1001), EX1(1001), DEX1(1001), VX(1001),  &
     &     XX(1001), RX(1001), RH(2048), VSP(101), VSTP(101), VXNE(101) &
     &     , VXND(101), VE3(101), VLR(101), VLIN(101), B5(101), PLAN(300&
     &     ), D(300), SA(101), SB(101)
      REAL   U, V, W, X, Y, Z, G, DU1, DU2, DU3, GRD, DEX, XI, EX, EX1, &
     &     DEX1, VX, XX, RX, RH, VSP, VSTP, VXNE, VXND, VE3, VLR, VLIN, &
     &     B5, PLAN, D, SA, SB
!...  /SPACE2/
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101), VY(101,25), VH( &
     &     101,7), VF(101,7), VG(101,7), VS(101,7), ZA(101,7), ZP(101,7)&
     &     , ZQ(101,7), ZR(101,7), ZM(101,7), ZB(101,7), ZU(101,7), ZV( &
     &     101,7), ZZ(101,7), B(64,64), C(64,64), H(64,64), U1(5,101,2) &
     &     , U2(5,101,2), U3(5,101,2)
      REAL   P, PX, CX, VY, VH, VF, VG, VS, ZA, ZP, ZQ, ZR, ZM, ZB, ZU, &
     &     ZV, ZZ, B, C, H, U1, U2, U3
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: AND, IT0, K, II, IPNTP, IPNT, I, LW, J, NL1, NL2, KX,  &
     &     KY, I1, J1, I2, J2, NG, NZ, LB, J4, INK, KN, JN, KB5I
      REAL, DIMENSION(1023) :: ZX
      REAL, DIMENSION(1500) :: XZ
      REAL :: FW, TEMP, DW, TW, SUM, SOM
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      INTEGER , EXTERNAL :: TEST
!-----------------------------------------------
!   S t a t e m e n t   F u n c t i o n s
!-----------------------------------------------
      INTEGER MOD2N
!-----------------------------------------------
      EQUIVALENCE ( ZX(1), Z(1)), ( XZ(1), X(1))
      MOD2N(i,j)= IAND(i,j-1)
!                             i  is Congruent to  MOD2N(i,j)   mod(j)
!     ******************************************************************
!
!
!
!
!
      CALL TRACE ('KERNEL  ')
!
      CALL SPACE
!
!PFM       call  OUTPFM( 0, ion)
      MPY = 1
      LOOPS2 = 1
      MPYLIM = LOOPS2
      L = 1
      LOOP = 1
      LP = LOOP
      IT0 = TEST(0)
!PFM  iflag1= 13579
!
!*******************************************************************************
!***  KERNEL 1      HYDRO FRAGMENT
!*******************************************************************************
!
!dir$ ivdep
 1001 CONTINUE
      X(:N) = Q + Y(:N)*(R*ZX(11:N+10)+T*ZX(12:N+11))
!
!...................
      IF (TEST(1) > 0) GO TO 1001
!                   we must execute    DO k= 1,n  repeatedly for accurate timing
!
!*******************************************************************************
!***  KERNEL 2      ICCG EXCERPT (INCOMPLETE CHOLESKY - CONJUGATE GRADIENT)
!*******************************************************************************
!
!
 1002 CONTINUE
      II = N
      IPNTP = 0
  222 CONTINUE
      IPNT = IPNTP
      IPNTP = IPNTP + II
      II = II/2
      I = IPNTP + 1
!dir$ ivdep
!:ibm_dir:ignore recrdeps (x)
!
      DO K = IPNT + 2, IPNTP, 2
           I = I + 1
           X(I) = X(K) - V(K)*X(K-1) - V(K+1)*X(K+1)
      END DO
      IF (II > 1) GO TO 222
!
!...................
      IF (TEST(2) > 0) GO TO 1002
!
!*******************************************************************************
!***  KERNEL 3      INNER PRODUCT
!*******************************************************************************
!
!
 1003 CONTINUE
      Q = 0.000D0
      Q = DOT_PRODUCT(Z(:N),X(:N))
!
!...................
      IF (TEST(3) > 0) GO TO 1003
!
!*******************************************************************************
!***  KERNEL 4      BANDED LINEAR EQUATIONS
!*******************************************************************************
!
      M = (1001 - 7)/2
      FW = 1.000D-25
!
 1004 CONTINUE
      DO K = 7, 1001, M
           LW = K - 6
           TEMP = XZ(K-1)
!dir$ ivdep
           DO J = 5, N, 5
                TEMP = TEMP - XZ(LW)*Y(J)
                LW = LW + 1
           END DO
           XZ(K-1) = Y(5)*TEMP
      END DO
!
!...................
      IF (TEST(4) > 0) GO TO 1004
!
!*******************************************************************************
!***  KERNEL 5      TRI-DIAGONAL ELIMINATION, BELOW DIAGONAL (NO VECTORS)
!*******************************************************************************
!
!
!dir$ novector
 1005 CONTINUE
      DO I = 2, N
           X(I) = Z(I)*(Y(I)-X(I-1))
      END DO
!dir$ vector
!
!...................
      IF (TEST(5) > 0) GO TO 1005
!
!*******************************************************************************
!***  KERNEL 6      GENERAL LINEAR RECURRENCE EQUATIONS
!*******************************************************************************
!
!
 1006 CONTINUE
      DO I = 2, N
           W(I) = 0.0100D0
           DO K = 1, I - 1
                W(I) = W(I) + B(I,K)*W(I-K)
           END DO
      END DO
!
!...................
      IF (TEST(6) > 0) GO TO 1006
!
!*******************************************************************************
!***  KERNEL 7      EQUATION OF STATE FRAGMENT
!*******************************************************************************
!
!
!dir$ ivdep
 1007 CONTINUE
      X(:N) = U(:N) + R*(Z(:N)+R*Y(:N)) + T*(U(4:N+3)+R*(U(3:N+2)+R*U(2:&
     &     N+1))+T*(U(7:N+6)+Q*(U(6:N+5)+Q*U(5:N+4))))
!
!...................
      IF (TEST(7) > 0) GO TO 1007
!
!
!*******************************************************************************
!***  KERNEL 8      A.D.I. INTEGRATION
!*******************************************************************************
!
!
 1008 CONTINUE
      NL1 = 1
      NL2 = 2
      FW = 2.000D0
!dir$ ivdep
      DU1(2:N) = U1(2,3:N+1,NL1) - U1(2,:N-1,NL1)
      DU2(2:N) = U2(2,3:N+1,NL1) - U2(2,:N-1,NL1)
      DU3(2:N) = U3(2,3:N+1,NL1) - U3(2,:N-1,NL1)
      U1(2,2:N,NL2) = U1(2,2:N,NL1) + A11*DU1(2:N) + A12*DU2(2:N) + A13*&
     &     DU3(2:N) + SIG*(U1(3,2:N,NL1)-FW*U1(2,2:N,NL1)+U1(1,2:N,NL1))
      U2(2,2:N,NL2) = U2(2,2:N,NL1) + A21*DU1(2:N) + A22*DU2(2:N) + A23*&
     &     DU3(2:N) + SIG*(U2(3,2:N,NL1)-FW*U2(2,2:N,NL1)+U2(1,2:N,NL1))
      U3(2,2:N,NL2) = U3(2,2:N,NL1) + A31*DU1(2:N) + A32*DU2(2:N) + A33*&
     &     DU3(2:N) + SIG*(U3(3,2:N,NL1)-FW*U3(2,2:N,NL1)+U3(1,2:N,NL1))
      DU1(2:N) = U1(3,3:N+1,NL1) - U1(3,:N-1,NL1)
      DU2(2:N) = U2(3,3:N+1,NL1) - U2(3,:N-1,NL1)
      DU3(2:N) = U3(3,3:N+1,NL1) - U3(3,:N-1,NL1)
      U1(3,2:N,NL2) = U1(3,2:N,NL1) + A11*DU1(2:N) + A12*DU2(2:N) + A13*&
     &     DU3(2:N) + SIG*(U1(4,2:N,NL1)-FW*U1(3,2:N,NL1)+U1(2,2:N,NL1))
      U2(3,2:N,NL2) = U2(3,2:N,NL1) + A21*DU1(2:N) + A22*DU2(2:N) + A23*&
     &     DU3(2:N) + SIG*(U2(4,2:N,NL1)-FW*U2(3,2:N,NL1)+U2(2,2:N,NL1))
      U3(3,2:N,NL2) = U3(3,2:N,NL1) + A31*DU1(2:N) + A32*DU2(2:N) + A33*&
     &     DU3(2:N) + SIG*(U3(4,2:N,NL1)-FW*U3(3,2:N,NL1)+U3(2,2:N,NL1))
!
!...................
      IF (TEST(8) > 0) GO TO 1008
!
!*******************************************************************************
!***  KERNEL 9      INTEGRATE PREDICTORS
!*******************************************************************************
!
!
 1009 CONTINUE
      PX(1,:N) = DM28*PX(13,:N) + DM27*PX(12,:N) + DM26*PX(11,:N) + DM25&
     &     *PX(10,:N) + DM24*PX(9,:N) + DM23*PX(8,:N) + DM22*PX(7,:N) + &
     &     C0*(PX(5,:N)+PX(6,:N)) + PX(3,:N)
!
!...................
      IF (TEST(9) > 0) GO TO 1009
!
!*******************************************************************************
!***  KERNEL 10     DIFFERENCE PREDICTORS
!*******************************************************************************
!
!
 1010 CONTINUE
      DO K = 1, N
           AR = CX(5,K)
           BR = AR - PX(5,K)
           PX(5,K) = AR
           CR = BR - PX(6,K)
           PX(6,K) = BR
           AR = CR - PX(7,K)
           PX(7,K) = CR
           BR = AR - PX(8,K)
           PX(8,K) = AR
           CR = BR - PX(9,K)
           PX(9,K) = BR
           AR = CR - PX(10,K)
           PX(10,K) = CR
           BR = AR - PX(11,K)
           PX(11,K) = AR
           CR = BR - PX(12,K)
           PX(12,K) = BR
           PX(14,K) = CR - PX(13,K)
           PX(13,K) = CR
      END DO
!
!...................
      IF (TEST(10) > 0) GO TO 1010
!
!*******************************************************************************
!***  KERNEL 11     FIRST SUM.   PARTIAL SUMS.              (NO VECTORS)
!*******************************************************************************
!
!
 1011 CONTINUE
      X(1) = Y(1)
!dir$ novector
      DO K = 2, N
           X(K) = X(K-1) + Y(K)
      END DO
!dir$ vector
!
!...................
      IF (TEST(11) > 0) GO TO 1011
!
!*******************************************************************************
!***  KERNEL 12     FIRST DIFF.
!*******************************************************************************
!
!
!dir$ ivdep
 1012 CONTINUE
      X(:N) = Y(2:N+1) - Y(:N)
!
!...................
      IF (TEST(12) > 0) GO TO 1012
!
!*******************************************************************************
!***  KERNEL 13      2-D PIC   Particle In Cell
!*******************************************************************************
!
      FW = 1.000D0
!
 1013 CONTINUE
      DO K = 1, N
           I1 = P(1,K)
           J1 = P(2,K)
           I1 = 1 + MOD2N(I1,64)
           J1 = 1 + MOD2N(J1,64)
           P(3,K) = P(3,K) + B(I1,J1)
           P(4,K) = P(4,K) + C(I1,J1)
           P(1,K) = P(1,K) + P(3,K)
           P(2,K) = P(2,K) + P(4,K)
           I2 = P(1,K)
           J2 = P(2,K)
           I2 = MOD2N(I2,64)
           J2 = MOD2N(J2,64)
           P(1,K) = P(1,K) + Y(I2+32)
           P(2,K) = P(2,K) + Z(J2+32)
           I2 = I2 + E(I2+32)
           J2 = J2 + F(J2+32)
           H(I2,J2) = H(I2,J2) + FW
      END DO
!
!...................
      IF (TEST(13) > 0) GO TO 1013
!
!*******************************************************************************
!***  KERNEL 14      1-D PIC   Particle In Cell
!*******************************************************************************
!
!
      FW = 1.000D0
!
 1014 CONTINUE
      VX(:N) = 0.0D0
      XX(:N) = 0.0D0
      IX(:N) = INT(GRD(:N))
      XI(:N) = REAL(IX(:N))
      EX1(:N) = EX(IX(:N))
      DEX1(:N) = DEX(IX(:N))
!
      DO K = 1, N
           VX(K) = VX(K) + EX1(K) + (XX(K)-XI(K))*DEX1(K)
           XX(K) = XX(K) + VX(K) + FLX
           IR(K) = XX(K)
           RX(K) = XX(K) - IR(K)
           IR(K) = MOD2N(IR(K),2048) + 1
           XX(K) = RX(K) + IR(K)
      END DO
!
      DO K = 1, N
           RH(IR(K)) = RH(IR(K)) + FW - RX(K)
           RH(IR(K)+1) = RH(IR(K)+1) + RX(K)
      END DO
!
!...................
      IF (TEST(14) > 0) GO TO 1014
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!*******************************************************************************
!***  KERNEL 15     CASUAL FORTRAN.  DEVELOPMENT VERSION.
!*******************************************************************************
!
!
!       CASUAL ORDERING OF SCALAR OPERATIONS IS TYPICAL PRACTICE.
!       THIS EXAMPLE DEMONSTRATES THE NON-TRIVIAL TRANSFORMATION
!       REQUIRED TO MAP INTO AN EFFICIENT MACHINE IMPLEMENTATION.
!
!
 1015 CONTINUE
      NG = 7
      NZ = N
      AR = 0.05300D0
      BR = 0.07300D0
      DO J = 2, NG
           DO K = 2, NZ
                IF (J - NG < 0) GO TO 31
   30           CONTINUE
                VY(K,J) = 0.0D0
                CYCLE 
   31           CONTINUE
                IF (VH(K,J+1) - VH(K,J) > 0.) THEN
   32                CONTINUE
                     T = AR
                ELSE
                     T = BR
                ENDIF
                IF (VF(K,J) - VF(K-1,J) < 0.) THEN
   35                CONTINUE
                     R = MAX(VH(K-1,J),VH(K-1,J+1))
                     S = VF(K-1,J)
                ELSE
                     R = MAX(VH(K,J),VH(K,J+1))
                     S = VF(K,J)
                ENDIF
                VY(K,J) = SQRT(VG(K,J)**2+R*R)*T/S
                IF (K - NZ < 0) GO TO 40
   39           CONTINUE
                VS(K,J) = 0.0D0
                CYCLE 
   40           CONTINUE
                IF (VF(K,J) - VF(K,J-1) < 0.) THEN
   41                CONTINUE
                     R = MAX(VG(K,J-1),VG(K+1,J-1))
                     S = VF(K,J-1)
                     T = BR
                ELSE
                     R = MAX(VG(K,J),VG(K+1,J))
                     S = VF(K,J)
                     T = AR
                ENDIF
                VS(K,J) = SQRT(VH(K,J)**2+R*R)*T/S
           END DO
      END DO
!
!...................
      IF (TEST(15) > 0) GO TO 1015
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!*******************************************************************************
!***  KERNEL 16     MONTE CARLO SEARCH LOOP
!*******************************************************************************
!
      II = N/3
      LB = II + II
      K2 = 0
      K3 = 0
!
!
 1016 CONTINUE
      M = 1
      I1 = M
  410 CONTINUE
      J2 = (N + N)*(M - 1) + 1
      DO K = 1, N
           K2 = K2 + 1
           J4 = J2 + K + K
           J5 = ZONE(J4)
           IF (J5 - N < 0) GO TO 420
           IF (J5 - N == 0) EXIT 
           GO TO 450
  415      CONTINUE
           IF (J5 - N + II < 0) GO TO 430
           GO TO 425
  420      CONTINUE
           IF (J5 - N + LB >= 0) GO TO 415
           GO TO 435
  425      CONTINUE
           IF (PLAN(J5) - R < 0.) GO TO 445
           IF (PLAN(J5) - R == 0.) GO TO 480
           GO TO 440
  430      CONTINUE
           IF (PLAN(J5) - S < 0.) GO TO 445
           IF (PLAN(J5) - S == 0.) GO TO 480
           GO TO 440
  435      CONTINUE
           IF (PLAN(J5) - T < 0.) GO TO 445
           IF (PLAN(J5) - T == 0.) GO TO 480
  440      CONTINUE
           IF (ZONE(J4-1) < 0) GO TO 455
           IF (ZONE(J4-1) == 0) GO TO 485
           CYCLE 
  445      CONTINUE
           IF (ZONE(J4-1) < 0) CYCLE 
           IF (ZONE(J4-1) == 0) GO TO 485
           GO TO 455
  450      CONTINUE
           K3 = K3 + 1
           IF (D(J5) - (D(J5-1)*(T-D(J5-2))**2+(S-D(J5-3))**2+(R-D(J5-4)&
     &          )**2) < 0.) GO TO 445
           IF (D(J5) - (D(J5-1)*(T-D(J5-2))**2+(S-D(J5-3))**2+(R-D(J5-4)&
     &          )**2) == 0.) GO TO 480
           GO TO 440
  455      CONTINUE
           M = M + 1
           IF (M - ZONE(1) > 0) THEN
  460           CONTINUE
                M = 1
           ENDIF
           IF (I1 - M /= 0) GO TO 410
           GO TO 480
      END DO
  480 CONTINUE
  485 CONTINUE
!
!...................
      IF (TEST(16) > 0) GO TO 1016
!
!*******************************************************************************
!***  KERNEL 17     IMPLICIT, CONDITIONAL COMPUTATION       (NO VECTORS)
!*******************************************************************************
!
!         RECURSIVE-DOUBLING VECTOR TECHNIQUES CAN NOT BE USED
!         BECAUSE CONDITIONAL OPERATIONS APPLY TO EACH ELEMENT.
!
      DW = 5.0000D0/3.0000D0
      FW = 1.0000D0/3.0000D0
      TW = 1.0300D0/3.0700D0
!dir$ novector
!
 1017 CONTINUE
      K = N
      J = 1
      INK = -1
      SCALE = DW
      XNM = FW
      E6 = TW
      GO TO 61
!                                            STEP MODEL
   60 CONTINUE
      E6 = XNM*VSP(K) + VSTP(K)
      VXNE(K) = E6
      XNM = E6
      VE3(K) = E6
      K = K + INK
      IF (K == J) GO TO 62
   61 CONTINUE
      E3 = XNM*VLR(K) + VLIN(K)
      XNEI = VXNE(K)
      VXND(K) = E6
      XNC = SCALE*E3
!                                            SELECT MODEL
      IF (XNM > XNC) GO TO 60
      IF (XNEI > XNC) GO TO 60
!                                            LINEAR MODEL
      VE3(K) = E3
      E6 = E3 + E3 - XNM
      VXNE(K) = E3 + E3 - XNEI
      XNM = E6
      K = K + INK
      IF (K /= J) GO TO 61
   62 CONTINUE
!dir$ vector
!
!...................
      IF (TEST(17) > 0) GO TO 1017
!
!*******************************************************************************
!***  KERNEL 18     2-D EXPLICIT HYDRODYNAMICS FRAGMENT
!*******************************************************************************
!
!
 1018 CONTINUE
      T = 0.003700D0
      S = 0.004100D0
      KN = 6
      JN = N
      ZA(2:JN,2:KN) = (ZP(:JN-1,3:KN+1)+ZQ(:JN-1,3:KN+1)-ZP(:JN-1,2:KN)-&
     &     ZQ(:JN-1,2:KN))*(ZR(2:JN,2:KN)+ZR(:JN-1,2:KN))/(ZM(:JN-1,2:KN&
     &     )+ZM(:JN-1,3:KN+1))
      ZB(2:JN,2:KN) = (ZP(:JN-1,2:KN)+ZQ(:JN-1,2:KN)-ZP(2:JN,2:KN)-ZQ(2:&
     &     JN,2:KN))*(ZR(2:JN,2:KN)+ZR(2:JN,:KN-1))/(ZM(2:JN,2:KN)+ZM(: &
     &     JN-1,2:KN))
!
      ZU(2:JN,2:KN) = ZU(2:JN,2:KN) + S*(ZA(2:JN,2:KN)*(ZZ(2:JN,2:KN)-ZZ&
     &     (3:JN+1,2:KN))-ZA(:JN-1,2:KN)*(ZZ(2:JN,2:KN)-ZZ(:JN-1,2:KN))-&
     &     ZB(2:JN,2:KN)*(ZZ(2:JN,2:KN)-ZZ(2:JN,:KN-1))+ZB(2:JN,3:KN+1)*&
     &     (ZZ(2:JN,2:KN)-ZZ(2:JN,3:KN+1)))
      ZV(2:JN,2:KN) = ZV(2:JN,2:KN) + S*(ZA(2:JN,2:KN)*(ZR(2:JN,2:KN)-ZR&
     &     (3:JN+1,2:KN))-ZA(:JN-1,2:KN)*(ZR(2:JN,2:KN)-ZR(:JN-1,2:KN))-&
     &     ZB(2:JN,2:KN)*(ZR(2:JN,2:KN)-ZR(2:JN,:KN-1))+ZB(2:JN,3:KN+1)*&
     &     (ZR(2:JN,2:KN)-ZR(2:JN,3:KN+1)))
!
      ZR(2:JN,2:KN) = ZR(2:JN,2:KN) + T*ZU(2:JN,2:KN)
      ZZ(2:JN,2:KN) = ZZ(2:JN,2:KN) + T*ZV(2:JN,2:KN)
!
!...................
      IF (TEST(18) > 0) GO TO 1018
!
!*******************************************************************************
!***  KERNEL 19      GENERAL LINEAR RECURRENCE EQUATIONS    (NO VECTORS)
!*******************************************************************************
!
 1019 CONTINUE
      KB5I = 0
!
!     IF( JR.LE.1 )  THEN
!dir$ novector
      DO K = 1, N
           B5(K+KB5I) = SA(K) + STB5*SB(K)
           STB5 = B5(K+KB5I) - STB5
      END DO
!     ELSE
!
      DO I = 1, N
           K = N - I + 1
           B5(K+KB5I) = SA(K) + STB5*SB(K)
           STB5 = B5(K+KB5I) - STB5
      END DO
!     ENDIF
!dir$ vector
!
!...................
      IF (TEST(19) > 0) GO TO 1019
!
!*******************************************************************************
!***  KERNEL 20     DISCRETE ORDINATES TRANSPORT: RECURRENCE (NO VECTORS)
!*******************************************************************************
!
      DW = 0.200D0
!dir$ novector
!
 1020 CONTINUE
      DO K = 1, N
           DI = Y(K) - G(K)/(XX(K)+DK)
           DN = DW
           IF (DI /= 0.0) DN = MAX(S,MIN(Z(K)/DI,T))
           X(K) = ((W(K)+V(K)*DN)*XX(K)+U(K))/(VX(K)+V(K)*DN)
           XX(K+1) = (X(K)-XX(K))*DN + XX(K)
      END DO
!dir$ vector
!
!...................
      IF (TEST(20) > 0) GO TO 1020
!
!*******************************************************************************
!***  KERNEL 21     MATRIX*MATRIX PRODUCT
!*******************************************************************************
!
!
 1021 CONTINUE
      DO K = 1, 25
           PX(:,:N) = PX(:,:N) + SPREAD(VY(:25,K),DIM = 2,NCOPIES = N)* &
     &          SPREAD(CX(K,:N),DIM = 1,NCOPIES = 25)
      END DO
!
!...................
      IF (TEST(21) > 0) GO TO 1021
!
!
!
!
!
!
!
!*******************************************************************************
!***  KERNEL 22     PLANCKIAN DISTRIBUTION
!*******************************************************************************
!
!
!      EXPMAX= 234.500d0
      EXPMAX = 20.0000D0
      FW = 1.00000D0
      U(N) = 0.99000D0*EXPMAX*V(N)
!
 1022 CONTINUE
!are       IF( U(k) .LT. EXPMAX*V(k))  THEN
      Y(:N) = U(:N)/V(:N)
!are                                   ELSE
!are                                        Y(k)= EXPMAX
!are    ENDIF
      W(:N) = X(:N)/(EXP(Y(:N))-FW)
!...................
      IF (TEST(22) > 0) GO TO 1022
!
!*******************************************************************************
!***  KERNEL 23     2-D IMPLICIT HYDRODYNAMICS FRAGMENT
!*******************************************************************************
!
      FW = 0.17500D0
!
 1023 CONTINUE
      DO J = 2, 6
           DO K = 2, N
                QA = ZA(K,J+1)*ZR(K,J) + ZA(K,J-1)*ZB(K,J) + ZA(K+1,J)* &
     &               ZU(K,J) + ZA(K-1,J)*ZV(K,J) + ZZ(K,J)
                ZA(K,J) = ZA(K,J) + FW*(QA - ZA(K,J))
           END DO
      END DO
!
!...................
      IF (TEST(23) > 0) GO TO 1023
!
!*******************************************************************************
!***  KERNEL 24     FIND LOCATION OF FIRST MINIMUM IN ARRAY
!*******************************************************************************
!
!      X( n/2)= -1.000d+50
      X(N/2) = -1.000D+10
!
 1024 CONTINUE
      M = 1
      DO K = 2, N
           IF (X(K) < X(M)) M = K
      END DO
!
!            m= imin1( n,x,1)        35 nanosec./element STACKLIBE/CRAY
!...................
      IF (TEST(24) /= 0) GO TO 1024
!
!*******************************************************************************
!
!PFM  iflag1= 0
      SUM = 0.00D0
      SOM = 0.00D0
      DO K = 1, MK
           SUM = SUM + TIME(K)
           TIMES(JR,IL,K) = TIME(K)
           TERRS(JR,IL,K) = TERR1(K)
           NPFS(JR,IL,K) = NPFS1(K)
           CSUMS(JR,IL,K) = CSUM(K)
           DOS(JR,IL,K) = TOTAL(K)
           FOPN(JR,IL,K) = FLOPN(K)
           SOM = SOM + FLOPN(K)*TOTAL(K)
      END DO
!
      TK(1) = TK(1) + SUM
      TK(2) = TK(2) + SOM
!                                  Dumpout Checksums
!     WRITE ( 7,706) jr, il
! 706 FORMAT(1X,2I3)
!     WRITE ( 7,707) ( CSUM(k), k= 1,mk)
! 707 FORMAT(5X,'&',1PE21.15,',',1PE21.15,',',1PE21.15,',')
!
      CALL TRACK ('KERNEL  ')
      RETURN 
      END SUBROUTINE KERNEL
!***********************************************
      SUBROUTINE PAGE(IOU)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU
!-----------------------------------------------
!***********************************************
      CALL TRACE ('PAGE    ')
      WRITE (IOU, 1)
    1 FORMAT('1')
!   1 FORMAT('')
       CALL TRACK ('PAGE    ')
      RETURN 
      END SUBROUTINE PAGE
!
!********************************************
      REAL FUNCTION RELERR (U, V)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL U, V
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL :: W, O, A, B
      DOUBLE PRECISION :: X, Y
!-----------------------------------------------
!
      CALL TRACE ('RELERR  ')
      W = 0.00D0
      IF (U /= V) THEN
           W = 1.00D0
           O = 1.00D0
           IF (SIGN(O,U) == SIGN(O,V)) THEN
                A = ABS(U)
                B = ABS(V)
                X = MAX(A,B)
                Y = MIN(A,B)
                IF (X /= 0.00D0) W = 1.00D0 - Y/X
           ENDIF
      ENDIF
!
      RELERR = W
      CALL TRACK ('RELERR  ')
      RETURN 
      END FUNCTION RELERR
!
!***********************************************************************
      SUBROUTINE REPORT(IOU,NTK,NEK,FLOPS,TR,RATES,LSPAN,WG,OSUM,ID)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU, NTK, NEK
      INTEGER, DIMENSION(141) :: LSPAN, ID
      REAL, DIMENSION(141) :: FLOPS, TR, RATES, WG, OSUM
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /SYSID/
      COMMON /SYSID/ KOMPUT, KONTRL, KOMPIL, KALEND, IDENTY
      CHARACTER   KOMPUT*24, KONTRL*24, KOMPIL*24, KALEND*24, IDENTY*24
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: NT = 4
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(10) :: LVL
      INTEGER, DIMENSION(5) :: LQ
      INTEGER,DIMENSION(141)::IN,MAP1,MAP2,MAP3,IN2,MAP,ISPAN1,ISPAN2
      INTEGER::KALL,MM,MEFF,NEFF,K,ND,LV,I,NQ,LO,I2,J,I1,LL,MRL
      REAL, DIMENSION(NT) :: RATE
      REAL, DIMENSION(12) :: HM
      REAL, DIMENSION(20) :: STAT1, STAT2
      REAL, DIMENSION(141) :: CSUM1, TV4, TV5, VL1, VL, TV, TV1, TV2,   &
     &     FLOPS1, RT1, WT1, FLOPS2, RT2, WT2
      REAL :: FUZZ, BL, BU, PRECIS, SOM, RNETO, TWT, PEAK, AVGEFF, FRAC
      DOUBLE PRECISION :: SUM
      CHARACTER, DIMENSION(NT) :: NAME*8

      SAVE LVL, KALL
!-----------------------------------------------
!   S t a t e m e n t   F u n c t i o n s
!-----------------------------------------------
      INTEGER MODI
!-----------------------------------------------
       MODI(i,mm)= (MOD( ABS(i)-1, mm) + 1)
      DATA KALL/0/
!
      CALL TRACE ('REPORT  ')
!
      IF (IOU >= 0) THEN
!
           MEFF = 0
           NEFF = 0
           FUZZ = 1.0D-9
           VL(:NTK) = LSPAN(:NTK)
!
           BL = 1.0D-5
           BU = 1.0D+5
           CALL VALID (TV, MAP, NEFF, BL, RATES, BU, NTK)
!
!      Compress valid data sets mapping on MAP.
!
           ND = 0
           DO K = 1, NEFF
                MAP1(K) = MODI(MAP(K),NEK)
                FLOPS1(K) = FLOPS(MAP(K))
                RT1(K) = TR(MAP(K))
                VL1(K) = VL(MAP(K))
                ISPAN1(K) = LSPAN(MAP(K))
                WT1(K) = WG(MAP(K))
                TV1(K) = RATES(MAP(K))
                CSUM1(K) = OSUM(MAP(K))
                ND = ID(MAP(K)) + ND
           END DO
           IF (ND <= 8*NEFF) ND = ND - 16*((NEFF - 1 + 24)/24)
           PRECIS = REAL(ND)/(REAL(NEFF) + FUZZ)
!
           SOM = 0.00D0
           SUM = 0.00D0
           DO K = 1, NEFF
                SOM = SOM + FLOPS1(K)
                SUM = SUM + RT1(K)
           END DO
           RNETO = SOM/(SUM + FUZZ)
!
           CALL STATW (STAT1, TV, IN, VL1, WT1, NEFF)
           LV = STAT1(1)
!
           CALL STATW (STAT1, TV, IN, TV1, WT1, NEFF)
           TWT = STAT1(6)
!                             compute average efficiency= GM/Max
           KALL = KALL + 1
           PEAK = 0.00D0
           IF (KALL<=1 .OR. IL==IM) PEAK = STAT1(4)
           AVGEFF = (100.0D0*STAT1(10))/(PEAK + FUZZ)
!
           WRITE (IOU, 7001)
            WRITE (IOU, 7001)
            WRITE (IOU, 7001)
            WRITE (IOU, 7001)
            WRITE (IOU, 7001)
            WRITE (IOU, 7001)
            CALL PAGE (IOU)
           WRITE (IOU, 7002)
!
            IF (NTK == NEK) THEN
                WRITE (IOU, 7003)
           ELSE
                 WRITE (IOU, 7090)
           ENDIF
!
            WRITE (IOU, 7002)
            WRITE (IOU, 7007) KOMPUT
           WRITE (IOU, 7057) KONTRL
           WRITE (IOU, 7008) KOMPIL
           WRITE (IOU, 7038) KALEND
           WRITE (IOU, 7039) IDENTY
           WRITE (IOU, 7061)
            WRITE (IOU, 7062)
            WRITE (IOU, 7063)
            WRITE (IOU, 7064)
            WRITE (IOU, 7065)
            WRITE (IOU, 7066)
            WRITE (IOU, 7067)
            WRITE (IOU, 7071)
            WRITE (IOU, 7072)
            WRITE (IOU, 7068)
            WRITE (IOU, 7069)
!         WRITE ( iou,7001)
            WRITE (IOU, 7004)
            WRITE (IOU, 7005)
            WRITE (IOU, 7011) (MAP1(K),FLOPS1(K),RT1(K),TV1(K),ISPAN1(K)&
     &          ,WT1(K),CSUM1(K),ID(K),K=1,NEFF)
           WRITE (IOU, 7005)
!
            WRITE (IOU, 7023) NEFF, SOM, SUM, RNETO, LV, ND
           WRITE (IOU, 7022)
            WRITE (IOU, 7009) LV
           WRITE (IOU, 7010) NTK
           WRITE (IOU, 7041) STAT1(4)
           WRITE (IOU, 7037) STAT1(14)
           WRITE (IOU, 7033) STAT1(1)
           WRITE (IOU, 7043) STAT1(10)
           WRITE (IOU, 7030) STAT1(7)
           WRITE (IOU, 7055) STAT1(5)
           WRITE (IOU, 7036) STAT1(13)
           WRITE (IOU, 7042) STAT1(3)
           WRITE (IOU, 7001)
            WRITE (IOU, 7044) STAT1(2)
           WRITE (IOU, 7091) AVGEFF
           WRITE (IOU, 7034) PRECIS
!
           IF (NTK /= NEK) THEN
                WRITE (*, 7001)
                 WRITE (*, 7002)
                 WRITE (*, 7090)
                 WRITE (*, 7002)
                 WRITE (*, 7007) KOMPUT
                WRITE (*, 7057) KONTRL
                WRITE (*, 7008) KOMPIL
                WRITE (*, 7038) KALEND
                WRITE (*, 7039) IDENTY
                WRITE (*, 7022)
                 WRITE (*, 7009) LV
                WRITE (*, 7010) NTK
                WRITE (*, 7041) STAT1(4)
                WRITE (*, 7037) STAT1(14)
                WRITE (*, 7033) STAT1(1)
                WRITE (*, 7043) STAT1(10)
                WRITE (*, 7030) STAT1(7)
                WRITE (*, 7055) STAT1(5)
                WRITE (*, 7036) STAT1(13)
                WRITE (*, 7042) STAT1(3)
                WRITE (*, 7001)
                 WRITE (*, 7044) STAT1(2)
                WRITE (*, 7091) AVGEFF
                WRITE (*, 7034) PRECIS
           ENDIF
!
!         WRITE ( iou,7031)  STAT1( 9)
!         WRITE ( iou,7032)  STAT1(15)
!
 7001 FORMAT(/)
 7002 FORMAT(  ' ******************************************** ')
 7003 FORMAT(  ' THE LIVERMORE  FORTRAN KERNELS:  M F L O P S  ')
 7090 FORMAT(  ' THE LIVERMORE  FORTRAN KERNELS:  * SUMMARY *  ')
 7004 FORMAT(/,' KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT  CH', &
     &'ECK-SUMS             OK ')
 7005 FORMAT(  ' ------  -----   --------   --------- ---- ------  --', &
     &'-------------------- -- ')
 7007 FORMAT(/,9X,'     Computer :  ',A )
 7057 FORMAT(  9X,'     System   :  ',A )
 7008 FORMAT(  9X,'     Compiler :  ',A )
 7038 FORMAT(  9X,'     Date     :  ',A )
 7039 FORMAT(  9X,'     Testor   :  ',A )
!7009 FORMAT(/,9X,'     Computer :  ',A8)
!7057 FORMAT(  9X,'     System   :  ',A8)
!7008 FORMAT(  9X,'     Compiler :  ',A8)
!7038 FORMAT(  9X,'     Date     :  ',A8)
 7009 FORMAT(  9X,'Mean DO Span   =  ',I5)
 7010 FORMAT(  9X,'Code Samples   =  ',I5)
 7011 FORMAT(1X,i2,1PE11.4,E11.4,0PF12.4,1X,I4,1X,F6.2,1PE24.16,1X,I2)
!7011 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,1X,F6.2,E35.25,1X,I2)
 7012 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,1X,F6.2)
 7023 FORMAT(1X,i2,E11.4,E11.4,F12.4,1X,I4,30X,I4)
!7022 FORMAT(/,' MFLOPS  RANGE:,23X,28HREPORT ALL RANGE STATISTICS: ')
 7022 FORMAT(/,9X,'MFLOPS    RANGE:',13X,'REPORT ALL RANGE STATISTICS:')
 7041 FORMAT(/,9X,'Maximum   Rate =  ',F12.4,' Mega-Flops/Sec. ')
 7037 FORMAT(  9X,'Quartile  Q3   =  ',F12.4,' Mega-Flops/Sec. ')
 7033 FORMAT(  9X,'Average   Rate =  ',F12.4,' Mega-Flops/Sec. ')
 7043 FORMAT(  9X,'Geometric Mean =  ',F12.4,' Mega-Flops/Sec. ')
 7030 FORMAT(  9X,'Median    Q2   =  ',F12.4,' Mega-Flops/Sec. ')
 7055 FORMAT(  9X,'Harmonic  Mean =  ',F12.4,' Mega-Flops/Sec. ')
 7036 FORMAT(  9X,'Quartile  Q1   =  ',F12.4,' Mega-Flops/Sec. ')
 7042 FORMAT(  9X,'Minimum   Rate =  ',F12.4,' Mega-Flops/Sec. ')
 7044 FORMAT(  9X,'Standard  Dev. =  ',F12.4,' Mega-Flops/Sec. ')
!7031 FORMAT(  9X,'Median    Dev. =  ',F12.4,' Mega-Flops/Sec. ')
!7032 FORMAT(  9X,'Geom.Mean Dev. =  ',F12.4,' Mega-Flops/Sec. ')
 7091 FORMAT(  9X,'Avg Efficiency =  ',F10.2,'%  Program & Processor')
 7034 FORMAT(  9X,'Mean Precision =  ',F10.2,'   Decimal Digits ')
 7053 FORMAT(/,9X,'Frac.  Weights =  ',F12.4)
 7104 FORMAT(/,' KERNEL  FLOPS   MICROSEC   MFLOP/SEC SPAN WEIGHT   ')
 7105 FORMAT(  ' ------  -----   --------   --------- ---- ------   ')
!
 7061 FORMAT(/,9X,'When the computer performance range is very large ')
 7062 FORMAT(9X,'the net Mflops rate of many Fortran programs and    ')
 7063 FORMAT(9X,'workloads will be in the sub-range between the equi-')
 7064 FORMAT(9X,'weighted Harmonic and Arithmetic means depending    ')
 7065 FORMAT(9X,'on the degree of code parallelism and optimization. ')
!7066 FORMAT(9X,'More accurate estimates of cpu workload rates depend')
!7067 FORMAT(9X,'on assigning appropriate weights for each kernel.   ')
!7066 FORMAT(9X,'The best central measure is the Geometric Mean of 72')
!7067 FORMAT(9X,'rates which must be quoted +- a standard deviation. ')
 7066 FORMAT(9X,'The least biased central measure is the Geometric ')
 7067 FORMAT(9X,'Mean of 72 rates,  quoted +- a standard deviation.')
 7068 FORMAT(9X,'LFK test measures a lower bound for a Multi-processor')
 7069 FORMAT(9X,'and N * LFK rates project an upper bound for N-procs.')
 7071 FORMAT(9X,'Mean Mflops rates imply the average efficiency of a')
 7072 FORMAT(9X,'computing system since the peak rate is well known.')
!
           NAME(1) = KOMPUT
           NAME(2) = KOMPUT
           NAME(3) = KOMPIL
           RATE(1) = STAT1(1)
           RATE(2) = STAT1(10)
           RATE(3) = STAT1(5)
           RATE(4) = STAT1(2)
!
           IF (NTK /= NEK) THEN
                WRITE (IOU, 7099)
                 WRITE (IOU, 7097)
                 WRITE (IOU, 7098)
                 WRITE (IOU, 7099)
 7097 FORMAT(' < BOTTOM-LINE:   72 SAMPLES LFK TEST RESULTS SUMMARY. >')
 7098 FORMAT(' < USE RANGE STATISTICS ABOVE FOR OFFICIAL QUOTATIONS. >')
 7099 FORMAT(' <<<<<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>')
                 CALL PAGE (IOU)
!
                IF (IOVEC == 1) THEN
                     WRITE (IOU, 7070)
 7070 FORMAT(//,' TOP QUARTILE: BEST ARCHITECTURE/APPLICATION MATCH ')
!
!      Compute compression index-list MAP1:  Non-zero weights.
!
                      BL = 1.0D-6
                     BU = 1.0D+6
                     CALL VALID (TV, MAP1, MEFF, BL, WT1, BU, NEFF)
!
!      Re-order data sets mapping on IN (descending order of MFlops).
!
                     MAP3(:MEFF) = IN(MAP1(:MEFF))
                     IF (MEFF > 0) CALL TRAP (MAP3, ' REPORT  ', 1, NEFF&
     &                    , MEFF)
!
                     DO K = 1, MEFF
                          I = MAP3(K)
                          FLOPS2(K) = FLOPS1(I)
                          RT2(K) = RT1(I)
                          ISPAN2(K) = ISPAN1(I)
                          WT2(K) = WT1(I)
                          TV2(K) = TV1(I)
                          MAP2(K) = MODI(MAP(I),NEK)
                     END DO
!                             Sort kernels by performance into quartiles
                     NQ = MEFF/4
                     LO = MEFF - 4*NQ
                     LQ(1) = NQ
                     LQ(2) = NQ + NQ + LO
                     LQ(3) = NQ
                     I2 = 0
!
                     DO J = 1, 3
                          I1 = I2 + 1
                          I2 = I2 + LQ(J)
                          LL = I2 - I1 + 1
                          CALL STATW(STAT2,TV,IN2,TV2(I1),WT2(I1),LL)
                          FRAC = STAT2(6)/(TWT + FUZZ)
!
                          WRITE (IOU, 7001)
                           WRITE (IOU, 7104)
                           WRITE (IOU, 7105)
                           WRITE (IOU, 7012) (MAP2(K),FLOPS2(K),RT2(K), &
     &                         TV2(K),ISPAN2(K),WT2(K),K=I1,I2)
                          WRITE (IOU, 7105)
!
                           WRITE (IOU, 7053) FRAC
                          WRITE (IOU, 7033) STAT2(1)
                          WRITE (IOU, 7055) STAT2(5)
                          WRITE (IOU, 7044) STAT2(2)
                     END DO
!
                ENDIF
!
           ENDIF
!
!           Sensitivity analysis of harmonic mean rate to 49 workloads
!
           CALL SENSIT(IOU,RATES,WG,IQ,SUMW,MAP,TV,TV4,TV2,TV5,NTK)
!
!
!           Sensitivity analysis of harmonic mean rate to SISD/SIMD model
!
           CALL SIMD (HM, IOU, RATES, WG, FR, 9, MAP, TV, TV4, TV2, NTK)
!
!
           IF (NTK /= NEK) THEN
                IF (IOVEC == 1) THEN
                     CALL PAGE (IOU)
                     MRL = NRUNS
                     IF (NRUNS > 8) MRL = 8
!
                     DO K = 1, MK
                          DO J = IM, ML
                               SUM = 0.0D0
                               DO I = 1, MRL
                                    SUM = SUM + CSUMS(I,J,K)
                                    CSUMS(I,J,K) = SUM
                               END DO
                          END DO
                     END DO
!
                     DO I = 1, MRL
                          IF (I/=1 .AND. I/=MRL) CYCLE 
                          WRITE (IOU, 76) I
                          WRITE (IOU, 77) (LVL(J),J=1,3)
   76       FORMAT( //,'  Cumulative Checksums:  RUN=',i5)
   77       FORMAT( /,'  k    VL=',i5,3i24)
!
                          DO K = 1, MK
                               WRITE (IOU, 78) K, (CSUMS(I,J,K),J=1,3)
   78       FORMAT( 1X,I2,4E24.16)
                          END DO
                     END DO
                ENDIF
!
                CALL SPEDUP (IOU, NAME, RATE)
           ENDIF
           LVL(IL) = LV
      ENDIF
      CALL TRACK ('REPORT  ')
      RETURN 
!
      END SUBROUTINE REPORT
!**********************************************
      SUBROUTINE RESULT(IOU,FLOPS,TR,RATES,LSPAN,WG,OSUM,TERR,ID)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU
      INTEGER, DIMENSION(141) :: LSPAN, ID
      REAL, DIMENSION(141) :: FLOPS, TR, RATES, WG, OSUM, TERR
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /PROOF/
      COMMON /PROOF/ SUMS(24,3,8)
      DOUBLE PRECISION ::SUMS
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: ISUM, LIMIT, J, K, IJK
      REAL :: TMIN
      DOUBLE PRECISION :: CS
!-----------------------------------------------
!
!
      CALL TRACE ('RESULT  ')
!
      CALL TALLY (IOU, 1)
!
!                             Push Result Arrays Down before entering new result
      ISUM = 0
      LIMIT = 141 - MK
      J = 141
      DO K = LIMIT, 1, -1
           FLOPS(J) = FLOPS(K)
           TR(J) = TR(K)
           RATES(J) = RATES(K)
           LSPAN(J) = LSPAN(K)
           WG(J) = WG(K)
           OSUM(J) = OSUM(K)
           TERR(J) = TERR(K)
           ID(J) = ID(K)
           J = J - 1
      END DO
!
!                             CALCULATE MFLOPS FOR EACH KERNEL
!                          setting RATES(k)= 0. deletes kernel k from REPORT.
      TMIN = 1.0D0*TSECOV
      DO K = 1, MK
           FLOPS(K) = FLOPN(K)*TOTAL(K)
           TR(K) = TIME(K)*1.0D+6
           RATES(K) = 0.0D0
           IF (TR(K) /= 0.0D0) RATES(K) = FLOPS(K)/TR(K)
           IF (WT(K) <= 0.0D0) RATES(K) = 0.0D0
           IF (TIME(K) < TMIN) RATES(K) = 0.0D0
           IF (TIME(K) <= 0.0D0) RATES(K) = 0.0D0
           LSPAN(K) = ISPAN(K,IL)
           WG(K) = WT(K)*WTP(IL)
           OSUM(K) = CSUM(K)
           TERR(K) = TERR1(K)
!
!                 compute relative error and digits of precision in CSUM
!
!
           IJK = 4
           IF (MULTI <= 1) IJK = 1
           IF (MULTI == 10) IJK = 2
           IF (MULTI == 50) IJK = 3
           IF (MULTI >= 100) IJK = 4
           CS = REAL(NRUNS)*SUMS(K,IL,IJK)
           TERR1(K) = CS
      END DO
!
      CALL SEQDIG (ID, ISUM, TERR1, CSUM, MK)
!
      CALL TRACK ('RESULT  ')
      RETURN 
      END SUBROUTINE RESULT
!
!
!**********************************************
      REAL FUNCTION SECOND (OLDSEC)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL OLDSEC
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL, DIMENSION(4) :: CPUTYM
      REAL :: XT
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: ETIME
!-----------------------------------------------
      XT = ETIME(CPUTYM)
      SECOND = CPUTYM(1)
!
! or
!        REAL*4 XTIME(4)
!        INTEGER    CLOCK
!        EXTERNAL   CLOCK
!        XT = REAL( CLOCK( XTIME)) * 1.00d-6
!        SECOND=  XT
!
!*******************************************************************************
!
!     The following statements were used on the DEC  VAX/780  VMS 3.0 .
!     Enable page-fault tallys in TEST by un-commenting LIB$STAT_TIMER calls.
!     Clock resolution is 0.01 Sec.
!
!       DATA  INITIA   /123/
!       IF(   INITIA.EQ.123 )  THEN
!             INITIA= 1
!             NSTAT = LIB$INIT_TIMER()
!       ELSE
!             NSTAT = LIB$STAT_TIMER(2,ISEC)
!             SECOND= REAL(ISEC)*0.01 - OLDSEC
!       ENDIF
!
!* OR less accurately:
!*        REAL    SECNDS
!*        SECOND= SECNDS( OLDSEC)
!
!*****************************************************************************
!
!     The following statements were used on the IBM RS/6000
!     Contrary to what the manual states, INTEGER FUNCTION MCLOCK()
!     returns the number of ticks with 100 ticks being one second.
!
!IBMRS          integer itemp, MCLOCK
!IBMRS          external MCLOCK
!
!IBMRS          itemp = MCLOCK()
!IBMRS          SECOND= REAL(itemp)/100.00d0
!
!*******************************************************************************
!     The following statements were used on the DEC PDP-11/23 RT-11 system.
!
!*       DIMENSION JT(2)
!*       CALL GTIM(JT)
!*       TIME1 = JT(1)
!*       TIME2 = JT(2)
!*       TIME = TIME1 * 65768. + TIME2
!*       SECOND=TIME/60. - OLDSEC
!*******************************************************************************
!
!     The following statements were used on the Hewlett-Packard HP 9000
!
!*       INTEGER*4 ITIME(4)
!*       CALL TIMES( ITIME(4))
!*       TIMEX= ITIME(1) + ITIME(2) + ITIME(3) + ITIME(4)
!*       SECOND= TIMEX/60. - OLDSEC
!
!*******************************************************************************
!
!     FOR THE GOULD 32/87 WITH MPX 3.2  (et seq. gratis D.Lindsay)
!
!     INTEGER*4 NSEC, NCLICK
!     REAL*8 CPUTIM
!
!      CALL M:CLOCK (NSEC, NCLICK)
!      CPUTIM = FLOAT(NSEC)
!      SECOND = CPUTIM + FLOAT(NCLICK)/60.
!
!*******************************************************************************
!
!  FOR THE HP 1000 RUNNING FORTRAN 77.
!  note that since the hp operating system has no facility for
!  returning cpu time, this routine only measures elapsed time.
!  therefore, the tests must be run stand-alone.
!
!     REAL*8 TOTIME
!     INTEGER*2 TIMEA(5)
!
!     CALL EXEC (11, TIMEA)
!     TOTIME = DBLE (TIMEA(1))/100.
!     TOTIME = TOTIME + DBLE (TIMEA(2))
!     TOTIME = TOTIME + DBLE (TIMEA(3)) * 60.
!     SECOND = TOTIME + DBLE (TIMEA(4)) * 3600.
!
!*******************************************************************************
!
!     FOR THE PR1ME SYSTEM UNDER PRIMOS
!
!     REAL*8 CPUTIM
!     INTEGER*2 TIMERS (28)
!
!     CALL TMDAT (TIMERS)
!     SECOND = DBLE (TIMERS(7))
!    .+ DBLE(TIMERS(8)) / DBLE(TIMERS(11))
!
!*******************************************************************************
!
!     The following statements were used on the Stellar
!
!      REAL DUMMY(8)
!      INTEGER*4 TIMES$
!      SAVE IOFSET
!      ITIME= TIMES$( DUMMY)
!      IF( IOFSET.EQ.0 )  IOFSET= ITIME
!      SECOND= (ITIME - IOFSET)/100.0  - OLDSEC
!*******************************************************************************
!
!     The following statements were used on the IBM 3090 VM system.
!     Clock resolution is 1 microsec.
!
!      SECOND= IOCPU(0.0d0)* 1.0d-6
!
!*******************************************************************************
!
!     The following statement was used on the IBM 3090  MVS
!
!**   CALL TODD( xtime)
!     TODD returns microsecs in REAL*8 form
!     TODD provides 1/16th of a microsecond precision
!**   xtime = xtime * 1.0D-6
!     SECOND= xtime - oldsec
!
!********************************
!     REAL*4 TIME(4)
!     xtime = 0.0D-6
!     CALL VCLOCK(time(1))
!     xtime = time(1)
!     SECOND= xtime - oldsec
!
!********************************
!     The following statement was used on the IBM 4381, 9370
!
!     real*8 elapsed(2),cpu(2)
!     call timer(elapsed,cpu)
!     second = cpu(1) - oldsec
!
!
!*******************************************************************************
!
!     The following statements were used on the IBM PC Professional Fortran.
!     Clock resolution is 0.01 Sec.
!
!      INTEGER*2 IHR,IMIN,ISEC,IS100
!      CALL GETTIM(IHR,IMIN,ISEC,IS100)
!      ISECT=(JFIX(IHR)*60+JFIX(IMIN))*60+JFIX(ISEC)
!      SECOND=FLOAT(ISECT)+FLOAT(IS100)/100.0
!
!*******************************************************************************
!
!     THE FOLLOWING STATEMENTS ARE USED ON IBM-PC WITH LAHEY COMPILER
!**   SECOND= REAL( MOD( ITICKS, 1000000)) * 1.0D-2
!
!**   INTEGER*4   ITICKS
!**   CALL TIMER( ITICKS)
!**   SECOND= REAL( ITICKS ) * 1.0D-2
!
!      INTEGER*4  I1, ITICK0, ITICKS
!      SAVE I1, ITICK0
!      DATA I1/-357/, ITICK0/0/
!C
!      IF(  I1.EQ.(-357)) THEN
!         CALL  TIMER( ITICK0)
!      ENDIF
!           I1 = 7
!         CALL  TIMER( ITICKS)
!       SECOND = REAL( ITICKS - ITICK0 ) * 1.0D-2
!
!
!*******************************************************************************
!
!  FOR THE IBM PC.
!  note that the pc's operating system has no facility for
!  returning cpu time; this routine only measures elapsed time.
!  also, the pc does not have real*8.  Remove all references to real*8
!
!      IMPLICIT INTEGER*4 (I-N)
!      LOGICAL FIRST
!      DATA FIRST /.TRUE./
!
!      CALL GETTIM (IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, IFRACT)
!
!  ifract is integer fractions of a second
!  in units of 1/32,768 seconds
!
!      IF (.NOT. FIRST) GO TO 10
!        FIRST = .FALSE.
!
!        LASTHR = IHOUR
!        BASETM = 0.
!10    CONTINUE
!
!  because of limited precision, do not include the time of day
!  in hours in the total time.  but correct for an hour change.
!
!      IF (LASTHR .EQ. IHOUR) GO TO 20
!        BASETM = BASETM + 3600.
!        LASTHR = IHOUR
!
!20    TOTIME = FLOAT(IMIN) * 60
!    . + FLOAT(ISEC)
!    . + FLOAT(IFRACT)/32768.
!      SECOND = TOTIME + BASETM
!
!
      RETURN 
      END FUNCTION SECOND
!
!
!
!
!***********************************************************************
      REAL FUNCTION SECOVT (IOU)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /FAKE1/
      COMMON/FAKE1/T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11(20),T12(20)
      REAL   T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12
!...  /FAKE2/
      COMMON /FAKE2/ TCUM(20)
      REAL   TCUM
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(20) :: INX
      INTEGER :: KLM, IO, JJ, J, I, K
      REAL, DIMENSION(20) :: TIM, TER, TMX
      REAL :: TSECO, ELAPST, TOLER, RERR
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: SECOND, RELERR
!-----------------------------------------------
!
      CALL TRACE ('SECOVT  ')
!
!***********************************************************************
!     Measure  tsecov:  Overhead time for calling SECOND
!***********************************************************************
!
      TSECO = 0.000D0
      KLM = 1600
      IO = ABS(IOU)
      JJ = 0
!
      DO J = 1, 15
!
           T12(:10) = 0.000D0
           TCUM(1) = 0.000D0
           T0 = SECOND(TCUM(1))
!                       assure that 10 calls to SECOND are NOT optimized
           DO K = 1, KLM
                TCUM(:10) = T12(:10)
                T1 = SECOND(TCUM(1))
                T2 = SECOND(TCUM(2))
                T3 = SECOND(TCUM(3))
                T4 = SECOND(TCUM(4))
                T5 = SECOND(TCUM(5))
                T6 = SECOND(TCUM(6))
                T7 = SECOND(TCUM(7))
                T8 = SECOND(TCUM(8))
                T9 = SECOND(TCUM(9))
                T10 = SECOND(TCUM(10))
           END DO
           ELAPST = T10 - T0
           TSECO = ELAPST/(REAL(10*KLM) + 1.0E-9)
           TOLER = 0.020D0
           RERR = 1.000D0
!
!                                  Convergence test:  Rel.error .LT. 1%
           IF (ELAPST > 1.00D04) GO TO 911
           IF (ELAPST<1.00D-10 .AND. J>10) GO TO 911
           IF (ELAPST > 1.00D-9) THEN
                JJ = JJ + 1
                TIM(JJ) = TSECO
                IF (JJ > 1) RERR = RELERR(TIM(JJ),TIM(JJ-1))
                TER(JJ) = RERR
           ENDIF
           IF (IOU > 0) WRITE (IOU, 64) 10*KLM, TSECO, RERR
           IF (RERR < TOLER) GO TO 825
           IF (ELAPST > 10.00D0) EXIT 
           KLM = KLM + KLM
      END DO
!                                  Poor accuracy on exit from loop
      IF (J <= 1) GO TO 911
      IF (JJ < 1) GO TO 911
      CALL SORDID (INX, TMX, TER, JJ, 1)
!
      I = 0
  823 CONTINUE
      I = I + 1
      TSECO = TIM(INX(I))
      RERR = TMX(I)
      IF (TSECO<=0.00D0 .AND. I<JJ) GO TO 823
      IF (RERR > 0.050D0) WRITE (IO, 63) 100.00D0*RERR
!                                  Good convergence, satifies 1% error tolerence
  825 CONTINUE
      SECOVT = TSECO
!
      CALL TRACK ('SECOVT  ')
      RETURN 
!
  911 CONTINUE
      WRITE (IO, 61)
       WRITE (IO, 62) ELAPST, J
      CALL WHERE (0)
!
   61 FORMAT(1X,'FATAL(SECOVT): cant measure overhead time subr SECOND')
   62 FORMAT(/,13X,'using SECOND:  elapst=',1E20.8,6X,'J=',I4)
   63 FORMAT(1X,'WARNING(SECOVT): SECOND overhead time relerr',f9.4,'%')
   64 FORMAT('SECOVT:',I10,E12.4,F11.4)
      END FUNCTION SECOVT
!
!***********************************************************************
      SUBROUTINE SENSIT(IOU,RATES,WG,IQ,SUMW,MAP,TV,TV1,TV2,TV3,N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU, N
      INTEGER, DIMENSION(7) :: IQ
      INTEGER, DIMENSION(N) :: MAP
      REAL, DIMENSION(N) :: RATES, WG
      REAL, DIMENSION(7) :: SUMW
      REAL, DIMENSION(N) :: TV, TV1, TV2, TV3
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(10) :: NR1, NR2
      INTEGER :: MEFF, NEFF, K, MQ, J, I, K1, K2, J1
      REAL, DIMENSION(20) :: STAT2
      REAL :: BL, BU, FUZZ, R, Q, SUMO, P, XT, OT
      CHARACTER, DIMENSION(4) :: TAG*8

      SAVE TAG
!-----------------------------------------------
      DATA (TAG(J1),J1=1,4)/'1st QT: ', '2nd QT: ', '3rd QT: ',         &
     &     '4th QT: '/
!    ./8H1st QT:  , 8H2nd QT:  , 8H3rd QT:  , 8H4th QT:  /
!
      CALL TRACE ('SENSIT  ')
!
!                 Compress valid data sets RATES,  mapping on MAP.
!
      MEFF = 0
      NEFF = 0
      BL = 1.0D-5
      BU = 1.0D+5
      CALL VALID (TV1, MAP, NEFF, BL, RATES, BU, N)
!
      TV3(:NEFF) = WG(MAP(:NEFF))
!
!
!                 Compress valid data sets WG,  mapping on MAP.
!
      CALL VALID (TV3, MAP, MEFF, BL, TV3, BU, NEFF)
!
      TV(:MEFF) = TV1(MAP(:MEFF))
!
!                 Sort selected rates into descending order
!
      CALL SORDID (MAP, TV2, TV, MEFF, 2)
!
!
!
      CALL PAGE (IOU)
      WRITE (IOU, 7001)
!
 7001 FORMAT(/)
 7301 FORMAT(9X,'           SENSITIVITY ANALYSIS ')
 7302 FORMAT(9X,'The sensitivity of the harmonic mean rate (Mflops)  ')
 7303 FORMAT(9X,'to various weightings is shown in the table below.  ')
 7304 FORMAT(9X,'Seven work distributions are generated by assigning ')
 7305 FORMAT(9X,'two distinct weights to ranked kernels by quartiles.')
 7306 FORMAT(9X,'Forty nine possible cpu workloads are then evaluated')
 7307 FORMAT(9X,'using seven sets of values for the total weights:   ')
 7341 FORMAT(3X,A ,6X,'O      O      O      O      O      X      X')
 7342 FORMAT(3X,A ,6X,'O      O      O      X      X      X      O')
 7343 FORMAT(3X,A ,6X,'O      X      X      X      O      O      O')
 7344 FORMAT(3X,A ,6X,'X      X      O      O      O      O      O')
!7341 FORMAT(3X,A7,6X,'O      O      O      O      O      X      X')
!7342 FORMAT(3X,A7,6X,'O      O      O      X      X      X      O')
!7343 FORMAT(3X,A7,6X,'O      X      X      X      O      O      O')
!7344 FORMAT(3X,A7,6X,'X      X      O      O      O      O      O')
 7346 FORMAT(13X,  '------ ------ ------ ------ ------ ------ ------')
 7348 FORMAT(3X,'Total',/,3X,'Weights',20X,'Net Mflops:',/,4X,'X    O')
 7349 FORMAT(2X,'---- ---- ')
 7220 FORMAT(/,1X,2F5.2,1X,7F7.2)
!
       WRITE (IOU, 7001)
       WRITE (IOU, 7001)
       WRITE (IOU, 7301)
       WRITE (IOU, 7001)
       WRITE (IOU, 7302)
       WRITE (IOU, 7303)
       WRITE (IOU, 7304)
       WRITE (IOU, 7305)
       WRITE (IOU, 7306)
       WRITE (IOU, 7307)
       WRITE (IOU, 7001)
       WRITE (IOU, 7346)
       WRITE (IOU, 7341) TAG(1)
      WRITE (IOU, 7342) TAG(2)
      WRITE (IOU, 7343) TAG(3)
      WRITE (IOU, 7344) TAG(4)
      WRITE (IOU, 7346)
       WRITE (IOU, 7348)
       WRITE (IOU, 7349)
!
       IF (MEFF > 0) THEN
           FUZZ = 1.0D-9
           R = MEFF
           MQ = (MEFF + 3)/4
           Q = MQ
           J = 1
           NR1(8) = J
           NR1(9) = J
           NR2(8) = J + MQ + MQ - 1
           NR2(9) = J + MQ - 1
           J = J + MQ
           NR1(6) = J
           NR1(7) = J
           NR2(6) = J + MQ + MQ - 1
           NR2(7) = J + MQ - 1
           J = J + MQ
           NR1(4) = J
           NR1(5) = J
           NR2(4) = J + MQ + MQ - 1
           NR2(5) = J + MQ - 1
           J = J + MQ
           NR1(2) = J
           NR1(3) = J
           NR2(2) = J + MQ + MQ - 1
           NR2(3) = J + MQ - 1
           J = J + MQ
!
           DO J = 1, 7
                SUMO = 1.0D0 - SUMW(J)
                DO I = 1, 7
                     P = IQ(I)*Q
                     XT = SUMW(J)/(P + FUZZ)
                     OT = SUMO/(R - P + FUZZ)
                     TV3(:MEFF) = OT
                     K1 = NR1(I+2)
                     K2 = NR2(I+2)
                     TV3(K1:K2) = XT
                     CALL STATW (STAT2, TV, MAP, TV2, TV3, MEFF)
                     TV1(I) = STAT2(5)
                END DO
                WRITE (IOU, 7220) SUMW(J), SUMO, (TV1(K),K=1,7)
           END DO
!
           WRITE (IOU, 7349)
            WRITE (IOU, 7346)
!
!
      ENDIF
       CALL TRACK ('SENSIT  ')
      RETURN 
      END SUBROUTINE SENSIT
!
!***************************************
      SUBROUTINE SEQDIG(ND, ISUM, A, B, NR)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER ISUM, NR
      INTEGER, DIMENSION(NR) :: ND
      REAL, DIMENSION(NR) :: A, B
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: MAXSD = 16
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: MAXS, K
      REAL :: RELER, ONE, SD, SUM, SE, RE
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: RELERR
!-----------------------------------------------
!   S t a t e m e n t   F u n c t i o n s
!-----------------------------------------------
      INTEGER ISIGDG
!-----------------------------------------------
      ISIGDG(reler)= INT( ABS( LOG10( ABS( reler))) + 0.500d0 )
      CALL TRACE ('SEQDIG  ')
!
!     Try to determine floating-point precision used: Max Sig Digits
!
      MAXS = MAXSD
      ONE = 1.00D0
      SD = 1073741824.00D0
      SUM = SD + ONE
      IF (SUM == SD) MAXS = 8
!
      ISUM = 0
      DO K = 1, NR
           SE = SIGN(ONE,A(K))*SIGN(ONE,B(K))
           IF (SE < 0.0) THEN
                ND(K) = 0
           ELSE
!
!             compute relative error and digits of precision in B.
!
                RE = RELERR(A(K),B(K))
                IF (RE>0.0D0 .AND. RE<1.0D0) THEN
                     ND(K) = ISIGDG(RE)
                ELSE IF (RE == 0.0D0) THEN
                     ND(K) = MAXS
                ELSE IF (RE >= 1.0D0) THEN
                     ND(K) = 0
                ENDIF
                ND(K) = MIN0(MAXS,ND(K))
           ENDIF
           ISUM = ISUM + ND(K)
      END DO
!
      CALL TRACK ('SEQDIG  ')
      RETURN 
      END SUBROUTINE SEQDIG
!
!
!***********************************************
      SUBROUTINE SIGNEL(V, SCALE, BIAS, N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER N
      REAL SCALE, BIAS
      REAL, DIMENSION(N) :: V
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K
      DOUBLE PRECISION :: SCALED, BIASED, FUZZ, BUZZ, FIZZ, ONE
!-----------------------------------------------
!
      CALL TRACE ('SIGNEL  ')
!
      SCALED = SCALE
      BIASED = BIAS
!
      SCALED = 10.00D0
      SCALED = 1.00D0/SCALED
      BIASED = 0.00D0
!
!         FUZZ= 1.234500d-9
      FUZZ = 1.234500D-3
      BUZZ = 1.000D0 + FUZZ
      FIZZ = 1.100D0*FUZZ
      ONE = 1.000D0
!
      DO K = 1, N
           BUZZ = (ONE - FUZZ)*BUZZ + FUZZ
           FUZZ = -FUZZ
!         V(k)=((BUZZ- FIZZ) -BIASED)*SCALED
           V(K) = (BUZZ - FIZZ)*SCALED
      END DO
!
      CALL TRACK ('SIGNEL  ')
      RETURN 
      END SUBROUTINE SIGNEL
!
!
!***********************************************************************
      SUBROUTINE SIMD(HM, IOU, RATES, WG, FR, M, MAP, TV1, TV2, TV3, N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU, M, N
      INTEGER, DIMENSION(N) :: MAP
      REAL, DIMENSION(M) :: HM
      REAL, DIMENSION(N) :: RATES, WG
      REAL, DIMENSION(M) :: FR
      REAL, DIMENSION(N) :: TV1, TV2, TV3
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: MEFF, NEFF, K, MED, LH, NQ
      REAL, DIMENSION(20) :: STAT2
      REAL :: BL, BU, VMF, SMF, FUZZ, G
!-----------------------------------------------
!
      CALL TRACE ('SIMD    ')
!
!                 Compress valid data sets RATES,  mapping on MAP.
!
      MEFF = 0
      NEFF = 0
      BL = 1.0D-5
      BU = 1.0D+5
      CALL VALID (TV1, MAP, NEFF, BL, RATES, BU, N)
!
      TV3(:NEFF) = WG(MAP(:NEFF))
!
!
!                 Compress valid data sets WG,  mapping on MAP.
!
      CALL VALID (TV3, MAP, MEFF, BL, TV3, BU, NEFF)
!
      TV2(:MEFF) = TV1(MAP(:MEFF))
!
!                 Sort RATES,WT into descending order.
!
      CALL STATW (STAT2, TV1, MAP, TV2, TV3, MEFF)
      MED = MEFF + 1 - INT(STAT2(8))
      LH = MEFF + 1 - MED
!
      TV2(:MEFF) = TV3(MAP(:MEFF))
!
!
!                 Estimate vector rate= HMean of top LFK quartile.
!
      NQ = MEFF/4
      CALL STATW (STAT2, TV3, MAP, TV1, TV2, NQ)
      VMF = STAT2(5)
!
!                 Estimate scalar rate= HMean of lowest two LFK quartiles.
!
      CALL STATW (STAT2, TV3, MAP, TV1(MED), TV2(MED), LH)
      SMF = STAT2(5)
      FUZZ = 1.0D-9
!
      G = 1.0D0 - SMF/(VMF + FUZZ)
      HM(1) = SMF
!
      HM(2:M) = SMF/(1.0D0 - FR(2:M)*G+FUZZ)
!
      IF (IOU > 0) THEN
!
           WRITE (IOU, 7001)
            WRITE (IOU, 7001)
            WRITE (IOU, 7001)
            WRITE (IOU, 7101)
            WRITE (IOU, 7102) (HM(K),K=1,9)
           WRITE (IOU, 7102) (FR(K),K=1,9)
           WRITE (IOU, 7103)
            WRITE (IOU, 7001)
 7001 FORMAT(/)
 7101 FORMAT(' SENSITIVITY OF NET MFLOPS RATE TO USE OF OPTIMAL FORTRAN &
     &CODE(SISD/SIMD MODEL)' )
 7102 FORMAT(/,1X,5F7.2,4F8.2)
 7103 FORMAT(3x,' Fraction Of Operations Run At Optimal Fortran Rates')
!
      ENDIF
!
       CALL TRACK ('SIMD    ')
      RETURN 
!
      END SUBROUTINE SIMD
!
!
!***********************************************
      SUBROUTINE SIZES(I)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER I
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /SPACER/
      COMMON /SPACER/ A11, A12, A13, A21, A22, A23, A31, A32, A33, AR,  &
     &     BR, C0, CR, DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28 &
     &     , DN, E3, E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5 &
     &     , T, XNC, XNEI, XNM
      REAL   A11, A12, A13, A21, A22, A23, A31, A32, A33, AR, BR, C0, CR&
     &     , DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28, DN, E3,  &
     &     E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5, T, XNC,  &
     &     XNEI, XNM
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NIF, IUP, IO
!-----------------------------------------------
!
!     ******************************************************************
!
      CALL TRACE ('SIZES   ')
!
      NIF = 0
!                        Set  mk .LE. 47  number of kernels to test.
      MK = 24
      IM = 1
      ML = 3
!                        Set  Nruns .LT. 8  number of timed runs of KERNEL test
!                        Set  Nruns= 1   to REDUCE RUN TIME for debug runs.
      NRUNS = 1
!                        Set  Nruns= 7   for Standard BENCHMARK Test. Maximum.
      NRUNS = 7
      NRUNS = MIN0(7,NRUNS)
!
!                        Set  Mruns= 7   for Standard BENCHMARK Test.
      MRUNS = NRUNS
!
!****************************************************************************
!         OPTIONAL LONG ENDURANCE TEST FOR NEW HARDWARE ACCEPTANCE TESTING.
!         OPTIONAL       Set  Mruns=     for Hardware ENDURANCE TRIAL
!
!         Mruns= Nruns * ( Desired Trial Time(sec) / totjob Time(sec))
!                          where totjob-time is LFK Standard benchmark
!                          test Job-time printed at end of output file.
!
!   e.g.  12 Hour run on CRAY-XMP :   laps = 43200./ 17.5 = 2468
!         12 Hour run on VaxS3500 :   laps = 43200./478.4 =   90
!
!          laps= 1
!****************************************************************************
!
      MRUNS = NRUNS*LAPS
      IF (MRUNS<NRUNS .OR. MRUNS>500000) MRUNS = NRUNS
!
      IF (I == (-1)) GO TO 73
!
!****************************************************************************
!     Domain tests follow to detect overstoring of controls for array opns.
!****************************************************************************
!
      NIF = 1
      IUP = 999000
      IUP = MAX0(65000,IUP)
      IF (I<1 .OR. I-1>24) GO TO 911
      IF (N<0 .OR. N>1001) GO TO 911
      IF (LOOP<0 .OR. LOOP>IUP) GO TO 911
!
      NIF = 2
      IF (IL<1 .OR. IL>3) GO TO 911
      N = ISPAN(I,IL)
      LOOP = IPASS(I,IL)*MUL(IL)
      LOOP = MULTI*LOOP
      LP = LOOP
!
!
!
! MULTI= 10
!        ------    ------    ------   -------   -------   ------------
!        kernel    L:Loop    n:loop   flops*1   flops*n   flops*n*Loop
!        ------    ------    ------   -------   -------   ------------
!   il= 1     1        70      1001         5      5005    350350
!             2       670        97         4       388    259960
!             3        90      1001         2      2002    180180
!             4       140       600         2      1200    168000
!             5       100      1000         2      2000    200000
!             6        30      1984         2      3968    119040
!             7        40       995        16     15920    636800
!             8       100       198        36      7128    712800
!             9       360       101        17      1717    618120
!            10       340       101         9       909    309060
!            11       110      1000         1      1000    110000
!            12       120      1000         1      1000    120000
!            13       360        64         7       448    161280
!            14        20      1001        11     11011    220220
!            15        10       500        33     16500    165000
!            16       250        53        10       530    132500
!            17       350       101         9       909    318150
!            18        20       495        44     21780    435600
!            19       390       101         6       606    236340
!            20        10      1000        26     26000    260000
!            21        10     63125         2    126250   1262500
!            22       110       101        17      1717    188870
!            23        80       495        11      5445    435600
!            24        50      1000         1      1000     50000
!   il= 2     1       800       101         5       505    404000
!             2       800        97         4       388    310400
!             3      1060       101         2       202    214120
!             4      1400        60         2       120    168000
!             5      1100       100         2       200    220000
!             6       140       480         2       960    134400
!             7       440       101        16      1616    711040
!             8       120       198        36      7128    855360
!             9       420       101        17      1717    721140
!            10       380       101         9       909    345420
!            11      1280       100         1       100    128000
!            12      1360       100         1       100    136000
!            13       820        32         7       224    183680
!            14       200       101        11      1111    222200
!            15        20       500        33     16500    330000
!            16       540        28        10       280    151200
!            17       400       101         9       909    363600
!            18        20       495        44     21780    435600
!            19       460       101         6       606    278760
!            20       160       100        26      2600    416000
!            21        20     31250         2     62500   1250000
!            22       140       101        17      1717    240380
!            23       100       495        11      5445    544500
!            24       620       100         1       100     62000
!   il= 3     1      2240        27         5       135    302400
!             2      3680        11         4        44    161920
!             3      2960        27         2        54    159840
!             4      3040        15         2        30     91200
!             5      3200        26         2        52    166400
!             6      1680        24         2        48     80640
!             7      1600        21        16       336    537600
!             8       720        26        36       936    673920
!             9      2080        15        17       255    530400
!            10      2000        15         9       135    270000
!            11      3680        26         1        26     95680
!            12      3840        26         1        26     99840
!            13      2480         8         7        56    138880
!            14       640        27        11       297    190080
!            15        80        70        33      2310    184800
!            16      1120        11        10       110    123200
!            17      2080        15         9       135    280800
!            18       160        65        44      2860    457600
!            19      2240        15         6        90    201600
!            20       560        26        26       676    378560
!            21        80     12500         2     25000   2000000
!            22       640        15        17       255    163200
!            23       560        65        11       715    400400
!            24      1840        26         1        26     47840
!
!omputers with high resolution clocks tic= O(microsec.) should use Loop= 1
!     to show un-initialized as well as encached execution rates.
!
!     Loop= 1
!
      LOOP = MAX0(1,LOOP)
      LP = LOOP
      L = 1
      MPY = 1
      NIF = 3
      IF (N<0 .OR. N>1001) GO TO 911
      IF (LOOP<0 .OR. LOOP>IUP) GO TO 911
      N1 = 1001
      N2 = 101
      N13 = 64
      N13H = 32
      N213 = 96
      N813 = 512
      N14 = 2048
      N16 = 75
      N416 = 300
      N21 = 25
!
      NT1 = 16*1001 + 13*101 + 2*300 + 2048
      NT2 = 4*512 + 3*25*101 + 121*101 + 3*64*64
!
   73 CONTINUE
      CALL TRACK ('SIZES   ')
      RETURN 
!
!
  911 CONTINUE
      IO = ABS(ION)
      IF (IO<=0 .OR. IO>10) IO = 6
      WRITE (IO, 913) I, NIF, N, LOOP, IL
  913 FORMAT('1',///,' FATAL OVERSTORE/ DATA LOSS.  TEST=  ',6I6)
      CALL WHERE (0)
!
      END SUBROUTINE SIZES
!***********************************************
      SUBROUTINE SORDID(I, W, V, N, KIND)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER N, KIND
      INTEGER, DIMENSION(N) :: I
      REAL, DIMENSION(N) :: W, V
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K, J, M
      REAL :: X
!-----------------------------------------------
!
      CALL TRACE ('SORDID  ')
!
      IF (N > 0) THEN
           DO K = 1, N
                W(K) = V(K)
                I(K) = K
           END DO
!
           IF (KIND == 1) THEN
!
                DO J = 1, N - 1
                     M = J
                     DO K = J + 1, N
                          IF (W(K) < W(M)) M = K
                     END DO
                     X = W(J)
                     K = I(J)
                     W(J) = W(M)
                     I(J) = I(M)
                     W(M) = X
                     I(M) = K
                END DO
!
!
           ELSE
!
                DO J = 1, N - 1
                     M = J
                     DO K = J + 1, N
                          IF (W(K) > W(M)) M = K
                     END DO
                     X = W(J)
                     K = I(J)
                     W(J) = W(M)
                     I(J) = I(M)
                     W(M) = X
                     I(M) = K
                END DO
           ENDIF
           IF (N > 0) CALL TRAP (I, ' SORDID  ', 1, N, N)
!
      ENDIF
      CALL TRACK ('SORDID  ')
      RETURN 
      END SUBROUTINE SORDID
!
!***********************************************
      SUBROUTINE SPACE
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ISPACE/
      COMMON /ISPACE/ E(96), F(96), IX(1001), IR(1001), ZONE(300)
      INTEGER   E, F, IX, IR, ZONE
!...  /SPACE1/
      COMMON /SPACE1/ U(1001), V(1001), W(1001), X(1001), Y(1001), Z(   &
     &     1001), G(1001), DU1(101), DU2(101), DU3(101), GRD(1001), DEX(&
     &     1001), XI(1001), EX(1001), EX1(1001), DEX1(1001), VX(1001),  &
     &     XX(1001), RX(1001), RH(2048), VSP(101), VSTP(101), VXNE(101) &
     &     , VXND(101), VE3(101), VLR(101), VLIN(101), B5(101), PLAN(300&
     &     ), D(300), SA(101), SB(101)
      REAL   U, V, W, X, Y, Z, G, DU1, DU2, DU3, GRD, DEX, XI, EX, EX1, &
     &     DEX1, VX, XX, RX, RH, VSP, VSTP, VXNE, VXND, VE3, VLR, VLIN, &
     &     B5, PLAN, D, SA, SB
!...  /SPACE2/
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101), VY(101,25), VH( &
     &     101,7), VF(101,7), VG(101,7), VS(101,7), ZA(101,7), ZP(101,7)&
     &     , ZQ(101,7), ZR(101,7), ZM(101,7), ZB(101,7), ZU(101,7), ZV( &
     &     101,7), ZZ(101,7), B(64,64), C(64,64), H(64,64), U1(5,101,2) &
     &     , U2(5,101,2), U3(5,101,2)
      REAL   P, PX, CX, VY, VH, VF, VG, VS, ZA, ZP, ZQ, ZR, ZM, ZB, ZU, &
     &     ZV, ZZ, B, C, H, U1, U2, U3
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
!
!     ******************************************************************
!
!//      COMMON /POINT/ ME,MF,MU,MV,MW,MX,MY,MZ,MG,MDU1,MDU2,MDU3,MGRD,
!//     1  MDEX,MIX,MXI,MEX,MEX1,MDEX1,MVX,MXX,MIR,MRX,MRH,MVSP,MVSTP,
!//     2  MVXNE,MVXND,MVE3,MVLR,MVLIN,MB5,MPLAN,MZONE,MD,MSA,MSB,
!//     3  MP,MPX,MCX,MVY,MVH,MVF,MVG,MVS,MZA,MZP,MZQ,MZR,MZM,MZB,MZU,
!//     4  MZV,MZZ,MB,MC,MH,MU1,MU2,MU3
!//C
!//CLLL. LOC(X) =.LOC.X
!//C
      CALL TRACE ('SPACE   ')
!//      ME     = LOC( E )
!//      MF     = LOC( F )
!//      MU     = LOC( U )
!//      MV     = LOC( V )
!//      MW     = LOC( W )
!//      MX     = LOC( X )
!//      MY     = LOC( Y )
!//      MZ     = LOC( Z )
!//      MG     = LOC( G )
!//      MDU1   = LOC( DU1 )
!//      MDU2   = LOC( DU2 )
!//      MDU3   = LOC( DU3 )
!//      MGRD   = LOC( GRD )
!//      MDEX   = LOC( DEX )
!//      MIX    = LOC( IX )
!//      MXI    = LOC( XI )
!//      MEX    = LOC( EX )
!//      MEX1   = LOC( EX1 )
!//      MDEX1  = LOC( DEX1 )
!//      MVX    = LOC( VX )
!//      MXX    = LOC( XX )
!//      MIR    = LOC( IR )
!//      MRX    = LOC( RX )
!//      MRH    = LOC( RH )
!//      MVSP   = LOC( VSP )
!//      MVSTP  = LOC( VSTP )
!//      MVXNE  = LOC( VXNE )
!//      MVXND  = LOC( VXND )
!//      MVE3   = LOC( VE3 )
!//      MVLR   = LOC( VLR )
!//      MVLIN  = LOC( VLIN )
!//      MB5    = LOC( B5 )
!//      MPLAN  = LOC( PLAN )
!//      MZONE  = LOC( ZONE )
!//      MD     = LOC( D )
!//      MSA    = LOC( SA )
!//      MSB    = LOC( SB )
!//      MP     = LOC( P )
!//      MPX    = LOC( PX )
!//      MCX    = LOC( CX )
!//      MVY    = LOC( VY )
!//      MVH    = LOC( VH )
!//      MVF    = LOC( VF )
!//      MVG    = LOC( VG )
!//      MVS    = LOC( VS )
!//      MZA    = LOC( ZA )
!//      MZP    = LOC( ZP )
!//      MZQ    = LOC( ZQ )
!//      MZR    = LOC( ZR )
!//      MZM    = LOC( ZM )
!//      MZB    = LOC( ZB )
!//      MZU    = LOC( ZU )
!//      MZV    = LOC( ZV )
!//      MZZ    = LOC( ZZ )
!//      MB     = LOC( B )
!//      MC     = LOC( C )
!//      MH     = LOC( H )
!//      MU1    = LOC( U1 )
!//      MU2    = LOC( U2 )
!//      MU3    = LOC( U3 )
!
      CALL TRACK ('SPACE   ')
      RETURN 
      END SUBROUTINE SPACE
!
!***********************************************************************
      SUBROUTINE SPEDUP(IOU, NAME, RATE)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: ND = 11
      INTEGER, PARAMETER :: NT = 4
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU
      REAL, DIMENSION(NT) :: RATE
      CHARACTER, DIMENSION(NT) :: NAME*8
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /TAGS/
      COMMON /TAGS/ NAMES(ND,NT)
      CHARACTER   NAMES*8
!...  /RATS/
      COMMON /RATS/ RATED(ND,NT)
      REAL   RATED
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: NSYS = 5
      INTEGER, PARAMETER :: NS = NSYS + 1
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K, INSERT, I, J, M
      REAL, DIMENSION(ND) :: RATIO
      REAL :: FUZZ
      CHARACTER :: IJK*8
      CHARACTER, DIMENSION(NT) :: IT*8
!-----------------------------------------------
!
      CALL TRACE ('SPEDUP  ')
!                            Rank computer NAME by its Geometric Mean.
      DO K = 1, NSYS
           IF (RATE(2) > RATED(K,2)) EXIT 
      END DO
      INSERT = K
!                            Pushdown Tables to allow insertion.
      NAMES(ND:1+INSERT:(-1),:NT) = NAMES(ND-1:INSERT:(-1),:NT)
      RATED(ND:1+INSERT:(-1),:NT) = RATED(ND-1:INSERT:(-1),:NT)
!                            Insert new computer NAME
      NAMES(INSERT,:NT) = NAME(:NT)
      RATED(INSERT,:NT) = RATE(:NT)
!                            Print Table of Speed-ups of Mean Rates.
      CALL PAGE (IOU)
      IT(1) = 'AM='
      IT(2) = 'GM='
      IT(3) = 'HM='
      IJK = '--------'
      FUZZ = 1.0D-9
      WRITE (IOU, 111)
       WRITE (IOU, 104)
  104 FORMAT(26X,'TABLE OF SPEED-UP RATIOS OF MEAN RATES (72 Samples)')
       WRITE (IOU, 105)
  105 FORMAT(/,26X,'Arithmetic, Geometric, Harmonic Means (AM,GM,HM)')
       WRITE (IOU, 106)
  106 FORMAT(26X,'The Geometric Mean is the least biased statistic.',/)
       WRITE (IOU, 109) (IJK,M=1,NS)
  109 FORMAT(1X,'--------  ----  ------  ',11(1X,A ))
      WRITE (IOU, 110) (NAMES(M,2),M=1,NS)
  110 FORMAT(1X,'SYSTEM    MEAN  MFLOPS',2X,11(1X,A ))
      WRITE (IOU, 109) (IJK,M=1,NS)
!
      DO I = 1, NS
           WRITE (IOU, 111)
  111 FORMAT(/)
!
            DO J = 1, NT - 1
!
                RATIO(:NS) = RATED(I,J)/(RATED(:NS,J)+FUZZ)
!
                WRITE (IOU, 112) NAMES(I,J), IT(J), RATED(I,J), (RATIO(M&
     &               ),M=1,NS)
  112 FORMAT(1X,A ,2X,A3,F9.3,' :',11F9.3)
           END DO
!
           WRITE (IOU, 114) RATED(I,4)
  114 FORMAT(11X,'SD=',F9.3)
      END DO
!
      CALL TRACK ('SPEDUP  ')
      RETURN 
      END SUBROUTINE SPEDUP
!***********************************************
      SUBROUTINE STATS(STAT, X, N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER N
      REAL, DIMENSION(20) :: STAT
      REAL, DIMENSION(N) :: X
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K
      REAL :: S, A, D, U, V, H
!-----------------------------------------------
!LLL. OPTIMIZE LEVEL G
!
      CALL TRACE ('STATS   ')
!
      STAT(:9) = 0.0
!
      IF (N > 0) THEN
!                             CALCULATE MEAN OF X.
           S = 0.0
           S = SUM(X)
           A = S/N
           STAT(1) = A
!                             CALCULATE STANDARD DEVIATION OF X.
           D = SUM((X - A)**2)
           D = D/N
           STAT(2) = SQRT(D)
!                             CALCULATE MINIMUM OF X.
           U = X(1)
!                             CALCULATE MAXIMUM OF X.
           V = X(1)
           U = MIN(U,MINVAL(X(2:N)))
           V = MAX(V,MAXVAL(X(2:N)))
           STAT(3) = U
           STAT(4) = V
!                             CALCULATE HARMONIC MEAN OF X.
           H = 0.0
           DO K = 1, N
                IF (X(K) /= 0.0) H = H + 1.0/X(K)
           END DO
           IF (H /= 0.0) H = REAL(N)/H
           STAT(5) = H
!
      ENDIF
      CALL TRACK ('STATS   ')
      RETURN 
      END SUBROUTINE STATS
!***********************************************
      SUBROUTINE STATW(STAT, OX, IX, X, W, N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER N
      INTEGER, DIMENSION(N) :: IX
      REAL, DIMENSION(20) :: STAT
      REAL, DIMENSION(N) :: OX, X, W
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K
      REAL :: STIN09, STIN13, STIN14, A, S, T, D, E, F, Q, U, B, V, H,  &
     &     EW, QT, R, G, POWTEN, DXD, GM
!-----------------------------------------------
!LLL. OPTIMIZE LEVEL G
!
      CALL TRACE ('STATW   ')
      STIN09 = 0.00D0
      STIN13 = 0.00D0
      STIN14 = 0.00D0
!
      STAT(:15) = 0.0D0
!
      IF (N > 0) THEN
!
           IF (N == 1) THEN
                STAT(1) = X(1)
                STAT(3) = X(1)
                STAT(4) = X(1)
                STAT(5) = X(1)
                STAT(6) = W(1)
                STAT(7) = X(1)
                STAT(8) = 1.0D0
                STAT(10) = X(1)
                GO TO 73
           ENDIF
!
!
!                             CALCULATE MEAN OF X.
           A = 0.0D0
           S = 0.0D0
           T = 0.0D0
!
           S = DOT_PRODUCT(W(:N),X(:N))
           T = SUM(W(:N))
           IF (T /= 0.0D0) A = S/T
           STAT(1) = A
!                             CALCULATE STANDARD DEVIATION OF X.
           D = 0.0D0
           E = 0.0D0
           F = 0.0D0
           Q = 0.0D0
           U = 0.0D0
!
           DO K = 1, N
                B = W(K)*(X(K)-A)**2
                D = D + B
                E = E + B*(X(K)-A)
                F = F + B*(X(K)-A)**2
           END DO
           IF (T /= 0.0D0) Q = 1.0D0/T
           D = D*Q
           E = E*Q
           F = F*Q
           IF (D >= 0.0D0) U = SQRT(D)
           STAT(2) = U
!                             CALCULATE MINIMUM OF X.
           U = X(1)
!                             CALCULATE MAXIMUM OF X.
           V = X(1)
           U = MIN(U,MINVAL(X(2:N)))
           V = MAX(V,MAXVAL(X(2:N)))
           STAT(3) = U
           STAT(4) = V
!                             CALCULATE HARMONIC MEAN OF X.
           H = 0.0D0
           DO K = 1, N
                IF (X(K) /= 0.0D0) H = H + W(K)/X(K)
           END DO
           IF (H /= 0.0D0) H = T/H
           STAT(5) = H
           STAT(6) = T
!                             CALCULATE WEIGHTED MEDIAN
           CALL SORDID (IX, OX, X, N, 1)
!
           EW = 0.0D0
           DO K = 2, N
                IF (W(1) /= W(K)) GO TO 75
           END DO
           EW = 1.0D0
   75      CONTINUE
!
           QT = 0.500D0
           CALL TILE (STAT(7), STAT(8), OX, IX, W, EW, T, QT, N)
!
           QT = 0.250D0
           CALL TILE (STAT(13), STIN13, OX, IX, W, EW, T, QT, N)
!
           QT = 0.750D0
           CALL TILE (STAT(14), STIN14, OX, IX, W, EW, T, QT, N)
!
!
!                           CALCULATE ROBUST MEDIAN ABSOLUTE DEVIATION (MAD)
           OX(:N) = ABS(X(:N)-STAT(7))
!
           CALL SORDID (IX, OX, OX, N, 1)
!
           QT = 0.700D0
           CALL TILE (STAT(9), STIN09, OX, IX, W, EW, T, QT, N)
!
!                             CALCULATE GEOMETRIC MEAN
           R = 0.0D0
           DO K = 1, N
                IF (X(K) <= 0.0D0) CYCLE 
                R = R + W(K)*LOG10(X(K))
           END DO
           U = R*Q
           G = 10.0D0
           IF (U < 0.0D0) G = 0.1D0
           POWTEN = 50.0D0
           IF (ABS(U) > POWTEN) U = SIGN(POWTEN,U)
           STAT(10) = G**ABS(U)
!
!                             CALCULATE MOMENTAL SKEWNESS
           G = 0.0D0
           DXD = D*D
           IF (DXD /= 0.0D0) G = 1.0D0/DXD
           STAT(11) = 0.50D0*E*G*STAT(2)
!
!                             CALCULATE KURTOSIS
           STAT(12) = 0.50D0*(F*G - 3.0D0)
!
!                             CALCULATE DEVIATION OF GEOMETRIC MEAN
           Q = 0.0D0
           U = 0.0D0
           GM = STAT(10)
!
           D = SUM(W(:N)*(X(:N)-GM)**2)
           IF (T /= 0.0D0) Q = 1.0D0/T
           D = D*Q
           IF (D >= 0.0D0) U = SQRT(D)
           STAT(15) = U
!
!                             CALCULATE DESCENDING ORDERED X.
           CALL SORDID (IX, OX, X, N, 2)
      ENDIF
!
   73 CONTINUE
      CALL TRACK ('STATW   ')
      RETURN 
      END SUBROUTINE STATW
!
!***********************************************
      REAL FUNCTION SUMO (V, N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER N
      REAL, DIMENSION(N) :: V
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K
      DOUBLE PRECISION :: S
!-----------------------------------------------
!
      CALL TRACE ('SUMO    ')
      S = 0.00D0
!
      DO K = 1, N
           S = S + REAL(K)*V(K)
      END DO
      SUMO = S
      CALL TRACK ('SUMO    ')
      RETURN 
      END FUNCTION SUMO
!
!***********************************************
      SUBROUTINE SUPPLY(I)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER I
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /CKSUMS/
      COMMON /CKSUMS/ CKSUMU, CKOLDU, CKSUMP, CKOLDP, CKSUMA, CKOLDA
      REAL   CKSUMU, CKOLDU, CKSUMP, CKOLDP, CKSUMA, CKOLDA
!...  /SPACE1/
      COMMON /SPACE1/ U(19977)
      REAL   U
!...  /SPACE2/
      COMMON /SPACE2/ P(34132)
      REAL   P
!...  /SPACER/
      COMMON /SPACER/ A11(39)
      REAL   A11
!...  /BASE1/
      COMMON /BASE1/ BUFU(19977)
      REAL   BUFU
!...  /BASE2/
      COMMON /BASE2/ BUFP(34132)
      REAL   BUFP
!...  /BASER/
      COMMON /BASER/ BUFA(39)
      REAL   BUFA
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: IP1, NT0, J, K, IOU
      REAL, DIMENSION(4,512) :: P0
      DOUBLE PRECISION :: DS, DW
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: SUMO
!-----------------------------------------------
      EQUIVALENCE(BUFP,P0)
!
!/C kleiner
!/      COMMON /BASE1/ BUFU( 2136)
!/      COMMON /BASE2/ BUFP( 2938)
!
      CALL TRACE ('SUPPLY  ')
!
      IP1 = I
      NT0 = 39
!               Execute SIGNEL calls only once; re-use generated data.
      IBUF = IBUF + 1
      IF (IBUF == 1) THEN
           CALL SIGNEL (BUFU, SKALE(IP1), BIAS(IP1), NT1)
           CALL SIGNEL (BUFP, SKALE(IP1), BIAS(IP1), NT2)
           CALL SIGNEL (BUFA, SKALE(IP1), BIAS(IP1), NT0)
           DS = 1.000D0
           DW = 0.500D0
           DO K = 1, 512
                P0(1,K) = DS
                DS = DS + DW
           END DO
           DO K = 1, 512
                P0(2,K) = DS
                DS = DS + DW
           END DO
           DO K = 1, 512
                P0(3,K) = DS
                DS = DS + DW
           END DO
           DO K = 1, 512
                P0(4,K) = DS
                DS = DS + DW
           END DO
      ENDIF
!
!                                       Test for Trashing Data in BUF
      IDEBUG = 0
      IF (IDEBUG==1 .OR. IBUF==1 .OR. I==24-1) THEN
!
           CKSUMU = SUMO(BUFU,NT1)
           CKSUMP = SUMO(BUFP,NT2)
           CKSUMA = SUMO(BUFA,NT0)
!
           IF (IBUF == 1) THEN
                CKOLDU = CKSUMU
                CKOLDP = CKSUMP
                CKOLDA = CKSUMA
           ELSE IF (CKSUMU/=CKOLDU .OR. CKSUMP/=CKOLDP .OR. CKSUMA/=    &
     &               CKOLDA) THEN
                IOU = ABS(ION)
                WRITE (IOU, 111) JR, IL, IK
                WRITE (IOU, 112) CKOLDU, CKOLDP, CKOLDA
                WRITE (IOU, 113) CKSUMU, CKSUMP, CKSUMA
  111 FORMAT(' SUPPLY: OVERSTORED! Trial=',I2,' Pass=',I2,' Kernel=',I3)
  112 FORMAT(' ckold:',3E24.15)
  113 FORMAT(' cksum:',3E24.15)
           ENDIF
      ENDIF
!                             Refill Work-Space from copies in Buffers
      A11(:NT0) = BUFA(:NT0)
      U(:NT1) = BUFU(:NT1)
      P(:NT2) = BUFP(:NT2)
!
      CALL TRACK ('SUPPLY  ')
      RETURN 
      END SUBROUTINE SUPPLY
!
!***********************************************************************
      SUBROUTINE TALLY(IOU, MODE)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU, MODE
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: M, J, K, I, NPFT, K4
      REAL, DIMENSION(20) :: S1, S2, S3, S4
      REAL, DIMENSION(47) :: T1, T4
      DOUBLE PRECISION :: CS
!-----------------------------------------------
!
      CALL TRACE ('TALLY   ')
!
      CALL SIZES (-1)
!
      M = 1
      IF (MODE == 2) M = 3
      CALL PAGE (IOU)
      WRITE (IOU, 99)
       WRITE (IOU, 100)
!                        Checks valid domain for min and max of data sets
       DO J = 1, NRUNS
           WRITE (IOU, 102) J, (SEE(K,1,J,IL),K=1,2)
           T1(J) = SEE(1,1,J,IL)
           I = 0
           IF (SEE(3,2,J,IL)<0.01 .OR. SEE(4,2,J,IL)>1.0) I = I + 1
           IF (SEE(3,3,J,IL)<0.01 .OR. SEE(4,3,J,IL)>1.0) I = I + 1
           IF (I > 0) WRITE (IOU, 131) J, IL
           IF (J==NRUNS .OR. I>0) THEN
                WRITE (IOU, 104) J, (SEE(K,2,J,IL),K=1,4)
                WRITE (IOU, 104) J, (SEE(K,3,J,IL),K=1,4)
           ENDIF
      END DO
!
      CALL STATS (S1, T1, NRUNS)
      WRITE (IOU, 102) NRUNS, (S1(K),K=1,4)
!
!
!
      WRITE (IOU, 120) NRUNS
      WRITE (IOU, 122)
       WRITE (IOU, 121)
       WRITE (IOU, 122)
!                        Computes and Checks experimental timing errors
       DO K = 1, MK
           NPFT = 0
           CS = 0.0D0
!
           NPFT = SUM(NPFS(:NRUNS,IL,K))
           CS = CS + SUM(DBLE(CSUMS(:NRUNS,IL,K)))
!
           CALL STATS (S2, TIMES(1,IL,K), NRUNS)
           TIME(K) = S2(M)
           CSUM(K) = CS
           TERR1(K) = 100.0D0*(S2(2)/(S2(1)+1.0D-9))
           T4(K) = TERR1(K)
!
!
!     If this clock resolution test fails, you must increase Loop (Subr. SIZES)
!
           CALL STATS (S3, TERRS(1,IL,K), NRUNS)
           IF (S3(1) > 15.0) WRITE (IOU, 113) K
!
           WRITE(IOU,123)K,S2(3),S2(1),S2(4),TERR1(K),S3(1),NPFT
           TERR1(K) = MAX(TERR1(K),S3(1))
           CALL STATS (S1, DOS(1,IL,K), NRUNS)
           TOTAL(K) = S1(1)
           IF (S1(1)<=0.0D0 .OR. ABS(S1(3)-S1(4))>1.0D-5) WRITE (IOU,   &
     &          131) IL, K, (S1(K4),K4=1,4)
           CALL STATS (S4, FOPN(1,IL,K), NRUNS)
           FLOPN(K) = S4(1)
           IF (S4(1)<=0.0D0 .OR. ABS(S4(3)-S4(4))>1.0D-5) WRITE (IOU,   &
     &          131) IL, K, (S4(K4),K4=1,4)
      END DO
!
      WRITE (IOU, 122)
       CALL STATS (S4, T4, MK)
      WRITE (*, 124)
       WRITE (*, 133)
       WRITE (*, 125) (S4(K),K=1,4)
      WRITE (IOU, 124)
       WRITE (IOU, 133)
       WRITE (IOU, 125) (S4(K),K=1,4)
!
      CALL TRACK ('TALLY   ')
      RETURN 
!
   99 FORMAT(//,' time TEST overhead (t err):  ')
  100 FORMAT(/,6X,'RUN',8X,'AVERAGE',8X,'STANDEV',8X,'MINIMUM',8X,      &
     & 'MAXIMUM')
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
      END SUBROUTINE TALLY
!
!***********************************************
      SUBROUTINE TDIGIT(DERR, NZD, S)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NZD
      REAL DERR, S
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: N, K
      DOUBLE PRECISION :: FUZZ, X, Y, V, Z
!-----------------------------------------------
!   S t a t e m e n t   F u n c t i o n s
!-----------------------------------------------
      DOUBLE PRECISION FRAC
!-----------------------------------------------
      frac(z)= ( ABS( ABS(z) - AINT(ABS(z))))
!
      CALL TRACE ('TDIGIT  ')
!
      X = 0.00D0
      N = 14
      X = ABS(S)
      FUZZ = 1.0D-6
      DERR = 100.0D0
      NZD = 0
      IF (X /= 0.0D0) THEN
!                                  Normalize x
           Y = LOG10(X)
           V = REAL(10**(ABS(INT(Y)) + 1))
!
           IF (Y>=0.0D0 .AND. V/=0.0D0) THEN
                X = (X/V)*10.0D0
           ELSE
                X = X*V
           ENDIF
!                                  Multiply x Until Trailing Digits= Fuzz
           DO K = 1, N
                IF (1.0D0 - FRAC(X)<=FUZZ .OR. FRAC(X)<=FUZZ) EXIT 
                X = 10.0D0*X
           END DO
!
           IF (X /= 0.0D0) THEN
                DERR = 50.0D0/X
                NZD = INT(LOG10(ABS(9.999999990D0*X)))
           ENDIF
!
      ENDIF
      CALL TRACK ('TDIGIT  ')
      RETURN 
      END SUBROUTINE TDIGIT
!
!*************************************************
      INTEGER FUNCTION TEST (I)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER I
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /ISPACE/
      COMMON /ISPACE/ E(96), F(96), IX(1001), IR(1001), ZONE(300)
      INTEGER   E, F, IX, IR, ZONE
!...  /SPACER/
      COMMON /SPACER/ A11, A12, A13, A21, A22, A23, A31, A32, A33, AR,  &
     &     BR, C0, CR, DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28 &
     &     , DN, E3, E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5 &
     &     , T, XNC, XNEI, XNM
      REAL   A11, A12, A13, A21, A22, A23, A31, A32, A33, AR, BR, C0, CR&
     &     , DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28, DN, E3,  &
     &     E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5, T, XNC,  &
     &     XNEI, XNM
!...  /SPACE1/
      COMMON /SPACE1/ U(1001), V(1001), W(1001), X(1001), Y(1001), Z(   &
     &     1001), G(1001), DU1(101), DU2(101), DU3(101), GRD(1001), DEX(&
     &     1001), XI(1001), EX(1001), EX1(1001), DEX1(1001), VX(1001),  &
     &     XX(1001), RX(1001), RH(2048), VSP(101), VSTP(101), VXNE(101) &
     &     , VXND(101), VE3(101), VLR(101), VLIN(101), B5(101), PLAN(300&
     &     ), D(300), SA(101), SB(101)
      REAL   U, V, W, X, Y, Z, G, DU1, DU2, DU3, GRD, DEX, XI, EX, EX1, &
     &     DEX1, VX, XX, RX, RH, VSP, VSTP, VXNE, VXND, VE3, VLR, VLIN, &
     &     B5, PLAN, D, SA, SB
!...  /SPACE2/
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101), VY(101,25), VH( &
     &     101,7), VF(101,7), VG(101,7), VS(101,7), ZA(101,7), ZP(101,7)&
     &     , ZQ(101,7), ZR(101,7), ZM(101,7), ZB(101,7), ZU(101,7), ZV( &
     &     101,7), ZZ(101,7), B(64,64), C(64,64), H(64,64), U1(5,101,2) &
     &     , U2(5,101,2), U3(5,101,2)
      REAL   P, PX, CX, VY, VH, VF, VG, VS, ZA, ZP, ZQ, ZR, ZM, ZB, ZU, &
     &     ZV, ZZ, B, C, H, U1, U2, U3
!...  /BASER/
      COMMON /BASER/ A110, A120, A130, A210, A220, A230, A310, A320,    &
     &     A330, AR0, BR0, C00, CR0, DI0, DK0, DM220, DM230, DM240,     &
     &     DM250, DM260, DM270, DM280, DN0, E30, E60, EXPMAX0, FLX0, Q0 &
     &     , QA0, R0, RI0, S0, SCALE0, SIG0, STB50, T0, XNC0, XNEI0,    &
     &     XNM0
      REAL   A110, A120, A130, A210, A220, A230, A310, A320, A330, AR0, &
     &     BR0, C00, CR0, DI0, DK0, DM220, DM230, DM240, DM250, DM260,  &
     &     DM270, DM280, DN0, E30, E60, EXPMAX0, FLX0, Q0, QA0, R0, RI0 &
     &     , S0, SCALE0, SIG0, STB50, T0, XNC0, XNEI0, XNM0
!...  /BASE1/
      COMMON /BASE1/ U0(1001), V0(1001), W0(1001), X0(1001), Y0(1001),  &
     &     Z0(1001), G0(1001), DU10(101), DU20(101), DU30(101), GRD0(   &
     &     1001), DEX0(1001), XI0(1001), EX0(1001), EX10(1001), DEX10(  &
     &     1001), VX0(1001), XX0(1001), RX0(1001), RH0(2048), VSP0(101) &
     &     , VSTP0(101), VXNE0(101), VXND0(101), VE30(101), VLR0(101),  &
     &     VLIN0(101), B50(101), PLAN0(300), D0(300), SA0(101), SB0(101)
      REAL   U0, V0, W0, X0, Y0, Z0, G0, DU10, DU20, DU30, GRD0, DEX0,  &
     &     XI0, EX0, EX10, DEX10, VX0, XX0, RX0, RH0, VSP0, VSTP0, VXNE0&
     &     , VXND0, VE30, VLR0, VLIN0, B50, PLAN0, D0, SA0, SB0
!...  /BASE2/
      COMMON /BASE2/ P0(4,512), PX0(25,101), CX0(25,101), VY0(101,25),  &
     &     VH0(101,7), VF0(101,7), VG0(101,7), VS0(101,7), ZA0(101,7),  &
     &     ZP0(101,7), ZQ0(101,7), ZR0(101,7), ZM0(101,7), ZB0(101,7),  &
     &     ZU0(101,7), ZV0(101,7), ZZ0(101,7), B0(64,64), CC0(64,64), H0&
     &     (64,64), U10(5,101,2), U20(5,101,2), U30(5,101,2)
      REAL   P0, PX0, CX0, VY0, VH0, VF0, VG0, VS0, ZA0, ZP0, ZQ0, ZR0, &
     &     ZM0, ZB0, ZU0, ZV0, ZZ0, B0, CC0, H0, U10, U20, U30
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: IK, NN, K, J
      REAL, DIMENSION(1023) :: ZX
      REAL, DIMENSION(1500) :: XZ
      REAL :: TEMPUS
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: SECOND
!-----------------------------------------------
      EQUIVALENCE ( ZX(1), Z(1)), ( XZ(1), X(1))
!
!
!*******************************************************************************
!         Repeat execution of each Kernel(i) :     DO 1 L= 1,Loop   etc.
!*******************************************************************************
!
!    From the beginning in 1970 each sample kernel was executed just
!    once since supercomputers had high resolution, microsecond clocks.
!    In 1982 a repetition Loop was placed around each of the 24 LFK
!    kernels in order to run each kernel long enough for accurate
!    timing on mini-computer systems with poor cpu-clock resolution since
!    the majority of systems could only measure cpu-time to 0.01 seconds.
!    By 1990 however, several compilers' optimizers were factoring or
!    hoisting invariant computation outside some repetition Loops thus
!    distorting those Fortran samples.  The effect was usually absurd
!    Mflop rates which had to be corrected with compiler directives.
!    Therefore, in April 1990 these repetition Loops were removed from
!    subroutine KERNEL and submerged in subroutine TEST beyond the scope
!    of compiler optimizations.   Thus the 24 samples are now foolproof
!    and it will no longer be necessary to double check the machine code.
!
!    Very accurate, convergent methods have been developed to measure the
!    overhead time used for subroutines SECOND and TEST in subroutines
!    SECOVT and TICK respectively.  Thus, the LFK test may use substantially
!    more cpu time on systems with poor cpu-clock resolution.
!    The 24 C verison tests in CERNEL have also been revised to correspond with
!    the Fortran KERNEL. The 24 computation samples have NOT been changed.
!
!*******************************************************************************
!
!bug  IF( (LP.NE.Loop).OR.(L.LT.1).OR.(L.GT.Loop)) THEN
!bug      CALL TRACE('TEST    ')
!bug      CALL WHERE(0)
!bug  ENDIF
!                                    Repeat kernel test:   Loop times.
      IF (L < LOOP) THEN
           L = L + 1
           TEST = L
           RETURN 
      ENDIF
!                                    Repeat kernel test:   Loop*Loops2
      IK = I
      IF (MPY < LOOPS2) THEN
           MPY = MPY + 1
           NN = N
!
           IF (I /= 0) THEN
                IF (I<0 .OR. I>24) THEN
                     CALL TRACE ('TEST    ')
                     CALL WHERE (0)
                ENDIF
!                   RE-INITIALIZE OVER-STORED INPUTS:
!
                GO TO (100,2,100,4,5,6,100,100,100,10,100,100,13,14,100 &
     &               ,16,17,18,19,20,21,100,23,100,100) I
!
!     When MULTI.GE.100 each kernel is executed over a million times
!     and the time used to re-intialize overstored input variables
!     is negligible.  Thus each kernel may be run arbitrarily many times
!     (MULTI >> 100) without overflow and produce verifiable checksums.
!
!***********************************************************************
!
    2           CONTINUE
                X(:NN) = X0(:NN)
                GO TO 100
!***************************************
!
    4           CONTINUE
                M = (1001 - 7)/2
                XZ(7:1001:M) = X0(7:1001:M)
                GO TO 100
!***************************************
!
    5           CONTINUE
                X(:NN) = X0(:NN)
                GO TO 100
!***************************************
!
    6           CONTINUE
                W(:NN) = W0(:NN)
                GO TO 100
!***************************************
!
   10           CONTINUE
                PX(5:13,:NN) = PX0(5:13,:NN)
                GO TO 100
!***************************************
!
   13           CONTINUE
                P(1,:NN) = P0(1,:NN)
                P(2,:NN) = P0(2,:NN)
                P(3,:NN) = P0(3,:NN)
                P(4,:NN) = P0(4,:NN)
!
                H = H0
                GO TO 100
!***************************************
!
   14           CONTINUE
                DO K = 1, NN
                     RH(IR(K)) = RH0(IR(K))
                     RH(IR(K)+1) = RH0(IR(K)+1)
                END DO
                GO TO 100
!***************************************
!
   16           CONTINUE
                K2 = 0
                K3 = 0
                GO TO 100
!***************************************
!
   17           CONTINUE
                VXNE(:NN) = VXNE0(:NN)
                GO TO 100
!***************************************
!
   18           CONTINUE
                ZU(2:NN,2:6) = ZU0(2:NN,2:6)
                ZV(2:NN,2:6) = ZV0(2:NN,2:6)
                ZR(2:NN,2:6) = ZR0(2:NN,2:6)
                ZZ(2:NN,2:6) = ZZ0(2:NN,2:6)
                GO TO 100
!***************************************
!
   19           CONTINUE
                STB5 = STB50
                GO TO 100
!***************************************
!
   20           CONTINUE
                XX(1) = XX0(1)
                GO TO 100
!***************************************
!
   21           CONTINUE
                PX(:,:NN) = PX0(:,:NN)
                GO TO 100
!***************************************
!
   23           CONTINUE
                ZA(2:NN,2:6) = ZA0(2:NN,2:6)
           ENDIF
!***********************************************************************
!
  100      CONTINUE
!
           L = 1
           TEST = 1
           RETURN 
      ENDIF
!
      MPY = 1
      L = 1
      TEST = 0
!                                   switchback to TICK to measure testov
      IF (I == (-73)) RETURN 
!
!***********************************************************************
!           t= second(0)  := cumulative cpu time for task in seconds.
!***********************************************************************
!
      CUMTIM(1) = 0.0D0
      TEMPUS = SECOND(CUMTIM(1)) - START
!
      CALL TRACE ('TEST    ')
!PFM      ikern= i
!PFM      call ENDPFM(ion)
!$C                           5 get number of page faults (optional)
!$      KSTAT= LIB$STAT_TIMER(5,KPF)
!$      NPF  = KPF - IPF
!
!
!                             Checksum results; re-initialize all inputs
      CALL TESTS (I, TEMPUS)
!
!
!$C                           5 get number of page faults (optional) VAX
!$      NSTAT= LIB$STAT_TIMER(5,IPF)
!
!PFM       IF( INIPFM( ion, 0) .NE. 0 )  THEN
!PFM           CALL WHERE(20)
!PFM       ENDIF
      CALL TRACK ('TEST    ')
!
!      The following pause can be used for stop-watch timing of each kernel.
!      You may have to increase the iteration count MULTI in Subr. VERIFY.
!
!/           PAUSE
!
      MPY = 1
      MPYLIM = LOOPS2
      L = 1
      LP = LOOP
      IK = I + 1
      TEST = 0
      CUMTIM(1) = 0.0D0
      START = SECOND(CUMTIM(1))
      RETURN 
!
!$      DATA  IPF/0/, KPF/0/
      END FUNCTION TEST
!
!***********************************************
      SUBROUTINE TESTS(I, TEMPUS)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER I
      REAL TEMPUS
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /SPACER/
      COMMON /SPACER/ A11, A12, A13, A21, A22, A23, A31, A32, A33, AR,  &
     &     BR, C0, CR, DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28 &
     &     , DN, E3, E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5 &
     &     , T, XNC, XNEI, XNM
      REAL   A11, A12, A13, A21, A22, A23, A31, A32, A33, AR, BR, C0, CR&
     &     , DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28, DN, E3,  &
     &     E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5, T, XNC,  &
     &     XNEI, XNM
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!...  /ISPACE/
      COMMON /ISPACE/ E(96), F(96), IX(1001), IR(1001), ZONE(300)
      INTEGER   E, F, IX, IR, ZONE
!...  /SPACE1/
      COMMON /SPACE1/ U(1001), V(1001), W(1001), X(1001), Y(1001), Z(   &
     &     1001), G(1001), DU1(101), DU2(101), DU3(101), GRD(1001), DEX(&
     &     1001), XI(1001), EX(1001), EX1(1001), DEX1(1001), VX(1001),  &
     &     XX(1001), RX(1001), RH(2048), VSP(101), VSTP(101), VXNE(101) &
     &     , VXND(101), VE3(101), VLR(101), VLIN(101), B5(101), PLAN(300&
     &     ), D(300), SA(101), SB(101)
      REAL   U, V, W, X, Y, Z, G, DU1, DU2, DU3, GRD, DEX, XI, EX, EX1, &
     &     DEX1, VX, XX, RX, RH, VSP, VSTP, VXNE, VXND, VE3, VLR, VLIN, &
     &     B5, PLAN, D, SA, SB
!...  /SPACE2/
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101), VY(101,25), VH( &
     &     101,7), VF(101,7), VG(101,7), VS(101,7), ZA(101,7), ZP(101,7)&
     &     , ZQ(101,7), ZR(101,7), ZM(101,7), ZB(101,7), ZU(101,7), ZV( &
     &     101,7), ZZ(101,7), B(64,64), C(64,64), H(64,64), U1(5,101,2) &
     &     , U2(5,101,2), U3(5,101,2)
      REAL   P, PX, CX, VY, VH, VF, VG, VS, ZA, ZP, ZQ, ZR, ZM, ZB, ZU, &
     &     ZV, ZZ, B, C, H, U1, U2, U3
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NP, NN, MM, K
      REAL :: OVERR
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: SUMO
!-----------------------------------------------
!
      IK = I
      CALL TRACE ('TESTS   ')
!
      NP = LOOP*LOOPS2
      LOOP = 1
      LP = LOOP
      NN = N
      IF (I<0 .OR. I>24) CALL WHERE (0)
!
      IF (I /= 0) THEN
           CALL SIZES (I)
!
!     Net Time=  Timing - Overhead Time
!
           TIME(I) = TEMPUS - REAL(NP)*TESTOV - TSECOV
!
!
           GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21 &
     &          ,22,23,24,25) I
!
!
!
!***********************************************************************
!
    1      CONTINUE
           CSUM(1) = SUMO(X,N)
           TOTAL(1) = NP*NN
           GO TO 100
!***********************************************************************
!
    2      CONTINUE
           CSUM(2) = SUMO(X,2*N)
           TOTAL(2) = NP*(NN - 4)
           GO TO 100
!***********************************************************************
!
    3      CONTINUE
           CSUM(3) = Q
           TOTAL(3) = NP*NN
           GO TO 100
!***********************************************************************
!
    4      CONTINUE
           MM = (1001 - 7)/2
           V(7:1001:MM) = X(7:1001:MM)
           CSUM(4) = SUMO(V,3)
           TOTAL(4) = NP*((NN - 5)/5 + 1)*3
           GO TO 100
!***********************************************************************
!
    5      CONTINUE
           CSUM(5) = SUMO(X(2),N-1)
           TOTAL(5) = NP*(NN - 1)
           GO TO 100
!***********************************************************************
!
    6      CONTINUE
           CSUM(6) = SUMO(W,N)
           TOTAL(6) = NP*NN*((NN - 1)/2)
           GO TO 100
!***********************************************************************
!
    7      CONTINUE
           CSUM(7) = SUMO(X,N)
           TOTAL(7) = NP*NN
           GO TO 100
!***********************************************************************
!
    8      CONTINUE
           CSUM(8) = SUMO(U1,5*N*2) + SUMO(U2,5*N*2) + SUMO(U3,5*N*2)
           TOTAL(8) = NP*(NN - 1)*2
           GO TO 100
!***********************************************************************
!
    9      CONTINUE
           CSUM(9) = SUMO(PX,15*N)
           TOTAL(9) = NP*NN
           GO TO 100
!***********************************************************************
!
   10      CONTINUE
           CSUM(10) = SUMO(PX,15*N)
           TOTAL(10) = NP*NN
           GO TO 100
!***********************************************************************
!
   11      CONTINUE
           CSUM(11) = SUMO(X(2),N-1)
           TOTAL(11) = NP*(NN - 1)
           GO TO 100
!***********************************************************************
!
   12      CONTINUE
           CSUM(12) = SUMO(X,N - 1)
           TOTAL(12) = NP*NN
           GO TO 100
!***********************************************************************
!
   13      CONTINUE
           CSUM(13) = SUMO(P,8*N) + SUMO(H,8*N)
           TOTAL(13) = NP*NN
           GO TO 100
!***********************************************************************
!
   14      CONTINUE
           CSUM(14) = SUMO(VX,N) + SUMO(XX,N) + SUMO(RH,67)
           TOTAL(14) = NP*NN
           GO TO 100
!***********************************************************************
!
   15      CONTINUE
           CSUM(15) = SUMO(VY,N*7) + SUMO(VS,N*7)
           TOTAL(15) = NP*(NN - 1)*5
           GO TO 100
!***********************************************************************
!
   16      CONTINUE
           CSUM(16) = REAL(K3 + K2 + J5 + M)
           FLOPN(16) = (K2 + K2 + 10*K3)*LOOPS2
           TOTAL(16) = 1.0D0
           GO TO 100
!***********************************************************************
!
   17      CONTINUE
           CSUM(17) = SUMO(VXNE,N) + SUMO(VXND,N) + XNM
           TOTAL(17) = NP*NN
           GO TO 100
!***********************************************************************
!
   18      CONTINUE
           CSUM(18) = SUMO(ZR,N*7) + SUMO(ZZ,N*7)
           TOTAL(18) = NP*(NN - 1)*5
           GO TO 100
!***********************************************************************
!
   19      CONTINUE
           CSUM(19) = SUMO(B5,N) + STB5
           TOTAL(19) = NP*NN
           GO TO 100
!***********************************************************************
!
   20      CONTINUE
           CSUM(20) = SUMO(XX(2),N)
           TOTAL(20) = NP*NN
           GO TO 100
!***********************************************************************
!
   21      CONTINUE
           CSUM(21) = SUMO(PX,25*N)
           TOTAL(21) = NP*25*25*NN
           GO TO 100
!***********************************************************************
!
   22      CONTINUE
           CSUM(22) = SUMO(W,N)
           TOTAL(22) = NP*NN
           GO TO 100
!***********************************************************************
!
   23      CONTINUE
           CSUM(23) = SUMO(ZA,N*7)
           TOTAL(23) = NP*(NN - 1)*5
           GO TO 100
!***********************************************************************
!
   24      CONTINUE
           CSUM(24) = REAL(M)
           TOTAL(24) = NP*(NN - 1)
           GO TO 100
!***********************************************************************
!
   25      CONTINUE
!***********************************************************************
!
  100      CONTINUE
!
!     delta( testov)= relerr * testov
           OVERR = SEE(2,1,JR,IL)*REAL(NP)*TESTOV
           TERR1(I) = 100.0
           IF (TIME(I) /= 0.0D0) TERR1(I) = TERR1(I)*(OVERR/TIME(I))
           NPFS1(I) = NPF
           IF (ION > 0) THEN
!
!     If this clock resolution test fails, you must increase Loop (Subr. SIZES)
!
                IF (TERR1(I) >= 15.0) THEN
                     WRITE (ION, 113) I
  113 FORMAT(/,1X,I2,' TESTS:  POOR TIMING OR ERROR. NEED LONGER RUN')
!
                ENDIF
                WRITE (ION, 115) I, TIME(I), TERR1(I), NPF
  115      FORMAT( 2X,i2,' Done  T= ',E11.4,'  T err= ',F8.2,'%' ,      &
     &             I8,'  Page-Faults ')
           ENDIF
!
      ENDIF
      IF (I>=0 .AND. I<24) THEN
           CALL VALUES (I + 1)
           CALL SIZES (I + 1)
      ENDIF
!
      CALL TRACK ('TESTS   ')
      RETURN 
      END SUBROUTINE TESTS
!
!***********************************************
      REAL FUNCTION TICK (IOU, NTIMES)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU, NTIMES
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /SPACER/
      COMMON /SPACER/ A11, A12, A13, A21, A22, A23, A31, A32, A33, AR,  &
     &     BR, C0, CR, DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28 &
     &     , DN, E3, E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5 &
     &     , T, XNC, XNEI, XNM
      REAL   A11, A12, A13, A21, A22, A23, A31, A32, A33, AR, BR, C0, CR&
     &     , DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28, DN, E3,  &
     &     E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5, T, XNC,  &
     &     XNEI, XNM
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!...  /ISPACE/
      COMMON /ISPACE/ E(96), F(96), IX(1001), IR(1001), ZONE(300)
      INTEGER   E, F, IX, IR, ZONE
!...  /SPACE1/
      COMMON /SPACE1/ U(1001), V(1001), W(1001), X(1001), Y(1001), Z(   &
     &     1001), G(1001), DU1(101), DU2(101), DU3(101), GRD(1001), DEX(&
     &     1001), XI(1001), EX(1001), EX1(1001), DEX1(1001), VX(1001),  &
     &     XX(1001), RX(1001), RH(2048), VSP(101), VSTP(101), VXNE(101) &
     &     , VXND(101), VE3(101), VLR(101), VLIN(101), B5(101), PLAN(300&
     &     ), D(300), SA(101), SB(101)
      REAL   U, V, W, X, Y, Z, G, DU1, DU2, DU3, GRD, DEX, XI, EX, EX1, &
     &     DEX1, VX, XX, RX, RH, VSP, VSTP, VXNE, VXND, VE3, VLR, VLIN, &
     &     B5, PLAN, D, SA, SB
!...  /SPACE2/
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101), VY(101,25), VH( &
     &     101,7), VF(101,7), VG(101,7), VS(101,7), ZA(101,7), ZP(101,7)&
     &     , ZQ(101,7), ZR(101,7), ZM(101,7), ZB(101,7), ZU(101,7), ZV( &
     &     101,7), ZZ(101,7), B(64,64), C(64,64), H(64,64), U1(5,101,2) &
     &     , U2(5,101,2), U3(5,101,2)
      REAL   P, PX, CX, VY, VH, VF, VG, VS, ZA, ZP, ZQ, ZR, ZM, ZB, ZU, &
     &     ZV, ZZ, B, C, H, U1, U2, U3
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: L4813 = 4*512
      INTEGER, PARAMETER :: L4813P = L4813 + 1
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(20) :: INX
      INTEGER :: NEFF, KLM, IO, JJ, NT, J, K
      REAL, DIMENSION(20) :: TIM, TER, TMX
      REAL, DIMENSION(L4813P) :: P1
      REAL :: RETEST, TESTO, T0, T1, ELAPST, TOLER, RERR

      SAVE RETEST
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      INTEGER , EXTERNAL :: TEST
      REAL , EXTERNAL :: SECOVT, SECOND, RELERR
!-----------------------------------------------
      EQUIVALENCE( P,P1)
      CALL TRACE ('TICK    ')
!
      ION = IOU
      KR = MK
      N = 0
      K2 = 0
      K3 = 0
      M = 0
      NEFF = 0
      IF (IL == 1) THEN
!
!***********************************************************************
!     Measure tsecov:  Overhead time for calling SECOND
!***********************************************************************
!
           TSECOV = SECOVT(IOU)
           TIC = TSECOV
!
!***********************************************************************
!     Measure testov:  Overhead time for calling TEST
!***********************************************************************
!
           TESTO = 0.00D0
           KLM = 8000
           IO = ABS(IOU)
           JJ = 0
           NT = NTIMES - 6
           J = NT
           IF (NT<8 .OR. NT>30) GO TO 911
!
           DO J = 1, NT
                L = 1
                MPY = 1
                LOOPS2 = 1
                MPYLIM = LOOPS2
                LOOP = KLM
                LP = LOOP
!                                  Measure overhead time for empty loop
                CUMTIM(1) = 0.0D0
                T0 = SECOND(CUMTIM(1))
  801           CONTINUE
                IF (TEST((-73)) > 0) GO TO 801
  802           CONTINUE
                IF (TEST((-73)) > 0) GO TO 802
  803           CONTINUE
                IF (TEST((-73)) > 0) GO TO 803
  804           CONTINUE
                IF (TEST((-73)) > 0) GO TO 804
  805           CONTINUE
                IF (TEST((-73)) > 0) GO TO 805
  806           CONTINUE
                IF (TEST((-73)) > 0) GO TO 806
  807           CONTINUE
                IF (TEST((-73)) > 0) GO TO 807
  808           CONTINUE
                IF (TEST((-73)) > 0) GO TO 808
  809           CONTINUE
                IF (TEST((-73)) > 0) GO TO 809
  810           CONTINUE
                IF (TEST((-73)) > 0) GO TO 810
                CUMTIM(1) = 0.0D0
                T1 = SECOND(CUMTIM(1)) - TSECOV
                ELAPST = T1 - T0
                TESTO = ELAPST/(REAL(10*KLM) + 1.0E-9)
                TOLER = 0.020D0
                RERR = 1.00D0
!
!                                  Convergence test:  Rel.error .LT. 1%
                IF (ELAPST > 1.00D04) GO TO 911
                IF (ELAPST<1.00D-9 .AND. J>8) GO TO 911
                IF (ELAPST > 1.00D-9) THEN
                     JJ = JJ + 1
                     TIM(JJ) = TESTO
                     IF (JJ > 1) RERR = RELERR(TIM(JJ),TIM(JJ-1))
                     TER(JJ) = RERR
                ENDIF
                IF (IOU > 0) WRITE (IOU, 64) 10*KLM, TESTO, RERR
                IF (RERR < TOLER) GO TO 825
                IF (ELAPST > 10.00D0) EXIT 
                KLM = KLM + KLM
           END DO
!                                  Poor accuracy on exit from loop
           IF (J <= 1) GO TO 911
           IF (JJ < 1) GO TO 911
           CALL SORDID (INX, TMX, TER, JJ, 1)
           TESTO = TIM(INX(1))
           RERR = TMX(1)
           WRITE (IO, 63) 100.00D0*RERR
!                                  Good convergence, satifies 1% error tolerence
  825      CONTINUE
           TESTOV = TESTO
           RETEST = RERR*TESTOV
      ENDIF
!
!***********************************************************************
!                                  Generate data sets
      SEE(1,1,JR,IL) = TESTOV
      SEE(2,1,JR,IL) = RETEST
      TICKS = TESTOV
      TICK = TESTOV
      L = 1
      LOOP = 1
      LP = LOOP
      J = TEST(0)
!
      TIME = 0.0D0
      CSUM = 0.0D0
!
      IF (IL == 1) THEN
           CALL STATS (SEE(1,2,JR,IL), U, NT1)
!         CALL  STATS( SEE(1,3,jr,il), P, nt2)
           CALL STATS (SEE(1,3,JR,IL), P1(L4813+1), NT2 - L4813)
      ELSE
           SEE(:,2,JR,IL) = SEE(:,2,JR,1)
           SEE(:,3,JR,IL) = SEE(:,3,JR,1)
      ENDIF
!
      IF (IOU > 0) THEN
           WRITE (IOU, 99)
            WRITE (IOU, 100)
            WRITE (IOU, 102) (SEE(K,1,JR,IL),K=1,2)
           WRITE (IOU, 104) (SEE(K,2,JR,IL),K=1,4)
           WRITE (IOU, 104) (SEE(K,3,JR,IL),K=1,4)
      ENDIF
!
      CALL TRACK ('TICK    ')
      RETURN 
!
  911 CONTINUE
      WRITE (IO, 61)
       WRITE (IO, 62) ELAPST, J
      CALL WHERE (0)
!
   61 FORMAT(1X,'FATAL(TICK): cant measure overhead time of subr. TEST')
   62 FORMAT(/,13X,'using SECOND:  elapst=',1E20.8,6X,'J=',I4)
   63 FORMAT(1X,'WARNING(TICK):  TEST overhead time relerr',f9.4,'%')
   64 FORMAT(1X,'testov(TICK)',I12,E12.4,F11.4)
   99 FORMAT(//,' CLOCK OVERHEAD:  ')
  100 FORMAT(/,14X,'AVERAGE',8X,'STANDEV',8X,'MINIMUM',8X,'MAXIMUM')
  102 FORMAT(/,1X,' TICK',4E15.6)
  104 FORMAT(/,1X,' DATA',4E15.6)
      END FUNCTION TICK
!
!***********************************************
      SUBROUTINE TILE(SM, SI, OX, IX, W, EW, T, TILES, N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER N
      REAL SM, SI, EW, T, TILES
      INTEGER, DIMENSION(N) :: IX
      REAL, DIMENSION(N) :: OX, W
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K
      REAL :: THRESH, R, S, Z, Y
!-----------------------------------------------
!
      CALL TRACE ('TILE    ')
!
      THRESH = TILES*T + 0.50D0*EW*W(1)
      R = 0.0D0
      S = R
      DO K = 1, N
           S = R
           R = R + W(IX(K))
           IF (R > THRESH) GO TO 7
      END DO
      K = N
    7 CONTINUE
      Z = 0.0D0
      Y = 0.0D0
      IF (K > 1) Y = OX(K-1)
      IF (R /= S) Z = (THRESH - S)/(R - S)
      SM = Y + Z*(OX(K)-Y)
      SI = REAL(K - 1) + Z
!
      CALL TRACK ('TILE    ')
      RETURN 
      END SUBROUTINE TILE
!
!***********************************************
      SUBROUTINE TRACE(NAME)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      CHARACTER NAME*8
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /DEBUG/
      COMMON /DEBUG/ ISTACK(20)
      CHARACTER   ISTACK*8
!...  /ORDER/
      COMMON /ORDER/ INSEQ, MATCH, NSTACK(20), ISAVE, IRET
      INTEGER   INSEQ, MATCH, NSTACK, ISAVE, IRET
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K
!-----------------------------------------------
!
!                              pushdown stack of subroutine names and call nrs.
      NSTACK(10:2:(-1)) = NSTACK(9:1:(-1))
      ISTACK(10:2:(-1)) = ISTACK(9:1:(-1))
!
      INSEQ = INSEQ + 1
      NSTACK(1) = INSEQ
      ISTACK(1) = NAME
      ISAVE = INSEQ
      IF (INSEQ == MATCH) CALL STOPS
!
      CALL WATCH (1)
!
      RETURN 
      END SUBROUTINE TRACE
!
!***********************************************
      SUBROUTINE STOPS
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!***********************************************
!
!     This routine is a convenient program break-point which is
!     selected by pre-setting:  match in COMMON /ORDER/  or by data
!     loading in BLOCK DATA  to equal the serial index of a
!     particular call to TRACE , as previously recorded in NSTACK.
!     The call to STOPS is selected in subroutine TRACE .
!
!     PAUSE 1
      RETURN 
      END SUBROUTINE STOPS
!
!***********************************************
      SUBROUTINE TRACK(NAME)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      CHARACTER NAME*8
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /DEBUG/
      COMMON /DEBUG/ ISTACK(20)
      CHARACTER   ISTACK*8
!...  /ORDER/
      COMMON /ORDER/ INSEQ, MATCH, NSTACK(20), ISAVE, IRET
      INTEGER   INSEQ, MATCH, NSTACK, ISAVE, IRET
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: K
!-----------------------------------------------
!
      IRET = IRET + 1
      CALL WATCH (2)
!                             pop stack of subroutine names
      IF (NAME == ISTACK(1)) THEN
           NSTACK(:9) = NSTACK(2:10)
           ISTACK(:9) = ISTACK(2:10)
      ELSE
           ISTACK(20) = NAME
           CALL WHERE (12)
      ENDIF
!
      RETURN 
      END SUBROUTINE TRACK
!
!***********************************************
      SUBROUTINE TRAP(I, NAME, MINI, MAXI, MEFF)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER MINI, MAXI, MEFF
      CHARACTER NAME*(*)
      INTEGER, DIMENSION(MEFF) :: I
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: LX, K, IO
!-----------------------------------------------
!
      CALL TRACE ('TRAP    ')
!
      LX = 0
      DO K = 1, MEFF
           IF (I(K)<MINI .OR. I(K)>MAXI) LX = K
      END DO
!
      IF (LX /= 0) THEN
           IO = ABS(ION)
           IF (IO<=0 .OR. IO>10) IO = 6
           WRITE (IO, 110) LX, NAME
  110   FORMAT(////,' TRAP: ERROR IN INDEX-LIST(',i4,')  IN SUBR:  ',A )
           WRITE (IO, 113) I
  113         FORMAT(1X,10I6)
!
           CALL WHERE (0)
      ENDIF
!
      CALL TRACK ('TRAP    ')
      RETURN 
      END SUBROUTINE TRAP
!
!***********************************************
      SUBROUTINE TRIAL(IOU, I, T0, TJ)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU, I
      REAL T0, TJ
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /PROOF/
      COMMON /PROOF/ SUMS(24,3,8)
      DOUBLE PRECISION ::SUMS
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: MALL = 24*3
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(MALL) :: ID, LD
      INTEGER :: ISUM, II, MM, IJK, LX, J, K
      REAL, DIMENSION(MALL) :: CS1, CS2
      REAL :: ESTIME

      SAVE ISUM
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: SECOND
!-----------------------------------------------
!   S t a t e m e n t   F u n c t i o n s
!-----------------------------------------------
      INTEGER MODI, NPER
!-----------------------------------------------
      MODI(ii,mm)= (MOD( ABS(ii)-1, mm) + 1)                             
      NPER(ii,mm)= ((ABS(ii)-1+mm)/(mm))
!
      CALL TRACE ('TRIAL   ')
!
      IF (I == 1) THEN
           ESTIME = (TJ - T0) + REAL(MRUNS)*(SECOND(0.0) - TJ)
           WRITE (IOU, 70) ESTIME, NRUNS
           WRITE (*, 70) ESTIME, NRUNS
   70 FORMAT(/,' ESTIMATED TOTAL JOB CPU-TIME:=' ,F10.3,' sec.',        &
     & '  ( Nruns=',I8,' Trials)',/)
      ENDIF
!
      IJK = 4
      IF (MULTI <= 1) IJK = 1
      IF (MULTI == 10) IJK = 2
      IF (MULTI == 50) IJK = 3
      IF (MULTI >= 100) IJK = 4
!
      LX = 0
      DO J = IM, ML
           CS1(LX+1:24+LX) = CSUMS(JR,J,:24)
           CS2(LX+1:24+LX) = SUMS(:,J,IJK)
           LX = 24 + LX
      END DO
!
      CALL SEQDIG (ID, ISUM, CS1, CS2, MALL)
!
      IF (I == 1) THEN
!
           LD(:MALL) = ID(:MALL)
      ELSE
           IF (ISUM==LAST .AND. ISUM>200) THEN
                NPASS = NPASS + 1
           ELSE
                NFAIL = NFAIL + 1
!
                DO K = 1, MALL
                     IF (ID(K) /= LD(K)) WRITE (IOU, 333) I, MODI(K,24) &
     &                    , NPER(K,24), ID(K), LD(K)
                END DO
           ENDIF
      ENDIF
!
!
      IF (I<=7 .OR. MODI(I,7)==1) THEN
           WRITE (IOU, 111) I, ISUM, NPASS, NFAIL
           WRITE (*, 111) I, ISUM, NPASS, NFAIL
  111 FORMAT(' Trial=',I7,13X,'ChkSum=',I5,4X,'Pass=',I7,5X,'Fail=',I7)
!
!     cumtim(1)= 0.0d0
!          tjob= SECOND( cumtim(1)) - t0
!     WRITE( iou,123)  tjob
!     WRITE(   *,123)  tjob
! 123 FORMAT(2X,'Tcpu=',4X,F10.2,' sec')
!
!     WRITE( iou,222) ( MODI(k,24), ID(k), CS1(k), CS2(k), k= 1,mall )
! 222 FORMAT(2X,2I6,3X,2E24.16)
  333 FORMAT(1X,'TRIAL:',I7,6X,'Kernel=',I5,6X,'j= ',I7,6X,'ERROR',2I7)
      ENDIF
      LAST = ISUM
      IBUF = 0
!
      CALL TRACK ('TRIAL   ')
      RETURN 
      END SUBROUTINE TRIAL
!
!***********************************************
      SUBROUTINE VALID(VX, MAP, LX, BL, X, BU, N)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER LX, N
      REAL BL, BU
      INTEGER, DIMENSION(N) :: MAP
      REAL, DIMENSION(N) :: VX, X
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: M, K
!-----------------------------------------------
!LLL. OPTIMIZE LEVEL G
!
      CALL TRACE ('VALID   ')
!
      M = 0
      LX = 0
      IF (N > 0) THEN
           DO K = 1, N
                IF (X(K)<=BL .OR. X(K)>=BU) CYCLE 
                M = M + 1
                MAP(M) = K
                VX(M) = X(K)
           END DO
!
           LX = M
           IF (M > 0) CALL TRAP (MAP, ' VALID  ', 1, N, M)
      ENDIF
      CALL TRACK ('VALID   ')
      RETURN 
      END SUBROUTINE VALID
!
!***********************************************
      SUBROUTINE VALUES(I)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER I
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /SPACER/
      COMMON /SPACER/ A11, A12, A13, A21, A22, A23, A31, A32, A33, AR,  &
     &     BR, C0, CR, DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28 &
     &     , DN, E3, E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5 &
     &     , T, XNC, XNEI, XNM
      REAL   A11, A12, A13, A21, A22, A23, A31, A32, A33, AR, BR, C0, CR&
     &     , DI, DK, DM22, DM23, DM24, DM25, DM26, DM27, DM28, DN, E3,  &
     &     E6, EXPMAX, FLX, Q, QA, R, RI, S, SCALE, SIG, STB5, T, XNC,  &
     &     XNEI, XNM
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!...  /ISPACE/
      COMMON /ISPACE/ E(96), F(96), IX(1001), IR(1001), ZONE(300)
      INTEGER   E, F, IX, IR, ZONE
!...  /SPACE1/
      COMMON /SPACE1/ U(1001), V(1001), W(1001), X(1001), Y(1001), Z(   &
     &     1001), G(1001), DU1(101), DU2(101), DU3(101), GRD(1001), DEX(&
     &     1001), XI(1001), EX(1001), EX1(1001), DEX1(1001), VX(1001),  &
     &     XX(1001), RX(1001), RH(2048), VSP(101), VSTP(101), VXNE(101) &
     &     , VXND(101), VE3(101), VLR(101), VLIN(101), B5(101), PLAN(300&
     &     ), D(300), SA(101), SB(101)
      REAL   U, V, W, X, Y, Z, G, DU1, DU2, DU3, GRD, DEX, XI, EX, EX1, &
     &     DEX1, VX, XX, RX, RH, VSP, VSTP, VXNE, VXND, VE3, VLR, VLIN, &
     &     B5, PLAN, D, SA, SB
!...  /SPACE2/
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101), VY(101,25), VH( &
     &     101,7), VF(101,7), VG(101,7), VS(101,7), ZA(101,7), ZP(101,7)&
     &     , ZQ(101,7), ZR(101,7), ZM(101,7), ZB(101,7), ZU(101,7), ZV( &
     &     101,7), ZZ(101,7), B(64,64), C(64,64), H(64,64), U1(5,101,2) &
     &     , U2(5,101,2), U3(5,101,2)
      REAL   P, PX, CX, VY, VH, VF, VG, VS, ZA, ZP, ZQ, ZR, ZM, ZB, ZU, &
     &     ZV, ZZ, B, C, H, U1, U2, U3
!...  /BASE2/
      COMMON /BASE2/ P0(4,512), PX0(25,101), CX0(25,101), VY0(101,25),  &
     &     VH0(101,7), VF0(101,7), VG0(101,7), VS0(101,7), ZA0(101,7),  &
     &     ZP0(101,7), ZQ0(101,7), ZR0(101,7), ZM0(101,7), ZB0(101,7),  &
     &     ZU0(101,7), ZV0(101,7), ZZ0(101,7), B0(64,64), CC0(64,64), H0&
     &     (64,64), U10(5,101,2), U20(5,101,2), U30(5,101,2)
      REAL   P0, PX0, CX0, VY0, VH0, VF0, VG0, VS0, ZA0, ZP0, ZQ0, ZR0, &
     &     ZM0, ZB0, ZU0, ZV0, ZZ0, B0, CC0, H0, U10, U20, U30
!...  /SPACE3/
      COMMON /SPACE3/ CACHE(8192)
      REAL   CACHE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: IP1, K, J, MMIN, MMAX, MC, LR, II, LX
      REAL :: FW, SC
      DOUBLE PRECISION :: DS, DW
!-----------------------------------------------
!
!     ******************************************************************
      CALL TRACE ('VALUES  ')
!
      CALL SIZES (I)
      IP1 = I
!              Initialize the dummy  Cache-memory with never used data-set.
      CACHE = 0.10
!
      CALL SUPPLY (I)
!
      IF (IP1 == 13) THEN
           DS = 1.000D0
           DW = 0.500D0
           DO J = 1, 4
                DO K = 1, 512
                     P(J,K) = DS
                     P0(J,K) = DS
                     DS = DS + DW
                END DO
           END DO
!
           E = 1
           F = 1
!
      ENDIF
      IF (IP1 == 14) THEN
!
           MMIN = 1
           MMAX = 1001
           CALL IQRANF (IX, MMIN, MMAX, 1001)
!
           DW = -100.000D0
           DEX = DW*DEX
           GRD = IX
           FLX = 0.00100D0
!
      ENDIF
      IF (IP1 == 16) THEN
!ONDITIONS:
           MC = 2
           LR = N
           II = LR/3
           FW = 1.000D-4
           D(1) = 1.0198048642876400D0
           DO K = 2, 300
                D(K) = D(K-1) + FW/D(K-1)
           END DO
           R = D(LR)
           FW = 1.000D0
           DO LX = 1, MC
                M = (LR + LR)*(LX - 1)
                DO K = 1, LR
                     M = M + 1
                     S = REAL(K)
                     PLAN(M) = R*((S + FW)/S)
                     ZONE(M) = K + K
                END DO
                DO K = 1, LR
                     M = M + 1
                     S = REAL(K)
                     PLAN(M) = R*((S + FW)/S)
                     ZONE(M) = K + K
                END DO
           END DO
           K = LR + LR + 1
           ZONE(K) = LR
           S = D(LR-1)
           T = D(LR-2)
!
      ENDIF
!               Clear the scalar Cache-memory with never used data-set.
!     fw= 1.000d0
!     CALL SIGNEL( CACHE, fw, 0.0d0, 8192)
!
      J = 0
      SC = 0.0D0
      DO K = 1, 8192
           IF (CACHE(K) == 0.0) THEN
                J = J + K
                SC = SC + REAL(J*K)
           ENDIF
      END DO
!
      CALL TRACK ('VALUES  ')
      RETURN 
      END SUBROUTINE VALUES
!
!***********************************************
      SUBROUTINE VERIFY(IOU)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER IOU
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /SPACE1/
      COMMON /SPACE1/ U(1001), V(1001), W(1001), X(1001), Y(1001), Z(   &
     &     1001), G(1001), DU1(101), DU2(101), DU3(101), GRD(1001), DEX(&
     &     1001), XI(1001), EX(1001), EX1(1001), DEX1(1001), VX(1001),  &
     &     XX(1001), RX(1001), RH(2048), VSP(101), VSTP(101), VXNE(101) &
     &     , VXND(101), VE3(101), VLR(101), VLIN(101), B5(101), PLAN(300&
     &     ), D(300), SA(101), SB(101)
      REAL   U, V, W, X, Y, Z, G, DU1, DU2, DU3, GRD, DEX, XI, EX, EX1, &
     &     DEX1, VX, XX, RX, RH, VSP, VSTP, VXNE, VXND, VE3, VLR, VLIN, &
     &     B5, PLAN, D, SA, SB
!...  /SPACE2/
      COMMON /SPACE2/ P(4,512), PX(25,101), CX(25,101), VY(101,25), VH( &
     &     101,7), VF(101,7), VG(101,7), VS(101,7), ZA(101,7), ZP(101,7)&
     &     , ZQ(101,7), ZR(101,7), ZM(101,7), ZB(101,7), ZU(101,7), ZV( &
     &     101,7), ZZ(101,7), B(64,64), C(64,64), H(64,64), U1(5,101,2) &
     &     , U2(5,101,2), U3(5,101,2)
      REAL   P, PX, CX, VY, VH, VF, VG, VS, ZA, ZP, ZQ, ZR, ZM, ZB, ZU, &
     &     ZV, ZZ, B, C, H, U1, U2, U3
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /SPACEI/
      COMMON /SPACEI/ WTP(3), MUL(3), ISPAN(47,3), IPASS(47,3)
      INTEGER   MUL, ISPAN, IPASS
      REAL   WTP
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: NTMP = 100
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(NTMP) :: LEN
      INTEGER :: K, NZD, NTICKS, ILIMIT, NJ, LO, I, J, NN, I2, LOOP12,  &
     &     LOOPS0, LOITER
      REAL, DIMENSION(NTMP) :: TIM, TUM, TAV, TER, TMX, SIG
      REAL :: FUZZ, DT, T1, CUM, T2, TTEST, T0, RTERR, REPEAT, TNN, RT, &
     &     RPERR, TASK, PASSES, FLOPS, TD, RATEMF
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      REAL , EXTERNAL :: SECOVT, SECOND
!-----------------------------------------------
!
!
!     CALL TRACE ('VERIFY  ')
!
      X(:101) = 0.0D0
      Y(:101) = 0.0D0
      CX(1,:) = 0.0D0
      NZD = 0
!
!***********************************************************************
!     Measure tsecov:  Overhead time for calling SECOND
!***********************************************************************
!
      TSECOV = SECOVT(IOU)
      TIC = TSECOV
!
!***********************************************************************
!     Measure time resolution of cpu-timer;  tclock= MIN t
!***********************************************************************
!
      FUZZ = 1.00D-12
      NTICKS = INT(1.00D0/(TSECOV + FUZZ))
      NTICKS = MAX0(1000,NTICKS)
      DT = 0.00D0
      T1 = SECOND(CUM)
      M = 0
!
      DO K = 1, NTICKS
           T2 = SECOND(CUM)
           IF (T2 /= T1) THEN
                M = M + 1
                DT = DT + (T2 - T1)
                T1 = T2
                IF (M >= 200) EXIT 
           ENDIF
      END DO
!
      IF (M <= 2) THEN
           TCLOCK = 1.00D0
           WRITE (*, 163)
            WRITE (IOU, 163)
      ELSE
            TCLOCK = DT/(REAL(M) + FUZZ)
      ENDIF
!
      WRITE (*, 164) M, TCLOCK
      WRITE (IOU, 164) M, TCLOCK
  163 FORMAT(1X,'WARNING(VERIFY): POOR Cpu-timer resolution; REPLACE?')
  164 FORMAT('VERIFY:',I10,E12.4,' =  Time Resolution of Cpu-timer')
!
!****************************************************************************
!         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
!****************************************************************************
!
!         VERIFY produced the following output on CRAY-XMP4 in a
!         fully loaded, multi-processing, multi-programming system:
!
!
!         VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY
!         -----     -------     -------    -------   --------
!         EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE
!         Loop      CPUTIME     CLOCK      CLOCK     TIMING
!         SIZE      SECONDS     ERROR      ERROR     ERROR
!         -----     -------     -------    -------   --------
!             1  5.0000e-06      10.00%     17.63%     14.26%
!             2  7.0000e-06       7.14%      6.93%      4.79%
!             4  1.6000e-05       3.12%      6.56%      7.59%
!             8  2.8000e-05       1.79%      2.90%      2.35%
!            16  6.1000e-05       0.82%      6.72%      4.50%
!            32  1.1700e-04       0.43%      4.21%      4.62%
!            64  2.2700e-04       0.22%      3.13%      2.41%
!           128  4.4900e-04       0.11%      3.14%      0.96%
!           256  8.8900e-04       0.06%      2.06%      2.50%
!           512  1.7740e-03       0.03%      1.92%      1.59%
!          1024  3.4780e-03       0.01%      0.70%      1.63%
!          1360              Current Run:    MULTI=   10.000
!          2048  7.0050e-03       0.01%      0.74%      1.28%
!          4096  1.3823e-02       0.00%      1.35%      0.78%
!         -----     -------     -------    -------   --------
!
!          Approximate Serial Job Time=   2.5e+01 Sec.    ( Nruns= 7 RUNS)
!
!****************************************************************************
!
      WRITE (IOU, 45)
       WRITE (IOU, 49)
       WRITE (IOU, 46)
       WRITE (IOU, 47)
       WRITE (IOU, 48)
       WRITE (IOU, 49)
   45 FORMAT(/,8X,'VERIFY ADEQUATE Loop SIZE VERSUS CPU CLOCK ACCURACY')
   46 FORMAT(8X,'EXTRA     MAXIMUM     DIGITAL    DYNAMIC   RELATIVE')
   47 FORMAT(8X,'Loop      CPUTIME     CLOCK      CLOCK     TIMING  ')
   48 FORMAT(8X,'SIZE      SECONDS     ERROR      ERROR     ERROR   ')
   49 FORMAT(8X,'-----     -------     -------    -------   --------')
!
!
!****************************************************************************
!     Measure Cpu Clock Timing Errors As A Function Of Loop Size(lo)
!****************************************************************************
!
       TTEST = 100.00D0*TCLOCK
      ILIMIT = 30
      NJ = 5
      LO = 128
      I = 0
!
   10 CONTINUE
      I = I + 1
      LO = LO + LO
      DO J = 1, NJ
           N = 100
           CUMTIM(1) = 0.0D0
           T0 = SECOND(CUMTIM(1))
!                                    Time Kernel 12
           DO M = 1, LO
                X(:N) = X(2:N+1) - X(:N)
           END DO
!
           CUMTIM(1) = 0.0D0
           TIM(J) = SECOND(CUMTIM(1)) - T0 - TSECOV
      END DO
!                                    Compute Dynamic Clock Error
!
      CALL STATS (TUM, TIM, NJ)
      RTERR = 100.0*(TUM(2)/(TUM(1)+FUZZ))
      IF (TUM(1) <= 0.00D0) RTERR = 100.00D0
!
!                                    Compute Digital Clock Error
!
      CALL TDIGIT (SIG(I), NZD, TUM(4))
!
      TAV(I) = TUM(1)
      TMX(I) = TUM(4)
      TER(I) = RTERR
      LEN(I) = LO
      IF (I>ILIMIT .AND. TUM(1)<FUZZ) THEN
           WRITE (*, 146) LO, TUM(1)
  146 FORMAT('VERIFY:',I12,' Repetitions.  Bad Timer=',E14.5,' sec.')
      ENDIF
      IF (I<=8 .OR. TUM(1)<TTEST .AND. I<NTMP) GO TO 10
      NN = I
!
!****************************************************************************
!     Compute Multiple-Pass Loop Counters MULTI and Loops2
!     Such that:  each Kernel is run at least 100 ticks of Cpu-timer.
!****************************************************************************
!
      I2 = 2
      MULTI = 1
      MUCHO = 1
      CALL SIZES (12)
      LOOP12 = IPASS(12,2)*MUL(2)
!
      MULTI=INT((REAL(LO)/(REAL(LOOP12)+FUZZ))*(TTEST/(TUM(1)+FUZZ)))
      MUCHO = MULTI
!
!     If timing errors are too large, you must increase MULTI...
!     When MULTI.GE.100 each kernel is executed over a million times
!     and the time used to re-intialize overstored input variables
!     is negligible.  Thus each kernel may be run arbitrarily many times
!     (MULTI >> 100) without overflow and produce verifiable checksums.
!
!     Each kernel's results are automatically checksummed for  MULTI :=
!
!     MULTI=   1      clock resolution << 0.01 SEC,  or Cpu << 1 Mflops
!     MULTI=  10      clock resolution << 0.01 SEC,  or Cpu <  2 Mflops
!     MULTI=  50      clock resolution <= 0.01 SEC,  or Cpu <  2 Mflops
!     MULTI= 100      clock resolution <= 0.01 SEC,  or Cpu <  5 Mflops
!     MULTI= 200      clock resolution <= 0.01 SEC,  or Cpu < 10 Mflops
!
!     MULTI=   1
!     MULTI=  10
!     MULTI=  50
!     MULTI= 100
!     MULTI= 200
!
      MPY = 1
      LOOPS2 = 1
      MPYLIM = LOOPS2
      IF (MULTI <= 1) THEN
           MULTI = 1
      ELSE IF (MULTI <= 10) THEN
           MULTI = 10
      ELSE IF (MULTI <= 50) THEN
           MULTI = 50
      ELSE IF (MULTI <= 100) THEN
           MULTI = 100
      ELSE
           LOOPS2 = (MULTI + 50)/100
           MPYLIM = LOOPS2
           MULTI = 100
      ENDIF
!
!
      MUCHO = MULTI
      LOOPS0 = LOOP12*MULTI*LOOPS2
      REPEAT = REAL(MULTI*LOOPS2)
      IF (LOOP == 1) REPEAT = 1.00D0/(REAL(LOOP12) + FUZZ)
!
!****************************************************************************
!     Estimate Timing Error By Comparing Time Of Each Run With Longest Run
!****************************************************************************
!
      M = 0
      TNN = (TAV(NN)+2.00D0*TAV(NN-1))*0.500D0
      FUZZ = 1.0D-12
      TNN = AMAX1(FUZZ,TNN)
      DO I = 1, NN
           RTERR = TER(I)
           LO = LEN(I)
!                                    Compute Relative Clock Error
!
           RT = 0.0D0
           IF (LEN(I) >= 0) RT = LEN(NN)/LEN(I)
           RPERR = 100.00D0
           IF (TNN > FUZZ) RPERR = 100.00D0*(ABS(TNN - RT*TAV(I))/TNN)
           WRITE (IOU, 64) LO, TMX(I), SIG(I), RTERR, RPERR
   64   FORMAT(6X,I7,E12.4,F11.2,'%',F10.2,'%',F10.2,'%')
!
!                                    Find loops0 Size Used
!
           IF (LOOPS0>=LO .AND. LOOPS0<=2*LO) THEN
                M = LO
                WRITE (IOU, 66) LOOPS0, REPEAT
                WRITE (*, 66) LOOPS0, REPEAT
                IF (RTERR > 10.00D0) THEN
                     WRITE (IOU, 67)
                      WRITE (IOU, 68)
                      WRITE (*, 67)
                      WRITE (*, 68)
                ENDIF
   66 FORMAT(7X,i6,7X,'Repetition Count = MULTI * Loops2 = ',F12.3)
   67 FORMAT(34X,'VERIFY: POOR TIMING OR ERROR. NEED LONGER RUN  ')
   68 FORMAT(34X,'INCREASE:   MULTI  IN SUBR. VERIFY     ')
           ENDIF
!
      END DO
       IF (M <= 0) THEN
           WRITE (IOU, 66) LOOPS0, REPEAT
           WRITE (*, 66) LOOPS0, REPEAT
      ENDIF
      WRITE (IOU, 49)
!
!
       WRITE (*, 991)
       WRITE (*, 992)
  991 FORMAT(/,16X,'    (C) Copyright 1983 the Regents of the     ')
  992 FORMAT(  16X,'University of California. All Rights Reserved.',/)
!
!****************************************************************************
!     Clock Calibration Test of Internal Cpu-timer SECOND;
!           Verify 10 Internal SECOND Intervals using External Stopwatch
!****************************************************************************
!
!
  106 FORMAT(//,' CLOCK CALIBRATION TEST OF INTERNAL CPU-TIMER: SECOND')
  107 FORMAT(' MONOPROCESS THIS TEST, STANDALONE, NO TIMESHARING.')
  108 FORMAT(' VERIFY TIMED INTERVALS SHOWN BELOW USING EXTERNAL CLOCK')
  109 FORMAT(' START YOUR STOPWATCH NOW !')
  113 FORMAT(/,11X,'Verify  T or DT  observe external clock(sec):',/)
  114 FORMAT('           -------     -------      ------      -----')
  115 FORMAT('           Total T ?   Delta T ?    Mflops ?    Flops')
  119 FORMAT(4X,I2,3F12.2,2E15.5)
  120 FORMAT(' END CALIBRATION TEST.',/)
       WRITE (IOU, 106)
       WRITE (IOU, 107)
       WRITE (IOU, 108)
       WRITE (IOU, 109)
       WRITE (IOU, 113)
       WRITE (IOU, 114)
       WRITE (IOU, 115)
       WRITE (IOU, 114)
       WRITE (*, 106)
       WRITE (*, 107)
       WRITE (*, 108)
       WRITE (*, 109)
       WRITE (*, 113)
       WRITE (*, 114)
       WRITE (*, 115)
       WRITE (*, 114)
!
       TASK = 10.00D0
      PASSES = REAL(LO)*(TASK/(TNN + FUZZ))
      LOITER = INT(PASSES)
      FLOPS = 0.00D0
      CUMTIM(1) = 0.0D0
      T1 = SECOND(CUMTIM(1))
      T2 = 0.00D0
!
      DO J = 1, 4
           N = 100
           T0 = T1
!                                    Time Kernel 12
           DO M = 1, LOITER
                X(:N) = X(2:N+1) - X(:N)
           END DO
!
           CUMTIM(1) = 0.0D0
           T1 = SECOND(CUMTIM(1))
           TD = T1 - T0 - TSECOV
           T2 = T2 + TD
           FLOPS = FLOPS + PASSES*REAL(N)
           RATEMF = (1.00D-6*FLOPS)/(T2 + FUZZ)
           WRITE (*, 119) J, T2, TD, RATEMF, FLOPS
           WRITE (IOU, 119) J, T2, TD, RATEMF, FLOPS
      END DO
      WRITE (IOU, 114)
       WRITE (IOU, 120)
       WRITE (*, 114)
       WRITE (*, 120)
!
!     CALL TRACK ('VERIFY  ')
       RETURN 
      END SUBROUTINE VERIFY
!
!***********************************************
      SUBROUTINE WATCH(MODE)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER MODE
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /DEBUG/
      COMMON /DEBUG/ ISTACK(20)
      CHARACTER   ISTACK*8
!...  /ORDER/
      COMMON /ORDER/ INSEQ, MATCH, NSTACK(20), ISAVE, IRET
      INTEGER   INSEQ, MATCH, NSTACK, ISAVE, IRET
!...  /ALPHA/
      COMMON/ALPHA/MK,IK,IM,ML,IL,MRUNS,NRUNS,JR,IOVEC,NPFS(8,3,47)
      INTEGER   MK, IK, IM, ML, IL, MRUNS, NRUNS, JR, IOVEC, NPFS
!...  /TAU/
      COMMON /TAU/ TCLOCK, TSECOV, TESTOV, CUMTIM(4)
      REAL   TCLOCK, TSECOV, TESTOV, CUMTIM
!...  /BETA/
      COMMON /BETA/ TIC, TIMES(8,3,47), SEE(5,3,8,3), TERRS(8,3,47),    &
     &     CSUMS(8,3,47), FOPN(8,3,47), DOS(8,3,47)
      REAL   TIC, TIMES, SEE, TERRS, CSUMS, FOPN, DOS
!...  /SPACE0/
      COMMON /SPACE0/ TIME(47), CSUM(47), WW(47), WT(47), TICKS, FR(9), &
     &     TERR1(47), SUMW(7), START, SKALE(47), BIAS(47), WS(95), TOTAL&
     &     (47), FLOPN(47), IQ(7), NPF, NPFS1(47)
      INTEGER   IQ, NPF, NPFS1
      REAL   TIME, CSUM, WW, WT, TICKS, FR, TERR1, SUMW, START, SKALE,  &
     &     BIAS, WS, TOTAL, FLOPN
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: NTESTS = 14
      INTEGER, PARAMETER :: KRS1 = 24 + 1
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(20) :: IE
      INTEGER :: K, IERR, IO, K1
      CHARACTER :: NAME*8
!-----------------------------------------------
!     LOGICAL BOUNDS
!     BOUNDS(A,X,B,E)= ((((A)*(1.-E)).LE.(X)).AND.((X).LE.((B)*(1.+E))))
!
!                                       Debug Trace Info
      NAME = 'watch'
!     IF( made.EQ.1 )  name= ' ENTRY  '
!     IF( made.EQ.2 )  name= ' RETURN '
!     WRITE(*,101) inseq, name, ISTACK(1)
! 101 FORMAT(1X,I6,5X,A ,1X,A )
!
!                                       Domain Tests of Critical Variables
      IE(:NTESTS) = 0
      IF (TESTOV /= TICKS) IE(1) = 1
      IF (TSECOV /= TIC) IE(2) = 2
      IF (INSEQ<=0 .OR. INSEQ/=ISAVE .OR. INSEQ>99999) IE(3) = 3
      IF (NRUNS<1 .OR. NRUNS>8) IE(4) = 4
      IF (IL<1 .OR. IL>3) IE(5) = 5
      IF (MK<1 .OR. MK>24) IE(6) = 6
      IF (IK<0 .OR. IK>KRS1) IE(7) = 7
      IF (JR<1 .OR. JR>8) IE(8) = 8
      IF (LOOPS2 < 1) IE(9) = 9
      IF (LOOPS2 /= MPYLIM) IE(10) = 10
      IF (MULTI < 1) IE(11) = 11
      IF (MULTI /= MUCHO) IE(12) = 12
      IF (LOOP < 1) IE(13) = 13
      IF (LOOP /= LP) IE(14) = 14
!
!                        Insert your debug data tests here
!     IF( BOUNDS( 1.7669e+5,CSUMS(jr,1,8),1.7669e+5,1.0e-3)) IE(15)= 15
!
      IERR = 0
      IERR = SUM(IE(:NTESTS))
      IF (IERR /= 0) THEN
           IO = ABS(ION)
           IF (IO<=0 .OR. IO>10) IO = 6
           K1 = 0
           K2 = 0
           WRITE (*, 111)
            WRITE (*, 112) (K,K=1,NTESTS)
           WRITE (*, 112) (IE(K),K=1,NTESTS)
           WRITE (*, 112) K1, K2, INSEQ, NRUNS, IL, MK, IK, JR, LOOPS2, &
     &          MPYLIM, MULTI, MUCHO, LOOP, LP
           WRITE (IO, 111)
            WRITE (IO, 112) (K,K=1,NTESTS)
           WRITE (IO, 112) (IE(K),K=1,NTESTS)
           WRITE (IO, 112) K1, K2, INSEQ, NRUNS, IL, MK, IK, JR, LOOPS2 &
     &          , MPYLIM, MULTI, MUCHO, LOOP, LP
  111         FORMAT(/,' WATCH: STORAGE FAULT DETECTED.  IE=')
  112         FORMAT(1X,15I5)
           CALL WHERE (MODE)
      ENDIF
      RETURN 
      END SUBROUTINE WATCH
!
!***********************************************
      SUBROUTINE WHERE(MODE)
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  21:40:04  12/02/92   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER MODE
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!...  /SPACES/
      COMMON /SPACES/ ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP,    &
     &     N13H, IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, &
     &     N14, N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2,    &
     &     MUCHO, MPYLIM, INTBUF(16)
      INTEGER   ION, J5, K2, K3, MULTI, LAPS, LOOP, M, KR, LP, N13H,    &
     &     IBUF, NX, L, NPASS, NFAIL, N, N1, N2, N13, N213, N813, N14,  &
     &     N16, N416, N21, NT1, NT2, LAST, IDEBUG, MPY, LOOPS2, MUCHO,  &
     &     MPYLIM, INTBUF
!...  /DEBUG/
      COMMON /DEBUG/ ISTACK(20)
      CHARACTER   ISTACK*8
!...  /ORDER/
      COMMON /ORDER/ INSEQ, MATCH, NSTACK(20), ISAVE, IRET
      INTEGER   INSEQ, MATCH, NSTACK, ISAVE, IRET
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: INSERT = 2
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: MADE, IO, K
      CHARACTER :: NAME*8
!-----------------------------------------------
!
      MADE = MOD(MODE,10)
      NAME = 'internal'
      IF (MADE == 1) NAME = ' ENTRY  '
      IF (MADE == 2) NAME = ' RETURN '
      IO = ABS(ION)
      IF (IO<=0 .OR. IO>10) IO = 6
!
      IF (MODE == 12) THEN
           WRITE (*, 112) ISTACK(20), ISTACK(1)
           WRITE (IO, 112) ISTACK(20), ISTACK(1)
  112      FORMAT(2X,'WHERE: SEQ.ERROR.  RETURN ',A  ,'.NE. CALL ',A  )
      ENDIF
!
!PFM  IF( mode.EQ.20 ) THEN
!PFM       WRITE( io,9)
!PFM9      FORMAT(2X,'WHERE: INIPFM FAILED.' )
!PFM  ENDIF
      WRITE (*, 110) NAME, ISTACK(1)
      WRITE (IO, 110) NAME, ISTACK(1)
  110 FORMAT(/,' WHERE:  ERROR detected at ',A  ,' point in: ',A  )
!
      IF (MADE==1 .OR. MADE==2) THEN
!                    Pushdown stack of subroutine names and call nrs.
           DO K = 12, INSERT + 1, -1
                NSTACK(K) = NSTACK(K-INSERT)
                ISTACK(K) = ISTACK(K-INSERT)
           END DO
!
           NSTACK(1) = INSEQ
           ISTACK(1) = 'WATCH   '
           NSTACK(2) = INSEQ
           ISTACK(2) = 'TRACE   '
           IF (MADE == 2) ISTACK(2) = 'TRACK   '
      ENDIF
      WRITE (*, 111)
       WRITE (*, 114)
       WRITE (*, 113)
       WRITE (*, 114)
       WRITE (*, 118) (ISTACK(K),NSTACK(K),K=1,12)
!
      WRITE (IO, 111)
       WRITE (IO, 114)
       WRITE (IO, 113)
       WRITE (IO, 114)
       WRITE (IO, 118) (ISTACK(K),NSTACK(K),K=1,12)
  111 FORMAT(/,' ACTIVE SUBROUTINE LINKAGE CHAIN:')
  114 FORMAT('          ----           -----------')
  113 FORMAT('          name           call number')
  118 FORMAT(10X,A  ,4X,I8)
!
      DO K = 1, 200
           WRITE (IO, 221)
  221 FORMAT(/,' ********* TERMINAL ERROR; FLUSH I/O BUFFER **********')
      END DO
!      PAUSE
       STOP 
!     RETURN
      END SUBROUTINE WHERE
