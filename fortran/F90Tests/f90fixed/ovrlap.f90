      SUBROUTINE OVRLAP (NDOM,IGEOM,IDMAX,JDMAX,XD,YD,I1,I2,J1,J2,LEVEL)
C
C     This subroutine calculates the overlap between domains
C
C     Called by: CASAR
C     Database manager: none
C
C     Author: John K. Prentice, Quetzal Computational Associates
C     written: 16/11/92
C
C
C***********************************************************************
C       NOTE:  The common blocks referred to below are used in the
C              actual production version of this code.  However,
C              they have been eliminated from this version, which
C              has been modified for use in the benchmark suite.
C***********************************************************************
C
C
C	Input:
C
C	NDOM			integer.
C				Number of domains in this problem.
C
C	IGEOM			integer.
C				Geometry of this problem.
C
C	IDMAX			integer array dimensioned (IGBMAX).
C				IDMAX(n) is the number of x cells
C				for domain n.
C
C	JDMAX			integer array dimensioned (IGBMAX).
C				JDMAX(n) is the number of y cells
C				for domain n.
C
C	XD			real array dimensioned (IGBMAX,IXMAX)
C				XD(*,n) are the x coordinates for mesh
C				n.
C
C	YD			real array dimensioned (IGBMAX,IXMAX)
C				YD(*,n) are the y coordinates for mesh
C				n.
C
C	I1			integer array dimensioned (IGBMAX).
C				I1(n) is the i coordinate of the first
C				interior cell for domain n.
C
C	I2			integer array dimensioned (IGBMAX).
C				I2(n) is the i coordinate of the last
C				interior cell for domain n.
C
C	J1			integer array dimensioned (IGBMAX).
C				J1(n) is the j coordinate of the first
C				interior cell for domain n.
C
C	J2			integer array dimensioned (IGBMAX).
C				J2(n) is the j coordinate of the last
C				interior cell for domain n.
C
C	LEVEL			integer array dimensioned (IGBMAX).
C				LEVEL(n) is the level number for
C				domain n.  The larger the number,
C				the higher up the domain is in the
C				domain hierarchy.
C
C
C	Output:
C
C       -- output is to the overlap_information ---
C
C
C     All real numbers are double precision.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Data to define the maximum number of geometry blocks
      PARAMETER (IGBMAX=3)
C     Data to define the maximum mesh dimensions
      PARAMETER (IXMAX=1003, IYMAX=1003, IZMAX=503)
C
      PARAMETER (ZERO=0.0,TOLER=1.E-6,PT5=0.5,ONE=1.0,BIG=1.E20)
C
      PARAMETER (MAXOVER = 10000)
      INTEGER I_INTERIOR(MAXOVER),J_INTERIOR(MAXOVER),
     &        N_INTERIOR(MAXOVER),I_BOUNDARY(MAXOVER),
     &        J_BOUNDARY(MAXOVER),N_BOUNDARY(MAXOVER),
     &        BOUNDARY_ASSOCIATE(MAXOVER)
      DIMENSION VOLUME_OVERLAP(MAXOVER)
C
      DIMENSION IDMAX(IGBMAX),JDMAX(IGBMAX),XD(IXMAX,IGBMAX),
     &          YD(IYMAX,IGBMAX),I1(IGBMAX),I2(IGBMAX),
     &          J1(IGBMAX),J2(IGBMAX),LEVEL(IGBMAX)
      LOGICAL OVERLAPPED,TOUCH,OVER1,OVER2
C
C
C 
C	/DDEQLX/ common for domain decomposition. 
C
C	MVXEQL		integer.
C			maximum number of x-boundary cells in all 
C 			blocks whose velocities will be equilibrated.
C			Passed in common /ddeqlx/.
C
C	NVXSAV		integer array dimensioned (2,IGBMAX).
C			IVXSAV(M) and JVXSAV(M) correspond to an
C			x-boundary cell in a CTH block whose velocity
C			will be equilibrated. The array is sorted such 
C			that cell indices for boundary cells are grouped 
C			together for a each block. 
C			Thus, for some block N, array locations MN1 
C			through MN2 in the IVXSAV and JVXSAV arrays 
C			contain the i,j indices of cells to equilibrate
C			velocity. These array indices 
C			are passed in NVXSAV, defined as: 
C				MN1 = NVXSAV(1,N)
C				MN2 = NVXSAV(2,N)
C			If NVXSAV(1,N) = NXSAV(2,N) = 0, then no cells
C			for block N are to be velocity equilibrated.
C		 	Note, IGBMAX, used in the dimension
C			of NVXSAV, is a parameter which defined the 
C			maximum number of geometry blocks.  
C			Passed in common /ddeqlx/.
C
C	IVXSAV		integer array dimensioned (MVXEQL).
C			see the description of NVXSAV.
C			Passed in common /ddeqlx/.
C
C	JVXSAV		integer array dimensioned (MVXEQL).
C			see the description of NVXSAV.
C			Passed in common /ddeqlx/.
C
C	NTOUCHX		integer.
C			number of common x boundaries for this
C			calculation.
C			passed in common /ddeqlx/
C
C	NVXPNT		integer array dimensioned (NSAVE)
C			The x component of the momentum and mass of 
C			the staggered cell whose i,j indices are 
C			IVXSAV(n) and JVXSAV(n) is save into
C			the SAVEPX(NVXPNT(n)) and SAVEMX(NVXPNT(n))
C			arrays			
C
C
      PARAMETER (MVXEQL = 20000)
      DIMENSION NVXSAV(2,IGBMAX),IVXSAV(MVXEQL),JVXSAV(MVXEQL),
     *          NVXPNT(MVXEQL)
C 
C	/DDEQLY/ common for domain decomposition. 
C
C	MVYEQL		integer.
C			maximum number of y-boundary cells in all 
C 			blocks whose velocities will be equilibrated.
C			Passed in common /ddeqly/.
C
C	NVYSAV		integer array dimensioned (2,IGBMAX).
C			IVYSAV(M) and JVYSAV(M) correspond to an
C			y-boundary cell in a CTH block whose velocity
C			will be equilibrated. The array is sorted such 
C			that cell indices for boundary cells are grouped 
C			together for a each block. 
C			Thus, for some block N, array locations MN1 
C			through MN2 in the IVYSAV and JVYSAV arrays 
C			contain the i,j indices of cells to equilibrate
C			velocity. These array indices 
C			are passed in NVYSAV, defined as: 
C				MN1 = NVYSAV(1,N)
C				MN2 = NVYSAV(2,N)
C			If NVYSAV(1,N) = NXSAV(2,N) = 0, then no cells
C			for block N are to be velocity equilibrated.
C		 	Note, IGBMAX, used in the dimension
C			of NVYSAV, is a parameter which defined the 
C			maximum number of geometry blocks.  
C			Passed in common /ddeqly/.
C
C	IVYSAV		integer array dimensioned (MVYEQL).
C			see the description of NVYSAV.
C			Passed in common /ddeqly/.
C
C	JVYSAV		integer array dimensioned (MVYEQL).
C			see the description of NVYSAV.
C			Passed in common /ddeqly/.
C
C	NTOUCHY		integer.
C			number of common y boundaries for this
C			calculation.
C			passed in common /ddeqly/
C
C	NVYPNT		integer array dimensioned (NSAVE)
C			The y component of the momentum and mass of 
C			the staggered cell whose i,j indices are 
C			IVYSAV(n) and JVYSAV(n) is save into
C			the SAVEPX(NVYPNT(n)) and SAVEMX(NVYPNT(n))
C			arrays			
C
C
      PARAMETER (MVYEQL = 20000)
      DIMENSION NVYSAV(2,IGBMAX),IVYSAV(MVYEQL),JVYSAV(MVYEQL),
     *          NVYPNT(MVYEQL)
C 
C	/LPUT/ common for domain decomposition. 
C
C	MXLPUT		integer.
C			maximum number of cells in all blocks that
C			can be filled from other blocks 
C
C	NLPUT		integer array dimensioned (2,MXLPUT).
C			ILGET(n) and JLGET(n) are the i and j indices of
C			a CTH cell in this block which is receiving
C			material from another domain.  The
C			material to be donated to this cell is
C		  	stored in the NLGTPT(n) element of the TMP
C			arrays.  The fraction of the material in the
C			TMP arrays to donate to this cell is given
C			by FLGET(n).  These arrays are sorted such that
C			cell indices for cells receiving  material 
C			are grouped together for a each block.
C			Thus, for some block N, array locations MN1 
C			through MN2 in the ILGET and JLGET, NLGTPT, 
C			and FLGET arrays contain the i,j indices and 
C			pointers for cells to receive material from.  
C			These array indices are passed in NLPUT, 
C			defined as: 
C				MN1 = NLPUT(1,N)
C				MN2 = NLPUT(2,N)
C			If NLPUT(1,N) = NLPUT(2,N) = 0, then no cells
C			for block N receive material from other domains.
C			Note, IGBMAX, used in the dimension
C			of NLPUT, is a parameter which defined the 
C			maximum number of geometry blocks.  
C
C	ILGET		integer array dimensioned (MXLPUT).
C			see description of NLPUT.
C
C	JLGET		integer array dimensioned (MXLPUT).
C			see description of NLPUT.
C
C	NLGTPT		integer array dimensioned (MXLPUT).
C			see description of NLPUT.
C
C	FLGET		real array dimensioned (2,IGBMAX).
C			see description of NLPUT.
C
      PARAMETER (MXLPUT = 20000)
      DIMENSION NLPUT(2,IGBMAX),ILGET(MXLPUT),JLGET(MXLPUT),
     *          NLGTPT(MXLPUT),FLGET(MXLPUT)
C 
C	/LPUTX/ common for domain decomposition. 
C
C	MXLPTX		integer.
C			maximum number of momentum half cells in all 
C			blocks that can be filled from other blocks 
C
C	NLPUTX		integer array dimensioned (2,MXLPTX).
C			ILGETX(n) and JLGETX(n) are the i and j indices 
C			of a CTH half cell in this block which is 
C			receiving momentum from another domain.  The
C			momentum to be donated to this cell is
C		  	stored in the NLGPTX(n) element of the TMP
C			arrays.  The fraction of the momentum in the
C			TMP arrays to donate to this cell is given
C			by FLGETX(n).  LLRPTX(n) determines whether
C			this momentum is to be donated to the left
C			or right half cells (1=left, 2=right).
C			These arrays are sorted such that
C			cell indices for cells receiving  momentum 
C			are grouped together for a each block.
C			Thus, for some block N, array locations MN1 
C			through MN2 in the ILGETX and JLGETX, NLGPTX, 
C			LLRPTX, and FLGETX arrays contain the i,j 
C			indices and pointers for cells to receive 
C			momentum from.  These array indices are passed 
C			in NLPUTX, defined as: 
C				MN1 = NLPUTX(1,N)
C				MN2 = NLPUTX(2,N)
C			If NLPUTX(1,N) = NLPUTX(2,N) = 0, then no cells
C			for block N receive momentum from other domains.
C			Note, IGBMAX, used in the dimension
C			of NLPUTX, is a parameter which defined the 
C			maximum number of geometry blocks.  
C
C	ILGETX		integer array dimensioned (MXLPTX).
C			see description of NLPUTX.
C
C	JLGETX		integer array dimensioned (MXLPTX).
C			see description of NLPUTX.
C
C	NLGPTX		integer array dimensioned (MXLPTX).
C			see description of NLPUTX.
C
C	FLGETX		real array dimensioned (2,IGBMAX).
C			see description of NLPUTX.
C
C	LLRPTX		integer array dimensioned (MXLPTX).
C			see description of NLPUTX.
C
      PARAMETER (MXLPTX = 20000)
      DIMENSION NLPUTX(2,IGBMAX),ILGETX(MXLPTX),JLGETX(MXLPTX),
     *          NLGPTX(MXLPTX),FLGETX(MXLPTX),LLRPTX(MXLPTX)
C 
C	/LPUTY/ common for domain decomposition. 
C
C	MXLPTY		integer.
C			maximum number of momentum half cells in all 
C			blocks that can be filled from other blocks 
C
C	NLPUTY		integer array dimensioned (2,MXLPTY).
C			ILGETY(n) and JLGETY(n) are the i and j indices 
C			of a CTH half cell in this block which is 
C			receiving momentum from another domain.  The
C			momentum to be donated to this cell is
C		  	stored in the NLGPTY(n) element of the TMP
C			arrays.  The fraction of the momentum in the
C			TMP arrays to donate to this cell is given
C			by FLGETY(n).  LLRPTY(n) determines whether
C			this momentum is to be donated to the left
C			or right half cells (1=left, 2=right).
C			These arrays are sorted such that
C			cell indices for cells receiving  momentum 
C			are grouped together for a each block.
C			Thus, for some block N, array locations MN1 
C			through MN2 in the ILGETY and JLGETY, NLGPTY, 
C			LLRPTY, and FLGETY arrays contain the i,j 
C			indices and pointers for cells to receive 
C			momentum from.  These array indices are passed 
C			in NLPUTY, defined as: 
C				MN1 = NLPUTY(1,N)
C				MN2 = NLPUTY(2,N)
C			If NLPUTY(1,N) = NLPUTY(2,N) = 0, then no cells
C			for block N receive momentum from other domains.
C			Note, IGBMAX, used in the dimension
C			of NLPUTY, is a parameter which defined the 
C			maximum number of geometry blocks.  
C
C	ILGETY		integer array dimensioned (MXLPTY).
C			see description of NLPUTY.
C
C	JLGETY		integer array dimensioned (MXLPTY).
C			see description of NLPUTY.
C
C	NLGPTY		integer array dimensioned (MXLPTY).
C			see description of NLPUTY.
C
C	FLGETY		real array dimensioned (2,IGBMAX).
C			see description of NLPUTY.
C
C	LLRPTY		integer array dimensioned (MXLPTY).
C			see description of NLPUTY.
C
      PARAMETER (MXLPTY = 20000)
      DIMENSION NLPUTY(2,IGBMAX),ILGETY(MXLPTY),JLGETY(MXLPTY),
     *          NLGPTY(MXLPTY),FLGETY(MXLPTY),LLRPTY(MXLPTY)
C 
C	/LSAVE/ common for domain decomposition. 
C
C	MXSAVE		integer.
C			maximum number of cells in all blocks that
C			can be filled for the purpose of donation to 
C			other blocks 
C
C	ISAVE		integer array dimensioned (MXSAVE).
C			ISAVE(n) and JSAVE(n) are the i and j indices of
C			a CTH cell in this block which donates 
C			material to another domain.  The
C			material to be stored for this cell is
C		  	stored in the NFILPT(n) element of the TMP
C			arrays.  The left half cell x momentum to be 
C			stored for this cell is stored in PXTMP(nn),
C			where nn=NPXPT(1,n).   The right half cell x 
C			momentum to be stored for this cell is stored in
C			PXTMP(nn), where nn=NPXPT(2,n).  The bottom half
C			cell y momentum to be stored for this cell is 
C			stored in PYTMP(nn), where nn=NPYPT(1,n).   The 
C			top half cell y momentum to be stored for this 
C			cell is stored in PYTMP(nn), where 
C			nn=NPYPT(2,n).  The array is sorted such that
C			cell indices for cells containing material to be
C			donated are grouped together for a each block.
C			Thus, for some block N, array locations MN1 
C			through MN2 in the ISAVE, JSAVE, NFILPT, NPXPT,
C			and NPYPT arrays contain the i,j indices and 
C			pointers of cells to donate material from.  
C			These array indices are passed in NLSAVE, 
C			defined as: 
C				MN1 = NLSAVE(1,N)
C				MN2 = NLSAVE(2,N)
C			If NLSAVE(1,N) = NLSAVE(2,N) = 0, then no cells
C			for block N contain material to be
C			be donated.  Note, IGBMAX, used in the dimension
C			of NLSAVE, is a parameter which defined the 
C			maximum number of geometry blocks.  
C
C	JSAVE		integer array dimensioned (MXSAVE).
C			see description of ISAVE.
C
C	NFILPT		integer array dimensioned (MXSAVE).
C			see description of ISAVE.
C
C	NPXPT		integer array dimensioned (2,MXSAVE).
C			see description of ISAVE.
C
C	NPYPT		integer array dimensioned (2,MXSAVE).
C			see description of ISAVE.
C
C	NLSAVE		integer array dimensioned (2,IGBMAX).
C			see description of ISAVE.
C
      PARAMETER (MXSAVE = 20000)
      DIMENSION NLSAVE(2,IGBMAX),ISAVE(MXSAVE),JSAVE(MXSAVE),
     *          NFILPT(MXSAVE),NPXPT(2,MXSAVE),NPYPT(2,MXSAVE)
C 
C	/YANKALL/ common for domain decomposition. 
C
C	MYANKA		integer.
C			maximum number of cells in all blocks that
C			can contain material to be removed.
C
C	NYPTA		integer array dimensioned (2,IGBMAX).
C			IYANKA(M) and JYANKA(M) correspond to a cell
C			in a CTH block for which all material is 
C			to be removed.  The array is sorted such that
C			cell indices for cells containing material to be
C			removed are grouped together for a each block.
C			Thus, for some block N, array locations MN1 
C			through MN2 in the IYANKA and JYANKA arrays 
C			contain the i,j indices of cells to remove
C			material from.  These array indices 
C			are passed in NYPTA, defined as: 
C				MN1 = NYPTA(1,N)
C				MN2 = NYPTA(2,N)
C			If NYPTA(1,N) = NYPTA(2,N) = 0, then no cells
C			for block N contain material to be
C			be removed.  Note, IGBMAX, used in the dimension
C			of NYPTA, is a parameter which defined the 
C			maximum number of geometry blocks.  
C
C	IYANKA		integer array dimensioned (MYANKA).
C			see the description of NYPTA.
C
C	JYANKA		integer array dimensioned (MYANKA).
C			see the description of NYPTA.
C
      PARAMETER (MYANKA = 20000)
      DIMENSION NYPTA(2,IGBMAX),IYANKA(MYANKA),JYANKA(MYANKA)
C
      EXTERNAL ECLIPSE,TOUCH
C
      NPOINT = 0
      NOVER = 0
C
C	if one dimensional, fake values for the y mesh
C
      IF (IGEOM.LT.20) THEN
        DO NNOW = 1,NDOM
          JDMAX(NNOW) = 3
          J1(NNOW) = 2
          J2(NNOW) = 2
          YD(1,NNOW) = -ONE
          YD(2,NNOW) = ZERO
          YD(3,NNOW) = ONE
        ENDDO
      ENDIF
C
C	loop over domains and calculate which cells to yank from
C
      DO NNOW = 1,NDOM
C
        NPNT = MIN (NPOINT+1,MYANKA)
        NYPTA(1,NNOW) = NPOINT + 1
C
        IMAX_NOW = IDMAX(NNOW)
        JMAX_NOW = JDMAX(NNOW)
        ISTART_NOW = MAX(2,I1(NNOW))
        ISTOP_NOW = MIN(IMAX_NOW-1,I2(NNOW))
        JSTART_NOW = MAX(2,J1(NNOW))
        JSTOP_NOW = MIN(JMAX_NOW-1,J2(NNOW))
        LEVEL_NOW = LEVEL(NNOW)
        XMIN_NOW = XD(2,NNOW)
        XMAX_NOW = XD(IMAX_NOW,NNOW)
        XMIN_INTERIOR_NOW = XD(ISTART_NOW,NNOW)
        XMAX_INTERIOR_NOW = XD(ISTOP_NOW+1,NNOW)
        YMIN_NOW = YD(2,NNOW)
        YMAX_NOW = YD(JMAX_NOW,NNOW)
        YMIN_INTERIOR_NOW = YD(JSTART_NOW,NNOW)
        YMAX_INTERIOR_NOW = YD(JSTOP_NOW+1,NNOW)
C
C	loop over other domains and look for overlaps with this
C	domain
C
        DO NOTHER = 1,NDOM
          IF (NOTHER.NE.NNOW) THEN
            IMAX_OTHER = IDMAX(NOTHER)
            JMAX_OTHER = JDMAX(NOTHER)
            ISTART_OTHER = MAX(2,I1(NOTHER))
            ISTOP_OTHER = MIN(IMAX_OTHER-1,I2(NOTHER))
            JSTART_OTHER = MAX(2,J1(NOTHER))
            JSTOP_OTHER = MIN(JMAX_OTHER-1,J2(NOTHER))
            LEVEL_OTHER = LEVEL(NOTHER)
            XMIN_INTERIOR_OTHER = XD(ISTART_OTHER,NOTHER)
            XMAX_INTERIOR_OTHER = XD(ISTOP_OTHER+1,NOTHER)
            YMIN_INTERIOR_OTHER = YD(JSTART_OTHER,NOTHER)
            YMAX_INTERIOR_OTHER = YD(JSTOP_OTHER+1,NOTHER)
C
C	if level of the other domain is greater than this domain,
C	yank any cells in this domain which are overlapped by the
C	interior of the higher domain
C
            IF (LEVEL_OTHER.GT.LEVEL_NOW) THEN
C
C	see if there is any overlap between the interior of the
C	upper domain and any part of the lower domain
C
              OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                        XMAX_INTERIOR_OTHER,YMIN_INTERIOR_OTHER,
     &                        YMAX_INTERIOR_OTHER,XMIN_NOW,XMAX_NOW,
     &                        YMIN_NOW,YMAX_NOW)  
              IF (OVER.GT.ZERO) THEN       
C
C	there is an overlap between these domains, identify the
C	cells in each mesh to loop over
C     
                DO IOTHER = ISTART_OTHER,ISTOP_OTHER 
                  IFIRST_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMIN_NOW,XMAX_NOW,ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 10
                ENDDO
C
   10           DO IOTHER = ISTOP_OTHER,ISTART_OTHER,-1
                  ISECOND_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMIN_NOW,XMAX_NOW,ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 20
                ENDDO
C
   20           DO INOW = 2,IMAX_NOW-1  
                  IFIRST_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 30
                ENDDO
C
   30           DO INOW = IMAX_NOW-1,2,-1
                  ISECOND_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 40
                ENDDO
C
   40           DO JOTHER = JSTART_OTHER,JSTOP_OTHER 
                  JFIRST_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMIN_NOW,YMAX_NOW)
                  IF (OVER.GT.ZERO) GO TO 50
                ENDDO
C
   50           DO JOTHER = JSTOP_OTHER,JSTART_OTHER,-1
                  JSECOND_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMIN_NOW,YMAX_NOW)
                  IF (OVER.GT.ZERO) GO TO 60
                ENDDO
C
   60           DO JNOW = 2,JMAX_NOW-1
                  JFIRST_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 70
                ENDDO
C
   70           DO JNOW = JMAX_NOW-1,2,-1
                  JSECOND_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 80
                ENDDO
C
C	loop over each domain and calculate the overlaps in the current
C	domain
C
   80           DO INOW = IFIRST_NOW,ISECOND_NOW
                  DO JNOW = JFIRST_NOW,JSECOND_NOW
                    OVERLAPPED = .FALSE.
                    DO IOTHER = IFIRST_OTHER,ISECOND_OTHER
                      DO JOTHER = JFIRST_OTHER,JSECOND_OTHER
                        OVER = ECLIPSE (IGEOM,XD(INOW,NNOW),
     &                                  XD(INOW+1,NNOW),YD(JNOW,NNOW),
     &                                  YD(JNOW+1,NNOW),
     &                                  XD(IOTHER,NOTHER),
     &                                  XD(IOTHER+1,NOTHER),
     &                                  YD(JOTHER,NOTHER),
     &                                  YD(JOTHER+1,NOTHER))
                        IF (OVER.GT.ZERO) THEN
                          OVERLAPPED = .TRUE.
C
                          NOVER = NOVER + 1
                          NPNT = MIN(NOVER,MAXOVER)
                          I_INTERIOR(NPNT) = IOTHER
                          IF (IGEOM.GE.20) THEN
                            J_INTERIOR(NPNT) = JOTHER
                          ELSE
                            J_INTERIOR(NPNT) = 1
                          ENDIF
                          N_INTERIOR(NPNT) = NOTHER
C
                          I_BOUNDARY(NPNT) = INOW
                          IF (IGEOM.GE.20) THEN
                            J_BOUNDARY(NPNT) = JNOW
                          ELSE
                            J_BOUNDARY(NPNT) = 1
                          ENDIF
                          N_BOUNDARY(NPNT) = NNOW
C
                          VOLUME_OVERLAP(NPNT) = OVER
C
                        ENDIF
C
                      ENDDO
                    ENDDO
                    IF (OVERLAPPED) THEN
                      NPOINT = NPOINT + 1
                      NPNT = MIN(NPOINT,MYANKA)
                      NYPTA(2,NNOW) = NPOINT
                      IYANKA(NPNT) = INOW
                      IF (IGEOM.GE.20) THEN
                        JYANKA(NPNT) = JNOW
                      ELSE
                        JYANKA(NPNT) = 1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
C
              ENDIF
C
C	if level of the other domain is less than or equal to this 
C	domain, yank any boundary cells in this domain that overlap
C	the interiors of meshes of other domains
C
            ELSE
C
C	see if there is any overlap between the interior of the
C	other domain and the left hand boundary cells of this
C	domain
C
              OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                        XMAX_INTERIOR_OTHER,YMIN_INTERIOR_OTHER,
     &                        YMAX_INTERIOR_OTHER,XMIN_NOW,
     &                        XMIN_INTERIOR_NOW,YMIN_NOW,YMAX_NOW)  
              IF (OVER.GT.ZERO) THEN  
C
C	there is an overlap between these domains, identify the
C	cells in each mesh to loop over
C     
                DO IOTHER = ISTART_OTHER,ISTOP_OTHER 
                  IFIRST_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMIN_NOW,XMIN_INTERIOR_NOW,ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 90
                ENDDO
C
   90           DO IOTHER = ISTOP_OTHER,ISTART_OTHER,-1
                  ISECOND_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMIN_NOW,XMIN_INTERIOR_NOW,ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 100
                ENDDO
C
  100           DO INOW = 2,ISTART_NOW-1 
                  IFIRST_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 110
                ENDDO
C
  110           DO INOW = ISTART_NOW-1,2,-1
                  ISECOND_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 120
                ENDDO
C
  120           DO JOTHER = JSTART_OTHER,JSTOP_OTHER 
                  JFIRST_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMIN_NOW,YMAX_NOW)
                  IF (OVER.GT.ZERO) GO TO 130
                ENDDO
C
  130           DO JOTHER = JSTOP_OTHER,JSTART_OTHER,-1
                  JSECOND_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMIN_NOW,YMAX_NOW)
                  IF (OVER.GT.ZERO) GO TO 140
                ENDDO
C
  140           DO JNOW = 2,JMAX_NOW-1
                  JFIRST_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 150
                ENDDO
C
  150           DO JNOW = JMAX_NOW-1,2,-1
                  JSECOND_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 160
                ENDDO
C
C	loop over each domain and calculate the overlaps in the current
C	domain
C
  160           DO INOW = IFIRST_NOW,ISECOND_NOW
                  DO JNOW = JFIRST_NOW,JSECOND_NOW
                    OVERLAPPED = .FALSE.
                    DO IOTHER = IFIRST_OTHER,ISECOND_OTHER
                      DO JOTHER = JFIRST_OTHER,JSECOND_OTHER
                        OVER = ECLIPSE (IGEOM,XD(INOW,NNOW),
     &                                  XD(INOW+1,NNOW),YD(JNOW,NNOW),
     &                                  YD(JNOW+1,NNOW),
     &                                  XD(IOTHER,NOTHER),
     &                                  XD(IOTHER+1,NOTHER),
     &                                  YD(JOTHER,NOTHER),
     &                                  YD(JOTHER+1,NOTHER))
                        IF (OVER.GT.ZERO) THEN
                          OVERLAPPED = .TRUE.
C
                          NOVER = NOVER + 1
                          NPNT = MIN(NOVER,MAXOVER)
                          I_INTERIOR(NPNT) = IOTHER
                          IF (IGEOM.GE.20) THEN
                            J_INTERIOR(NPNT) = JOTHER
                          ELSE
                            J_INTERIOR(NPNT) = 1
                          ENDIF
                          N_INTERIOR(NPNT) = NOTHER
C
                          I_BOUNDARY(NPNT) = INOW
                          IF (IGEOM.GE.20) THEN
                            J_BOUNDARY(NPNT) = JNOW
                          ELSE
                            J_BOUNDARY(NPNT) = 1
                          ENDIF
                          N_BOUNDARY(NPNT) = NNOW
C
                          VOLUME_OVERLAP(NPNT) = OVER
C
                        ENDIF
                      ENDDO
                    ENDDO
                    IF (OVERLAPPED) THEN
                      NPOINT = NPOINT + 1
                      NPNT = MIN(NPOINT,MYANKA)
                      NYPTA(2,NNOW) = NPOINT
                      IYANKA(NPNT) = INOW
                      IF (IGEOM.GE.20) THEN
                        JYANKA(NPNT) = JNOW
                      ELSE
                        JYANKA(NPNT) = 1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
C
              ENDIF
C
C	see if there is any overlap between the interior of the
C	other domain and the right hand boundary cells of this
C	domain
C
              OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                        XMAX_INTERIOR_OTHER,YMIN_INTERIOR_OTHER,
     &                        YMAX_INTERIOR_OTHER,XMAX_INTERIOR_NOW,
     &                        XMAX_NOW,YMIN_NOW,YMAX_NOW)  
              IF (OVER.GT.ZERO) THEN  
C
C	there is an overlap between these domains, identify the
C	cells in each mesh to loop over
C     
                DO IOTHER = ISTART_OTHER,ISTOP_OTHER 
                  IFIRST_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMAX_INTERIOR_NOW,XMAX_NOW,ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 170
                ENDDO
C
  170           DO IOTHER = ISTOP_OTHER,ISTART_OTHER,-1
                  ISECOND_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMAX_INTERIOR_NOW,XMAX_NOW,ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 180
                ENDDO
C
  180           DO INOW = ISTOP_NOW+1,IMAX_NOW
                  IFIRST_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 190
                ENDDO
C
  190           DO INOW = IMAX_NOW-1,ISTOP_NOW+1,-1
                  ISECOND_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 200
                ENDDO
C
  200           DO JOTHER = JSTART_OTHER,JSTOP_OTHER 
                  JFIRST_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMIN_NOW,YMAX_NOW)
                  IF (OVER.GT.ZERO) GO TO 210
                ENDDO
C
  210           DO JOTHER = JSTOP_OTHER,JSTART_OTHER,-1
                  JSECOND_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMIN_NOW,YMAX_NOW)
                  IF (OVER.GT.ZERO) GO TO 220
                ENDDO
C
  220           DO JNOW = 2,JMAX_NOW-1
                  JFIRST_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 230
                ENDDO
C
  230           DO JNOW = JMAX_NOW-1,2,-1
                  JSECOND_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 240
                ENDDO
C
C	loop over each domain and calculate the overlaps in the current
C	domain
C
  240           DO INOW = IFIRST_NOW,ISECOND_NOW
                  DO JNOW = JFIRST_NOW,JSECOND_NOW
                    OVERLAPPED = .FALSE.
                    DO IOTHER = IFIRST_OTHER,ISECOND_OTHER
                      DO JOTHER = JFIRST_OTHER,JSECOND_OTHER
                        OVER = ECLIPSE (IGEOM,XD(INOW,NNOW),
     &                                  XD(INOW+1,NNOW),YD(JNOW,NNOW),
     &                                  YD(JNOW+1,NNOW),
     &                                  XD(IOTHER,NOTHER),
     &                                  XD(IOTHER+1,NOTHER),
     &                                  YD(JOTHER,NOTHER),
     &                                  YD(JOTHER+1,NOTHER))
                        IF (OVER.GT.ZERO) THEN
                          OVERLAPPED = .TRUE.
C
                          NOVER = NOVER + 1
                          NPNT = MIN(NOVER,MAXOVER)
                          I_INTERIOR(NPNT) = IOTHER
                          IF (IGEOM.GE.20) THEN
                            J_INTERIOR(NPNT) = JOTHER
                          ELSE
                            J_INTERIOR(NPNT) = 1
                          ENDIF
                          N_INTERIOR(NPNT) = NOTHER
C
                          I_BOUNDARY(NPNT) = INOW
                          IF (IGEOM.GE.20) THEN
                            J_BOUNDARY(NPNT) = JNOW
                          ELSE
                            J_BOUNDARY(NPNT) = 1
                          ENDIF
                          N_BOUNDARY(NPNT) = NNOW
C
                          VOLUME_OVERLAP(NPNT) = OVER
C
                        ENDIF
                      ENDDO
                    ENDDO
                    IF (OVERLAPPED) THEN
                      NPOINT = NPOINT + 1
                      NPNT = MIN(NPOINT,MYANKA)
                      NYPTA(2,NNOW) = NPOINT
                      IYANKA(NPNT) = INOW
                      IF (IGEOM.GE.20) THEN
                        JYANKA(NPNT) = JNOW
                      ELSE
                        JYANKA(NPNT) = 1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
C
              ENDIF
C
C	see if there is any overlap between the interior of the
C	other domain and the bottom boundary cells of this
C	domain
C
              IF (IGEOM.LT.20) THEN
                  YMIN_NOW = ZERO
                  YMAX_NOW = ZERO
              ENDIF
C
              OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                        XMAX_INTERIOR_OTHER,YMIN_INTERIOR_OTHER,
     &                        YMAX_INTERIOR_OTHER,XMIN_NOW,
     &                        XMIN_INTERIOR_NOW,YMIN_NOW,YMAX_NOW)  
              IF (OVER.GT.ZERO) THEN  
C
C	there is an overlap between these domains, identify the
C	cells in each mesh to loop over
C     
                DO JOTHER = JSTART_OTHER,JSTOP_OTHER 
                  JFIRST_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMIN_NOW,YMIN_INTERIOR_NOW)
                  IF (OVER.GT.ZERO) GO TO 250
                ENDDO
C
  250           DO JOTHER = JSTOP_OTHER,JSTART_OTHER,-1
                  JSECOND_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMIN_NOW,YMIN_INTERIOR_NOW)
                  IF (OVER.GT.ZERO) GO TO 260
                ENDDO
C
  260           DO JNOW = 2,JSTART_NOW-1 
                  JFIRST_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 270
                ENDDO
C
  270           DO JNOW = JSTART_NOW-1,2,-1
                  JSECOND_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 280
                ENDDO
C
  280           DO IOTHER = ISTART_OTHER,ISTOP_OTHER 
                  IFIRST_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMIN_INTERIOR_NOW,XMAX_INTERIOR_NOW,
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 290
                ENDDO
C
  290           DO IOTHER = ISTOP_OTHER,ISTART_OTHER,-1
                  ISECOND_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMIN_INTERIOR_NOW,XMAX_INTERIOR_NOW,
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 300
                ENDDO
C
  300           DO INOW = ISTART_NOW,ISTOP_NOW
                  IFIRST_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 310
                ENDDO
C
  310           DO INOW = ISTOP_NOW,ISTART_NOW,-1
                  ISECOND_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 320
                ENDDO
C
C	loop over each domain and calculate the overlaps in the current
C	domain
C
  320           DO INOW = IFIRST_NOW,ISECOND_NOW
                  DO JNOW = JFIRST_NOW,JSECOND_NOW
                    OVERLAPPED = .FALSE.
                    DO IOTHER = IFIRST_OTHER,ISECOND_OTHER
                      DO JOTHER = JFIRST_OTHER,JSECOND_OTHER
                        OVER = ECLIPSE (IGEOM,XD(INOW,NNOW),
     &                                  XD(INOW+1,NNOW),YD(JNOW,NNOW),
     &                                  YD(JNOW+1,NNOW),
     &                                  XD(IOTHER,NOTHER),
     &                                  XD(IOTHER+1,NOTHER),
     &                                  YD(JOTHER,NOTHER),
     &                                  YD(JOTHER+1,NOTHER))
                        IF (OVER.GT.ZERO) THEN
                          OVERLAPPED = .TRUE.
C
                          NOVER = NOVER + 1
                          NPNT = MIN(NOVER,MAXOVER)
                          I_INTERIOR(NPNT) = IOTHER
                          IF (IGEOM.GE.20) THEN
                            J_INTERIOR(NPNT) = JOTHER
                          ELSE
                            J_INTERIOR(NPNT) = 1
                          ENDIF
                          N_INTERIOR(NPNT) = NOTHER
C
                          I_BOUNDARY(NPNT) = INOW
                          IF (IGEOM.GE.20) THEN
                            J_BOUNDARY(NPNT) = JNOW
                          ELSE
                            J_BOUNDARY(NPNT) = 1
                          ENDIF
                          N_BOUNDARY(NPNT) = NNOW
C
                          VOLUME_OVERLAP(NPNT) = OVER
C
                        ENDIF
                      ENDDO
                    ENDDO
                    IF (OVERLAPPED) THEN
                      NPOINT = NPOINT + 1
                      NPNT = MIN(NPOINT,MYANKA)
                      NYPTA(2,NNOW) = NPOINT
                      IYANKA(NPNT) = INOW
                      IF (IGEOM.GE.20) THEN
                        JYANKA(NPNT) = JNOW
                      ELSE
                        JYANKA(NPNT) = 1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
C
              ENDIF
C
C	see if there is any overlap between the interior of the
C	other domain and the top boundary cells of this
C	domain
C
              OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                        XMAX_INTERIOR_OTHER,YMIN_INTERIOR_OTHER,
     &                        YMAX_INTERIOR_OTHER,XMAX_INTERIOR_NOW,
     &                        XMAX_NOW,YMIN_NOW,YMAX_NOW)  
              IF (OVER.GT.ZERO) THEN  
C
C	there is an overlap between these domains, identify the
C	cells in each mesh to loop over
C     
                DO JOTHER = JSTART_OTHER,JSTOP_OTHER 
                  JFIRST_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMAX_INTERIOR_NOW,YMAX_NOW)
                  IF (OVER.GT.ZERO) GO TO 330
                ENDDO
C
  330           DO JOTHER = JSTOP_OTHER,JSTART_OTHER,-1
                  JSECOND_OTHER = JOTHER
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JOTHER,NOTHER),
     &                            YD(JOTHER+1,NOTHER),ZERO,ONE,
     &                            YMAX_INTERIOR_NOW,YMAX_NOW)
                  IF (OVER.GT.ZERO) GO TO 340
                ENDDO
C
  340           DO JNOW = JSTOP_NOW+1,JMAX_NOW
                  JFIRST_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 350
                ENDDO
C
  350           DO JNOW = JMAX_NOW-1,JSTOP_NOW+1,-1
                  JSECOND_NOW = JNOW
                  OVER = ECLIPSE (IGEOM,ZERO,ONE,YMIN_INTERIOR_OTHER,
     &                            YMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            YD(JNOW,NNOW),YD(JNOW+1,NNOW))
                  IF (OVER.GT.ZERO) GO TO 360
                ENDDO
C
  360           DO IOTHER = ISTART_OTHER,ISTOP_OTHER 
                  IFIRST_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMIN_INTERIOR_NOW,XMAX_INTERIOR_NOW,
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 370
                ENDDO
C
  370           DO IOTHER = ISTOP_OTHER,ISTART_OTHER,-1
                  ISECOND_OTHER = IOTHER
                  OVER = ECLIPSE (IGEOM,XD(IOTHER,NOTHER),
     &                            XD(IOTHER+1,NOTHER),ZERO,ONE,
     &                            XMIN_INTERIOR_NOW,XMAX_INTERIOR_NOW,
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 380
                ENDDO
C
  380           DO INOW = ISTART_NOW,ISTOP_NOW
                  IFIRST_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 390
                ENDDO
C
  390           DO INOW = ISTOP_NOW,ISTART_NOW,-1
                  ISECOND_NOW = INOW
                  OVER = ECLIPSE (IGEOM,XMIN_INTERIOR_OTHER,
     &                            XMAX_INTERIOR_OTHER,ZERO,ONE,
     &                            XD(INOW,NNOW),XD(INOW+1,NNOW),
     &                            ZERO,ONE)
                  IF (OVER.GT.ZERO) GO TO 400
                ENDDO
C
C	loop over each domain and calculate the overlaps in the current
C	domain
C
  400           DO INOW = IFIRST_NOW,ISECOND_NOW
                  DO JNOW = JFIRST_NOW,JSECOND_NOW
                    OVERLAPPED = .FALSE.
                    DO IOTHER = IFIRST_OTHER,ISECOND_OTHER
                      DO JOTHER = JFIRST_OTHER,JSECOND_OTHER
                        OVER = ECLIPSE (IGEOM,XD(INOW,NNOW),
     &                                  XD(INOW+1,NNOW),YD(JNOW,NNOW),
     &                                  YD(JNOW+1,NNOW),
     &                                  XD(IOTHER,NOTHER),
     &                                  XD(IOTHER+1,NOTHER),
     &                                  YD(JOTHER,NOTHER),
     &                                  YD(JOTHER+1,NOTHER))
                        IF (OVER.GT.ZERO) THEN
                          OVERLAPPED = .TRUE.
C
                          NOVER = NOVER + 1
                          NPNT = MIN(NOVER,MAXOVER)
                          I_INTERIOR(NPNT) = IOTHER
                          IF (IGEOM.GE.20) THEN
                            J_INTERIOR(NPNT) = JOTHER
                          ELSE
                            J_INTERIOR(NPNT) = 1
                          ENDIF
                          N_INTERIOR(NPNT) = NOTHER
C
                          I_BOUNDARY(NPNT) = INOW
                          IF (IGEOM.GE.20) THEN
                            J_BOUNDARY(NPNT) = JNOW
                          ELSE
                            J_BOUNDARY(NPNT) = 1
                          ENDIF
                          N_BOUNDARY(NPNT) = NNOW
C
                          VOLUME_OVERLAP(NPNT) = OVER
C
                        ENDIF
                      ENDDO
                    ENDDO
                    IF (OVERLAPPED) THEN
                      NPOINT = NPOINT + 1
                      NPNT = MIN(NPOINT,MYANKA)
                      NYPTA(2,NNOW) = NPOINT
                      IYANKA(NPNT) = INOW
                      IF (IGEOM.GE.20) THEN
                        JYANKA(NPNT) = JNOW
                      ELSE
                        JYANKA(NPNT) = 1
                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
C
              ENDIF
C
            ENDIF
C                
          ENDIF
        ENDDO
      ENDDO
C
C	make sure we haven't exceeded the available storage for the
C	interior/boundary arrays
C
      IF (NOVER.GT.MAXOVER) THEN
        WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MAXOVER is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NOVER
        CALL ABORT
      ENDIF
C
C	if there were no overlaps, correct the NYPTA array
C
      DO N = 1,NDOM
        IF (NYPTA(1,N).GT.NYPTA(2,N)) THEN
          NYPTA(1,N) = 0
          NYPTA(2,N) = 0
        ENDIF
      ENDDO
C
C	make sure we haven't exceeded the available storage
C
      IF (NPOINT.GT.MYANKA) THEN
        WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MYANKA is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
        CALL ABORT
      ENDIF
C
C	write out the yank information to the overlap file
C*************** COMMENTED OUT TO MINIMIZE OUTPUT
C      DO N = 1,NDOM
C        WRITE(55,*) NYPTA(1,N),NYPTA(2,N)
C      ENDDO
C
C      IF (NPOINT.GT.0) THEN
C       DO N=1,NPOINT
C            WRITE(55,*) IYANKA(N),JYANKA(N)
C       ENDDO
C      ENDIF
C
C	now build the save arrays
C
C	loop over each domain and pick out cells for which to save
C	information in that domain.  
C************************************************
      NPOINT = 0
      NSAVE = 0
      NPX = 0
      NPY = 0
C
      DO NNOW = 1,NDOM
        N1 = NPOINT + 1
        N2 = N1 - 1
        DO N = 1,NOVER
          IF (N_INTERIOR(N).EQ.NNOW) THEN
            I = I_INTERIOR(N)
            J = J_INTERIOR(N)
C
C	see if we already have saved this cell
C
            OVERLAPPED = .FALSE.
            IF (N2.GE.N1) THEN
C
              DO NN = N1,N2
                IF (ISAVE(NN).EQ.I.AND.JSAVE(NN).EQ.J) THEN
                  OVERLAPPED = .TRUE.
                  N_ASSOCIATE = NN
                  GO TO 410
                ENDIF
              ENDDO
  410         CONTINUE
C
            ENDIF
C
C	if this is a new cell, save it
C
            IF (.NOT.OVERLAPPED) THEN
              NPOINT = NPOINT + 1
              NPNT = MIN(NPOINT,MXSAVE)
              ISAVE(NPNT) = I
              JSAVE(NPNT) = J
              N2 = NPOINT
              N_ASSOCIATE = NPOINT
C
C	compute pointers from this cell to TMP arrays
C
              NSAVE = NSAVE + 1
              NPX = NPX + 2
              NPY = NPY + 2
              NFILPT(NPNT) = NSAVE
              NPXPT(1,NPNT) = NPX-1
              NPXPT(2,NPNT) = NPX
              NPYPT(1,NPNT) = NPY-1
              NPYPT(2,NPNT) = NPY
C
            ENDIF
C
C	save the association between the boundary cell and this
C	saved cell
C
            BOUNDARY_ASSOCIATE(N) = N_ASSOCIATE  
          ENDIF
        ENDDO
C
C	set NLSAVE pointers for this domain
C
        IF (N2.GE.N1) THEN
          NLSAVE(1,NNOW) = N1
          NLSAVE(2,NNOW) = N2
        ELSE
          NLSAVE(1,NNOW) = 0
          NLSAVE(2,NNOW) = 0
        ENDIF
      ENDDO         
C
C	make sure we haven't exceeded the available storage
C
      IF (NPOINT.GT.MXSAVE) THEN
        WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MXSAVE is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
        CALL ABORT
      ENDIF
C
C	write out the save information to the overlap file
C
C*************** COMMENTED OUT TO MINIMIZE OUTPUT
C      DO N = 1,NDOM
C        WRITE(55,*) NLSAVE(1,N),NLSAVE(2,N)
C      ENDDO
C
C      IF (NPOINT.GT.0) THEN
C        DO N=1,NPOINT
C            WRITE(55,*) ISAVE(N),JSAVE(N),NFILPT(N),NPXPT(1,N),
C     &                                  NPXPT(2,N),NPYPT(1,N),NPYPT(2,N)
C        ENDDO
C      ENDIF
C************************************************
C
C	now build the put arrays
C
C	loop over each domain and pick out cells that need
C	information from another domain.  
C
      NPOINT = 0
C
      DO NNOW = 1,NDOM
        N1 = NPOINT + 1
        N2 = N1 - 1
        DO N = 1,NOVER
          IF (N_BOUNDARY(N).EQ.NNOW) THEN
            I = I_BOUNDARY(N)
            J = J_BOUNDARY(N)
            N_INT = N_INTERIOR(N)
            I_INT = I_INTERIOR(N)
            J_INT = J_INTERIOR(N)
            NPOINT = NPOINT + 1
            NPNT = MIN(NPOINT,MXLPUT)
            ILGET(NPNT) = I
            JLGET(NPNT) = J
            N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
            NLGTPT(NPNT) = NFILPT(N_ASSOCIATE)
            DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,-BIG,BIG,
     &             XD(I_INT,N_INT),XD(I_INT+1,N_INT),YD(J_INT,N_INT),
     &             YD(J_INT+1,N_INT))
            FLGET(NPNT) = VOLUME_OVERLAP(N)/DONATING_CELL_VOLUME
            IF (ABS(FLGET(NPNT)-ONE).LE.TOLER) FLGET(NPNT) = ONE
            N2 = NPOINT
          ENDIF
        ENDDO
C
C	set NLPUT pointers for this domain
C
        IF (N2.GE.N1) THEN
          NLPUT(1,NNOW) = N1
          NLPUT(2,NNOW) = N2
        ELSE
          NLPUT(1,NNOW) = 0
          NLPUT(2,NNOW) = 0
        ENDIF
      ENDDO         
C
C	make sure we haven't exceeded the available storage
C
      IF (NPOINT.GT.MXLPUT) THEN
        WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MXLPUT is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
        CALL ABORT
      ENDIF
C
C	write out the put information to the overlap file
C
C*************** COMMENTED OUT TO MINIMIZE OUTPUT
C      DO N = 1,NDOM
C        WRITE(55,*) NLPUT(1,N),NLPUT(2,N)
C      ENDDO
C
C      IF (NPOINT.GT.0) THEN
C        DO N=1,NPOINT
C            WRITE(55,*) ILGET(N),JLGET(N),NLGTPT(N),FLGET(N)
C        ENDDO
C      ENDIF
C************************************************
C
C	now build the putx arrays
C
C	loop over each domain and pick out cells that need
C	information from another domain.  
C
      NPOINT = 0
C
      DO NNOW = 1,NDOM
        N1 = NPOINT + 1
        N2 = N1 - 1
        DO N = 1,NOVER
          IF (N_BOUNDARY(N).EQ.NNOW) THEN
            I = I_BOUNDARY(N)
            J = J_BOUNDARY(N)
            XMID = PT5*(XD(I,NNOW) + XD(I+1,NNOW))
            N_INT = N_INTERIOR(N)
            I_INT = I_INTERIOR(N)
            J_INT = J_INTERIOR(N)
            XMID_INT = PT5*(XD(I_INT,N_INT) + XD(I_INT+1,N_INT))
C
C	look for overlap between the left hand side of the donating
C	cell and the left hand side of the receiving cell.  skip
C	any cells which are immediately adjacent to the right
C	hand interior boundary of this domain.  
C
            XBNDY = XD(I1(N_INT),N_INT)
            IF (.NOT.TOUCH(XBNDY,XD(I,NNOW)).AND.I.NE.I2(NNOW)+1) THEN
              OVER = ECLIPSE (IGEOM,XD(I,NNOW),XMID,YD(J,NNOW),
     &                        YD(J+1,NNOW),XD(I_INT,N_INT),XMID_INT,
     &                        YD(J_INT,N_INT),YD(J_INT+1,N_INT))
              IF (OVER.GT.ZERO) THEN
                NPOINT = NPOINT + 1
                NPNT = MIN(NPOINT,MXLPTX)
                ILGETX(NPNT) = I
                JLGETX(NPNT) = J
                N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
                NLGPTX(NPNT) = NPXPT(1,N_ASSOCIATE)
                LLRPTX(NPNT) = 1
                DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,-BIG,BIG,
     &                         XD(I_INT,N_INT),XMID_INT,YD(J_INT,N_INT),
     &                                                YD(J_INT+1,N_INT))
                FLGETX(NPNT) = OVER/DONATING_CELL_VOLUME
                IF (ABS(FLGETX(NPNT)-ONE).LE.TOLER) FLGETX(NPNT) = ONE
                N2 = NPOINT
              ENDIF
            ENDIF
C
C	look for overlap between the left hand side of the donating
C	cell and the right hand side of the receiving cell.  skip
C	any cells which are immediately adjacent to the left 
C	hand interior boundary of this domain.
C
            XBNDY = XD(I2(N_INT)+1,N_INT)
            IF (.NOT.TOUCH(XBNDY,XD(I+1,NNOW)).AND.I.NE.I1(NNOW)-1) THEN
              OVER = ECLIPSE (IGEOM,XMID,XD(I+1,NNOW),YD(J,NNOW),
     &                        YD(J+1,NNOW),XD(I_INT,N_INT),XMID_INT,
     &                        YD(J_INT,N_INT),YD(J_INT+1,N_INT))
              IF (OVER.GT.ZERO) THEN
                NPOINT = NPOINT + 1
                NPNT = MIN(NPOINT,MXLPTX)
                ILGETX(NPNT) = I+1
                JLGETX(NPNT) = J
                N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
                NLGPTX(NPNT) = NPXPT(1,N_ASSOCIATE)
                LLRPTX(NPNT) = 2
                DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,-BIG,BIG,
     &                         XD(I_INT,N_INT),XMID_INT,YD(J_INT,N_INT),
     &                                                YD(J_INT+1,N_INT))
                FLGETX(NPNT) = OVER/DONATING_CELL_VOLUME
                IF (ABS(FLGETX(NPNT)-ONE).LE.TOLER) FLGETX(NPNT) = ONE
                N2 = NPOINT
              ENDIF
            ENDIF
C
C	look for overlap between the right hand side of the donating
C	cell and the left hand side of the receiving cell.  skip
C	any cells which are immediately adjacent to the right
C	hand interior boundary of this domain.
C
            XBNDY = XD(I1(N_INT),N_INT)
            IF (.NOT.TOUCH(XBNDY,XD(I,NNOW)).AND.I.NE.I2(NNOW)+1) THEN
              OVER = ECLIPSE (IGEOM,XD(I,NNOW),XMID,YD(J,NNOW),
     &                        YD(J+1,NNOW),XMID_INT,XD(I_INT+1,N_INT),
     &                        YD(J_INT,N_INT),YD(J_INT+1,N_INT))
              IF (OVER.GT.ZERO) THEN
                NPOINT = NPOINT + 1
                NPNT = MIN(NPOINT,MXLPTX)
                ILGETX(NPNT) = I
                JLGETX(NPNT) = J
                N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
                NLGPTX(NPNT) = NPXPT(2,N_ASSOCIATE)
                LLRPTX(NPNT) = 1
                DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,-BIG,BIG,
     &                       XMID_INT,XD(I_INT+1,N_INT),YD(J_INT,N_INT),
     &                                                YD(J_INT+1,N_INT))
                FLGETX(NPNT) = OVER/DONATING_CELL_VOLUME
                IF (ABS(FLGETX(NPNT)-ONE).LE.TOLER) FLGETX(NPNT) = ONE
                N2 = NPOINT
              ENDIF
            ENDIF
C
C	look for overlap between the right hand side of the donating
C	cell and the right hand side of the receiving cell.  skip
C	any cells which are immediately adjacent to the left 
C	hand interior boundary of this domain.
C
            XBNDY = XD(I2(N_INT)+1,N_INT)
            IF (.NOT.TOUCH(XBNDY,XD(I+1,NNOW)).AND.I.NE.I1(NNOW)-1) THEN
              OVER = ECLIPSE (IGEOM,XMID,XD(I+1,NNOW),YD(J,NNOW),
     &                        YD(J+1,NNOW),XMID_INT,XD(I_INT+1,N_INT),
     &                        YD(J_INT,N_INT),YD(J_INT+1,N_INT))
              IF (OVER.GT.ZERO) THEN
                NPOINT = NPOINT + 1
                NPNT = MIN(NPOINT,MXLPTX)
                ILGETX(NPNT) = I+1
                JLGETX(NPNT) = J
                N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
                NLGPTX(NPNT) = NPXPT(2,N_ASSOCIATE)
                LLRPTX(NPNT) = 2
                DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,-BIG,BIG,
     &                       XMID_INT,XD(I_INT+1,N_INT),YD(J_INT,N_INT),
     &                                                YD(J_INT+1,N_INT))
                FLGETX(NPNT) = OVER/DONATING_CELL_VOLUME
                IF (ABS(FLGETX(NPNT)-ONE).LE.TOLER) FLGETX(NPNT) = ONE
                N2 = NPOINT
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
C	set NLPUTX pointers for this domain
C
        IF (N2.GE.N1) THEN
          NLPUTX(1,NNOW) = N1
          NLPUTX(2,NNOW) = N2
        ELSE
          NLPUTX(1,NNOW) = 0
          NLPUTX(2,NNOW) = 0
        ENDIF
      ENDDO         
C
C	make sure we haven't exceeded the available storage
C
      IF (NPOINT.GT.MXLPTX) THEN
        WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MXLPTX is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
        CALL ABORT
      ENDIF
C
C	write out the put information to the overlap file
C
C*************** COMMENTED OUT TO MINIMIZE OUTPUT
C      DO N = 1,NDOM
C        WRITE(55,*) NLPUTX(1,N),NLPUTX(2,N)
C      ENDDO
C
C      IF (NPOINT.GT.0) THEN
C        DO N=1,NPOINT
C            WRITE(55,*) ILGETX(N),JLGETX(N),NLGPTX(N),FLGETX(N),
C     &                                                         LLRPTX(N)
C        ENDDO
C      ENDIF
C************************************************
C
C	now build the puty arrays
C
      IF (IGEOM.GE.20) THEN
C
C	loop over each domain and pick out cells that need
C	information from another domain.  
C
        NPOINT = 0
C
        DO NNOW = 1,NDOM
          N1 = NPOINT + 1
          N2 = N1 - 1
          DO N = 1,NOVER
            IF (N_BOUNDARY(N).EQ.NNOW) THEN
              I = I_BOUNDARY(N)
              J = J_BOUNDARY(N)
              YMID = PT5*(YD(J,NNOW) + YD(J+1,NNOW))
              N_INT = N_INTERIOR(N)
              I_INT = I_INTERIOR(N)
              J_INT = J_INTERIOR(N)
              YMID_INT = PT5*(YD(J_INT,N_INT) + YD(J_INT+1,N_INT))
C
C	look for overlap between the bottom of the donating
C	cell and the bottom of the receiving cell.   skip
C	any cells which are immediately adjacent to the top
C	hand interior boundary of this domain.  
C
              YBNDY = YD(J1(N_INT),N_INT)
              IF (.NOT.TOUCH(YBNDY,YD(J,NNOW)).AND.J.NE.J2(NNOW)+1) THEN
                OVER = ECLIPSE (IGEOM,XD(I,NNOW),XD(I+1,NNOW),
     &                          YD(J,NNOW),YMID,XD(I_INT,N_INT),
     &                          XD(I_INT+1,N_INT),YD(J_INT,N_INT),
     &                          YMID_INT)
                IF (OVER.GT.ZERO) THEN
                  NPOINT = NPOINT + 1
                  NPNT = MIN(NPOINT,MXLPTY)
                  ILGETY(NPNT) = I
                  JLGETY(NPNT) = J
                  N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
                  NLGPTY(NPNT) = NPYPT(1,N_ASSOCIATE)
                  LLRPTY(NPNT) = 1
                  DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,-BIG,
     &                         BIG,XD(I_INT,N_INT),XD(I_INT+1,N_INT),
     &                         YD(J_INT,N_INT),YMID_INT)
                  FLGETY(NPNT) = OVER/DONATING_CELL_VOLUME
                  IF (ABS(FLGETY(NPNT)-ONE).LE.TOLER) FLGETY(NPNT) = ONE
                  N2 = NPOINT
                ENDIF
              ENDIF
C
C	look for overlap between the bottom of the donating
C	cell and the top of the receiving cell.  skip
C	any cells which are immediately adjacent to the bottom 
C	hand interior boundary of this domain.
C
              YBNDY = YD(J2(N_INT)+1,N_INT)
              IF (.NOT.TOUCH(YBNDY,YD(J+1,NNOW)).AND.J.NE.J1(NNOW)-1) 
     &                                                              THEN
                OVER = ECLIPSE (IGEOM,XD(I,NNOW),XD(I+1,NNOW),
     &                          YMID,YD(J+1,NNOW),XD(I_INT,N_INT),
     &                          XD(I_INT+1,N_INT),YD(J_INT,N_INT),
     &                          YMID_INT)
                IF (OVER.GT.ZERO) THEN
                  NPOINT = NPOINT + 1
                  NPNT = MIN(NPOINT,MXLPTY)
                  ILGETY(NPNT) = I
                  JLGETY(NPNT) = J+1
                  N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
                  NLGPTY(NPNT) = NPYPT(1,N_ASSOCIATE)
                  LLRPTY(NPNT) = 2
                  DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,
     &                              -BIG,BIG,
     &                              XD(I_INT,N_INT),XD(I_INT+1,N_INT),
     &                              YD(J_INT,N_INT),YMID_INT)
                  FLGETY(NPNT) = OVER/DONATING_CELL_VOLUME
                  IF (ABS(FLGETY(NPNT)-ONE).LE.TOLER) FLGETY(NPNT) = ONE
                  N2 = NPOINT
                ENDIF
              ENDIF
C
C	look for overlap between the top of the donating
C	cell and the bottom of the receiving cell.   skip
C	any cells which are immediately adjacent to the top
C	hand interior boundary of this domain.  
C
              YBNDY = YD(J1(N_INT),N_INT)
              IF (.NOT.TOUCH(YBNDY,YD(J,NNOW)).AND.J.NE.J2(NNOW)+1) THEN
                OVER = ECLIPSE (IGEOM,XD(I,NNOW),XD(I+1,NNOW),
     &                          YD(J,NNOW),YMID,XD(I_INT,N_INT),
     &                          XD(I_INT+1,N_INT),YMID_INT,
     &                          YD(J_INT+1,N_INT))
                IF (OVER.GT.ZERO) THEN
                  NPOINT = NPOINT + 1
                  NPNT = MIN(NPOINT,MXLPTY)
                  ILGETY(NPNT) = I
                  JLGETY(NPNT) = J
                  N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
                  NLGPTY(NPNT) = NPYPT(2,N_ASSOCIATE)
                  LLRPTY(NPNT) = 1
                  DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,
     &                               -BIG,BIG,
     &                                XD(I_INT,N_INT),XD(I_INT+1,N_INT),
     &                                YMID_INT,YD(J_INT+1,N_INT))
                  FLGETY(NPNT) = OVER/DONATING_CELL_VOLUME
                  IF (ABS(FLGETY(NPNT)-ONE).LE.TOLER) FLGETY(NPNT) = ONE
                  N2 = NPOINT
                ENDIF
              ENDIF
C
C	look for overlap between the top of the donating
C	cell and the top of the receiving cell.  skip
C	any cells which are immediately adjacent to the bottom 
C	hand interior boundary of this domain.
C
              YBNDY = YD(J2(N_INT)+1,N_INT)
              IF (.NOT.TOUCH(YBNDY,YD(J+1,NNOW)).AND.J.NE.J1(NNOW)-1) 
     &                                                              THEN
                OVER = ECLIPSE (IGEOM,XD(I,NNOW),XD(I+1,NNOW),
     &                        YMID,YD(J+1,NNOW),XD(I_INT,N_INT),
     &                        XD(I_INT+1,N_INT),YMID_INT,
     &                        YD(J_INT+1,N_INT))
                IF (OVER.GT.ZERO) THEN
                  NPOINT = NPOINT + 1
                  NPNT = MIN(NPOINT,MXLPTY)
                  ILGETY(NPNT) = I
                  JLGETY(NPNT) = J+1
                  N_ASSOCIATE = BOUNDARY_ASSOCIATE(N)
                  NLGPTY(NPNT) = NPYPT(2,N_ASSOCIATE)
                  LLRPTY(NPNT) = 2
                  DONATING_CELL_VOLUME = ECLIPSE(IGEOM,-BIG,BIG,
     &                                -BIG,BIG,
     &                                XD(I_INT,N_INT),XD(I_INT+1,N_INT),
     &                                YMID_INT,YD(J_INT+1,N_INT))
                  FLGETY(NPNT) = OVER/DONATING_CELL_VOLUME
                  IF (ABS(FLGETY(NPNT)-ONE).LE.TOLER) FLGETY(NPNT) = ONE
                  N2 = NPOINT
                ENDIF
              ENDIF
            ENDIF
          ENDDO
C
C	set NLPUTY pointers for this domain
C
          IF (N2.GE.N1) THEN
            NLPUTY(1,NNOW) = N1
            NLPUTY(2,NNOW) = N2
          ELSE
            NLPUTY(1,NNOW) = 0
            NLPUTY(2,NNOW) = 0
          ENDIF
        ENDDO         
C
C	make sure we haven't exceeded the available storage
C
        IF (NPOINT.GT.MXLPTY) THEN
          WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MXLPTY is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
          CALL ABORT
        ENDIF
C
C	write out the put information to the overlap file
C
C*************** COMMENTED OUT TO MINIMIZE OUTPUT
C        DO N = 1,NDOM
C          WRITE(55,*) NLPUTY(1,N),NLPUTY(2,N)
C        ENDDO
C
C        IF (NPOINT.GT.0) THEN
C          DO N=1,NPOINT
C              WRITE(55,*) ILGETY(N),JLGETY(N),NLGPTY(N),FLGETY(N),
C     &                                                         LLRPTY(N)
C          ENDDO
C        ENDIF
C************************************************
C
      ENDIF
C
      NPOINT = 0
      NOVER = 0
C
C	loop over domains and calculate for which cells to equilibrate x
C	velocity 
C
      DO NNOW = 1,NDOM
C
        IMAX_NOW = IDMAX(NNOW)
        JMAX_NOW = JDMAX(NNOW)
        ISTART_NOW = MAX(2,I1(NNOW))
        ISTOP_NOW = MIN(IMAX_NOW-1,I2(NNOW))
        JSTART_NOW = MAX(2,J1(NNOW))
        JSTOP_NOW = MIN(JMAX_NOW-1,J2(NNOW))
C
C	loop over other domains and look for overlaps with this
C	domain
C
        DO NOTHER = 1,NDOM
          IF (NOTHER.NE.NNOW) THEN
            IMAX_OTHER = IDMAX(NOTHER)
            JMAX_OTHER = JDMAX(NOTHER)
            ISTART_OTHER = MAX(2,I1(NOTHER))
            ISTOP_OTHER = MIN(IMAX_OTHER-1,I2(NOTHER))
            JSTART_OTHER = MAX(2,J1(NOTHER))
            JSTOP_OTHER = MIN(JMAX_OTHER-1,J2(NOTHER))
C
C	look at the left boundary of the current domain and see if
C	if intersects any vertical cell boundary of the other domain
C
            IF (ISTART_NOW.GT.2) THEN
              INOW = ISTART_NOW
              XNOW = XD(ISTART_NOW,NNOW)
              OVERLAPPED = .FALSE.
              DO I = ISTART_OTHER,ISTOP_OTHER+1
                IF (TOUCH(XNOW,XD(I,NOTHER))) THEN
                  OVERLAPPED = .TRUE.
                  IOTHER = I
                  GO TO 420
                ENDIF
              ENDDO
C
 420          IF (OVERLAPPED) THEN
C
C	there is an intersection, loop over the J index of both
C	domains and save the indices of the overlapped cells
C
                DO JNOW = JSTART_NOW,JSTOP_NOW
                  DO JOTHER = JSTART_OTHER,JSTOP_OTHER
                    OVER1 = .FALSE.
                    OVER2 = .FALSE.
                    OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JNOW,NNOW),
     &                              YD(JNOW+1,NNOW),ZERO,ONE,
     &                              YD(JOTHER,NOTHER),
     &                              YD(JOTHER+1,NOTHER))
                    IF (OVER.GT.ZERO) THEN
C
C	make sure we haven't connected the cell in this domain to
C	another domain yet.  if so, skip the calculations
C
                      IF (NPOINT.GT.0) THEN
                        NPNT = MIN(NPOINT,MAXOVER)
                        DO N = 1,NPNT
                          IF (INOW.EQ.I_INTERIOR(N).AND.
     &                        JNOW.EQ.J_INTERIOR(N).AND.
     &                        NNOW.EQ.N_INTERIOR(N)) THEN
                                  OVER1 = .TRUE.
                                  NOVER_NOW = N_BOUNDARY(N)
                          ENDIF
C
                          IF (IOTHER.EQ.I_INTERIOR(N).AND.
     &                        JOTHER.EQ.J_INTERIOR(N).AND.
     &                        NOTHER.EQ.N_INTERIOR(N)) THEN
                                  OVER2 = .TRUE.
                                  NOVER_NOW = N_BOUNDARY(N)
                          ENDIF
                        ENDDO
                      ENDIF
C
                      IF (.NOT.OVER1.OR..NOT.OVER2) THEN
C
                        IF (.NOT.OVER1.AND..NOT.OVER2) THEN
                          NOVER = NOVER + 1
                          NOVER_NOW = NOVER
                        ENDIF
C
                        IF (.NOT.OVER1) THEN
                            NPOINT = NPOINT + 1
                            NPNT = MIN(NPOINT,MAXOVER)
                            I_INTERIOR(NPNT) = INOW
                            J_INTERIOR(NPNT) = JNOW
                            N_INTERIOR(NPNT) = NNOW
                            N_BOUNDARY(NPNT) = NOVER_NOW
                        ENDIF
C
                        IF (.NOT.OVER2) THEN
                            NPOINT = NPOINT + 1
                            NPNT = MIN(NPOINT,MAXOVER)
                            I_INTERIOR(NPNT) = IOTHER
                            J_INTERIOR(NPNT) = JOTHER
                            N_INTERIOR(NPNT) = NOTHER
                            N_BOUNDARY(NPNT) = NOVER_NOW
                        ENDIF
C
                      ENDIF
C
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
            ENDIF
C
C	look at the right boundary of the current domain and see if
C	if intersects any vertical cell boundary of the other domain
C
            IF (ISTOP_NOW.LT.IMAX_NOW-1) THEN
              INOW = ISTOP_NOW+1
              XNOW = XD(ISTOP_NOW+1,NNOW)
              OVERLAPPED = .FALSE.
              DO I = ISTART_OTHER,ISTOP_OTHER+1
                IF (TOUCH(XNOW,XD(I,NOTHER))) THEN
                  OVERLAPPED = .TRUE.
                  IOTHER = I
                  GO TO 430
                ENDIF
              ENDDO
C
 430          IF (OVERLAPPED) THEN
C
C	there is an intersection, loop over the J index of both
C	domains and save the indices of the overlapped cells
C
                DO JNOW = JSTART_NOW,JSTOP_NOW
                  DO JOTHER = JSTART_OTHER,JSTOP_OTHER
                    OVER1 = .FALSE.
                    OVER2 = .FALSE.
                    OVER = ECLIPSE (IGEOM,ZERO,ONE,YD(JNOW,NNOW),
     &                              YD(JNOW+1,NNOW),ZERO,ONE,
     &                              YD(JOTHER,NOTHER),
     &                              YD(JOTHER+1,NOTHER))
                    IF (OVER.GT.ZERO) THEN
C
C	make sure we haven't connected the cell in this domain to
C	another domain yet.  if so, skip the calculations
C
                      IF (NPOINT.GT.0) THEN
                        NPNT = MIN(NPOINT,MAXOVER)
                        DO N = 1,NPNT
                          IF (INOW.EQ.I_INTERIOR(N).AND.
     &                        JNOW.EQ.J_INTERIOR(N).AND.
     &                        NNOW.EQ.N_INTERIOR(N)) THEN
                                  OVER1 = .TRUE.
                                  NOVER_NOW = N_BOUNDARY(N)
                          ENDIF
C
                          IF (IOTHER.EQ.I_INTERIOR(N).AND.
     &                        JOTHER.EQ.J_INTERIOR(N).AND.
     &                        NOTHER.EQ.N_INTERIOR(N)) THEN
                                  OVER2 = .TRUE.
                                  NOVER_NOW = N_BOUNDARY(N)
                          ENDIF
                        ENDDO
                      ENDIF
C
C
                      IF (.NOT.OVER1.OR..NOT.OVER2) THEN
C
                        IF (.NOT.OVER1.AND..NOT.OVER2) THEN
                          NOVER = NOVER + 1
                          NOVER_NOW = NOVER
                        ENDIF
C
                        IF (.NOT.OVER1) THEN
                            NPOINT = NPOINT + 1
                            NPNT = MIN(NPOINT,MAXOVER)
                            I_INTERIOR(NPNT) = INOW
                            J_INTERIOR(NPNT) = JNOW
                            N_INTERIOR(NPNT) = NNOW
                            N_BOUNDARY(NPNT) = NOVER_NOW
                        ENDIF
C
                        IF (.NOT.OVER2) THEN
                            NPOINT = NPOINT + 1
                            NPNT = MIN(NPOINT,MAXOVER)
                            I_INTERIOR(NPNT) = IOTHER
                            J_INTERIOR(NPNT) = JOTHER
                            N_INTERIOR(NPNT) = NOTHER
                            N_BOUNDARY(NPNT) = NOVER_NOW
                        ENDIF
C
                      ENDIF
C
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
            ENDIF
C
          ENDIF
        ENDDO
      ENDDO
C
C	make sure we haven't exceeded the available storage
C
      IF (NPOINT.GT.MAXOVER) THEN
         WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MAXOVER is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
         CALL ABORT
      ENDIF
C
C	now build arrays for the /ddeqlx/ common block
C
      NSAVE = NPOINT
      NPOINT = 0     
      DO NNOW = 1,NDOM
        N1 = NPOINT + 1
        N2 = N1-1
        DO N = 1,NSAVE
          IF (N_INTERIOR(N).EQ.NNOW) THEN
            NPOINT = NPOINT + 1
            N2 = NPOINT
            NPNT = MIN(NPOINT,MVXEQL)
            IVXSAV(NPNT) = I_INTERIOR(N)
            IF (IGEOM.GE.20) THEN
              JVXSAV(NPNT) = J_INTERIOR(N)
            ELSE
              JVXSAV(NPNT) = 1
            ENDIF
            NVXPNT(NPNT) = N_BOUNDARY(N)
          ENDIF
        ENDDO
C
C	set NVXSAV pointers for this domain
C
        IF (N2.GE.N1) THEN
          NVXSAV(1,NNOW) = N1
          NVXSAV(2,NNOW) = N2
        ELSE
          NVXSAV(1,NNOW) = 0
          NVXSAV(2,NNOW) = 0
        ENDIF
C
      ENDDO
C
C	make sure we haven't exceeded the available storage
C
      IF (NPOINT.GT.MVXEQL) THEN
        WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MVXEQL is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
        CALL ABORT
      ENDIF
C
C	write out the put information to the overlap file
C
C*************** COMMENTED OUT TO MINIMIZE OUTPUT
C      WRITE(55,*) NOVER
C
C      IF (NOVER.GT.0) THEN
C        DO N = 1,NDOM
C          WRITE(55,*) NVXSAV(1,N),NVXSAV(2,N)
C        ENDDO
C
C        IF (NPOINT.GT.0) THEN
C          DO N=1,NPOINT
C              WRITE(55,*) IVXSAV(N),JVXSAV(N),NVXPNT(N)
C          ENDDO
C        ENDIF
C
C      ENDIF
C************************************************
C
C	loop over domains and calculate for which cells to equilibrate y
C	velocity 
C
      IF (IGEOM.GE.20) THEN
        DO NNOW = 1,NDOM
C
          IMAX_NOW = IDMAX(NNOW)
          JMAX_NOW = JDMAX(NNOW)
          ISTART_NOW = MAX(2,I1(NNOW))
          ISTOP_NOW = MIN(IMAX_NOW-1,I2(NNOW))
          JSTART_NOW = MAX(2,J1(NNOW))
          JSTOP_NOW = MIN(JMAX_NOW-1,J2(NNOW))
C
C	loop over other domains and look for overlaps with this
C	domain
C
          DO NOTHER = 1,NDOM
            IF (NOTHER.NE.NNOW) THEN
              IMAX_OTHER = IDMAX(NOTHER)
              JMAX_OTHER = JDMAX(NOTHER)
              ISTART_OTHER = MAX(2,I1(NOTHER))
              ISTOP_OTHER = MIN(IMAX_OTHER-1,I2(NOTHER))
              JSTART_OTHER = MAX(2,J1(NOTHER))
              JSTOP_OTHER = MIN(JMAX_OTHER-1,J2(NOTHER))
C
C	look at the bottom boundary of the current domain and see if
C	if intersects any horizontal cell boundary of the other domain
C
              IF (JSTART_NOW.GT.2) THEN
                JNOW = JSTART_NOW
                YNOW = YD(JSTART_NOW,NNOW)
                OVERLAPPED = .FALSE.
                DO J = JSTART_OTHER,JSTOP_OTHER+1
                  IF (TOUCH(YNOW,YD(J,NOTHER))) THEN
                    OVERLAPPED = .TRUE.
                    JOTHER = J
                    GO TO 440
                  ENDIF
                ENDDO
C
 440          IF (OVERLAPPED) THEN
C
C	there is an intersection, loop over the I index of both
C	domains and save the indices of the overlapped cells
C
                  DO INOW = ISTART_NOW,ISTOP_NOW
                    DO IOTHER = ISTART_OTHER,ISTOP_OTHER
                      OVER1 = .FALSE.
                      OVER2 = .FALSE.
                      OVER = ECLIPSE (IGEOM,XD(INOW,NNOW),
     &                                XD(INOW+1,NNOW),ZERO,ONE,
     &                                XD(IOTHER,NOTHER),
     &                                XD(IOTHER+1,NOTHER),ZERO,ONE)
                      IF (OVER.GT.ZERO) THEN
C
C	make sure we haven't connected the cell in this domain to
C	another domain yet.  if so, skip the calculations
C
                        IF (NPOINT.GT.0) THEN
                          NPNT = MIN(NPOINT,MAXOVER)
                          DO N = 1,NPNT
                            IF (INOW.EQ.I_INTERIOR(N).AND.
     &                          JNOW.EQ.J_INTERIOR(N).AND.
     &                          NNOW.EQ.N_INTERIOR(N)) THEN
                                    OVER1 = .TRUE.
                                    NOVER_NOW = N_BOUNDARY(N)
                            ENDIF
C
                            IF (IOTHER.EQ.I_INTERIOR(N).AND.
     &                          JOTHER.EQ.J_INTERIOR(N).AND.
     &                          NOTHER.EQ.N_INTERIOR(N)) THEN
                                    OVER2 = .TRUE.
                                    NOVER_NOW = N_BOUNDARY(N)
                            ENDIF
                          ENDDO
                        ENDIF
C
                        IF (.NOT.OVER1.OR..NOT.OVER2) THEN
C
                          IF (.NOT.OVER1.AND..NOT.OVER2) THEN
                            NOVER = NOVER + 1
                            NOVER_NOW = NOVER
                          ENDIF
C
                          IF (.NOT.OVER1) THEN
                              NPOINT = NPOINT + 1
                              NPNT = MIN(NPOINT,MAXOVER)
                              I_INTERIOR(NPNT) = INOW
                              J_INTERIOR(NPNT) = JNOW
                              N_INTERIOR(NPNT) = NNOW
                              N_BOUNDARY(NPNT) = NOVER_NOW
                          ENDIF
C
                          IF (.NOT.OVER2) THEN
                              NPOINT = NPOINT + 1
                              NPNT = MIN(NPOINT,MAXOVER)
                              I_INTERIOR(NPNT) = IOTHER
                              J_INTERIOR(NPNT) = JOTHER
                              N_INTERIOR(NPNT) = NOTHER
                              N_BOUNDARY(NPNT) = NOVER_NOW
                          ENDIF
C
                        ENDIF
C
                      ENDIF
                    ENDDO
                  ENDDO
                ENDIF
              ENDIF
C
C	look at the top boundary of the current domain and see if
C	if intersects any vertical cell boundary of the other domain
C
              IF (JSTOP_NOW.LT.JMAX_NOW-1) THEN
                JNOW = JSTOP_NOW+1
                YNOW = YD(JSTOP_NOW+1,NNOW)
                OVERLAPPED = .FALSE.
                DO J = JSTART_OTHER,JSTOP_OTHER+1
                  IF (TOUCH(YNOW,YD(J,NOTHER))) THEN
                    OVERLAPPED = .TRUE.
                    JOTHER = J
                    GO TO 450
                  ENDIF
                ENDDO
C
 450            IF (OVERLAPPED) THEN
C
C	there is an intersection, loop over the I index of both
C	domains and save the indices of the overlapped cells
C
                  DO INOW = ISTART_NOW,ISTOP_NOW
                    DO IOTHER = ISTART_OTHER,ISTOP_OTHER
                      OVERLAPPED = .FALSE.
                      OVER1 = .FALSE.
                      OVER2 = .FALSE.
                      OVER = ECLIPSE (IGEOM,XD(INOW,NNOW),
     &                              XD(INOW+1,NNOW),ZERO,ONE,
     &                              XD(IOTHER,NOTHER),
     &                              XD(IOTHER+1,NOTHER),ZERO,ONE)
                      IF (OVER.GT.ZERO) THEN
C
C	make sure we haven't connected the cell in this domain to
C	another domain yet.  if so, skip the calculations
C
                        IF (NPOINT.GT.0) THEN
                          NPNT = MIN(NPOINT,MAXOVER)
                          DO N = 1,NPNT
                            IF (INOW.EQ.I_INTERIOR(N).AND.
     &                          JNOW.EQ.J_INTERIOR(N).AND.
     &                          NNOW.EQ.N_INTERIOR(N)) THEN
                                    OVER1 = .TRUE.
                                    NOVER_NOW = N_BOUNDARY(N)
                            ENDIF
C
                            IF (IOTHER.EQ.I_INTERIOR(N).AND.
     &                          JOTHER.EQ.J_INTERIOR(N).AND.
     &                          NOTHER.EQ.N_INTERIOR(N)) THEN
                                    OVER2 = .TRUE.
                                    NOVER_NOW = N_BOUNDARY(N)
                            ENDIF
                          ENDDO
                        ENDIF
C
                        IF (.NOT.OVER1.OR..NOT.OVER2) THEN
C
                          IF (.NOT.OVER1.AND..NOT.OVER2) THEN
                            NOVER = NOVER + 1
                            NOVER_NOW = NOVER
                          ENDIF
C
                          IF (.NOT.OVER1) THEN
                              NPOINT = NPOINT + 1
                              NPNT = MIN(NPOINT,MAXOVER)
                              I_INTERIOR(NPNT) = INOW
                              J_INTERIOR(NPNT) = JNOW
                              N_INTERIOR(NPNT) = NNOW
                              N_BOUNDARY(NPNT) = NOVER_NOW
                          ENDIF
C
                          IF (.NOT.OVER2) THEN
                              NPOINT = NPOINT + 1
                              NPNT = MIN(NPOINT,MAXOVER)
                              I_INTERIOR(NPNT) = IOTHER
                              J_INTERIOR(NPNT) = JOTHER
                              N_INTERIOR(NPNT) = NOTHER
                              N_BOUNDARY(NPNT) = NOVER_NOW
                          ENDIF
C
                        ENDIF
C
                      ENDIF
                    ENDDO
                  ENDDO
                ENDIF
              ENDIF
C
            ENDIF
          ENDDO
        ENDDO
C
C	make sure we haven't exceeded the available storage
C
        IF (NPOINT.GT.MAXOVER) THEN
           WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MAXOVER is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
           CALL ABORT
        ENDIF
C
C	now build arrays for the /ddeqly/ common block
C
        NSAVE = NPOINT
        NPOINT = 0     
        DO NNOW = 1,NDOM
          N1 = NPOINT + 1
          N2 = N1-1
          DO N = 1,NSAVE
            IF (N_INTERIOR(N).EQ.NNOW) THEN
              NPOINT = NPOINT + 1
              N2 = NPOINT
              NPNT = MIN(NPOINT,MVYEQL)
              IVYSAV(NPNT) = I_INTERIOR(N)
              JVYSAV(NPNT) = J_INTERIOR(N)
              NVYPNT(NPNT) = N_BOUNDARY(N)
            ENDIF
          ENDDO
C
C    	set NVYSAV pointers for this domain
C
          IF (N2.GE.N1) THEN
            NVYSAV(1,NNOW) = N1
            NVYSAV(2,NNOW) = N2
          ELSE
            NVYSAV(1,NNOW) = 0
            NVYSAV(2,NNOW) = 0
          ENDIF
C
        ENDDO
C
C	make sure we haven't exceeded the available storage
C
        IF (NPOINT.GT.MVYEQL) THEN
          WRITE(*,'(/'' Storage exeeded in subroutine OVRLAP, '',
     &             ''abort.''/'' MVYEQL is too small, it should be '',
     &             ''set to at least '',i10,'' for this problem.''/)')
     &             NPOINT
          CALL ABORT
        ENDIF
C
C	write out the put information to the overlap file
C
C*************** COMMENTED OUT TO MINIMIZE OUTPUT
C        WRITE(55,*) NOVER
C
C        IF (NOVER.GT.0) THEN
C          DO N = 1,NDOM
C            WRITE(55,*) NVYSAV(1,N),NVYSAV(2,N)
C          ENDDO
C    
C          IF (NPOINT.GT.0) THEN
C            DO N=1,NPOINT
C                WRITE(55,*) IVYSAV(N),JVYSAV(N),NVYPNT(N)
C            ENDDO
C          ENDIF
C  
C        ENDIF
C************************************************
C
      ENDIF
C
      END
