! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail.com)                                              
                                                                                                         
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and      
! associated documentation files (the "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to   
! the following conditions:                                                                              
                                                                                                         
! The above copyright notice and this permission notice shall be included in all copies or substantial   
! portions of the Software and documentation.                                                                              
                                                                                                         
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS                                
! OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                            
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                            
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                                 
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,                          
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN                              
! THE SOFTWARE.                                                                                          
! _______________________________________________________________________________________________________
                                                                                                        
! End MIT license text.                                                                                      

       SUBROUTINE EIG_INV_PWR
  
! Solves for eigenvalues and eigenvectors when method is INV. Code is only valid for the 1st eigenval/vec. Inverse Power is an
! iterative method
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, KMSM_SDIA, LINKNO, NDOFL, NTERM_KLL, NTERM_KLLD, NTERM_KMSM,     &
                                         NTERM_KMSMs, NTERM_MLL, NUM_EIGENS, NVEC, SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  BAILOUT, EPSIL, KLLRAT, MXITERI, SOLLIB, SPARSTOR, SUPINFO, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  EIG_INV_PWR_BEGEND
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VAL, EIGEN_VEC, MODE_NUM
      USE MODEL_STUF, ONLY            :  EIG_N2, EIG_SIGMA
      USE SPARSE_MATRICES, ONLY       :  I_KLL, J_KLL, KLL, I_KLLD, J_KLLD, KLLD, I_MLL, J_MLL, MLL,                               &
                                         I_KMSM, I2_KMSM, J_KMSM, KMSM, I_KMSMs, I2_KMSMs, J_KMSMs, KMSMs 
      USE SPARSE_MATRICES, ONLY       :  SYM_KLL, SYM_KLLD, SYM_MLL
      USE LAPACK_LIN_EQN_DPB
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE EIG_INV_PWR_USE_IFs

      IMPLICIT NONE
  
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EIG_INV_PWR'
      CHARACTER(  1*BYTE)             :: EQUED             ! 'Y' if KLL stiff matrix was equilibrated in subr EQUILIBRATE    
      CHARACTER(44*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run.

      INTEGER(LONG)                   :: DEB_PRT(2)        ! Debug numbers to say whether to write ABAND and/or its decomp to output
!                                                            file in called subr SYM_MAT_DECOMP_LAPACK (ABAND = band form of KLL)

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: INFO        = 0   ! Input value for subr SYM_MAT_DECOMP_LAPACK (quit on sing KRRCB)
      INTEGER(LONG)                   :: ITER_NUM          ! Number of iterations in converging on eigenvalue 

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EIG_INV_PWR_BEGEND

      REAL(DOUBLE)                    :: EIGEN_VAL_APPROX(0:MXITERI)
                                                           ! Eigenvalue at a given iteration number

      REAL(DOUBLE)                    :: K_INORM           ! Inf norm of KOO matrix
      REAL(DOUBLE)                    :: MVEC(NDOFL,1)     ! MLL*EIGEN_VEC (or KLLD*EIGEN_VEC for BUCKLING)
      REAL(DOUBLE)                    :: MAX_VALUE         ! Max value from EIGEN_VEC(I,1)
      REAL(DOUBLE)                    :: NULL_SCALE_FACS(NDOFL)
                                                           ! KMSM will not be equilibrated so set these to zero
      REAL(DOUBLE)                    :: PERCENT_CHANGE    ! % change in eigenvalue estimate between two successive iterations
      REAL(DOUBLE)                    :: RCOND             ! Recrip of cond no. of the KLL. Det in  subr COND_NUM

      INTRINSIC                       :: MIN

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Check that user did not ask for more than 1 eigen (currently Inverse Power can't be used to find more than 1 eigen).
! If request is > NDOFL-1, decrease request to 1 and give warning

      IF (EIG_N2 > 1) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,4901) EIG_N2
         IF (SUPWARN == 'N') THEN
            WRITE(F06,4901) EIG_N2
         ENDIF
         EIG_N2 = 1
      ENDIF

      CALL OURTIM
      MODNAM = 'SOLVE FOR EIGENVALS/VECTORS - INVERSE POWER METH'
      WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

! Calc KMSM = KLL - EIG_SIGMA*MLL (or - EIG_SIGMA*KLLD for BUCKLING) where EIG_SIGMA = shift freq

      IF (SOL_NAME(1:8) == 'BUCKLING') THEN
         CALL MATADD_SSS_NTERM ( NDOFL, 'KLL',  NTERM_KLL , I_KLL , J_KLL , SYM_KLL ,  'eig_sigma*KLLD',                           &
                                                NTERM_KLLD, I_KLLD, J_KLLD, SYM_KLLD, 'KMSM', NTERM_KMSM )
         CALL ALLOCATE_SPARSE_MAT ( 'KMSM', NDOFL, NTERM_KMSM, SUBR_NAME )
         CALL MATADD_SSS       ( NDOFL, 'KLL' , NTERM_KLL , I_KLL , J_KLL , KLL , ONE, 'eig_sigma*KLLD',                           &
                                                NTERM_KLLD, I_KLLD, J_KLLD, KLLD, EIG_SIGMA,                                       &
                                        'KMSM', NTERM_KMSM, I_KMSM, J_KMSM, KMSM )

      ELSE
         CALL MATADD_SSS_NTERM ( NDOFL, 'KLL',  NTERM_KLL , I_KLL , J_KLL , SYM_KLL ,  '-eig_sigma*MLL',                           &
                                                NTERM_MLL , I_MLL , J_MLL , SYM_MLL , 'KMSM', NTERM_KMSM )
         CALL ALLOCATE_SPARSE_MAT ( 'KMSM', NDOFL, NTERM_KMSM, SUBR_NAME )
         CALL MATADD_SSS       ( NDOFL, 'KLL' , NTERM_KLL , I_KLL , J_KLL , KLL , ONE, '-eig_sigma*MLL',                           &
                                                NTERM_MLL , I_MLL , J_MLL , MLL, -EIG_SIGMA,                                       &
                                        'KMSM', NTERM_KMSM, I_KMSM, J_KMSM, KMSM )
      ENDIF

! If this is not a CB or BUCKLING soln, dellocate arrays for KLL.      ! Keep arrays MLL, KLLD. Need them later to calc gen mass

      IF ((SOL_NAME(1:12) /= 'GEN CB MODEL' ) .AND. (SOL_NAME(1:8) /= 'BUCKLING')) THEN
         CALL OURTIM
         MODNAM = 'DEALLOCATE SPARSE KLL ARRAYS'
         WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         WRITE(SC1,32345,ADVANCE='NO') '       Deallocate KLL', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'KLL' )
      ENDIF

! Allocate arrays for eigen matrices

      CALL ALLOCATE_EIGEN1_MAT ( 'EIGEN_VEC', NDOFL, EIG_N2, SUBR_NAME )
      CALL ALLOCATE_EIGEN1_MAT ( 'MODE_NUM' , NDOFL, 1, SUBR_NAME )
      CALL ALLOCATE_EIGEN1_MAT ( 'EIGEN_VAL', NDOFL, 1, SUBR_NAME )

!***********************************************************************************************************************************
! Factor KMSM

      DEB_PRT(1) = 44
      DEB_PRT(2) = 45

      EQUED = 'N'
      IF (SOLLIB == 'BANDED') THEN

         INFO = 0
         CALL SYM_MAT_DECOMP_LAPACK ( SUBR_NAME, 'KMSM', 'L ', NDOFL, NTERM_KMSM, I_KMSM, J_KMSM, KMSM, 'Y', KLLRAT, 'N', 'N',     &
                                      DEB_PRT, EQUED, KMSM_SDIA, K_INORM, RCOND, NULL_SCALE_FACS, INFO )

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,9991) SUBR_NAME, SOLLIB
         WRITE(F06,9991) SUBR_NAME, SOLLIB
         CALL OUTA_HERE ( 'Y' )

      ENDIF

      IF (EQUED == 'Y') THEN                               ! If EQUED == 'Y' then error. We don't want KMSM equilibrated from the
         WRITE(ERR,4001) SUBR_NAME, EQUED                  ! call (above) to SYM_MAT_DECOMP_LAPACK 
         WRITE(F06,4001) SUBR_NAME, EQUED
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      DO I=1,NDOFL
         NULL_SCALE_FACS(I) = ZERO
      ENDDO

!***********************************************************************************************************************************
! Get starting value for eigenvector

      DO I=1,NDOFL
         EIGEN_VEC(I,1) = ONE
      ENDDO

! Loop until convergence

      EIGEN_VAL_APPROX(0) = ONE

      IF (DEBUG(46) == 1) THEN
         WRITE(F06,4902) EIGEN_VAL_APPROX(0)
      ENDIF

      ITER_NUM = 0
iters:DO

         ITER_NUM = ITER_NUM + 1

! Mult MLL*EIGEN_VEC (or KLLD*EIGEN_VEC for BUCKLING) to get MVEC. This is the "RHS" in the solution
! [KLL - sigma*MLL]*Vec = alpha*MLL*Vec (EIGEN_VAL = sigma + 1/alpha)

         IF (SOL_NAME(1:8) == 'BUCKLING') THEN
            CALL MATMULT_SFF ('KLLD', NDOFL, NDOFL, NTERM_KLLD, SYM_KLLD, I_KLLD, J_KLLD, KLLD, 'EIGENVEC', NDOFL, 1,              &
                               EIGEN_VEC, 'N', 'MVEC',-ONE, MVEC)
         ELSE
            CALL MATMULT_SFF ('MLL' , NDOFL, NDOFL, NTERM_MLL , SYM_MLL , I_MLL , J_MLL , MLL , 'EIGENVEC', NDOFL, 1,              &
                               EIGEN_VEC, 'N', 'MVEC', ONE, MVEC)
         ENDIF

         IF      (SOLLIB == 'BANDED') THEN
            CALL FBS_LAPACK ( 'N', NDOFL, KMSM_SDIA, NULL_SCALE_FACS, MVEC )
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,9991) SUBR_NAME, SOLLIB
            WRITE(F06,9991) SUBR_NAME, SOLLIB
            CALL OUTA_HERE ( 'Y' )
         ENDIF

         MAX_VALUE = ZERO                                  ! Determine max term in approximate MVEC and check for > 0
         DO I=1,NDOFL
            IF (DABS(MVEC(I,1)) > DABS(MAX_VALUE)) THEN
               MAX_VALUE = MVEC(I,1)
            ENDIF
         ENDDO

         IF (DABS(MAX_VALUE) > EPSIL(1)) THEN              ! If max value in eigenvector > 0 then get next eigenvalue approx

                                                           ! If MVEC had its max numerical value repeated with a different sign,
!                                                            the algorithm could converge to the negative of the actual eigenvalue.
!                                                            This algorithm is valid only for positive eigens, so use ABS(MAX_VALUE)
            EIGEN_VAL_APPROX(ITER_NUM) = ONE/DABS(MAX_VALUE)

         ELSE                                              ! If all values in eigenvector are zero, write messages and quit

            FATAL_ERR = FATAL_ERR + 1
            IF (ITER_NUM > 1) THEN
               IF (DEBUG(46) == 1) THEN
                  WRITE(F06,*)
               ELSE
                  CALL WRITE_ITER_RESULTS
               ENDIF
            ENDIF
            WRITE(ERR,4007) ITER_NUM, MAX_VALUE
            WRITE(F06,4007) ITER_NUM, MAX_VALUE
            CALL OUTA_HERE ( 'Y' )

         ENDIF
                                                           ! Calc % change in 2 successive estimates (if den /= 0)
         IF (DABS(EIGEN_VAL_APPROX(ITER_NUM-1)) > EPSIL(1)) THEN
            PERCENT_CHANGE = 1.0D2*(EIGEN_VAL_APPROX(ITER_NUM) - EIGEN_VAL_APPROX(ITER_NUM-1))/EIGEN_VAL_APPROX(ITER_NUM-1)
            WRITE(SC1,12345,ADVANCE='NO') ITER_NUM, EIG_SIGMA+EIGEN_VAL_APPROX(ITER_NUM), PERCENT_CHANGE, CR13
            IF (DEBUG(46) == 1) THEN
               WRITE(F06,4903) ITER_NUM, EIG_SIGMA+EIGEN_VAL_APPROX(ITER_NUM), PERCENT_CHANGE
            ENDIF
         ELSE
            WRITE(SC1,22345,ADVANCE='NO') ITER_NUM, EIG_SIGMA+EIGEN_VAL_APPROX(ITER_NUM), CR13
            IF (DEBUG(46) == 1) THEN
               WRITE(F06,4904) ITER_NUM, EIG_SIGMA+EIGEN_VAL_APPROX(ITER_NUM)
            ENDIF
         ENDIF

         DO I=1,NDOFL                                      ! Normalize eigenvector to max value
            EIGEN_VEC(I,1) = MVEC(I,1)/MAX_VALUE
         ENDDO

         IF (DABS(PERCENT_CHANGE) < EPSIL(3)) THEN         ! Test for convergence
            IF (DEBUG(46) == 1) THEN
               WRITE(F06,*)
            ENDIF
            EXIT iters
         ELSE
            IF (ITER_NUM < MXITERI) THEN
               CYCLE iters
            ELSE
               IF (ITER_NUM > 1) THEN
                  IF (DEBUG(46) == 1) THEN
                     WRITE(F06,*)
                  ELSE
                     CALL WRITE_ITER_RESULTS
                  ENDIF
               ENDIF
               WRITE(ERR,4006) MXITERI, EPSIL(3)
               WRITE(F06,4006) MXITERI, EPSIL(3)
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF
         ENDIF

      ENDDO iters

      NVEC = 1
      NUM_EIGENS = 1
      EIGEN_VAL(1) = EIG_SIGMA + EIGEN_VAL_APPROX(ITER_NUM)
      WRITE(ERR,4008) ITER_NUM, EPSIL(3)
      IF (SUPINFO == 'N') THEN
         WRITE(F06,4008) ITER_NUM, EPSIL(3)
      ENDIF

      DO I=1,NUM_EIGENS
         MODE_NUM(I) = I
      ENDDO

      WRITE(SC1,32345,ADVANCE='NO') '       Deallocate KMSM'
      CALL DEALLOCATE_SPARSE_MAT ( 'KMSM' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

 9892 FORMAT('               THIS IS FOR ROW AND COL IN THE MATRIX FOR GRID POINT ',I8,' COMPONENT ',I3)

 4001 FORMAT(' *ERROR  4001: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' MATRIX KMSM WAS EQUILIBRATED: EQUED = ',A,'. CODE NOT WRITTEN TO ALLOW THIS AS YET')

 4006 FORMAT(' *ERROR  4006: MAXIMUM ITERATIONS = ',I8,' HAVE BEEN TAKEN WITHOUT CONVERGING ON AN EIGENVALUE.'                     &
                    ,/,14X,' BULK DATA PARAM MXITERI CAN BE USED TO INCREASE THE NUMBER OF ITERATIONS OR PARAM EPSIL 3 = ',1ES14.6 &
                    ,/,14X,' (THE ITERATION % CHANGE CRITERIA) CAN BE CHANGED')

 4007 FORMAT(' *ERROR  4007: CANNOT CONTINUE EIGENVALUE ITERATION. ALL OF THE TERMS IN THE EIGENVECTOR FOR ITERATION NUMBER ',I4,  &
                           ' ARE LESS THAN ',1ES15.6)

 4008 FORMAT(' *INFORMATION: THE INVERSE POWER METHOD PERFORMED ',I8,' ITERATIONS TO CONVERGE TO THE FIRST EIGENVALUE WITHIN '     &
                            ,1ES9.1,'%')

 4092 FORMAT(1X,I2,'/',A44,18X,2X,I2,':',I2,':',I2,'.',I3)

 4901 FORMAT(' *WARNING    : REQUEST FOR ',I8,' EIGENVALUES CANNOT BE HONORED. INVERSE POWER CAN BE USED TO FIND NO MORE THAN ONE' &
                 ,I8,/,14X,' ATTEMPT WILL BE MADE TO FIND ONE EIGENVALUE')

 4902 FORMAT(43X,'Results of Inverse Power iteration on eigenvalue    1',//,42X,'Iter No.       Approx Eigenvalue     ',           &
                 '% Change from last',//,52X,1ES23.14)
 
 4903 FORMAT(45X,I4,3X,1ES23.14,8X,1ES9.2)

 4904 FORMAT(45X,I4,3X,1ES23.14)

 9991 FORMAT(' *ERROR  9991: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' SOLLIB = ',A,' NOT PROGRAMMED ',A)

12345 FORMAT(10X,I4,3X,1ES15.6,2X,1ES15.2,A)

22345 FORMAT(10X,I4,3X,1ES15.6)

32345 FORMAT(A,10X)

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE WRITE_ITER_RESULTS

      IMPLICIT NONE

      INTEGER(LONG)                   :: II                ! DO loop index

! **********************************************************************************************************************************
      WRITE(f06,4912) EIGEN_VAL_APPROX(0)

      DO II=1,ITER_NUM
         WRITE(F06,4913) II, EIG_SIGMA+EIGEN_VAL_APPROX(II)
      ENDDO
      WRITE(F06,*)

! **********************************************************************************************************************************
 4912 FORMAT(39X,'Results of Inverse Power iteration on eigenvalue    1',//,52X,'Iter No.  Approx Eigenvalue',//,62X,1ES15.6)
 
 4913 FORMAT(55X,I4,3X,1ES15.6)

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_ITER_RESULTS

      END SUBROUTINE EIG_INV_PWR
