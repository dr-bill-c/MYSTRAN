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

      SUBROUTINE SOLVE6_SETUP ( CALLING_SUBR )

! Sets up the decomp of KLL or KLLs for either subr SOLVE_DLR or subr SOLVE_PHIZL1.

! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, KLL_SDIA, NDOFL, NTERM_KLL, NTERM_KLLs
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  EPSIL, SOLLIB, SPARSTOR
      USE SUBR_BEGEND_LEVELS, ONLY    :  SOLVE6_SETUP_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KLL, J_KLL, KLL, I_KLLs, J_KLLs, KLLs
      USE LAPACK_LIN_EQN_DPB

      USE SOLVE6_SETUP_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SOLVE6_SETUP'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Name of subr that called thi one
      CHARACTER(  1*BYTE)             :: EQUED             ! 'Y' if KLL stiff matrix was equilibrated in subr EQUILIBRATE    
 
      INTEGER(LONG)                   :: DEB_PRT(2)        ! Debug numbers to say whether to write ABAND and/or its decomp to output
!                                                            file in called subr SYM_MAT_DECOMP_LAPACK (ABAND = band form of KOO)

      INTEGER(LONG)                   :: INFO        = 0   ! Input value for subr SYM_MAT_DECOMP_LAPACK (quit on sing KRRCB)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SOLVE6_SETUP_BEGEND

      REAL(DOUBLE)                    :: EQUIL_SCALE_FACS(NDOFL)
                                                           ! LAPACK_S values returned from subr SYM_MAT_DECOMP_LAPACK

      REAL(DOUBLE)                    :: K_INORM           ! Inf norm of KLL matrix (det in  subr COND_NUM)
      REAL(DOUBLE)                    :: RCOND             ! Recrip of cond no. of the KLL. Det in  subr COND_NUM
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DEB_PRT(1) = 64
      DEB_PRT(2) = 65

      IF (SOLLIB == 'BANDED') THEN

         INFO = 0
         EQUED = 'N'
         CALL SYM_MAT_DECOMP_LAPACK ( SUBR_NAME, 'KLL', 'L ', NDOFL, NTERM_KLL, I_KLL, J_KLL, KLL, 'Y', 'N', 'N', 'N', DEB_PRT,    &
                                      EQUED, KLL_SDIA, K_INORM, RCOND, EQUIL_SCALE_FACS, INFO )
         IF (EQUED == 'Y') THEN                            ! If EQUED == 'Y' then error. We don't want KLL equilibrated
            WRITE(ERR,6001) SUBR_NAME, EQUED
            WRITE(F06,6001) SUBR_NAME, EQUED
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF

      ELSE IF (SOLLIB == 'SPARSE  ') THEN

         ! Add sparse matrix code here to decompose the KLL stiffness matrix

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,9991) SUBR_NAME, SOLLIB
         WRITE(F06,9991) SUBR_NAME, SOLLIB
         CALL OUTA_HERE ( 'Y' )

      ENDIF

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

  972 FORMAT(' *ERROR   972: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INVALID ARG CALLING_SUBR = ',A)

 6001 FORMAT(' *ERROR  6001: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' MATRIX KLL WAS EQUILIBRATED: EQUED = ',A,'. CODE NOT WRITTEN TO ALLOW THIS AS YET')

 9991 FORMAT(' *ERROR  9991: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' SOLLIB = ',A,' NOT PROGRAMMED ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE SOLVE6_SETUP
