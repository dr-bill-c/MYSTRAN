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
 
      SUBROUTINE SPARSE_CRS_TO_FULL ( MATIN_NAME, NTERM_IN, NROWS, NCOLS, SYM_IN, I_MATIN, J_MATIN, MATIN, MATOUT )

! Converts matrices in sparse compressed row storage format to full format 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_CRS_TO_FULL_BEGEND
 
      USE SPARSE_CRS_TO_FULL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_CRS_TO_FULL'
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_IN              ! 'Y' or 'N' symmetry indicator for input matrix.
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME          ! Name of matrix
 
      INTEGER(LONG), INTENT(IN)       :: NCOLS               ! Number of cols in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: NROWS               ! Number of rows in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERM_IN            ! Number of nonzero terms in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)    ! I_MATIN(I+1) - I_MATIN(I) are the number of nonzeros in MATIN row I
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERM_IN)   ! Col numbers for nonzero terms in MATIN
      INTEGER(LONG)                   :: I,J,K               ! DO loop indices or counters
      INTEGER(LONG)                   :: ROW_I_NTERMS        ! No. terms in row I of input matrix MATIN
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_CRS_TO_FULL_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERM_IN)     ! Real nonzero values in input  matrix MATIN
      REAL(DOUBLE) , INTENT(OUT)      :: MATOUT(NROWS,NCOLS) ! Real nonzero values in output matrix MATOUT

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NROWS
         DO J=1,NCOLS
            MATOUT(I,J) = ZERO
         ENDDO
      ENDDO

! Calc outputs

      DO I=1,NROWS
         DO J=1,NCOLS
            MATOUT(I,J) = ZERO
         ENDDO 
      ENDDO 

! Create full matrix MATOUT from sparse input matrix MATIN

      K = 0                                                
      DO I=1,NROWS
         ROW_I_NTERMS = I_MATIN(I+1) - I_MATIN(I)
         DO J=1,ROW_I_NTERMS
            K = K + 1
            IF (K > NTERM_IN) CALL ARRAY_SIZE_ERROR_1( SUBR_NAME, NTERM_IN, MATIN_NAME )
            MATOUT(I,J_MATIN(K)) = MATIN(K)
         ENDDO 
      ENDDO

! If input matrix was tagged as symmetric, then lower triang portion was not in MATIN, so set lower triang portion:

      IF (SYM_IN == 'Y') THEN
         DO I=1,NROWS
            DO J=1,I-1
               MATOUT(I,J) = MATOUT(J,I)
            ENDDO 
         ENDDO 
      ENDIF


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE SPARSE_CRS_TO_FULL
