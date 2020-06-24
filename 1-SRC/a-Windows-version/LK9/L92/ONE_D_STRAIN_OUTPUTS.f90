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
 
      SUBROUTINE ONE_D_STRAIN_OUTPUTS ( SIZE_ALLOCATED, NUM1, NUM_FEMAP_ROWS, WRITE_OGEL, WRITE_FEMAP )
 
! Calculates element strains for BUSHS element
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR 
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  STRAIN, TYPE
      USE LINK9_STUFF, ONLY           :  MSPRNT, OGEL
      USE FEMAP_ARRAYS, ONLY          :  FEMAP_EL_VECS
      USE PARAMS, ONLY                :  POST
      USE SUBR_BEGEND_LEVELS, ONLY    :  ONE_D_STRAIN_OUTPUTS_BEGEND

      USE ONE_D_STRAIN_OUTPUTS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ONE_D_STRAIN_OUTPUTS'
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_OGEL         ! If 'Y' then write data to array OGEL
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_FEMAP        ! If 'Y' then write data to array FEMAP_EL_VECS
 
      INTEGER(LONG), INTENT(IN)       :: SIZE_ALLOCATED     ! No. of rows allocated to array that will be written to
      INTEGER(LONG), INTENT(IN)       :: NUM_FEMAP_ROWS     ! Number of rows that will be written to FEMAP arrays
      INTEGER(LONG), INTENT(INOUT)    :: NUM1               ! Cum rows written to OGEL prior to running this subr
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ONE_D_STRAIN_OUTPUTS_BEGEND
 
      INTRINSIC DMAX1,DMIN1
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Calc engineering strains from array STRAIN and put into array OGEL
 
      IF (TYPE(1:4) == 'BUSH') THEN                        ! Strains for BUSH elements

         IF (WRITE_OGEL == 'Y') THEN
            NUM1 = NUM1 + 1    
            IF (NUM1 > SIZE_ALLOCATED) THEN
               WRITE(ERR,9200) SUBR_NAME,SIZE_ALLOCATED
               WRITE(F06,9200) SUBR_NAME,SIZE_ALLOCATED
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF   
            OGEL(NUM1,1) = STRAIN(1)
            OGEL(NUM1,2) = STRAIN(2)
            OGEL(NUM1,3) = STRAIN(3)
            OGEL(NUM1,4) = STRAIN(4)
            OGEL(NUM1,5) = STRAIN(5)
            OGEL(NUM1,6) = STRAIN(6)
         ENDIF
         IF ((POST /= 0) .AND. (WRITE_FEMAP == 'Y')) THEN
            FEMAP_EL_VECS(NUM_FEMAP_ROWS,1) = STRAIN(1)
            FEMAP_EL_VECS(NUM_FEMAP_ROWS,2) = STRAIN(2)
            FEMAP_EL_VECS(NUM_FEMAP_ROWS,3) = STRAIN(3)
            FEMAP_EL_VECS(NUM_FEMAP_ROWS,4) = STRAIN(4)
            FEMAP_EL_VECS(NUM_FEMAP_ROWS,5) = STRAIN(5)
            FEMAP_EL_VECS(NUM_FEMAP_ROWS,6) = STRAIN(6)
         ENDIF
 
      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,9203) SUBR_NAME, TYPE
         WRITE(F06,9203) SUBR_NAME, TYPE
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
 9200 FORMAT(' *ERROR  9200: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ARRAY OGEL WAS ALLOCATED TO HAVE ',I12,' ROWS. ATTEMPT TO WRITE TO OGEL BEYOND THIS')
 
! **********************************************************************************************************************************
 9203 FORMAT(' *ERROR  9203: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INCORRECT ELEMENT TYPE = "',A,'"')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE ONE_D_STRAIN_OUTPUTS
