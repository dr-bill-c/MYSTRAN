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
 
      SUBROUTINE BUSH ( OPT, WRITE_WARN )
 
! Calculates, for BUSH element

!  1) KE        = element stiffness matrix if OPT(4) = 'Y' 
!  2) SE1, STE1 = element stress data recovery matrices if OPT(3) = 'Y'
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, TWO, FOUR
      USE MODEL_STUF, ONLY            :  BE1, BE2, EOFF, EPROP, KE, ELEM_LEN_AB, OFFDIS, SE1, SE2
      USE SUBR_BEGEND_LEVELS, ONLY    :  BUSH_BEGEND

      USE BUSH_USE_IFs

      IMPLICIT NONE 
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BUSH'
      CHARACTER(LEN=*) , INTENT(IN)   :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BUSH_BEGEND
      INTEGER(LONG)                   :: I,J               ! DO loop indices

      REAL(DOUBLE)                    :: DELK2, DELK3      ! Intermediate terms in KE calculation
      REAL(DOUBLE)                    :: DUM0(6,12)        ! Intermediate matrix in a calc
      REAL(DOUBLE)                    :: DUM1(6,12)        ! Intermediate matrix in a calc
      REAL(DOUBLE)                    :: STIFF(12,12)      ! 12x12 partition from KE (complete stiff matrix for this elem)
      REAL(DOUBLE)                    :: KB(6)             ! The 6 stiffness values input on the PBUSH entry          
      REAL(DOUBLE)                    :: L                 ! ELEM_LEN_AB
      REAL(DOUBLE)                    :: STRE_RCV(2)       ! Two stress recovery values
      REAL(DOUBLE)                    :: STRN_RCV(2)       ! Two strain recovery values

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Calc stiffness matrix

      IF ((OPT(3) == 'Y') .OR. (OPT(4) == 'Y')) THEN

         L = ELEM_LEN_AB

         DO I=1,6
            KB(I) = EPROP(I)
         ENDDO

         DELK2     =  KB(2)*(L*L/FOUR)
         DELK3     =  KB(3)*(L*L/FOUR)

         KE( 1, 1) =  KB(1)
         KE( 2, 2) =  KB(2)
         KE( 3, 3) =  KB(3)
         KE( 4, 4) =  KB(4)
         KE( 5, 5) =  KB(5) + DELK3
         KE( 6, 6) =  KB(6) + DELK2

         KE( 7, 7) =  KB(1)
         KE( 8, 8) =  KB(2)
         KE( 9, 9) =  KB(3)
         KE(10,10) =  KB(4)
         KE(11,11) =  KE(5,5)
         KE(12,12) =  KE(6,6)

         KE( 1, 7) = -KB(1)
         KE( 2, 8) = -KB(2)
         KE( 3, 9) = -KB(3)
         KE( 4,10) = -KB(4)
         KE( 5,11) = -KB(5) + DELK3
         KE( 6,12) = -KB(6) + DELK2

         KE( 2, 6) =  KB(2)*L/TWO
         KE( 2,12) =  KE(2,6)
         KE( 3, 5) = -KB(3)*L/TWO
         KE( 3,11) =  KE(3,5)
         KE( 5, 9) = -KE(3,5)
         KE( 6, 8) = -KE(2,6)
         KE( 8,12) = -KE(2,6)
         KE( 9,11) = -KE(3,5)

         DO I=1,12
            DO J=I,12
               KE(J,I) = KE(I,J)
            ENDDO
         ENDDO

         DO I=1,12
            DO J=1,12
               STIFF(I,J) = KE(I,J)
            ENDDO
         ENDDO

      ENDIF

! Calc stress and strain recovery matrices. The stress recovery coefficients on the PBUSH entry (the vals in EPROP(14), EPROP(15))
! are intended to be multiplied times the element nodal forces to get stresses. In order to calc stresses in the MYSTRAN fasion of
! multiplying  SEi times displacements, the SEi matrices are generated from dUM0, below. DUM0 is the matrix that would be multiplied
! times nodal forces to get stresses. To get SEi, multiply DUM0 by the element stiffness matrix (since KE*UEL are nodal forces)

      IF (OPT(3) == 'Y') THEN
                                                           ! Calc SEi stress recovery matrices
         STRE_RCV(1) = EPROP(19)                           ! --- Stress/strain recovery coefficients from PBUSH entry
         STRE_RCV(2) = EPROP(20)
         STRN_RCV(1) = EPROP(21)
         STRN_RCV(2) = EPROP(22)

         DO I=1,6
            DO J=1,12
               DUM0(I,J) = ZERO
            ENDDO
         ENDDO

         DUM0(1, 1) = -HALF*STRE_RCV(1)                    ! --- Dummy 6x12 matrix with stress recovery coefficients
         DUM0(1, 7) =  HALF*STRE_RCV(1)

         DUM0(2, 2) = -HALF*STRE_RCV(1)
         DUM0(2, 8) =  HALF*STRE_RCV(1)

         DUM0(3, 3) = -HALF*STRE_RCV(1)
         DUM0(3, 9) =  HALF*STRE_RCV(1)

         DUM0(4, 4) = -HALF*STRE_RCV(2)
         DUM0(4,10) =  HALF*STRE_RCV(2)

         DUM0(5, 5) = -HALF*STRE_RCV(2)
         DUM0(5,11) =  HALF*STRE_RCV(2)

         DUM0(6, 6) = -HALF*STRE_RCV(2)
         DUM0(6,12) =  HALF*STRE_RCV(2)

         CALL MATMULT_FFF ( DUM0, STIFF, 6, 12, 12, DUM1 ) ! --- Dummy 6x12 matrix that has the SEi matrix data

         DO I=1,3                                          ! --- SEi
            DO J=1,12
               SE1(I,J,1) = DUM1(I  ,J)
               SE2(I,J,1) = DUM1(I+3,J)
            ENDDO
         ENDDO

         BE1(1, 1,1) = -STRN_RCV(1)                        ! Calc BEi strain recovery matrices
         BE1(1, 7,1) =  STRN_RCV(1)

         BE1(2, 2,1) = -STRN_RCV(1)
         BE1(2, 8,1) =  STRN_RCV(1)

         BE1(3, 3,1) = -STRN_RCV(1)
         BE1(3, 9,1) =  STRN_RCV(1)

         BE2(1, 4,1) = -STRN_RCV(2)
         BE2(1,10,1) =  STRN_RCV(2)

         BE2(2, 5,1) = -STRN_RCV(2)
         BE2(2,11,1) =  STRN_RCV(2)

         BE2(3, 6,1) = -STRN_RCV(2)
         BE2(3,12,1) =  STRN_RCV(2)

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE BUSH