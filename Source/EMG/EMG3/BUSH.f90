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
 
      SUBROUTINE BUSH ( INT_ELEM_ID, OPT, WRITE_WARN )
 
! Calculates, for BUSH element

!  1) KE        = element stiffness matrix if OPT(4) = 'Y' 
!  2) SE1, STE1 = element stress data recovery matrices if OPT(3) = 'Y'
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NCORD
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, QUARTER
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  BE1, BE2, BUSH_OCID, BUSHOFF, BUSH_DXA, BUSH_DXB, BUSH_DY, BUSH_DZ, CORD, EOFF, EPROP, KE,&
                                         ELEM_LEN_12, OFFDIS_B, RCORD, SE1, SE2, TE, XEB
      USE SUBR_BEGEND_LEVELS, ONLY    :  BUSH_BEGEND

      USE BUSH_USE_IFs

      IMPLICIT NONE 
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BUSH'
      CHARACTER(LEN=*) , INTENT(IN)   :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BUSH_BEGEND
      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: JCORD             ! Row in coord sys array TN where transformation data is for BUSH_OCID

      REAL(DOUBLE)                    :: BUSH_S_0(3)       ! 
      REAL(DOUBLE)                    :: BUSH_S_L(3)       ! 
      REAL(DOUBLE)                    :: DUM0(6,12)        ! Intermediate matrix in a calc
      REAL(DOUBLE)                    :: DUM1(6,12)        ! Intermediate matrix in a calc
      REAL(DOUBLE)                    :: DXA               ! Offset dist of BUSH elem from end A in elem Xe dir.
      REAL(DOUBLE)                    :: DXB               ! Offset dist of BUSH elem from end B in elem Xe dir.=L - DXA
      REAL(DOUBLE)                    :: DY                ! Offset distance of BUSH elem from  in elem Ye direction
      REAL(DOUBLE)                    :: DZ                ! Offset distance of BUSH elem from  in elem Ze direction
      REAL(DOUBLE)                    :: KB(6)             ! The 6 stiffness values input on the PBUSH entry          
      REAL(DOUBLE)                    :: L                 ! ELEM_LEN_12
      REAL(DOUBLE)                    :: STIFF(12,12)      ! 12x12 partition from KE (complete stiff matrix for this elem)
      REAL(DOUBLE)                    :: STRE_RCV(2)       ! Two stress recovery values
      REAL(DOUBLE)                    :: STRN_RCV(2)       ! Two strain recovery values

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize:

      DO I=1,3
         BUSH_S_L(I) = ZERO
         BUSH_S_0(I) = ZERO
      ENDDO

      IF (DEBUG(204) > 0) THEN
         CALL DEBUG_BUSH ( 98 )
      ENDIF

! Transform offset values in BUSH_OCID coord system to basic and then to local BUSH elem coord system

off:  IF (EOFF(INT_ELEM_ID) == 'Y') THEN

         DO I=1,3
            BUSH_S_L(I) = ZERO
         ENDDO

         DO J=1,NCORD
            IF (CORD(J,2) == BUSH_OCID) THEN
               JCORD = J
               EXIT
            ENDIF
         ENDDO

         IF (BUSH_OCID > 0) THEN                           ! The offset is specified by S1,S2,S3

            DO I=1,3                                       ! Basic coords of BUSH at offset point (rel to Grid A)
               BUSH_S_0(I) = OFFDIS_B(1,I) + RCORD(JCORD,I) - XEB(1,I)
            ENDDO
                                                           ! BUSH_S_L = local elem coords of BUSH at offset point (rel to Grid A)
            CALL MATMULT_FFF (TE, BUSH_S_0, 3, 3, 1, BUSH_S_L)
         ELSE

            BUSH_S_L(1) = BUSHOFF(1,1)
            BUSH_S_L(2) = ZERO
            BUSH_S_L(3) = ZERO

         ENDIF

         IF ((DEBUG(204) == 1) .OR. (DEBUG(204) == 9)) THEN
            CALL DEBUG_BUSH ( 1 )
         ENDIF

      ENDIF off

      L = ELEM_LEN_12                                      ! Want ELEM_LEN_12 here (not ELEM_LEN_AB which is 0. for BUSH)

      IF (BUSH_OCID == -1) THEN                            ! CBUSH: use S, for offset, instead of S1,S2,S3. S is ratio of offset/L
         DXA = L*BUSH_S_L(1)
         DY  = L*BUSH_S_L(2)
         DZ  = L*BUSH_S_L(3)
      ELSE                                                 ! CBUSH: use S1,S2,S3 instead of S. Si offsets are actual distances
         DXA = BUSH_S_L(1)
         DY  = BUSH_S_L(2)
         DZ  = BUSH_S_L(3)
      ENDIF
      DXB = L - DXA

! Calc stiffness matrix

      IF ((OPT(3) == 'Y') .OR. (OPT(4) == 'Y')) THEN

         BUSH_DXA = DXA
         BUSH_DXB = DXB
         BUSH_DY  = DY
         BUSH_DZ  = DZ
         
         DO I=1,6
            KB(I) = EPROP(I)
         ENDDO

         KE( 1, 1) =  KB(1)
         KE( 1, 5) =  DZ*KB(1)
         KE( 1, 6) = -DY*KB(1)
         KE( 1, 7) = -KB(1)
         KE( 1,11) = -DZ*KB(1)
         KE( 1,12) =  DY*KB(1)

         KE( 2, 2) =  KB(2)
         KE( 2, 4) = -DZ*KB(2)
         KE( 2, 6) =  DXA*KB(2)
         KE( 2, 8) = -KB(2)
         KE( 2,10) =  DZ*KB(2)
         KE( 2,12) =  DXB*KB(2)

         KE( 3, 3) =  KB(3)
         KE( 3, 4) =  DY*KB(3)
         KE( 3, 5) = -DXA*KB(3)
         KE( 3, 9) = -KB(3)
         KE( 3,10) = -DY*KB(3)
         KE( 3,11) = -DXB*KB(3)

         KE( 4, 4) =  KB(4) + DY*DY*KB(3)   + DZ*DZ*KB(2)
         KE( 4, 5) = -DXA*DY*KB(3)
         KE( 4, 6) = -DXA*DZ*KB(2)
         KE( 4, 8) =  DZ*KB(2)
         KE( 4, 9) = -DY*KB(3)
         KE( 4,10) = -KB(4) - DY*DY*KB(3)   - DZ*DZ*KB(2)
         KE( 4,11) = -DXB*DY*KB(3)
         KE( 4,12) = -DXB*DZ*KB(2)

         KE( 5, 5) =  KB(5) + DXA*DXA*KB(3) + DZ*DZ*KB(1)
         KE( 5, 6) = -DY*DZ*KB(1)
         KE( 5, 7) = -DZ*KB(1)
         KE( 5, 9) =  DXA*KB(3)
         KE( 5,10) =  DXA*DY*KB(3)
         KE( 5,11) = -KB(5) + DXA*DXB*KB(3) - DZ*DZ*KB(1)
         KE( 5,12) =  DY*DZ*KB(1)

         KE( 6, 6) =  KB(6) + DXA*DXA*KB(2) + DY*DY*KB(1)
         KE( 6, 7) =  DY*KB(1)
         KE( 6, 8) = -DXA*KB(2)
         KE( 6,10) =  DXA*DZ*KB(2)
         KE( 6,11) =  DY*DZ*KB(1)
         KE( 6,12) = -KB(6) + DXA*DXB*KB(2) - DY*DY*KB(1)

         KE( 7, 7) =  KB(1)
         KE( 7,11) =  DZ*KB(1)
         KE( 7,12) = -DY*KB(1)

         KE( 8, 8) =  KB(2)
         KE( 8,10) = -DZ*KB(2)
         KE( 8,12) = -DXB*KB(2)

         KE( 9, 9) =  KB(3)
         KE( 9,10) =  DY*KB(3)
         KE( 9,11) =  DXB*KB(3)

         KE(10,10) =  KB(4) + DY*DY*KB(3)   + DZ*DZ*KB(2)
         KE(10,11) =  DXB*DY*KB(3)
         KE(10,12) =  DXB*DZ*KB(2)

         KE(11,11) =  KB(5) + DXB*DXB*KB(3) + DZ*DZ*KB(1)
         KE(11,12) = -DY*DZ*KB(1)

         KE(12,12) =  KB(6) + DXB*DXB*KB(2) + DY*DY*KB(1)

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

         IF ((DEBUG(204) == 2) .OR. (DEBUG(204) == 9)) THEN
            CALL DEBUG_BUSH ( 2 )
         ENDIF

      ENDIF

! Calc stress and strain recovery matrices. The stress recovery coefficients on the PBUSH Bulk Data entry are intended to be
! multiplied times the element ENGINEERING (NOT NODE) forces to get stresses. In order to calc stresses in the MYSTRAN fashion
! of multiplying SEi times displacements, the SEi matrices are generated with the aid of the matrix that converts element
! nodal forces to element engineering forces. The DUM0 matrix (below), if all of the STRE_RCV numbers were 1.0 is that matrix
! (see conversion code in subr OFP3_ELFE_1D. With the STRE_RCV values in DUM0, then DUM0 times KE would be the SE matrices
! that convert displacements to stresses

      IF (OPT(3) == 'Y') THEN
                                                           ! Calc SEi stress recovery matrices
         STRE_RCV(1) = EPROP(19)                           ! --- Stress/strain recovery coefficients from PBUSH entry
         STRE_RCV(2) = EPROP(20)
         STRN_RCV(1) = EPROP(21)
         STRN_RCV(2) = EPROP(22)

         DO I=1,6
            DO J=1,12
               DUM0(I,J) = ZERO
               DUM1(I,J) = ZERO
            ENDDO
         ENDDO

         DUM0(1, 7) =  STRE_RCV(1)
         DUM0(2, 8) =  STRE_RCV(1)
         DUM0(3, 9) =  STRE_RCV(1)

         DUM0(4,10) =  STRE_RCV(2)
         DUM0(5,11) =  STRE_RCV(2)
         DUM0(6,12) =  STRE_RCV(2)
         
         DUM0(4, 8) =  STRE_RCV(2)*DZ
         DUM0(4, 9) = -STRE_RCV(2)*DY
         DUM0(5, 7) = -STRE_RCV(2)*DZ
         DUM0(5, 9) = -STRE_RCV(2)*DXB
         DUM0(6, 7) =  STRE_RCV(2)*DY
         DUM0(6, 8) =  STRE_RCV(2)*DXB

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

      IF (DEBUG(204) > 0) THEN
         CALL DEBUG_BUSH ( 99 )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEBUG_BUSH ( WHAT )
 
      USE MODEL_STUF, ONLY            :  EID

      IMPLICIT NONE
 
      CHARACTER(14*BYTE)              :: KE_CHAR(12)       ! Character representation of the real data in one row of REAL_VAR

      INTEGER(LONG), INTENT(IN)       :: WHAT              ! Indicator of what to debug

! **********************************************************************************************************************************

      IF (WHAT == 98) THEN

         WRITE(F06,*)
         WRITE(F06,98720)
         WRITE(F06,9101) EID

      ELSE IF ((WHAT == 1) .OR. (WHAT == 9)) THEN
       
         WRITE(F06,*)
         WRITE(F06,9102)
         WRITE(f06,9108) ' BUSH_OCID, JCORD =', BUSH_OCID, JCORD
         WRITE(f06,*)

         IF (BUSH_OCID > 0) THEN

            WRITE(f06,9109) ' RCORD .....................', (RCORD(JCORD,J),J=1,6)
            WRITE(f06,9110)  (RCORD(JCORD,J),J= 7, 9)
            WRITE(f06,9110)  (RCORD(JCORD,J),J=10,12)
            WRITE(f06,*)
            WRITE(f06,9109) ' TE (transforms basic sys to local element sys).......................', (TE(1,J),J=1,3)
            WRITE(f06,9110)  (TE(2,J),J=1,3)                                                                      
            WRITE(f06,9110)  (TE(3,J),J=1,3)                                                                      
            WRITE(f06,*)                                                                                        
            WRITE(f06,9111) ' Coords of offset in OCID coord sys ..................................', (BUSHOFF(1,I),I=1,3)
            WRITE(f06,*)
            WRITE(f06,9111) ' OFFDIS_B: offsets in basic coord sys rel to OCID origin ..........(1)', (OFFDIS_B(1,I),I=1,3)
            WRITE(f06,9111) ' Coords of OCID origin in basic coord sys .........................(2)', (RCORD(JCORD,I),I=1,3)
            WRITE(f06,9111) ' Coords of grid A in basic coord sys ..............................(3)', (XEB(1,I),I=1,3)
            WRITE(f06,*)
            WRITE(f06,9111) ' Coords of offset in basic coord sys is     : BUSH_S_0 = (1)+(2)-(3)..', (BUSH_S_0(I),I=1,3)
            WRITE(f06,*)
            WRITE(f06,9111) ' Coords of offset in local elem coord sys is: BUSH_S_L = TExBUSH_S_0..', (BUSH_S_L(I),I=1,3)
            WRITE(f06,*)

         ELSE

            WRITE(f06,9111) ' Coords of offset in local elem coord sys ............................', (BUSH_S_L(I),I=1,3)
            WRITE(f06,*)

         ENDIF

      ELSE IF ((WHAT == 2) .OR. (WHAT == 9)) THEN
 
         WRITE(F06,9112) L, DXA, DXB, DY, DZ

         WRITE(F06,*)
         WRITE(F06,9202)
         WRITE(F06,9203)
         DO I=1,6 
            WRITE(F06,9204) I, EPROP(I)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,9205)

         DO I=1,12
            CALL WRT_REAL_TO_CHAR_VAR ( KE, 12, 12, I, KE_CHAR )
            WRITE(F06,9206) (KE_CHAR(J),J=1,6), (KE_CHAR(J),J=7,12)
            IF (I==6) THEN
               WRITE(F06,9207)
            ENDIF
         ENDDO

         WRITE(F06,*)

      ELSE IF (WHAT == 99) THEN

         WRITE(F06,98799)
         WRITE(F06,*)

      ENDIF
 
! **********************************************************************************************************************************
 9101 FORMAT(54X, 'For bush element ',I8,/,54X, '-------------------------')

 9102 FORMAT(4X,'Output from calculation of offsets in local element coordinate system',/,                                         &
             4X,'---------------------------------------------------------------------')

 9108 FORMAT(3X,A,2I10)

 9109 FORMAT(3X,A,12(1ES14.6))

 9110 FORMAT(3X,70X,3(1ES14.6))

 9111 FORMAT(3X,A, 3(1ES14.6))

 9112 FORMAT(3X,' L, DXA, DXB, DY, DZ .....................', 5(1ES14.6))

 9202 FORMAT(4X,'Output from calculation of local element stiffness matrix',/,                                                     &
             4X,'---------------------------------------------------------')

 9203 FORMAT(4X,'Element properties')

 9204 FORMAT(4X,'I, EPROP(I) = ', I3,1ES14.6)

 9205 FORMAT(4X,'KE element stiffness matrix in local element coordinate system')

 9206 FORMAT(2X,6A14,1X,'|',6A14)

 9207 FORMAT('   ----------------------------------------------------------------------------------------------------------------',&
             '-----------------------------------------------------------')

98720 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' :::::::::::::::::::::::::::::::::::::::::::START DEBUG(204) OUTPUT FROM SUBROUTINE BUSH:::::::::::::::::::::::::::',&
             ':::::::::::::::::')

98799 FORMAT(' ::::::::::::::::::::::::::::::::::::::::::::END DEBUG(204) OUTPUT FROM SUBROUTINE BUSH::::::::::::::::::::::::::::',&
             ':::::::::::::::::'                                                                                                ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE DEBUG_BUSH

      END SUBROUTINE BUSH