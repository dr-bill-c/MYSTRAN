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
 
      SUBROUTINE ELEM_STRE_STRN_ARRAYS ( STR_PT_NUM )
 
! Calculates element stress and strain arrays (arrays STRESS and STRAIN). Stresses are calculated for all engineering elements
! (1-D, 2-D, 3-D elements). Strains are calculated for the BUSH element, all 2-D and 3-D elements. The default method for
! calculating stresses is to first calculate strains by multiplying the element strain-displ matrices (BEi) times the elem displ's
! (in local elem coords) and then calc stresses from those strains using material props.

! For the TRIA3 and QUAD4 there is a DEBUG option to calculate stresses directly by multiplying the stress-displ matrices (SEi)
! times the elem displ's

! The arrays STRESS and STRAIN are calculated at the mid plane of 2-D elements and have to be processed later to get element
! specific outputs (e.g. stresses at the top and bottom of the 2-D element). The same is true for some of the 1-D elements (e.g.
! the BAR element stresses at the 4 points on the cross-section have to be processed from the STRESS array generated here)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, INT_SC_NUM, JTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELEM_STRE_STRN_ARRAYS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, one, four
      USE MODEL_STUF, ONLY            :  ALPVEC, BE1, BE2, BE3, DT, EM, EB, ES, ET, ELDOF, PHI_SQ, STRAIN, STRESS, SUBLOD,         &
                                         TREF, TYPE, UEL, SE1, SE2, SE3, STE1, STE2, STE3
      USE DEBUG_PARAMETERS
      USE PARAMS, ONLY                :  STR_CID


      USE ELEM_STRE_STRN_ARRAYS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELEM_STRE_STRN_ARRAYS'

      INTEGER(LONG), INTENT(IN)       :: STR_PT_NUM        ! Which point (3rd index in SEi matrices) this call is for
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: K                 ! Counter
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELEM_STRE_STRN_ARRAYS_BEGEND
 
      REAL(DOUBLE)                    :: ALPT(6)           ! Col of ALPVEC times temperatures
      REAL(DOUBLE)                    :: ALPTM(3)          ! Col of ALPVEC times temperatures
      REAL(DOUBLE)                    :: ALPTB(3)          ! Col of ALPVEC times temperatures
      REAL(DOUBLE)                    :: ALPTT(3)          ! Col oF ALPVEC times temperatures
      REAL(DOUBLE)                    :: DUM31(3)          ! Array used in an intermediate calc
      REAL(DOUBLE)                    :: DUM32(3)          ! Array used in an intermediate calc
      REAL(DOUBLE)                    :: DUM33(3)          ! Array used in an intermediate calc
      REAL(DOUBLE)                    :: ET3(3,3)          ! Material matrix ET expanded TO 3x3
      REAL(DOUBLE)                    :: STRAIN1(3)        ! 1st 3 rows of array STRAIN
      REAL(DOUBLE)                    :: STRAIN2(3)        ! 2nd 3 rows of array STRAIN
      REAL(DOUBLE)                    :: STRAIN3(3)        ! 3rd 3 rows of array STRAIN
      REAL(DOUBLE)                    :: STRESS1(3)        ! 1st 3 rows of array STRESS
      REAL(DOUBLE)                    :: STRESS2(3)        ! 2nd 3 rows of array STRESS
      REAL(DOUBLE)                    :: STRESS3(3)        ! 3rd 3 rows of array STRESS
      REAL(DOUBLE)                    :: STRESS_THERM(6)   ! Part of array STRESS
      REAL(DOUBLE)                    :: STRESS1_THERM(3)  ! Part of array STRESS1
      REAL(DOUBLE)                    :: STRESS2_THERM(3)  ! Part of array STRESS2
      REAL(DOUBLE)                    :: STRESS3_THERM(3)  ! Part of array STRESS3
      REAL(DOUBLE)                    :: STRESS_MECH(6)    ! Part of array STRESS
      REAL(DOUBLE)                    :: STRESS1_MECH(3)   ! Part of array STRESS1
      REAL(DOUBLE)                    :: STRESS2_MECH(3)   ! Part of array STRESS2
      REAL(DOUBLE)                    :: STRESS3_MECH(3)   ! Part of array STRESS3
      REAL(DOUBLE)                    :: TBAR              ! Average elem temperature 
      REAL(DOUBLE)                    :: STR_TENSOR(3,3)   ! 2D stress or strain tensor

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      DO I=1,9
         STRAIN(I) = ZERO
         STRESS(I) = ZERO
      ENDDO

! **********************************************************************************************************************************
! Calc stresses for 1D elements

      IF ((TYPE(1:3) == 'BAR') .OR. (TYPE(1:4) == 'BUSH') .OR. (TYPE(1:4) == 'ELAS') .OR. (TYPE(1:3) == 'ROD') .OR.                &
          (TYPE(1:5) == 'USER1')) THEN

         DO I=1,3
            STRESS(I) = ZERO
            DO J=1,ELDOF
               STRESS(I) = STRESS(I) + SE1(I,J,STR_PT_NUM)*UEL(J)
               if (dabs(uel(j)) > 1.e-15) then
               endif
            ENDDO
            IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
               STRESS(I) = STRESS(I) - STE1(I,JTSUB,STR_PT_NUM)
            ENDIF
         ENDDO
 
         IF ((TYPE(1:3) == 'BAR') .OR. (TYPE(1:4) == 'BUSH')) THEN
            K = 0
            DO I=4,6
               STRESS(I) = ZERO
               K = K + 1
               IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
                  STRESS(I) = -STE2(K,JTSUB,STR_PT_NUM)
               ENDIF
               DO J=1,ELDOF
                  STRESS(I) = STRESS(I) + SE2(K,J,STR_PT_NUM)*UEL(J)
               ENDDO 
            ENDDO   
         ENDIF


      ENDIF

! **********************************************************************************************************************************
! Calc strains for 1D elements

      IF (TYPE(1:4) == 'BUSH') THEN

         DO I=1,3
            STRAIN(I) = ZERO
            DO J=1,ELDOF
               STRAIN(I) = STRAIN(I) + BE1(I,J,STR_PT_NUM)*UEL(J)
            ENDDO
         ENDDO
 
         K = 0
         DO I=4,6
            STRAIN(I) = ZERO
            K = K + 1
            DO J=1,ELDOF
               STRAIN(I) = STRAIN(I) + BE2(K,J,STR_PT_NUM)*UEL(J)
            ENDDO 
         ENDDO   
 

! **********************************************************************************************************************************
! Calc strains, then stresses for 2D elements

      ELSE IF ((TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'SHEAR') .OR. (TYPE(1:5) == 'USER1')) THEN

         DO I=1,3
            STRAIN(I) = ZERO
            STRAIN(I+3) = ZERO
            DO J=1,ELDOF
               STRAIN(I)   = STRAIN(I)   + BE1(I,J,STR_PT_NUM)*UEL(J)
               STRAIN(I+3) = STRAIN(I+3) + BE2(I,J,STR_PT_NUM)*UEL(J)
            ENDDO
         ENDDO   

         DO I=1,2
            STRAIN(I+6) = ZERO
            DO J=1,ELDOF
               STRAIN(I+6) = STRAIN(I+6) + BE3(I,J,STR_PT_NUM)*UEL(J)
            ENDDO
         ENDDO   

         DO I=1,3                                          ! Calc stresses from strains

            STRESS1(I)       = ZERO
            STRESS2(I)       = ZERO
            STRESS3(I)       = ZERO

            STRESS1_MECH(I)  = ZERO
            STRESS2_MECH(I)  = ZERO
            STRESS3_MECH(I)  = ZERO

            STRESS1_THERM(I) = ZERO
            STRESS2_THERM(I) = ZERO
            STRESS3_THERM(I) = ZERO

         ENDDO

         DO I=1,3
            STRAIN1(I) = STRAIN(I)
            STRAIN2(I) = STRAIN(I+3)
            STRAIN3(I) = STRAIN(I+6)
         ENDDO

         DO I=1,3
            IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
               TBAR = (DT(1,JTSUB) + DT(2,JTSUB) + DT(3,JTSUB) + DT(4,JTSUB))/FOUR
               ALPTM(I) = ALPVEC(I  ,1)*(TBAR - TREF(1))
               ALPTB(I) = ALPVEC(I  ,2)*DT(5,JTSUB)
               ALPTT(I) = ALPVEC(I+3,3)*(TBAR - TREF(1))
            ELSE
               ALPTM(I) = ZERO
               ALPTB(I) = ZERO
               ALPTT(I) = ZERO
            ENDIF
         ENDDO
      

         DO I=1,3
            DO J=1,3
               ET3(I,J) = ZERO
            ENDDO
         ENDDO

         DO I=1,2
            DO J=1,2
               ET3(I,J) = ET(I,J)
            ENDDO
         ENDDO

         CALL MATMULT_FFF ( EM , STRAIN1, 3, 3, 1, DUM31 )
         CALL MATMULT_FFF ( EB , STRAIN2, 3, 3, 1, DUM32 )
         CALL MATMULT_FFF ( ET3, STRAIN3, 3, 3, 1, DUM33 )
         DO I=1,3
            STRESS1_MECH(I) =        DUM31(I)
            STRESS2_MECH(I) =        DUM32(I)
                                                           ! Need PHI_SQ on transv shear stress since this calc is from strains and
                                                           ! BE3, not SE3. If DEBUG(176) > 0 then stresses are calc'd from the SE3
            STRESS3_MECH(I) = PHI_SQ*DUM33(I)              ! below and SE3 has PHI_SQ incorporated in subrs QPLT1, QPLT3, TPLT2.
         ENDDO
  
         IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
            CALL MATMULT_FFF ( EM , ALPTM  , 3, 3, 1, STRESS1_THERM )
            CALL MATMULT_FFF ( EB , ALPTB  , 3, 3, 1, STRESS2_THERM )
            CALL MATMULT_FFF ( ET3, ALPTT  , 3, 3, 1, STRESS3_THERM )
         ENDIF

         CALL MATADD_FFF  ( STRESS1_MECH, STRESS1_THERM, 3, 1, ONE, -ONE, 0, STRESS1 )
         CALL MATADD_FFF  ( STRESS2_MECH, STRESS2_THERM, 3, 1, ONE, -ONE, 0, STRESS2 )
         CALL MATADD_FFF  ( STRESS3_MECH, STRESS3_THERM, 3, 1, ONE, -ONE, 0, STRESS3 )
 
         DO I=1,3
            STRESS(I)   = STRESS1(I)
            STRESS(I+3) = STRESS2(I)
            STRESS(I+6) = STRESS3(I)
         ENDDO

         IF (DEBUG(176) > 0) THEN                          ! If DEBUG(176) > 0, calc stresses using SEi instead of above from STRAIN
            DO I=1,3                                       ! NOTE: PHI_SQ is incorporated into SE3
               STRESS(I  ) = ZERO
               STRESS(I+3) = ZERO
               DO J=1,ELDOF
                  STRESS(I)   = STRESS(I)   + SE1(I,J,STR_PT_NUM)*UEL(J)
                  STRESS(I+3) = STRESS(I+3) + SE2(I,J,STR_PT_NUM)*UEL(J)
               ENDDO
               IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
                  STRESS(I)   = STRESS(I)   - STE1(I,JTSUB,STR_PT_NUM)
                  STRESS(I+3) = STRESS(I+3) - STE2(I,JTSUB,STR_PT_NUM)
               ENDIF
            ENDDO
            DO I=1,2
               STRESS(I+6) = ZERO
               DO J=1,ELDOF
                  STRESS(I+6) = STRESS(I+6) + SE3(I,J,STR_PT_NUM)*UEL(J)
               ENDDO
               IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
                  STRESS(I+6) = STRESS(I+6) - STE3(I,JTSUB,STR_PT_NUM)
               ENDIF
            ENDDO
         ENDIF










! **********************************************************************************************************************************
! Calc strains, then stresses for 3D elements

      ELSE IF ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN

         DO I=1,6
            STRESS_MECH  = ZERO
            STRESS_THERM = ZERO
         ENDDO

         DO I=1,3
            DO J=1,ELDOF
               STRAIN(I)   = STRAIN(I  ) + BE1(I,J,STR_PT_NUM)*UEL(J)
               STRAIN(I+3) = STRAIN(I+3) + BE2(I,J,STR_PT_NUM)*UEL(J)
            ENDDO
         ENDDO

         DO I=1,6
            IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
               TBAR = (DT(1,JTSUB) + DT(2,JTSUB) + DT(3,JTSUB) + DT(4,JTSUB))/FOUR
               ALPT(I) = ALPVEC(I,1)*(TBAR - TREF(1))
            ELSE
               ALPT(I) = ZERO
            ENDIF
         ENDDO
      
         CALL MATMULT_FFF ( ES, STRAIN, 6, 6, 1, STRESS_MECH )

         IF (SUBLOD(INT_SC_NUM,2) > 0) THEN
            CALL MATMULT_FFF ( ES, ALPT, 6, 6, 1, STRESS_THERM )
         ENDIF

         CALL MATADD_FFF  ( STRESS_MECH, STRESS_THERM, 6, 1, ONE, -ONE, 0, STRESS )

      ENDIF

! **********************************************************************************************************************************
! Transform coord for STRESS/STRAIN arrays, if requested (and if for 2D or 3D elements)

      IF (STR_CID /= -1) THEN                              ! User req diff stress/strain/engr force output coord sys than elem local

         IF      ((TYPE (1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'TRIA3')) THEN         
                                                           ! Transform 2D membrane and transverse shear stresses
            STR_TENSOR(1,1) = STRESS(1)   ;   STR_TENSOR(1,2) = STRESS(3)   ;   STR_TENSOR(1,3) = STRESS(7)
            STR_TENSOR(2,1) = STRESS(3)   ;   STR_TENSOR(2,2) = STRESS(2)   ;   STR_TENSOR(2,3) = STRESS(8)
            STR_TENSOR(3,1) = STRESS(7)   ;   STR_TENSOR(3,2) = STRESS(8)   ;   STR_TENSOR(3,3) = ZERO

            STRESS(1) = STR_TENSOR(1,1)
            STRESS(2) = STR_TENSOR(2,2)
            STRESS(3) = STR_TENSOR(1,2)
            STRESS(7) = STR_TENSOR(1,3)
            STRESS(8) = STR_TENSOR(2,3)

            CALL STR_TENSOR_TRANSFORM ( STR_TENSOR, STR_CID )
                                                           ! Transform 2D bending stresses
            STR_TENSOR(1,1) = STRESS(4)   ;   STR_TENSOR(1,2) = STRESS(6)   ;   STR_TENSOR(1,3) = ZERO
            STR_TENSOR(2,1) = STRESS(6)   ;   STR_TENSOR(2,2) = STRESS(5)   ;   STR_TENSOR(2,3) = ZERO
            STR_TENSOR(3,1) = ZERO        ;   STR_TENSOR(3,2) = ZERO        ;   STR_TENSOR(3,3) = ZERO

            STRESS(4) = STR_TENSOR(1,1)
            STRESS(5) = STR_TENSOR(2,2)
            STRESS(6) = STR_TENSOR(1,2)

            CALL STR_TENSOR_TRANSFORM ( STR_TENSOR, STR_CID )
                                                           ! Transform 2D membrane and transverse shear strains
            STR_TENSOR(1,1) = STRAIN(1)   ;   STR_TENSOR(1,2) = STRAIN(3)   ;   STR_TENSOR(1,3) = STRAIN(7)
            STR_TENSOR(2,1) = STRAIN(3)   ;   STR_TENSOR(2,2) = STRAIN(2)   ;   STR_TENSOR(2,3) = STRAIN(8)
            STR_TENSOR(3,1) = STRAIN(7)   ;   STR_TENSOR(3,2) = STRAIN(8)   ;   STR_TENSOR(3,3) = ZERO

            STRAIN(1) = STR_TENSOR(1,1)
            STRAIN(2) = STR_TENSOR(2,2)
            STRAIN(3) = STR_TENSOR(1,2)
            STRAIN(7) = STR_TENSOR(1,3)
            STRAIN(8) = STR_TENSOR(2,3)

            CALL STR_TENSOR_TRANSFORM ( STR_TENSOR, STR_CID )
                                                           ! Transform 2D bending strains
            STR_TENSOR(1,1) = STRAIN(4)   ;   STR_TENSOR(1,2) = STRAIN(6)   ;   STR_TENSOR(1,3) = ZERO
            STR_TENSOR(2,1) = STRAIN(6)   ;   STR_TENSOR(2,2) = STRAIN(5)   ;   STR_TENSOR(2,3) = ZERO
            STR_TENSOR(3,1) = ZERO        ;   STR_TENSOR(3,2) = ZERO        ;   STR_TENSOR(3,3) = ZERO

            STRAIN(4) = STR_TENSOR(1,1)
            STRAIN(5) = STR_TENSOR(2,2)
            STRAIN(6) = STR_TENSOR(1,2)

            CALL STR_TENSOR_TRANSFORM ( STR_TENSOR, STR_CID )

         ELSE IF ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN
                                                           ! Transform 3D stresses
            STR_TENSOR(1,1) = STRESS(1)   ;   STR_TENSOR(1,2) = STRESS(4)   ;   STR_TENSOR(1,3) = STRESS(6)
            STR_TENSOR(2,1) = STRESS(4)   ;   STR_TENSOR(2,2) = STRESS(2)   ;   STR_TENSOR(2,3) = STRESS(5)
            STR_TENSOR(3,1) = STRESS(6)   ;   STR_TENSOR(3,2) = STRESS(5)   ;   STR_TENSOR(3,3) = STRESS(3)

            STRESS(1) = STR_TENSOR(1,1)
            STRESS(2) = STR_TENSOR(2,2)
            STRESS(3) = STR_TENSOR(3,3)
            STRESS(4) = STR_TENSOR(1,2)
            STRESS(5) = STR_TENSOR(2,3)
            STRESS(6) = STR_TENSOR(1,3)

            CALL STR_TENSOR_TRANSFORM ( STR_TENSOR, STR_CID )
                                                           ! Transform 3D strains
            STR_TENSOR(1,1) = STRAIN(1)   ;   STR_TENSOR(1,2) = STRAIN(4)   ;   STR_TENSOR(1,3) = STRAIN(6)
            STR_TENSOR(2,1) = STRAIN(4)   ;   STR_TENSOR(2,2) = STRAIN(2)   ;   STR_TENSOR(2,3) = STRAIN(5)
            STR_TENSOR(3,1) = STRAIN(6)   ;   STR_TENSOR(3,2) = STRAIN(5)   ;   STR_TENSOR(3,3) = STRAIN(3)

            STRAIN(1) = STR_TENSOR(1,1)
            STRAIN(2) = STR_TENSOR(2,2)
            STRAIN(3) = STR_TENSOR(3,3)
            STRAIN(4) = STR_TENSOR(1,2)
            STRAIN(5) = STR_TENSOR(2,3)
            STRAIN(6) = STR_TENSOR(1,3)

            CALL STR_TENSOR_TRANSFORM ( STR_TENSOR, STR_CID )

         ELSE

            WRITE(ERR,9203) TYPE
            WRITE(F06,9203) TYPE
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )

         ENDIF

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9203 FORMAT(' *ERROR  9203: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INCORRECT ELEMENT TYPE = "',A,'"')
 


















! ##################################################################################################################################

      END SUBROUTINE ELEM_STRE_STRN_ARRAYS
