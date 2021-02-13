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
  
      SUBROUTINE WRITE_ELEM_STRAINS ( JSUB, NUM, IHDR, NUM_PTS )
  
! Writes blocks of element strains for one subcase and one element type for elements that do not have PCOMP properties, including
! all 2-D, 3-D  plus several 1-D elements (i.e. that have strain calculations).
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, BARTOR, INT_SC_NUM, MAX_NUM_STR, NDOFR, NUM_CB_DOFS,             &
                                         NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ELEM_STRAINS_BEGEND
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, GID_OUT_ARRAY, OGEL, POLY_FIT_ERR, POLY_FIT_ERR_INDEX
      USE MODEL_STUF, ONLY            :  ELEM_ONAME, ELMTYP, LABEL, SCNUM, STITLE, TITLE, TYPE
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  STRN_LOC, STRN_OPT
  
      USE WRITE_ELEM_STRAINS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ELEM_STRAINS'
      CHARACTER(LEN=*), INTENT(IN)    :: IHDR              ! Indicator of whether to write an output header

                                                           ! Array of different notes to write regarding poly fit errors
      CHARACTER( 50*BYTE)             :: ERR_INDEX_NOTE(MAX_NUM_STR)
      CHARACTER(119*BYTE)             :: FILL              ! Padding for output format
      CHARACTER(LEN=LEN(ELEM_ONAME))  :: ONAME             ! Element name to write out in F06 file
      CHARACTER( 1*BYTE)              :: WRITE_NOTES = 'N' ! Indicator of whether to write any WRT_ERR_INDEX_NOTE(i)

                                                           ! Indicators of whether to write note on indices of POLY_FIT_ERR
      CHARACTER( 1*BYTE)              :: WRT_ERR_INDEX_NOTE(MAX_NUM_STR)
  
      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG), INTENT(IN)       :: NUM_PTS           ! Num diff strain points for one element (3rd dim in arrays SEi, STEi)
      INTEGER(LONG)                   :: BDY_COMP          ! Component (1-6) for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_GRID          ! Grid for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_DOF_NUM       ! DOF number for BDY_GRID/BDY_COMP
      INTEGER(LONG)                   :: I,J,L             ! DO loop indices
      INTEGER(LONG)                   :: K                 ! Counter
      INTEGER(LONG)                   :: NCOLS             ! Num of cols to write out
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ELEM_STRAINS_BEGEND
  
      REAL(DOUBLE)                    :: ABS_ANS(11)       ! Max ABS for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MAX_ANS(11)       ! Max for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MIN_ANS(11)       ! Min for all grids output for each of the 6 disp components

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      DO I=1,MAX_NUM_STR
         WRT_ERR_INDEX_NOTE(I) = 'N'
      ENDDO

      ERR_INDEX_NOTE(1) = ' (1): Polynomial fit error is in X  normal  strain'
      ERR_INDEX_NOTE(2) = ' (2): Polynomial fit error is in Y  normal  strain'
      ERR_INDEX_NOTE(3) = ' (3): Polynomial fit error is in XY shear   strain'
      ERR_INDEX_NOTE(4) = ' (4): Polynomial fit error is in X  bending strain'
      ERR_INDEX_NOTE(5) = ' (5): Polynomial fit error is in Y  bending strain'
      ERR_INDEX_NOTE(6) = ' (6): Polynomial fit error is in XY twist   strain'
      ERR_INDEX_NOTE(7) = ' (7): Polynomial fit error is in XZ shear   strain'
      ERR_INDEX_NOTE(8) = ' (8): Polynomial fit error is in YZ shear   strain'
      ERR_INDEX_NOTE(9) = ''

      FILL(1:) = ' '

! Get element output name
 
      ONAME(1:) = ' '
      CALL GET_ELEM_ONAME ( ONAME )

! Write output headers if this is not the first use of this subr.

      IF (IHDR == 'Y') THEN

         WRITE(F06,*)                                                   ;   IF (DEBUG(200) > 0) WRITE(ANS,*)
         WRITE(F06,*)                                                   ;   IF (DEBUG(200) > 0) WRITE(ANS,*)

! -- F06 header: OUTPUT FOR SUBCASE, EIGENVECTOR or CRAIG-BAMPTON DOF

         IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN

            WRITE(F06,101) SCNUM(JSUB)                                  ;   IF (DEBUG(200) > 0) WRITE(ANS,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN

            WRITE(F06,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN

            WRITE(F06,102) JSUB

         ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

            WRITE(F06,102) JSUB                                         ;   IF (DEBUG(200) > 0) WRITE(ANS,102) JSUB

         ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN

            IF ((JSUB <= NDOFR) .OR. (JSUB >= NDOFR+NVEC)) THEN 
               IF (JSUB <= NDOFR) THEN
                  BDY_DOF_NUM = JSUB
               ELSE
                  BDY_DOF_NUM = JSUB-(NDOFR+NVEC)
               ENDIF
               CALL GET_GRID_AND_COMP ( 'R ', BDY_DOF_NUM, BDY_GRID, BDY_COMP  )
            ENDIF

            IF       (JSUB <= NDOFR) THEN
               WRITE(F06,103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
            ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NVEC)) THEN
               WRITE(F06,104) JSUB, NUM_CB_DOFS, JSUB-NDOFR
            ELSE
               WRITE(F06,103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
            ENDIF

            IF (DEBUG(200) > 0) THEN
               IF       (JSUB <= NDOFR) THEN
                  WRITE(ANS,103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
               ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NVEC)) THEN
                  WRITE(ANS,104) JSUB, NUM_CB_DOFS, JSUB-NDOFR
               ELSE
                  WRITE(ANS,103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
               ENDIF
            ENDIF

         ENDIF

! -- F06 header for TITLE, SUBTITLE, LABEL (but only to F06)

         IF (TITLE(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,201) TITLE(INT_SC_NUM)
         ENDIF

         IF (STITLE(INT_SC_NUM)(1:) /= ' ') THEN
            WRITE(F06,201) STITLE(INT_SC_NUM)
         ENDIF

         IF (LABEL(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,201) LABEL(INT_SC_NUM)
         ENDIF

         WRITE(F06,*)                                                ; IF (DEBUG(200) > 0) WRITE(ANS,*)

! -- F06 1st 2 header lines for strain output description

         IF      ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN
            IF (STRN_OPT == 'VONMISES') THEN
               IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
                  WRITE(F06,302) FILL(1: 15)                         ; IF (DEBUG(200) > 0) WRITE(ANS,302) FILL(1: 15)
               ELSE
                  WRITE(F06,301) FILL(1: 27)                         ; IF (DEBUG(200) > 0) WRITE(ANS,301) FILL(1: 27)
               ENDIF
               WRITE(F06,401) FILL(1: 55), ONAME                     ; IF (DEBUG(200) > 0) WRITE(ANS,401) FILL(1: 55), ONAME
            ELSE
               IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
                  WRITE(F06,302) FILL(1: 22)                         ; IF (DEBUG(200) > 0) WRITE(ANS,302) FILL(1: 22)
               ELSE
                  WRITE(F06,301) FILL(1: 33)                         ; IF (DEBUG(200) > 0) WRITE(ANS,301) FILL(1: 33)
               ENDIF
               WRITE(F06,401) FILL(1: 61), ONAME                     ; IF (DEBUG(200) > 0) WRITE(ANS,401) FILL(1: 61), ONAME
            ENDIF

         ELSE IF (TYPE(1:5) == 'QUAD4') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1: 20)                            ; IF (DEBUG(200) > 0) WRITE(ANS,302) FILL(1: 20)
            ELSE
               WRITE(F06,301) FILL(1: 42)                            ; IF (DEBUG(200) > 0) WRITE(ANS,301) FILL(1: 42)
            ENDIF
            WRITE(F06,401) FILL(1: 71), ONAME                        ; IF (DEBUG(200) > 0) WRITE(ANS,401) FILL(1: 71), ONAME

         ELSE IF (TYPE(1:5) == 'SHEAR') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1: 20)                            ; IF (DEBUG(200) > 0) WRITE(ANS,302) FILL(1: 36)
            ELSE
               WRITE(F06,301) FILL(1: 13)                            ; IF (DEBUG(200) > 0) WRITE(ANS,301) FILL(1: 52)
            ENDIF
            WRITE(F06,401) FILL(1: 42), ONAME                        ; IF (DEBUG(200) > 0) WRITE(ANS,401) FILL(1: 81), ONAME

         ELSE IF (TYPE(1:5) == 'TRIA3') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1: 20)                            ; IF (DEBUG(200) > 0) WRITE(ANS,302) FILL(1: 36)
            ELSE
               WRITE(F06,301) FILL(1: 36)                            ; IF (DEBUG(200) > 0) WRITE(ANS,301) FILL(1: 52)
            ENDIF
            WRITE(F06,401) FILL(1: 65), ONAME                        ; IF (DEBUG(200) > 0) WRITE(ANS,401) FILL(1: 81), ONAME

         ELSE IF (TYPE(1:4) == 'BUSH') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:  0)                            ; IF (DEBUG(200) > 0) WRITE(ANS,302) FILL(1: 36)
            ELSE
               WRITE(F06,301) FILL(1: 10)                            ; IF (DEBUG(200) > 0) WRITE(ANS,301) FILL(1: 52)
            ENDIF
            WRITE(F06,401) FILL(1: 39), ONAME                        ; IF (DEBUG(200) > 0) WRITE(ANS,401) FILL(1: 81), ONAME

         ENDIF

! -- F06 header lines describing strain columns

         IF     ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN
            IF (STRN_OPT == 'VONMISES') THEN
               WRITE(F06,1301) FILL(1:20), FILL(1:20)                ; IF (DEBUG(200) > 0) WRITE(ANS,1301) FILL(1:17), FILL(1:17)
            ELSE 
               WRITE(F06,1302) FILL(1:20), FILL(1:20)                ; IF (DEBUG(200) > 0) WRITE(ANS,1302) FILL(1:17), FILL(1:17)
            ENDIF

         ELSE IF (TYPE(1:5) == 'QUAD4') THEN
            IF (STRN_OPT == 'VONMISES') THEN
               WRITE(F06,1401) FILL(1:1), FILL(1:1), FILL(1:1)       ; IF (DEBUG(200) > 0) WRITE(ANS,1401) FILL(1:16), FILL(1:16),&
                                                                                                           FILL(1:16)
            ELSE
               WRITE(F06,1402) FILL(1:1), FILL(1:1)                  ; IF (DEBUG(200) > 0) WRITE(ANS,1402) FILL(1:16), FILL(1:16)
            ENDIF

         ELSE IF (TYPE(1:5) == 'SHEAR') THEN
               WRITE(F06,1601) FILL(1:1), FILL(1:1)                  ; IF (DEBUG(200) > 0) WRITE(ANS,1601) FILL(1:16), FILL(1:16),&
                                                                                                            FILL(1:16)
         ELSE IF (TYPE(1:5) == 'TRIA3') THEN
            IF (STRN_OPT == 'VONMISES') THEN
               WRITE(F06,1701) FILL(1:1), FILL(1:1), FILL(1:1)       ; IF (DEBUG(200) > 0) WRITE(ANS,1701) FILL(1:16), FILL(1:16),&
                                                                                                           FILL(1:16)
            ELSE
               WRITE(F06,1702) FILL(1:1), FILL(1:1)                  ; IF (DEBUG(200) > 0) WRITE(ANS,1702) FILL(1:16), FILL(1:16)
            ENDIF

         ELSE IF  (TYPE == 'BUSH    ') THEN
            WRITE(F06,1801) FILL(1:  1), FILL(1:  1)

         ELSE IF  (TYPE == 'USERIN  ') THEN
            WRITE(F06,1901) FILL(1:  1), FILL(1:  1)

         ENDIF

      ENDIF

! Write the element strain output
  
      IF     ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN

         IF (STRN_OPT == 'VONMISES') THEN
            NCOLS = 7
         ELSE
            NCOLS = 8
         ENDIF

         DO I=1,NUM
            WRITE(F06,1303) EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,NCOLS)    ; IF (DEBUG(200) > 0) WRITE(ANS,1313) EID_OUT_ARRAY(I,1),  &
                                                                                                              (OGEL(I,J),J=1,NCOLS)
         ENDDO

         CALL GET_MAX_MIN_ABS_STR ( NUM, NCOLS, 'N', MAX_ANS, MIN_ANS, ABS_ANS )

         IF (STRN_OPT == 'VONMISES') THEN
            WRITE(F06,1304) (MAX_ANS(J),J=1,7), (MIN_ANS(J),J=1,7), (ABS_ANS(J),J=1,7)
            IF (DEBUG(200) > 0) THEN
               WRITE(ANS,1314) (MAX_ANS(J),J=1,7), (MIN_ANS(J),J=1,7), (ABS_ANS(J),J=1,7)
            ENDIF
         ELSE
            WRITE(F06,1305) (MAX_ANS(J),J=1,8), (MIN_ANS(J),J=1,8), (ABS_ANS(J),J=1,8)
            IF (DEBUG(200) > 0) THEN
               WRITE(ANS,1315) (MAX_ANS(J),J=1,8), (MIN_ANS(J),J=1,8), (ABS_ANS(J),J=1,8)
            ENDIF
         ENDIF

      ELSE IF (TYPE(1:5) == 'QUAD4') THEN

         K = 0
         DO I=1,NUM,NUM_PTS

            K = K + 1
            WRITE(F06,*)                                                ; IF (DEBUG(200) > 0) WRITE(ANS,*)
            WRITE(F06,1403) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(K,J),J=1,10)                                                      
                                                                        ; IF (DEBUG(200) > 0) WRITE(ANS,1413) EID_OUT_ARRAY(I,1),  &
                                                                                                              (OGEL(K,J),J=1,10)
            K = K + 1
            WRITE(F06,1404) FILL(1: 0), (OGEL(K,J),J=1,8)               ; IF (DEBUG(200) > 0) WRITE(ANS,1414) (OGEL(K,J),J=1,8)


            DO L=1,NUM_PTS-1
            
               K = K + 1
               WRITE(F06,*)                                             ; IF (DEBUG(200) > 0) WRITE(ANS,*)
               IF (DABS(POLY_FIT_ERR(I+L)) >= 0.01D0) THEN
                  WRITE(F06,1405) FILL(1: 0), GID_OUT_ARRAY(I,L+1),(OGEL(K,J),J=1,10), POLY_FIT_ERR(I+L), POLY_FIT_ERR_INDEX(I+L)
                  WRT_ERR_INDEX_NOTE(POLY_FIT_ERR_INDEX(I+L)) = 'Y'
               ELSE
                  WRITE(F06,1406) FILL(1: 0), GID_OUT_ARRAY(I,L+1),(OGEL(K,J),J=1,10), POLY_FIT_ERR(I+L)
               ENDIF
               IF (DEBUG(200) > 0) THEN
                  WRITE(ANS,1415) GID_OUT_ARRAY(I,L+1),(OGEL(K,J),J=1,10), POLY_FIT_ERR(I+L), POLY_FIT_ERR_INDEX(I+L)
               ENDIF

               K = K + 1
               WRITE(F06,1407) FILL(1: 0), (OGEL(K,J),J=1,8)            ; IF (DEBUG(200) > 0) WRITE(ANS,1417) (OGEL(K,J),J=1,8)

            ENDDO

         ENDDO

         CALL GET_MAX_MIN_ABS_STR ( NUM, 10, 'Y', MAX_ANS, MIN_ANS, ABS_ANS )

         MAX_ANS(11) = ZERO                                ! Get max POLY_FIT_ERR
         K = 0
         DO I=1,NUM
            K = K + 1
            IF (POLY_FIT_ERR(I) > MAX_ANS(11)) THEN
               MAX_ANS(11) = POLY_FIT_ERR(I)
            ENDIF
            K = K + 1
         ENDDO

         MIN_ANS(11) = MAX_ANS(11)

         K = 0                                             ! Get min POLY_FIT_ERR
         DO I=1,NUM
            K = K + 1
            IF (POLY_FIT_ERR(I) < MIN_ANS(11)) THEN
               MIN_ANS(11) = POLY_FIT_ERR(I)
            ENDIF
            K = K + 1
         ENDDO
                                                           ! Get abs POLY_FIT_ERR
         ABS_ANS(11) = MAX( DABS(MAX_ANS(11)), DABS(MIN_ANS(11)) )

         IF (STRN_LOC == 'CORNER  ') THEN 
            WRITE(F06,1408) FILL(1: 0), FILL(1: 0), MAX_ANS(2),MAX_ANS(3),MAX_ANS(4),MAX_ANS(6),MAX_ANS(7),MAX_ANS(8),MAX_ANS(9),  &
                                        MAX_ANS(10),MAX_ANS(11),                                                                   &
                                        FILL(1: 0), MIN_ANS(2),MIN_ANS(3),MIN_ANS(4),MIN_ANS(6),MIN_ANS(7),MIN_ANS(8),MIN_ANS(9),  &
                                                    MIN_ANS(10),MIN_ANS(11),                                                       &
                                        FILL(1: 0), ABS_ANS(2),ABS_ANS(3),ABS_ANS(4),ABS_ANS(6),ABS_ANS(7),ABS_ANS(8),ABS_ANS(9),  &
                                                    ABS_ANS(10),ABS_ANS(11), FILL(1: 0)  
         ELSE
            WRITE(F06,1408) FILL(1: 0), FILL(1: 0), MAX_ANS(2),MAX_ANS(3),MAX_ANS(4),MAX_ANS(6),MAX_ANS(7),MAX_ANS(8),MAX_ANS(9),  &
                                                    MAX_ANS(10),MAX_ANS(11),                                                       &
                                        FILL(1: 0), MIN_ANS(2),MIN_ANS(3),MIN_ANS(4),MIN_ANS(6),MIN_ANS(7),MIN_ANS(8),MIN_ANS(9),  &
                                                    MIN_ANS(10),MIN_ANS(11),                                                       &
                                        FILL(1: 0), ABS_ANS(2),ABS_ANS(3),ABS_ANS(4),ABS_ANS(6),ABS_ANS(7),ABS_ANS(8),ABS_ANS(9),  &
                                                    ABS_ANS(10),ABS_ANS(11), FILL(1: 0)
         ENDIF

         IF (DEBUG(200) > 0) THEN
            IF (STRN_LOC == 'CORNER  ') THEN
               WRITE(ANS,1418)MAX_ANS(2),MAX_ANS(3),MAX_ANS(4),MAX_ANS(6),MAX_ANS(7),MAX_ANS(8),MAX_ANS(9),MAX_ANS(10),MAX_ANS(11),&
                              MIN_ANS(2),MIN_ANS(3),MIN_ANS(4),MIN_ANS(6),MIN_ANS(7),MIN_ANS(8),MIN_ANS(9),MIN_ANS(10),MIN_ANS(11),&
                              ABS_ANS(2),ABS_ANS(3),ABS_ANS(4),ABS_ANS(6),ABS_ANS(7),ABS_ANS(8),ABS_ANS(9),ABS_ANS(10)
            ELSE
               WRITE(ANS,1418)MAX_ANS(2),MAX_ANS(3),MAX_ANS(4),MAX_ANS(6),MAX_ANS(7),MAX_ANS(8),MAX_ANS(9),MAX_ANS(10),MAX_ANS(11),&
                              MIN_ANS(2),MIN_ANS(3),MIN_ANS(4),MIN_ANS(6),MIN_ANS(7),MIN_ANS(8),MIN_ANS(9),MIN_ANS(10),MIN_ANS(11),&
                              ABS_ANS(2),ABS_ANS(3),ABS_ANS(4),ABS_ANS(6),ABS_ANS(7),ABS_ANS(8),ABS_ANS(9),ABS_ANS(10),ABS_ANS(11)
            ENDIF
         ENDIF
         

         WRITE_NOTES = 'N'
         DO I=1,MAX_NUM_STR
            IF (WRT_ERR_INDEX_NOTE(I) == 'Y') THEN
               WRITE_NOTES = 'Y'
            ENDIF
         ENDDO

         IF (WRITE_NOTES == 'Y') THEN
            WRITE(F06,1498)
            DO I=1,MAX_NUM_STR
               IF (WRT_ERR_INDEX_NOTE(I) == 'Y') THEN
                  WRITE(F06,1499) ERR_INDEX_NOTE(I)
               ENDIF
            ENDDO
         ENDIF

      ELSE IF (TYPE(1:5) == 'SHEAR') THEN

         DO I=1,NUM,2
            IF (I+1 <= NUM) THEN
               WRITE(F06,1603) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,3), EID_OUT_ARRAY(I+1,1),(OGEL(I+1,J),J=1,3)
            ELSE
               WRITE(F06,1603) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,3)
            ENDIF
         ENDDO

         CALL GET_MAX_MIN_ABS_STR ( NUM, 3, 'N', MAX_ANS, MIN_ANS, ABS_ANS )

         WRITE(F06,1604) FILL(1: 0), FILL(1: 0), MAX_ANS(1),MAX_ANS(2),MAX_ANS(3),                                                 &
                         FILL(1: 0),             MIN_ANS(1),MIN_ANS(2),MIN_ANS(3),                                                 &
                         FILL(1: 0),             ABS_ANS(1),ABS_ANS(2),ABS_ANS(3)

      ELSE IF (TYPE(1:5) == 'TRIA3') THEN

         K = 0
         DO I=1,NUM

            K = K + 1
            WRITE(F06,*)                                                ; WRITE(ANS,*)
            WRITE(F06,1703) EID_OUT_ARRAY(I,1),(OGEL(K,J),J=1,10)       ; IF (DEBUG(200) > 0) WRITE(ANS,1713) EID_OUT_ARRAY(I,1),  &
                                                                                                              (OGEL(K,J),J=1,10)
            K = K + 1
            WRITE(F06,1704) (OGEL(K,J),J=1,8)                           ; IF (DEBUG(200) > 0) WRITE(ANS,1714) (OGEL(K,J),J=1,10)

         ENDDO

         CALL GET_MAX_MIN_ABS_STR ( NUM, 10, 'Y', MAX_ANS, MIN_ANS, ABS_ANS )

         WRITE(F06,1705) MAX_ANS(2),MAX_ANS(3),MAX_ANS(4),MAX_ANS(6),MAX_ANS(7),MAX_ANS(8),MAX_ANS(9),MAX_ANS(10),                 &
                         MIN_ANS(2),MIN_ANS(3),MIN_ANS(4),MIN_ANS(6),MIN_ANS(7),MIN_ANS(8),MIN_ANS(9),MIN_ANS(10),                 &
                         ABS_ANS(2),ABS_ANS(3),ABS_ANS(4),ABS_ANS(6),ABS_ANS(7),ABS_ANS(8),ABS_ANS(9),ABS_ANS(10)
                         
         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,1715) MAX_ANS(2),MAX_ANS(3),MAX_ANS(4),MAX_ANS(6),MAX_ANS(7),MAX_ANS(8),MAX_ANS(9),MAX_ANS(10),              &
                            MIN_ANS(2),MIN_ANS(3),MIN_ANS(4),MIN_ANS(6),MIN_ANS(7),MIN_ANS(8),MIN_ANS(9),MIN_ANS(10),              &
                            ABS_ANS(2),ABS_ANS(3),ABS_ANS(4),ABS_ANS(6),ABS_ANS(7),ABS_ANS(8),ABS_ANS(9),ABS_ANS(10)
         ENDIF

      ELSE IF (TYPE == 'BUSH    ') THEN
         DO I=1,NUM
            WRITE(F06,1802) EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,6)        ; IF (DEBUG(200) > 0) WRITE(ANS,1812) EID_OUT_ARRAY(I,1),  &
                                                                                                              (OGEL(I,J),J=1,6)
         ENDDO

      ELSE IF (TYPE == 'USERIN  ') THEN
         DO I=1,NUM
            WRITE(F06,1902) EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,6)        ; IF (DEBUG(200) > 0) WRITE(ANS,1912) EID_OUT_ARRAY(I,1),  &
                                                                                                              (OGEL(I,J),J=1,6)
         ENDDO

      ELSE
         WRITE(ERR,9300) SUBR_NAME,TYPE                           
         WRITE(F06,9300) SUBR_NAME,TYPE                          
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error (elem type not valid) , so quit
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' OUTPUT FOR SUBCASE ',I8)

  102 FORMAT(' OUTPUT FOR EIGENVECTOR ',I8)

  103 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (boundary ',A,' for grid',I8,' component',I2,')')

  104 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (modal acceleration for mode ',I8,')')

  201 FORMAT(1X,A)

  301 FORMAT(1X,A,'E L E M E N T   S T R A I N S   I N   L O C A L   E L E M E N T   C O O R D I N A T E   S Y S T E M')
  
  302 FORMAT(1X,A,'C B   E L E M E N T   S T R A I N S   O T M   I N   L O C A L   E L E M E N T   C O O R D I N A T E',           &
  '   S Y S T E M')
  
  401 FORMAT(A,'F O R   E L E M E N T   T Y P E   ',A11)
 


! 3D Elems >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1301 FORMAT(A,'Element    Sigma-xx      Sigma-yy      Sigma-zz       Tau-xy        Tau-yz        Tau-zx      von Mises'           &
          ,/,A,'   ID')
  
 1302 FORMAT(A,'Element    Sigma-xx      Sigma-yy      Sigma-zz       Tau-xy        Tau-yz        Tau-zx         ',                &
             'Octahedral Strain'                                                                                                   &
          ,/,A,'   ID',91X,'Direct        Shear')
  
 1303 FORMAT(19X,I8,8(1ES14.6))

 1304 FORMAT(28X,'------------- ------------- ------------- ------------- ------------- ------------- -------------',/,            &
             1X,'MAX (for output set):     ',7(ES14.6),/,                                                                          &
             1X,'MIN (for output set):     ',7(ES14.6),//,                                                                         &
             1X,'ABS (for output set):     ',7(ES14.6))

 1305 FORMAT(27X,' ------------- ------------- ------------- ------------- ------------- ------------- -------------',             &
                 ' -------------',/,                                                                                               &
             1X,'MAX (for output set):     ',8(ES14.6),/,                                                                          &
             1X,'MIN (for output set):     ',8(ES14.6),//,                                                                         &
             1X,'ABS (for output set):     ',8(ES14.6))

 1313 FORMAT(16X,I8,8(1ES14.6))

 1314 FORMAT(28X,'------------- ------------- ------------- ------------- ------------- ------------- -------------',/,            &
             1X,'MAX (for output set):  ',7(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',7(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',7(ES14.6),/,                                                                             &
             1X,'*for output set')

 1315 FORMAT(28X,'------------- ------------- ------------- ------------- ------------- ------------- -------------',              &
                 ' -------------',/,                                                                                               &
             1X,'MAX (for output set):  ',8(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',8(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',8(ES14.6),/,                                                                             &
             1X,'*for output set')

! QUAD4 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1401 FORMAT(1X,A,'Elem  Location         Fibre      Strains In Element Coord System       Principal Strains (Zero Shear)',        &
  '                 Transverse   Transverse   % Poly',/,1X,A,                                                                      &
  ' ID                   Distance   Normal-X     Normal-Y     Shear-XY     Angle      Major        Minor      von Mises',          &
  '    Shear-XZ     Shear-YZ    Fit Err',A)

 1402 FORMAT(1X,A,'Elem  Location         Fibre      Strains In Element Coord System       Principal Strains (Zero Shear)',        &
  '       Max     Transverse   Transverse   % Poly',/,1X,A,                                                                        &
  ' ID                     Distance    Normal-X     Normal-Y     Shear-XY     Angle     Major        Minor      Shear-XY',         &
  '     Shear-XZ     Shear-YZ   Fit Err',A)

 1403 FORMAT(1X,A,I8,2X,'CENTER  ',3X,1ES11.3,3(1ES13.5),0PF8.2,5(1ES13.5))

 1404 FORMAT(1X,A,21X,1ES11.3,3(1ES13.5),0PF8.2,3(1ES13.5))

 1405 FORMAT(1X,A,10X,'GRD',I8,1ES11.3,3(1ES13.5),0PF8.2,5(1ES13.5),E9.1,'(',I1,')')

 1406 FORMAT(1X,A,10X,'GRD',I8,1ES11.3,3(1ES13.5),0PF8.2,5(1ES13.5),E9.1)

 1407 FORMAT(1X,A,21X,1ES11.3,3(1ES13.5),0PF8.2,3(1ES13.5))

 1408 FORMAT(1X,A,32X,' ------------ ------------ ------------         ------------ ------------ ------------ ------------',       &
                 ' ------------ --------',/,                                                                                       &
             1X,A,'MAX* : ',25x,3(ES13.5),8X,5(ES13.5),E9.1,/,                                                                     &
             1X,A,'MIN* : ',25x,3(ES13.5),8X,5(ES13.5),E9.1,//,                                                                    &
             1X,A,'ABS* : ',25x,3(ES13.5),8X,5(ES13.5),E9.1,/,                                                                     &
             1X,A,'*for output set')

 1413 FORMAT(1X,I8,2X,'CENTER  ',5X,4(1ES14.6),0PF14.3,5(1ES14.6))

 1414 FORMAT(9X,15X,4(1ES14.6),0PF14.3,3(1ES14.6))

 1415 FORMAT(21X,'GRID',I8,1X,4(1ES14.6),0PF9.3,5(1ES14.6),F13.2,'% (',I1,')')

 1417 FORMAT(9X,15X,4(1ES14.6),0PF9.3,3(1ES14.6))

 1418 FORMAT(39X,'------------- ------------- -------------               ------------- ------------- ------------- -------------',&
                 ' ------------- -------------',/,  &
             1X,'MAX (for output set): ',15X,3(ES14.6),14X,5(ES14.6),F14.2,/,                                                      &
             1X,'MIN (for output set): ',15X,3(ES14.6),14X,5(ES14.6),F14.2,//,                                                     &
             1X,'ABS (for output set): ',15X,3(ES14.6),14X,5(ES14.6),F14.2)

 1498 FORMAT(' NOTE: Explanation of errors in the polynomial fit to extrapolate element corner point strains from values at the',  &
                   ' Gauss points:')

 1499 FORMAT(6X,A)

! SHEAR ----------------------------------------------------------------------------------------------------------------------------
 1601 FORMAT(1X,A,'Element               S t r a i n s                            Element               S t r a i n s'             &
          ,/,1X,A,'   ID      Normal-X      Normal-Y      Shear-XY                   ID      Normal-X      Normal-Y'               &
                 ,'      Shear-XY')
  
 1603 FORMAT(1X,A,I8,3(1ES14.6),13X,I8,3(1ES14.6))

 1604 FORMAT(1X,A,'         ------------- ------------- ------------- ',20X,' ------------- ------------- ------------- ',/,       &
             1X,A,'MAX* : ',1X,3(ES14.6),/,                                                                                        &
             1X,A,'MIN* : ',1X,3(ES14.6),//,                                                                                       &
             1X,A,'ABS* : ',1X,3(ES14.6),/,                                                                                        &
             1X,A,'*for output set')

! TRIA3 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1701 FORMAT(1X,A,'Element    Location      Fibre        Strains In Element Coord System       Principal Strains (Zero Shear)',    &
                '                 Transverse   Transverse'                                                                         &
          ,/,1X,A,'   ID                   Distance     Normal-X     Normal-Y      Shear-XY     Angle     Major        Minor'      &
          ,'      von Mises    Shear-XZ     Shear-YZ',A)
  
 1702 FORMAT(1X,A,'Element    Location      Fibre        Strains In Element Coord System       Principal Strains (Zero Shear)',    &
  '      Max        Transverse   Transverse'                                                                                       &
          ,/,1X,A,'   ID                   Distance     Normal-X     Normal-Y      Shear-XY     Angle     Major        Minor',     &
          '      Shear-XY    Shear-XZ     Shear-YZ',A)
  
 1703 FORMAT(1X,I8,4X,'Anywhere',2X,4(1ES13.5),0PF9.3,5(1ES13.5))

 1704 FORMAT(13X,'in elem',3X,4(1ES13.5),0PF9.3,5(1ES13.5))

 1705 FORMAT(37X,'------------ ------------ ------------          ------------ ------------ ------------ ------------',            &
                 ' ------------',/,                                                                                                &
             1X,'MAX* : ',28x,3(ES13.5),9X,5(ES13.5),/,                                                                            &
             1X,'MIN* : ',28x,3(ES13.5),9X,5(ES13.5),//,                                                                           &
             1X,'ABS* : ',28x,3(ES13.5),9X,5(ES13.5),/,                                                                            &
             1X,'*for output set')

 1713 FORMAT(1X,I8,4X,'Anywhere',3X,4(1ES14.6),0PF14.3,5(1ES14.6))

 1714 FORMAT(13X,'in elem',4X,4(1ES14.6),0PF14.3,5(1ES14.6))

 1715 FORMAT(39X,'------------- ------------- -------------               ------------- ------------- ------------- ---------',/,  &
             1X,'MAX (for output set): ',15X,3(ES14.6),14X,5(ES14.6),/,                                                            &
             1X,'MIN (for output set): ',15X,3(ES14.6),14X,5(ES14.6),//,                                                           &
             1X,'ABS (for output set): ',15X,3(ES14.6),14X,5(ES14.6),/)

! BUSH >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1801 FORMAT(20X,A,'Element    Strain-1      Strain-2      Strain-3      Strain-4      Strain-5      Strain-6'                     &
          ,/,20X,A,'   ID')
  
 1802 FORMAT(19X,I8,6(1ES14.6))

 1812 FORMAT(16X,I8,6(1ES14.6))

! USERIN >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1901 FORMAT(20X,A,'Element    Strain-1      Strain-2      Strain-3      Strain-4      Strain-5      Strain-6'                     &
          ,/,20X,A,'   ID')
  
 1902 FORMAT(19X,I8,6(1ES14.6))

 1912 FORMAT(19X,I8,6(1ES14.6))

! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 9300 FORMAT(' *ERROR  9300: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NO OUTPUT FORMAT AVAILABLE FOR ELEMENT TYPE = ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_ELEM_STRAINS
