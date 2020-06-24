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
  
      SUBROUTINE WRITE_ELEM_ENGR_FORCE ( JSUB, NUM, IHDR )
  
! Writes blocks of element engineering force output for one element type, one subcase. Elements that can have engineering force
! output are the ones enumerated below fin the IF(TYPE == ???). 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, INT_SC_NUM, NDOFR, NUM_CB_DOFS, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, OGEL
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ELEM_ENGR_FORCE_BEGEND
      USE MODEL_STUF, ONLY            :  ELEM_ONAME, LABEL, SCNUM, STITLE, TITLE, TYPE
      USE WRITE_ELEM_ENGR_FORCE_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ELEM_ENGR_FORCE'
      CHARACTER(LEN=*), INTENT(IN)    :: IHDR              ! Indicator of whether to write an output header
      CHARACTER(128*BYTE)             :: FILL              ! Padding for output format
      CHARACTER(LEN=LEN(ELEM_ONAME))  :: ONAME             ! Element name to write out in F06 file
  
      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG)                   :: BDY_COMP          ! Component (1-6) for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_GRID          ! Grid for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_DOF_NUM       ! DOF number for BDY_GRID/BDY_COMP
      INTEGER(LONG)                   :: I,J,J1            ! DO loop indices or counters
      INTEGER(LONG)                   :: NUM_TERMS         ! Number of terms to write out for shell elems
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ELEM_ENGR_FORCE_BEGEND
  
      REAL(DOUBLE)                    :: ABS_ANS(8)        ! Max ABS for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MAX_ANS(8)        ! Max for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MIN_ANS(8)        ! Min for all grids output for each of the 6 disp components

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      FILL(1:) = ' '

! Get element output name
 
      ONAME(1:) = ' '
      CALL GET_ELEM_ONAME ( ONAME )

! Write output headers.

headr:IF (IHDR == 'Y') THEN

!--- Subcase num, TITLE, SUBT, LABEL:

         WRITE(F06,*)
         WRITE(F06,*)
         IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN

            WRITE(F06,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN

            WRITE(F06,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN

            WRITE(F06,102) JSUB

         ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

            WRITE(F06,102) JSUB

         ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! Write info on what CB DOF the output is for

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
  
         ENDIF

         IF (TITLE(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,201) TITLE(INT_SC_NUM)
         ENDIF

         IF (STITLE(INT_SC_NUM)(1:) /= ' ') THEN
            WRITE(F06,201) STITLE(INT_SC_NUM)
         ENDIF

         IF (LABEL(INT_SC_NUM)(1:)  /= ' ') THEN
            WRITE(F06,201) LABEL(INT_SC_NUM)
         ENDIF

         WRITE(F06,*)

!--- 1st 2 lines of element specific headers - general info on what type of output:

         IF      (TYPE(1:3) == 'BAR') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:33)
            ELSE
               WRITE(F06,301) FILL(1:39)
            ENDIF
            WRITE(F06,401) FILL(1:45), ONAME

         ELSE IF (TYPE(1:4) == 'BUSH') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:19)
            ELSE
               WRITE(F06,301) FILL(1:25)
            ENDIF
            WRITE(F06,401) FILL(1:29), ONAME

         ELSE IF (TYPE(1:4) == 'ELAS') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:27)
            ELSE
               WRITE(F06,301) FILL(1:33)
            ENDIF
            WRITE(F06,401) FILL(1:37), ONAME

         ELSE IF (TYPE(1:3) == 'ROD') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:27)
            ELSE
               WRITE(F06,301) FILL(1:33)
            ENDIF
            WRITE(F06,401) FILL(1:37), ONAME

         ELSE IF (TYPE(1:5) == 'SHEAR') THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:22)
            ELSE
               WRITE(F06,301) FILL(1:29)
            ENDIF
            WRITE(F06,401) FILL(1:32), ONAME

         ELSE IF((TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4')) THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(F06,302) FILL(1:33)
            ELSE
               WRITE(F06,301) FILL(1:39)
            ENDIF
            WRITE(F06,401) FILL(1:43), ONAME

         ENDIF

!--- Header lines describing columns of output for an element type:

         IF      (TYPE(1:3) == 'BAR'  ) THEN
            WRITE(F06,1101) FILL(1: 0), FILL(1: 0)

         ELSE IF (TYPE(1:4) == 'ELAS') THEN
            WRITE(F06,1201) FILL(1: 0), FILL(1: 0)

         ELSE IF (TYPE(1:3) == 'ROD') THEN
            WRITE(F06,1301) FILL(1: 0), FILL(1: 0)

         ELSE IF (TYPE(1:5) == 'SHEAR') THEN
            WRITE(F06,1401) FILL(1: 0), FILL(1: 0)

         ELSE IF ((TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4')) THEN 
            WRITE(F06,1501) FILL(1: 0), FILL(1: 0), FILL(1: 0)

         ELSE IF (TYPE(1:4) == 'BUSH') THEN
            WRITE(F06,1601) FILL(1: 0), FILL(1: 0)

         ENDIF

         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'Headers' )

      ENDIF headr

! Write element force output
  
      IF      (TYPE == 'BAR     ') THEN

         DO I=1,NUM
            WRITE(F06,1102) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,8)
         ENDDO   
         CALL GET_MAX_MIN_ABS ( 1, 8 )
         WRITE(F06,1103) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,8), FILL(1: 0), (MIN_ANS(J),J=1,8), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,8), FILL(1: 0)
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'BAR' )

      ELSE IF (TYPE(1:4) == 'ELAS') THEN                   ! Engr force for ELAS was put into OGEL(I,1)

         J1 = 1
         DO I=1,NUM,5
            IF (J1+4 <= NUM) THEN
               WRITE(F06,1202) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,1), J=J1,J1+4)
               J1 = J1 + 5
            ELSE
               WRITE(F06,1202) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,1), J=J1,NUM)
            ENDIF
         ENDDO
         CALL GET_MAX_MIN_ABS ( 1, 1 )
         WRITE(F06,1203) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,1), FILL(1: 0), (MIN_ANS(J),J=1,1), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,1), FILL(1: 0)
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'ELAS' )
 
      ELSE IF (TYPE == 'ROD     ') THEN

         J1 = 1
         DO I=1,NUM,3
            IF (J1+2 <= NUM) THEN
               WRITE(F06,1302) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,7), OGEL(J,8), J=J1,J1+2)
               J1 = J1 + 3
            ELSE
               WRITE(F06,1302) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,7), OGEL(J,8), J=J1,NUM)
            ENDIF
         ENDDO
         CALL GET_MAX_MIN_ABS ( 7, 8 )
         WRITE(F06,1303) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=7,8), FILL(1: 0), (MIN_ANS(J),J=7,8), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=7,8), FILL(1: 0)
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'ROD' )
  
      ELSE IF (TYPE == 'SHEAR   ') THEN  

         J1 = 1
         DO I=1,NUM,2
            IF      (J1+1 <= NUM) THEN
               WRITE(F06,1402) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,1), OGEL(J,2), OGEL(J,3), J=J1,J1+1)
               J1 = J1 + 2
            ELSE
               WRITE(F06,1402) FILL(1: 0), (EID_OUT_ARRAY(J,1), OGEL(J,1), OGEL(J,2), OGEL(J,3), J=J1,NUM)
            ENDIF
         ENDDO
         CALL GET_MAX_MIN_ABS ( 1, 3 )
         WRITE(F06,1403) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,3), FILL(1: 0), (MIN_ANS(J),J=1,3), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,3), FILL(1: 0)
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'SHEAR' )
  
      ELSE IF ((TYPE == 'TRIA3K  ') .OR. (TYPE == 'QUAD4K  ')) THEN 

         DO I=1,NUM
            WRITE(F06,1512) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,6)
         ENDDO   
         CALL GET_MAX_MIN_ABS ( 1, 8 )
         WRITE(F06,1513) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,6), FILL(1: 0), (MIN_ANS(J),J=1,6), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,6), FILL(1: 0)
         NUM_TERMS = 6
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'SHELL' )

      ELSE IF ((TYPE == 'TRIA3   ') .OR. (TYPE == 'QUAD4   ')) THEN 

         DO I=1,NUM
            WRITE(F06,1522) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,8)
         ENDDO   
         CALL GET_MAX_MIN_ABS ( 1, 8 )
         WRITE(F06,1523) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,8), FILL(1: 0), (MIN_ANS(J),J=1,8), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,8), FILL(1: 0)
         NUM_TERMS = 8
         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'SHELL' )

      ELSE IF (TYPE(1:4) == 'BUSH') THEN                   ! Engr force for BUSH was put into OGEL(I,1-6)

         DO I=1,NUM
            WRITE(F06,1602) FILL(1: 0), EID_OUT_ARRAY(I,1),(OGEL(I,J),J=1,6)
         ENDDO   
         CALL GET_MAX_MIN_ABS ( 1, 6 )
         WRITE(F06,1603) FILL(1: 0), FILL(1: 0), (MAX_ANS(J),J=1,6), FILL(1: 0), (MIN_ANS(J),J=1,6), FILL(1: 0),                   &
                                                 (ABS_ANS(J),J=1,6), FILL(1: 0)

         IF (DEBUG(200) > 0) CALL WRITE_ANS ( 'BUSH' )
   
      ENDIF
  
! **********************************************************************
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

  301 FORMAT(1X,A,'E L E M E N T   E N G I N E E R I N G   F O R C E S')

  302 FORMAT(1X,A,'C B   E L E M E N T   E N G I N E E R I N G   F O R C E   O T M')

  401 FORMAT(1X,A,'F O R   E L E M E N T   T Y P E   ',A11)

! BAR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1101 FORMAT(1X,A,' Element       Bend-Moment End A           Bend-Moment End B              - Shear -              Axial'         &
          ,'         Torque'  &
          ,/,1X,A,'    ID       Plane 1       Plane 2       Plane 1       Plane 2      Plane 1       Plane 2        Force')
 
 1102 FORMAT(1X,A,I8,8(1ES14.6))
 
 1103 FORMAT(1X,A,'         ------------- ------------- ------------- ------------- ------------- ------------- -------------',    &
                        ' -------------',/,                                                                                        &
             1X,A,'MAX* :  ',8(ES14.6),/,                                                                                          &
             1X,A,'MIN* :  ',8(ES14.6),//,                                                                                         &
             1X,A,'ABS* :  ',8(ES14.6),/,                                                                                          &
             1X,A,'*for output set')

! ELAS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1201 FORMAT(1X,A,' Element     Force      Element     Force      Element     Force      Element     Force      Element     Force' &
          ,/,1X,A,'    ID                     ID                     ID                     ID                     ID')
 
 1202 FORMAT(1X,A,5(I8,1ES14.6))
  
 1203 FORMAT(1X,A,'         -------------',/,                                                                                      &
             1X,A,'MAX* :  ',1(ES14.6),/,                                                                                          &
             1X,A,'MIN* :  ',1(ES14.6),//,                                                                                         &
             1X,A,'ABS* :  ',1(ES14.6),/,                                                                                          &
             1X,A,'*for output set')

! ROD >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1301 FORMAT(1X,A,' Element     Axial        Torque      Element     Axial        Torque      Element     Axial        Torque'     &
          ,/,1X,A,'    ID       Force                       ID       Force                       ID       Force')
 
 1302 FORMAT(1X,A,3(I8,1ES14.6,1ES14.6))
 
 1303 FORMAT(1X,A,'         ------------- -------------',/,                                                                        &
             1X,A,'MAX* :  ',2(1ES14.6),/,                                                                                         &
             1X,A,'MIN* :  ',2(1ES14.6),//,                                                                                        &
             1X,A,'ABS* :  ',2(1ES14.6),/,                                                                                         &
             1X,A,'*for output set')

! SHEAR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1401 FORMAT(1X,A,' Element        N o r m a l   F o r c e s           Element        N o r m a l   F o r c e s          '         &
          ,/,1X,A,'    ID        Nxx           Nyy           Nxy          ID        Nxx           Nyy           Nxy')
 
 1402 FORMAT(1X,A,2(I8,3(1ES14.6),1X))
 
 1403 FORMAT(1X,A,'         ------------- ------------- -------------',/,                   &
             1X,A,'MAX* :  ',3ES14.6,/,                                                   &
             1X,A,'MIN* :  ',3ES14.6,//,                                                  &
             1X,A,'ABS* :  ',3ES14.6,/,                                                   &
             1X,A,'*for output set')

! SHELL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1501 FORMAT(1X,A,' Element         N o r m a l   F o r c e s                       M o m e n t s'                                 &
          ,19X,'T r a n s v e r s e',/1X,A,'    ID',89X,'S h e a r   F o r c e s'                                                  &
          ,/,1X,A,'              Nxx           Nyy           Nxy           Mxx           Myy           Mxy            Qx          '&
          ,'  Qy')

!            WRITE(F06,1501) FILL(1: 0), FILL(1: 0), FILL(1: 0)
 1512 FORMAT(1X,A,I8,6(1ES14.6))
 
 1513 FORMAT(1X,A,'          ------------- ------------- ------------- ------------- ------------- -------------',/,               &
             1X,A,'MAX* :  ',6(ES14.6),/,                                                                                          &
             1X,A,'MIN* :  ',6(ES14.6),//,                                                                                         &
             1X,A,'ABS* :  ',6(ES14.6),/,                                                                                          &
             1X,A,'*for output set')

 1522 FORMAT(1X,A,I8,8(1ES14.6))
 
 1523 FORMAT(1X,A,'          ------------- ------------- ------------- ------------- ------------- ------------- -------------',   &
                        ' -------------',/,                                                                                        &
             1X,A,'MAX* :  ',8(ES14.6),/,                                                                                          &
             1X,A,'MIN* :  ',8(ES14.6),//,                                                                                         &
             1X,A,'ABS* :  ',8(ES14.6),/,                                                                                          &
             1X,A,'*for output set')

! BUSH >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1601 FORMAT(1X,A,' Element      Force         Force         Force        Moment        Moment        Moment'                      &
          ,/,1X,A,'    ID         XE            YE            ZE            XE            YE            ZE')
 
 1602 FORMAT(1X,A,I8,6(1ES14.6))
  
 1603 FORMAT(1X,A,'          ------------- ------------- ------------- ------------- ------------- ------------- ',/,              &
             1X,A,'MAX* :  ',6(ES14.6),/,                                                                                          &
             1X,A,'MIN* :  ',6(ES14.6),//,                                                                                         &
             1X,A,'ABS* :  ',6(ES14.6),/,                                                                                          &
             1X,A,'*for output set')

! **********************************************************************************************************************************
  
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE WRITE_ANS ( WHICH )

      USE PENTIUM_II_KIND, ONLY       :  BYTE

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: WHICH             ! Which of the below to write during this call

      INTEGER(LONG)                   :: II,JJ             ! DO loop indices or counters

! **********************************************************************************************************************************
      IF (WHICH == 'Headers') THEN

         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,*)
            WRITE(ANS,*)
            IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN

               WRITE(ANS,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN

            WRITE(F06,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN

            WRITE(F06,102) JSUB

            ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

               WRITE(ANS,102) JSUB

            ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! Write info on what CB DOF the output is for

               IF ((JSUB <= NDOFR) .OR. (JSUB >= NDOFR+NVEC)) THEN 
                  IF (JSUB <= NDOFR) THEN
                     BDY_DOF_NUM = JSUB
                  ELSE
                     BDY_DOF_NUM = JSUB-(NDOFR+NVEC)
                  ENDIF
                  CALL GET_GRID_AND_COMP ( 'R ', BDY_DOF_NUM, BDY_GRID, BDY_COMP  )
               ENDIF

               IF       (JSUB <= NDOFR) THEN
                  WRITE(ANS,103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
               ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NVEC)) THEN
                  WRITE(ANS,104) JSUB, NUM_CB_DOFS, JSUB-NDOFR
               ELSE
                  WRITE(ANS,103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
               ENDIF
  
            ENDIF

            WRITE(ANS,*)

            IF ((TYPE(1:4) /= 'HEXA') .AND. (TYPE(1:5) /= 'PENTA') .AND. (TYPE(1:5) /= 'TETRA')) THEN
            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(ANS,302) FILL(1:16)
            ELSE
               WRITE(ANS,301) FILL(1:16)
            ENDIF
               WRITE(ANS,401) FILL(1:16), ONAME
            ENDIF

            IF      (TYPE(1:4) == 'ELAS') THEN
               WRITE(ANS,1111) FILL(1:16), FILL(1:16)

            ELSE IF (TYPE == 'BAR     ') THEN
               WRITE(ANS,1211) FILL(1:16), FILL(1:16)

            ELSE IF (TYPE == 'ROD     ') THEN
               WRITE(ANS,1311) FILL(1:16), FILL(1:16)

            ELSE IF (TYPE == 'SHEAR   ') THEN
               WRITE(ANS,1411) FILL(1:16), FILL(1:16)

            ELSE IF ((TYPE(1:5)== 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4')) THEN 
               WRITE(ANS,1511) FILL(1:16), FILL(1:16), FILL(1:16)

            ELSE IF (TYPE == 'BUSH    ') THEN
               WRITE(ANS,1611) FILL(1:16), FILL(1:16)

            ENDIF

         ENDIF

      ELSE IF (WHICH == 'ELAS' ) THEN
         WRITE(ANS,1112) FILL(1:16), (EID_OUT_ARRAY(II,1),OGEL(II,1),II=1,NUM)
         WRITE(ANS,1113) (MAX_ANS(JJ),JJ=1,1),(MIN_ANS(JJ),JJ=1,1),(ABS_ANS(JJ),JJ=1,1)

      ELSE IF (WHICH == 'BAR'  ) THEN
         DO II=1,NUM
            WRITE(ANS,1212) FILL(1:16), EID_OUT_ARRAY(II,1),(OGEL(II,JJ),JJ=1,8)
         ENDDO   
         WRITE(ANS,1213) (MAX_ANS(JJ),JJ=1,8),(MIN_ANS(JJ),JJ=1,8),(ABS_ANS(JJ),JJ=1,8)

      ELSE IF (WHICH == 'ROD'  ) THEN
         WRITE(ANS,1312) (FILL(1:16), EID_OUT_ARRAY(II,1), OGEL(II,7), OGEL(II,8),II=1,NUM)
         WRITE(ANS,1313) (MAX_ANS(JJ),JJ=7,8),(MIN_ANS(JJ),JJ=7,8),(ABS_ANS(JJ),JJ=7,8)

      ELSE IF (WHICH == 'SHEAR') THEN
         DO II=1,NUM
            WRITE(ANS,1412) FILL(1:16), EID_OUT_ARRAY(II,1),(OGEL(II,JJ),JJ=1,3)
         ENDDO   
         WRITE(ANS,1413) (MAX_ANS(JJ),JJ=1,8),(MIN_ANS(JJ),JJ=1,8),(ABS_ANS(JJ),JJ=1,3)

      ELSE IF (WHICH == 'SHELL') THEN
         DO II=1,NUM
            WRITE(ANS,1512) FILL(1:16), EID_OUT_ARRAY(II,1),(OGEL(II,JJ),JJ=1,NUM_TERMS)
         ENDDO   
         WRITE(ANS,1513) (MAX_ANS(JJ),JJ=1,8),(MIN_ANS(JJ),JJ=1,8),(ABS_ANS(JJ),JJ=1,NUM_TERMS)

      ELSE IF (WHICH == 'BUSH' ) THEN
         DO II=1,NUM
            WRITE(ANS,1612) FILL(1:16), EID_OUT_ARRAY(II,1),(OGEL(II,JJ),JJ=1,3)
         ENDDO   
         WRITE(ANS,1613) (MAX_ANS(JJ),JJ=1,6),(MIN_ANS(JJ),JJ=1,6),(ABS_ANS(JJ),JJ=1,6)

      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' OUTPUT FOR SUBCASE ',I8)

  102 FORMAT(' OUTPUT FOR EIGENVECTOR ',I8)

  103 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (boundary ',A,' for grid',I8,' component',I2,')')

  104 FORMAT(' OUTPUT FOR CRAIG-BAMPTON DOF ',I8,' OF ',I8,' (modal acceleration for mode ',I8,')')

  301 FORMAT(39X,A,'E L E M E N T   E N G I N E E R I N G   F O R C E S')

  302 FORMAT(33X,A,'C B   E L E M E N T   E N G I N E E R I N G   F O R C E   O T M')

  401 FORMAT(44X,A,'F O R   E L E M E N T   T Y P E   ',A11)

! ELAS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1111 FORMAT(2X,A,'Element     Force'                                                                                              &
          ,/,2X,A,'   ID')
 
 1112 FORMAT(A,5(I8,1ES14.6))
  
 1113 FORMAT(11X,'              -------------',/,                                                                                  &
             1X,'MAX (for output set):  ',1(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',1(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',1(ES14.6))

! BAR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1211 FORMAT(2X,A,'Element       Bend-Moment End A           Bend-Moment End B              - Shear -              Axial'          &
          ,'         Torque'  &
          ,/,2X,A,'   ID       Plane 1       Plane 2       Plane 1       Plane 2      Plane 1       Plane 2        Force')
 
 1212 FORMAT(A,I8,8(1ES14.6))

 1213 FORMAT(11X,'              ------------- ------------- ------------- ------------- ------------- ------------- -------------',&
                             ' -------------',/,                                                                                   &
             1X,'MAX (for output set):  ',8(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',8(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',8(ES14.6))

! ROD >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1311 FORMAT(2X,A,'Element     Axial        Torque'                                                                                &
          ,/,2X,A,'   ID       Force')
 
 1312 FORMAT(A,I8,1ES14.6,1ES14.6)
 
 1313 FORMAT(11X,'              ------------- -------------',/,                                                                    &
             1X,'MAX (for output set):  ',2(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',2(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',2(ES14.6))

! SHEAR >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1411 FORMAT(1X,A,'                                        Element        N o r m a l   F o r c e s'                               &
          ,/,1X,A,'                                           ID        Nxx           Nyy           Nxy')
 
 1412 FORMAT(A,40X,I8,3(1ES14.6))
 
 1413 FORMAT(1X,A,'                                                ------------- ------------- -------------',/,                   &
             1X,A,'                                        MAX* : ',3(ES14.6),/,                                                   &
             1X,A,'                                        MIN* : ',3(ES14.6),//,                                                  &
             1X,A,'                                        ABS* : ',3(ES14.6),/,                                                   &
             1X,A,'                                        *for output set')

! SHELL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1511 FORMAT(2X,A,'Element        N o r m a l   F o r c e s                       M o m e n t s'                                   &
          ,19X,'T r a n s v e r s e',/,A,95x,'S h e a r   F o r c e s'                                                             &
          ,/,2X,A,'   ID       Nxx           Nyy           Nxy           Mxx           Myy           Mxy            Qx          '  &
          ,'  Qy')

 1512 FORMAT(A,I8,8(1ES14.6))
 
 1513 FORMAT(11X,'              ------------- ------------- ------------- ------------- ------------- ------------- -------------',&
                             ' -------------',/,                                                                                   &
             1X,'MAX (for output set):  ',8(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',8(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',8(ES14.6))

! BUSH >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 1611 FORMAT(1X,A,' Element      Force         Force         Force         Force         Force         Force' &
          ,/,1X,A,'    ID         TX            TY            TZ            RX            RY            RZ')
 
 1612 FORMAT(A,I8,6(1ES14.6))
  
 1613 FORMAT(1X,A,'          ------------- ------------- ------------- ------------- ------------- ------------- ',/,              &
             1X,'MAX (for output set):  ',6(ES14.6),/,                                                                             &
             1X,'MIN (for output set):  ',6(ES14.6),//,                                                                            &
             1X,'ABS (for output set):  ',6(ES14.6))

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_ANS

! ##################################################################################################################################

      SUBROUTINE GET_MAX_MIN_ABS ( BEG_COL, END_COL )

      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: BEG_COL           ! Col number in OGEL where to beg for averaging to get max, min, abs
      INTEGER(LONG), INTENT(IN)       :: END_COL           ! Col number in OGEL where to end for averaging to get max, min, abs
      INTEGER(LONG)                   :: II,JJ             ! DO loop indices or counters

! **********************************************************************************************************************************
! Get MAX, MIN, ABS values

      DO JJ=BEG_COL,END_COL
         MAX_ANS(JJ) = -MACH_LARGE_NUM
      ENDDO 

      DO II=1,NUM
         DO JJ=BEG_COL,END_COL
            IF (OGEL(II,JJ) > MAX_ANS(JJ)) THEN
               MAX_ANS(JJ) = OGEL(II,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO JJ=BEG_COL,END_COL
         MIN_ANS(JJ) = MAX_ANS(JJ)
      ENDDO

      DO II=1,NUM
         DO JJ=BEG_COL,END_COL
            IF (OGEL(II,JJ) < MIN_ANS(JJ)) THEN
               MIN_ANS(JJ) = OGEL(II,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO II=BEG_COL,END_COL
         ABS_ANS(II) = MAX( DABS(MAX_ANS(II)), DABS(MIN_ANS(II)) )
      ENDDO

      END SUBROUTINE GET_MAX_MIN_ABS

      END SUBROUTINE WRITE_ELEM_ENGR_FORCE
