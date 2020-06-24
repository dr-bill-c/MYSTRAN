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
  
      SUBROUTINE WRITE_PLY_STRESSES ( JSUB, NUM, IHDR )
  
! Writes blocks of element ply stresses for one subcase one element type for elements with PCOMP properties.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, BARTOR, INT_SC_NUM, LPCOMP_PLIES, NDOFR, NUM_CB_DOFS,            &
                                         SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_PLY_STRESSES_BEGEND
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, FTNAME, OGEL
      USE MODEL_STUF, ONLY            :  ANY_FAILURE_THEORY, ELEM_ONAME, LABEL, PCOMP, SCNUM, STITLE, TITLE
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  STRE_OPT
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  
  
      USE WRITE_PLY_STRESSES_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_PLY_STRESSES'
      CHARACTER(LEN=*), INTENT(IN)    :: IHDR              ! Indicator of whether to write an output header
      CHARACTER(128*BYTE)             :: FILL              ! Padding for output format
      CHARACTER(LEN=LEN(ELEM_ONAME))  :: ONAME             ! Element name to write out in F06 file
  
      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG)                   :: BDY_COMP          ! Component (1-6) for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_GRID          ! Grid for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_DOF_NUM       ! DOF number for BDY_GRID/BDY_COMP
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_PLY_STRESSES_BEGEND
  
      REAL(DOUBLE)                    :: ABS_ANS(10)       ! Max ABS for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MAX_ANS(10)       ! Max for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MIN_ANS(10)       ! Min for all grids output for each of the 6 disp components

      INTRINSIC                       :: MAX, MIN, DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      FILL(1:) = ' '

      DO J=1,10
         ABS_ANS(J) = ZERO
         MAX_ANS(J) = -MACH_LARGE_NUM 
         MIN_ANS(J) = ZERO
      ENDDO 

! Get element output name
 
      ONAME(1:) = ' '
      CALL GET_ELEM_ONAME ( ONAME )

! Write output headers if this is not the first use of this subr.

      IF (IHDR == 'Y') THEN

! -- F06 header: OUTPUT FOR SUBCASE, EIGENVECTOR or CRAIG-BAMPTON DOF

         WRITE(F06,*)
         WRITE(F06,*)
         IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN

            WRITE(F06,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN

            WRITE(F06,101) SCNUM(JSUB)

         ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN

            WRITE(F06,101) JSUB

         ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

            WRITE(F06,102) JSUB

         ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! Write info on what CB DOF the output is for

            IF ((JSUB <= NDOFR) .OR. (JSUB >= NDOFR+NUM_CB_DOFS)) THEN 
               IF (JSUB <= NDOFR) THEN
                  BDY_DOF_NUM = JSUB
               ELSE
                  BDY_DOF_NUM = JSUB-(NDOFR+NUM_CB_DOFS)
               ENDIF
               CALL GET_GRID_AND_COMP ( 'R ', BDY_DOF_NUM, BDY_GRID, BDY_COMP  )
            ENDIF

            IF       (JSUB <= NDOFR) THEN
               WRITE(F06,103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
            ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NUM_CB_DOFS)) THEN
               WRITE(F06,104) JSUB, NUM_CB_DOFS, JSUB-NDOFR
            ELSE
               WRITE(F06,103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
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

         WRITE(F06,*)

! -- F06 1st 2 header lines for stress output description

         IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
            WRITE(F06,302) FILL(1: 0)
         ELSE
            WRITE(F06,301)  FILL(1: 0)
         ENDIF
         IF (ANY_FAILURE_THEORY == 'N') THEN
            IF (STRE_OPT == 'VONMISES') THEN
               WRITE(F06,1401) FILL(1: 0), ONAME, FILL(1: 0), FILL(1: 0)
               WRITE(F06,1499) FILL(1: 0), FILL(1: 0), FILL(1: 0), FILL(1: 0)
            ELSE
               WRITE(F06,1402) FILL(1: 0), ONAME, FILL(1: 0), FILL(1: 0)
               WRITE(F06,1499) FILL(1: 0), FILL(1: 0), FILL(1: 0), FILL(1: 0)
            ENDIF
         ELSE
            IF (STRE_OPT == 'VONMISES') THEN
               WRITE(F06,1403) FILL(1: 0), ONAME, FILL(1: 0), FILL(1: 0)
               WRITE(F06,1499) FILL(1: 0), FILL(1: 0), FILL(1: 0), FILL(1: 0)
            ELSE
               WRITE(F06,1404) FILL(1: 0), ONAME, FILL(1: 0), FILL(1: 0)
               WRITE(F06,1499) FILL(1: 0), FILL(1: 0), FILL(1: 0), FILL(1: 0)
            ENDIF
         ENDIF

         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,*)
            WRITE(ANS,*)
            IF    ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN

               WRITE(ANS,101) SCNUM(JSUB)

            ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN

               WRITE(ANS,101) SCNUM(JSUB)

            ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN

               WRITE(ANS,101) JSUB

            ELSE IF (SOL_NAME(1:5) == 'MODES') THEN

               WRITE(ANS,102) JSUB

            ELSE IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN   ! Write info on what CB DOF the output is for

               IF ((JSUB <= NDOFR) .OR. (JSUB >= NDOFR+NUM_CB_DOFS)) THEN 
                  IF (JSUB <= NDOFR) THEN
                     BDY_DOF_NUM = JSUB
                  ELSE
                     BDY_DOF_NUM = JSUB-(NDOFR+NUM_CB_DOFS)
                  ENDIF
                  CALL GET_GRID_AND_COMP ( 'R ', BDY_DOF_NUM, BDY_GRID, BDY_COMP  )
               ENDIF

               IF       (JSUB <= NDOFR) THEN
                  WRITE(ANS,103) JSUB, NUM_CB_DOFS, 'acceleration', BDY_GRID, BDY_COMP
               ELSE IF ((JSUB > NDOFR) .AND. (JSUB <= NDOFR+NUM_CB_DOFS)) THEN
                  WRITE(ANS,104) JSUB, NUM_CB_DOFS, JSUB-NDOFR
               ELSE
                  WRITE(ANS,103) JSUB, NUM_CB_DOFS, 'displacement', BDY_GRID, BDY_COMP
               ENDIF

            ENDIF

            WRITE(ANS,*)

            IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
               WRITE(ANS,302) FILL(1:16)
            ELSE
               WRITE(ANS,301) FILL(1:16)
            ENDIF
            IF (ANY_FAILURE_THEORY == 'N') THEN
               IF (STRE_OPT == 'VONMISES') THEN
                  WRITE(ANS,1401) FILL(1:16), ONAME, FILL(1: 0), FILL(1: 0)
                  WRITE(F06,1499) FILL(1: 0), FILL(1: 0), FILL(1: 0), FILL(1: 0)
               ELSE
                  WRITE(ANS,1402) FILL(1:16), ONAME, FILL(1: 0), FILL(1: 0)
                  WRITE(F06,1499) FILL(1: 0), FILL(1: 0), FILL(1: 0), FILL(1: 0)
               ENDIF
            ELSE
               IF (STRE_OPT == 'VONMISES') THEN
                  WRITE(ANS,1403) FILL(1:16), ONAME, FILL(1: 0), FILL(1: 0)
                  WRITE(F06,1499) FILL(1: 0), FILL(1: 0), FILL(1: 0), FILL(1: 0)
               ELSE
                  WRITE(ANS,1404) FILL(1:16), ONAME, FILL(1: 0), FILL(1: 0)
                  WRITE(F06,1499) FILL(1: 0), FILL(1: 0), FILL(1: 0), FILL(1: 0)
               ENDIF
            ENDIF

         ENDIF
 
      ENDIF
 
! Write the element stress output
  
      DO I=1,NUM

         IF (EID_OUT_ARRAY(I,2) == 1) THEN
            WRITE(F06,*)
            IF (ANY_FAILURE_THEORY == 'Y') THEN
               IF (FTNAME(I) /= 'None') THEN
                  WRITE(F06,1405) FILL(1: 0), EID_OUT_ARRAY(I,1), EID_OUT_ARRAY(I,2), (OGEL(I,J),J=1,10), FTNAME(I)
               ELSE
                  WRITE(F06,1406) FILL(1: 0), EID_OUT_ARRAY(I,1), EID_OUT_ARRAY(I,2), (OGEL(I,J),J=1,9), FTNAME(I)
               ENDIF
            ELSE
               WRITE(F06,1406) FILL(1: 0), EID_OUT_ARRAY(I,1), EID_OUT_ARRAY(I,2), (OGEL(I,J),J=1,9)
            ENDIF
         ELSE
            IF (ANY_FAILURE_THEORY == 'Y') THEN
               IF (FTNAME(I) /= 'None') THEN
                  WRITE(F06,1407) FILL(1: 0), EID_OUT_ARRAY(I,2), (OGEL(I,J),J=1,10)
               ELSE
                  WRITE(F06,1408) FILL(1: 0), EID_OUT_ARRAY(I,2), (OGEL(I,J),J=1,9)
               ENDIF
            ELSE
               WRITE(F06,1408) FILL(1: 0), EID_OUT_ARRAY(I,2), (OGEL(I,J),J=1,9)
            ENDIF
         ENDIF
         IF (DEBUG(200) > 0) THEN
            IF (ANY_FAILURE_THEORY == 'Y') THEN
               WRITE(ANS,1416) EID_OUT_ARRAY(I,1), EID_OUT_ARRAY(I,2), (OGEL(I,J),J=1,9)
            ELSE
               WRITE(ANS,1416) EID_OUT_ARRAY(I,1), EID_OUT_ARRAY(I,2), (OGEL(I,J),J=1,9)
            ENDIF
         ENDIF

      ENDDO

      DO I=1,NUM

         DO J=1,10
            IF (OGEL(I,J) > MAX_ANS(J)) THEN
               MAX_ANS(J) = OGEL(I,J)
            ENDIF
         ENDDO

      ENDDO

      DO J=1,10
         MIN_ANS(J) = MAX_ANS(J)
      ENDDO

      DO I=1,NUM

         DO J=1,10
            IF (OGEL(I,J) < MIN_ANS(J)) THEN
               MIN_ANS(J) = OGEL(I,J)
            ENDIF
         ENDDO

      ENDDO

      DO I=1,10                                          ! Get absolute max stresses
         ABS_ANS(I) = MAX( DABS(MAX_ANS(I)), DABS(MIN_ANS(I)) )
      ENDDO

      WRITE(F06,1409) FILL(1: 0), FILL(1 :0), (MAX_ANS(I),I=1,10),                                                                 &
                      FILL(1: 0)            , (MIN_ANS(I),I=1,10),                                                                 &
                      FILL(1: 0)            , (ABS_ANS(I),I=1,10), FILL(1: 0)

      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,1419) (MAX_ANS(I),I=1,10), (MIN_ANS(I),I=1,10), (ABS_ANS(I),I=1,10)
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

  301 FORMAT(45X,A,'S T R E S S E S   I N   L A Y E R E D   C O M P O S I T E   E L E M E N T S')

  302 FORMAT(9X,A,'C B   E L E M E N T   S T R E S S   O T M   I N   L O C A L   E L E M E N T   C O O R D I N A T E   S Y S T E M')
  
 1401 FORMAT(53X,A,'F O R   E L E M E N T   T Y P E   ',A11,/,                                                                     &
      1X,A,' Element   Ply  Stresses in fiber and matrix directions    Inter-laminar stresses     Principal stresses (zero shear)',&
           '      von',/,                                                                                                          &
      1X,A,'    ID     Num     Normal-1     Normal-2     Shear-12      Shear-13      Shear-23      Angle     Major        Minor',  &
           '       Mises',//)

 1402 FORMAT(53X,A,'F O R   E L E M E N T   T Y P E   ',A11,/,                                                                     &
      1X,A,' Element   Ply  Stresses in fiber and matrix directions    Inter-laminar stresses     Principal stresses (zero shear)',&
           '      Max',/,                                                                                                          &
      1X,A,'    ID     Num     Normal-1     Normal-2     Shear-12      Shear-13      Shear-23      Angle     Major        Minor',  &
           '       Shear',//)

 1403 FORMAT(53X,A,'F O R   E L E M E N T   T Y P E   ',A11,/,                                                                     &
      1X,A,' Element   Ply  Stresses in fiber and matrix directions    Inter-laminar stresses     Principal stresses (zero shear)',&
           '      von          Failure',/,                                                                                         &
      1X,A,'    ID     Num     Normal-1     Normal-2     Shear-12      Shear-13      Shear-23      Angle     Major        Minor',  &
           '       Mises      Index   Theory',//)

 1404 FORMAT(53X,A,'F O R   E L E M E N T   T Y P E   ',A11,/,                                                                     &
      1X,A,' Element   Ply  Stresses in fiber and matrix directions    Inter-laminar stresses     Principal stresses (zero shear)',&
           '      Max          Failure',/,                                                                                         &
      1X,A,'    ID     Num     Normal-1     Normal-2     Shear-12      Shear-13      Shear-23      Angle     Major        Minor',  &
           '       Shear      Index   Theory',//)

 1499 FORMAT(15X,                                                                                                                  &
 '--------------------------------------------------------------------------------------------------------------------' ,/,14X,A,  &
'| N O T E:  TRANSVERSE SHEAR STRESSES IN ALL PLIES ARE BASED ON THE TRANSVERSE SHEAR STRAINS AND PARAMETER PCMPTSTM. |',/,14X,A,  &
'|                     THIS DOES NOT TAKE INTO CONSIDERATION ANY THEORETICAL (e.g. PARABOLIC) DISTRIBUTION            |',/,14X,A,  &
'|                                       THAT MAY EXIST FOR THE TRANSVERSE SHEAR STRESSES                             |',/,15X,A,  &
 '--------------------------------------------------------------------------------------------------------------------')

!5001 FORMAT(1X,A,I8,I6,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),12X,A)
 1406 FORMAT(1X,A,I8,I6,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),12X,A)

!5002 FORMAT(1X,A,8X,I6,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5))
 1408 FORMAT(1X,A,8X,I6,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5))

!5003 FORMAT(1X,A,I8,I6,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),1ES10.2,2X,A)
 1405 FORMAT(1X,A,I8,I6,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),1ES10.2,2X,A)

!5004 FORMAT(1X,A,8X,I6,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),1ES10.2,2X)
 1407 FORMAT(1X,A,8X,I6,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),1ES10.2,2X)

 1409 FORMAT(14X,A,'  ------------ ------------ ------------    ------------  ------------  ------- ------------ ------------',    &
                 ' ------------ ---------',/,                                                                                      &
             1X,A,'MAX* :  ',6X,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),1ES10.2,/,                                              &
             1X,A,'MIN* :  ',6X,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),1ES10.2,//,                                             &
             1X,A,'ABS* :  ',6X,3(1ES13.5),2X,2(1ES14.5),0PF9.3,3(1ES13.5),1ES10.2,/,                                              &
             1X,A,'*for output set')

!5011 FORMAT(11X,I8,I5,5(1ES14.6),0PF14.3,3(1ES14.6),1ES14.6)
 1416 FORMAT(11X,I8,I5,5(1ES14.6),0PF14.3,3(1ES14.6),1ES14.6)

 1419 FORMAT(30X,'------------- ------------- ------------- ------------- ------------- ------------- ------------- -------------',&
                ' ------------- -------------',/,&
             1X,'MAX (for output set): ',1X,5(ES14.6),0PF14.3,4(ES14.6),/,                                                         &
             1X,'MIN (for output set): ',1X,5(ES14.6),0PF14.3,4(ES14.6),//,                                                        &
             1X,'ABS (for output set): ',1X,5(ES14.6),0PF14.3,4(ES14.6))

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_PLY_STRESSES
