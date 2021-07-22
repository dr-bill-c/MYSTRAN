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
 
      SUBROUTINE OFP3_ELFE_1D ( JVEC, FEMAP_SET_ID, ITE, OT4_EROW )

! Processes element engr force output requests for 1D (ELAS, BUSH, ROD, BAR) elements for one subcase. Results go into array OGEL
! for later output in LINK9
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELOUT_ELFE_BIT, FATAL_ERR, IBIT, INT_SC_NUM, MBUG, MOGEL,&
                                         NELE, NCBAR, NCBUSH, NCELAS1, NCELAS2, NCELAS3, NCELAS4, NCROD, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  OFP3_ELFE_1D_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, HALF
      USE FEMAP_ARRAYS, ONLY          :  FEMAP_EL_NUMS, FEMAP_EL_VECS
      USE PARAMS, ONLY                :  OTMSKIP, POST
      USE MODEL_STUF, ONLY            :  ANY_ELFE_OUTPUT, BUSH_DXA, BUSH_DXB, BUSH_DY, BUSH_DZ, EDAT, ELAS_COMP, EPNT, ETYPE, EID, &
                                         ELMTYP, ELOUT, FCONV, METYPE, NUM_EMG_FATAL_ERRS, PEL, PLY_NUM, STRESS, TYPE, XEL
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, MAXREQ, OGEL
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ELFE, TXT_ELFE
  
      USE OFP3_ELFE_1D_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'OFP3_ELFE_1D'
      CHARACTER( 1*BYTE), PARAMETER   :: IHDR      = 'Y'   ! An input to subr WRITE_GRID_OUTPUTS, called herein
      CHARACTER(20*BYTE)              :: FORCE_ITEM(8)     ! Char description of element engineering forces
      CHARACTER( 1*BYTE)              :: OPT(6)            ! Option indicators for subr EMG, called herein
      CHARACTER(31*BYTE)              :: OT4_DESCRIPTOR    ! Descriptor for rows of OT4 file
      CHARACTER(30*BYTE)              :: REQUEST           ! Text for error message
 
      INTEGER(LONG), INTENT(IN)       :: FEMAP_SET_ID      ! Set ID for FEMAP output
      INTEGER(LONG), INTENT(IN)       :: ITE               ! Unit number for text files for OTM row descriptors 
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Solution vector number
      INTEGER(LONG), INTENT(INOUT)    :: OT4_EROW          ! Row number in OT4 file for elem related OTM descriptors
      INTEGER(LONG)                   :: ELOUT_ELFE        ! If > 0, there are ELFORCE(ENGR) requests for some elems                
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices
      INTEGER(LONG)                   :: IERROR       = 0  ! Local error count
      INTEGER(LONG)                   :: NELREQ(METYPE)    ! Count of the no. of requests for ELFORCE(NODE or ENGR) or STRESS
      INTEGER(LONG)                   :: NDUM              ! An arg passed to CALC_ELEM_STRESSES
      INTEGER(LONG)                   :: NUM_ELEM          ! No. elems processed prior to writing results to F06 file
      INTEGER(LONG)                   :: NUM_FROWS         ! No. elems processed for FEMAP
      INTEGER(LONG)                   :: NUM_OGEL          ! No. rows written to array OGEL prior to writing results to F06 file
!                                                            (this can be > NUM_ELEM since more than 1 row is written to OGEL
!                                                            for ELFORCE(NODE) - elem nodal forces)
                                                           ! Indicator for output of elem data to BUG file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OFP3_ELFE_1D_BEGEND
 
      REAL(DOUBLE)                    :: DUM0(6,12)        ! Intermediate matrix in a calc
      REAL(DOUBLE)                    :: DUM1(6)           ! Intermediate matrix in a calc
      REAL(DOUBLE)                    :: DXA               ! Offset dist of BUSH elem from end A in elem Xe dir.
      REAL(DOUBLE)                    :: DXB               ! Offset dist of BUSH elem from end B in elem Xe dir.=L - DXA
      REAL(DOUBLE)                    :: DY                ! Offset distance of BUSH elem from Xe axis in elem Ye direction
      REAL(DOUBLE)                    :: DZ                ! Offset distance of BUSH elem from Xe axis in elem Ze direction
      REAL(DOUBLE)                    :: FORCES(12)        ! Forces at the grid points
      REAL(DOUBLE)                    :: LENGTH

      ! OP2 parameters
      INTEGER(LONG)                   :: ITABLE            ! the op2 subtable number
      CHARACTER(8*BYTE)               :: TABLE_NAME        ! the op2 table name

      INTRINSIC IAND
  
! **********************************************************************************************************************************
!     Initialize
      TABLE_NAME = "OEF ERR "
      ITABLE = 0

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Process element engineering force requests for BAR, BUSH, ELAS, ROD. Use subr CALC_ELEM_NODE_FORCES and then convert the node
! forces to engineering forces (see equations below after subr CALC_ELEM_NODE_FORCES is called)
 
      OPT(1) = 'N'                                         ! OPT(1) is for calc of ME
      OPT(2) = 'Y'                                         ! OPT(2) is for calc of PTE
      OPT(3) = 'Y'                                         ! OPT(3) is for calc of SEi, STEi
      OPT(4) = 'Y'                                         ! OPT(4) is for calc of KE-linear
      OPT(5) = 'N'                                         ! OPT(5) is for calc of PPE
      OPT(6) = 'N'                                         ! OPT(6) is for calc of KE-diff stiff

      FORCE_ITEM(1) = 'M1a: Mom Plane1 EndA'
      FORCE_ITEM(2) = 'M1b: Mom Plane2 EndA'
      FORCE_ITEM(3) = 'M2a: Mom Plane1 EndB'
      FORCE_ITEM(4) = 'M2b: Mom Plane2 EndB'
      FORCE_ITEM(5) = 'V1 : Shear Plane1   '
      FORCE_ITEM(6) = 'V2 : Shear Plane2   ' 
      FORCE_ITEM(7) = 'FX : Axial force    '
      FORCE_ITEM(8) = 'T  : Torque         '

! Find out how many output requests were made for each element type.

      DO I=1,METYPE                                        ! Initialize the array containing the no. requests/elem.
         NELREQ(I) = 0
      ENDDO 
 
      DO I=1,METYPE
         DO J=1,NELE
            IF ((ETYPE(J)(1:3) == 'BAR') .OR. (ETYPE(J)(1:4) == 'BUSH') .OR. (ETYPE(J)(1:4) == 'ELAS') .OR.                        &
                (ETYPE(J)(1:3) == 'ROD'))THEN
               IF (ETYPE(J) == ELMTYP(I)) THEN
                  ELOUT_ELFE = IAND(ELOUT(J,INT_SC_NUM),IBIT(ELOUT_ELFE_BIT))
                  IF (ELOUT_ELFE > 0) THEN
                     NELREQ(I) = NELREQ(I) + 1
                   ENDIF
               ENDIF
            ENDIF
         ENDDO 
      ENDDO   

      DO I=1,MAXREQ
         DO J=1,MOGEL
            OGEL(I,J) = ZERO
         ENDDO 
      ENDDO   
 
      OT4_DESCRIPTOR = 'Element engineering force, ELFO'
reqs2:DO I=1,METYPE
         IF (NELREQ(I) == 0) CYCLE reqs2
         NUM_ELEM  = 0
         NUM_OGEL = 0
 
elems_2: DO J = 1,NELE
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF ((ETYPE(J)(1:3) == 'BAR') .OR. (ETYPE(J)(1:4) == 'BUSH') .OR. (ETYPE(J)(1:4) == 'ELAS') .OR.                        &
                (ETYPE(J)(1:3) == 'ROD'))THEN

               IF (ETYPE(J) == ELMTYP(I)) THEN
                  DO K=0,MBUG-1
                     WRT_BUG(K) = 0
                  ENDDO
                  ELOUT_ELFE = IAND(ELOUT(J,INT_SC_NUM),IBIT(ELOUT_ELFE_BIT))
                  IF (ELOUT_ELFE > 0) THEN

                     PLY_NUM = 0                           ! 'N' in call to EMG means do not write to BUG file
                     CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' )
                     IF (NUM_EMG_FATAL_ERRS > 0) THEN
                        IERROR = IERROR + 1
                        CYCLE elems_2
                     ENDIF
                     LENGTH = XEL(2,1)
                     CALL ELMDIS

                     CALL CALC_ELEM_NODE_FORCES            ! Use NODE to get engr forces (SE matrices don't have torque)
 
                     NUM_OGEL = NUM_OGEL + 1
                     IF (NUM_OGEL > MAXREQ) THEN
                        WRITE(ERR,9200) SUBR_NAME, MAXREQ
                        WRITE(F06,9200) SUBR_NAME, MAXREQ
                        FATAL_ERR = FATAL_ERR + 1
                        CALL OUTA_HERE ( 'Y' )             ! Coding error (dim of array OGEL too small), so quit
                     ENDIF   
                                                           ! Set engr forces based on the node force values
                     IF (ETYPE(J)(1:4) == 'ELAS') THEN
                        CALL ELEM_STRE_STRN_ARRAYS ( 1 )
                        NDUM = 0
                        CALL CALC_ELEM_STRESSES ( 1, NDUM, 0, 'N', 'N' )
                        IF (FCONV(1) > ZERO) THEN                  ! ELAS engr force is stress/FCONV
                           OGEL(NUM_OGEL,1) = STRESS(1)/FCONV(1)
                        ELSE
                           OGEL(NUM_OGEL,1) = ZERO
                        ENDIF
                     ENDIF

                     IF (ETYPE(J)(1:4) == 'BUSH') THEN

                       DXA = BUSH_DXA
                       DXB = BUSH_DXB
                       DY  = BUSH_DY
                       DZ  = BUSH_DZ

                       OGEL(NUM_OGEL,1) = PEL(7)
                       OGEL(NUM_OGEL,2) = PEL(8)
                       OGEL(NUM_OGEL,3) = PEL(9)
                       OGEL(NUM_OGEL,4) = PEL(8)*DZ - PEL(9)*DY  + PEL(10)
                       OGEL(NUM_OGEL,5) =-PEL(7)*DZ - PEL(9)*DXB + PEL(11)
                       OGEL(NUM_OGEL,6) = PEL(7)*DY + PEL(8)*DXB + PEL(12)

                     ENDIF

                     IF (ETYPE(J)(1:3) == 'ROD') THEN
                        OGEL(NUM_OGEL,7) = -PEL(1)                 ! Fx  (axial force for ROD)
                        OGEL(NUM_OGEL,8) = -PEL(4)                 ! T   (torque for ROD)
                     ENDIF

                     IF (ETYPE(J)(1:3) == 'BAR') THEN
                        OGEL(NUM_OGEL,1) = -PEL(6)                 ! M1a (bending moment, plane 1, end a for BAR)
                        OGEL(NUM_OGEL,2) =  PEL(5)                 ! M2a (bending moment, plane 2, end a for BAR)
                        OGEL(NUM_OGEL,3) = -PEL(6) + PEL(2)*LENGTH ! M1b (bending moment, plane 1, end b for BAR)
                        OGEL(NUM_OGEL,4) =  PEL(5) + PEL(3)*LENGTH ! M2b (bending moment, plane 2, end b for BAR)
                        OGEL(NUM_OGEL,5) = -PEL(2)                 ! V1  (plane 1 shear for BAR)
                        OGEL(NUM_OGEL,6) = -PEL(3)                 ! V2  (plane 2 shear for BAR)
                        OGEL(NUM_OGEL,7) = -PEL(1)                 ! Fx  (axial force for BAR)
                        OGEL(NUM_OGEL,8) = -PEL(4)                 ! T   (torque for BAR)
                     ENDIF

                     IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN

                        IF (ETYPE(J)(1:4) == 'ELAS') THEN
                           DO K=1,1                        ! ELAS has only 1 engr force
                              OT4_EROW = OT4_EROW + 1
                              OTM_ELFE(OT4_EROW,JVEC) = OGEL(NUM_OGEL,K)
                              IF (JVEC == 1) THEN
                                 WRITE(TXT_ELFE(OT4_EROW), 9192) OT4_EROW, OT4_DESCRIPTOR, TYPE, EID, FORCE_ITEM(K)
                              ENDIF
                           ENDDO
                        ENDIF

                        IF (ETYPE(J)(1:4) == 'BUSH') THEN
                           DO K=1,6
                              OT4_EROW = OT4_EROW + 1
                              OTM_ELFE(OT4_EROW,JVEC) = OGEL(NUM_OGEL,K)
                              IF (JVEC == 1) THEN
                                 WRITE(TXT_ELFE(OT4_EROW), 9192) OT4_EROW, OT4_DESCRIPTOR, TYPE, EID, FORCE_ITEM(K)
                              ENDIF
                           ENDDO
                        ENDIF

                        IF (ETYPE(J)(1:3) == 'ROD') THEN
                           DO K=7,8
                              OT4_EROW = OT4_EROW + 1
                              OTM_ELFE(OT4_EROW,JVEC) = OGEL(NUM_OGEL,K)
                              IF (JVEC == 1) THEN
                                 WRITE(TXT_ELFE(OT4_EROW), 9192) OT4_EROW, OT4_DESCRIPTOR, TYPE, EID, FORCE_ITEM(K)
                              ENDIF
                           ENDDO
                        ENDIF

                        IF (ETYPE(J)(1:3) == 'BAR') THEN
                           DO K=1,8
                              OT4_EROW = OT4_EROW + 1
                              OTM_ELFE(OT4_EROW,JVEC) = OGEL(NUM_OGEL,K)
                              IF (JVEC == 1) THEN
                                 WRITE(TXT_ELFE(OT4_EROW), 9192) OT4_EROW, OT4_DESCRIPTOR, TYPE, EID, FORCE_ITEM(K)
                              ENDIF
                           ENDDO
                        ENDIF

                        IF (ETYPE(J)(1:4) == 'BEAM') THEN
                           FATAL_ERR = FATAL_ERR + 1
                           WRITE(ERR,963) SUBR_NAME, ETYPE(J)
                           WRITE(ERR,963) SUBR_NAME, ETYPE(J)

                        ENDIF

                     ENDIF
 
                     IF ((SOL_NAME(1:12) == 'GEN CB MODEL') .AND. (JVEC == 1) .AND. (OT4_EROW >= 1)) THEN
                        DO K=1,OTMSKIP                     ! Write OTMSKIP blank separator lines
                           OT4_EROW = OT4_EROW + 1
                           WRITE(TXT_ELFE(OT4_EROW), 9199)
                        ENDDO
                     ENDIF

                     NUM_ELEM = NUM_ELEM + 1
                     EID_OUT_ARRAY(NUM_ELEM,1) = EID
                     IF (NUM_ELEM == NELREQ(I)) THEN
                        CALL CHK_OGEL_ZEROS ( NUM_OGEL )

 100                    FORMAT("*DEBUG:      ",A,"; ELEMENT_TYPE=",A,"; TABLE_NAME=",A,"; ITABLE=",I8)
                        WRITE(ERR,100) "F1",ETYPE(J),TABLE_NAME,ITABLE
                        CALL SET_OEF_TABLE_NAME(ETYPE(J), TABLE_NAME, ITABLE)
                        WRITE(ERR,100) "F2",ETYPE(J),TABLE_NAME,ITABLE
                        CALL WRITE_ELEM_ENGR_FORCE ( JVEC, NUM_ELEM, IHDR, ITABLE )
                        EXIT
                     ENDIF
 
                  ENDIF
 
               ENDIF

            ENDIF
 
         ENDDO elems_2
 
      ENDDO reqs2
 10   FORMAT("*DEBUG:      OEF_END 1D:    TABLE_NAME",A)
      WRITE(ERR,10) TABLE_NAME
      IF ((TABLE_NAME .NE. "OEF ERR ") .AND. (ITABLE < 0)) THEN
        CALL END_OP2_TABLE(ITABLE)
      ENDIF

      IF ((POST /= 0) .AND. (ANY_ELFE_OUTPUT > 0)) THEN

         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCBAR, 8, SUBR_NAME )
         DO J=1,NELE                                       ! Write out BAR engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:3) == 'BAR') THEN
               NUM_FROWS= NUM_FROWS+ 1
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               LENGTH = XEL(2,1)
               CALL ELMDIS
               CALL CALC_ELEM_NODE_FORCES
               FEMAP_EL_VECS(NUM_FROWS,1) = -PEL(6)                 ! M1a (bending moment, plane 1, end a for BAR)
               FEMAP_EL_VECS(NUM_FROWS,2) = -PEL(6) + PEL(2)*LENGTH ! M1b (bending moment, plane 1, end b for BAR)
               FEMAP_EL_VECS(NUM_FROWS,3) =  PEL(5)                 ! M2a (bending moment, plane 2, end a for BAR)
               FEMAP_EL_VECS(NUM_FROWS,4) =  PEL(5) + PEL(3)*LENGTH ! M2b (bending moment, plane 2, end b for BAR)
               FEMAP_EL_VECS(NUM_FROWS,5) = -PEL(2)                 ! V1  (plane 1 shear for BAR)
               FEMAP_EL_VECS(NUM_FROWS,6) = -PEL(3)                 ! V2  (plane 2 shear for BAR)
               FEMAP_EL_VECS(NUM_FROWS,7) = -PEL(1)                 ! Fx  (axial force for BAR or ROD)
               FEMAP_EL_VECS(NUM_FROWS,8) = -PEL(4)                 ! T   (torque for BAR or ROD)
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'BAR     ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCBUSH, 6, SUBR_NAME )
         DO J=1,NELE                                       ! Write out BUSH engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:4) == 'BUSH') THEN
               NUM_FROWS= NUM_FROWS+ 1
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL CALC_ELEM_NODE_FORCES
               FEMAP_EL_VECS(NUM_FROWS,1) =  PEL(7)                            !  Prior to 2021-06-09 was = -PEL(1)
               FEMAP_EL_VECS(NUM_FROWS,2) =  PEL(8)                            !  Prior to 2021-06-09 was = -PEL(2)
               FEMAP_EL_VECS(NUM_FROWS,3) =  PEL(9)                            !  Prior to 2021-06-09 was = -PEL(3)
               FEMAP_EL_VECS(NUM_FROWS,4) =  PEL(8)*DZ - PEL(9)*DY  + PEL(10)  !  Prior to 2021-06-09 was = -PEL(4)
               FEMAP_EL_VECS(NUM_FROWS,5) = -PEL(7)*DZ - PEL(9)*DXB + PEL(11)  !  Prior to 2021-06-09 was = -PEL(5)
               FEMAP_EL_VECS(NUM_FROWS,6) =  PEL(7)*DY + PEL(8)*DXB + PEL(12)  !  Prior to 2021-06-09 was = -PEL(6)
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'BUSH    ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCROD, 8, SUBR_NAME )
         DO J=1,NELE                                       ! Write out ROD engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:3) == 'ROD') THEN
               NUM_FROWS= NUM_FROWS+ 1
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL CALC_ELEM_NODE_FORCES
               FEMAP_EL_VECS(NUM_FROWS,7) = -PEL(1)                 ! Fx  (axial force for BAR or ROD)
               FEMAP_EL_VECS(NUM_FROWS,8) = -PEL(4)                 ! T   (torque for BAR or ROD)
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'ROD     ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

! For ELAS we need to calculate elem engr forces from the stresses since there is no "local" elem coord system

         NDUM = 0
         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCELAS1, 2, SUBR_NAME )
         DO J=1,NELE                                       ! Write out ELAS1 engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:5) == 'ELAS1') THEN
               NUM_FROWS= NUM_FROWS+ 1
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               CALL CALC_ELEM_STRESSES ( NCELAS1, NDUM, NUM_FROWS, 'N', 'Y' )
               IF (FCONV(1) > 0.D0) THEN
                  FEMAP_EL_VECS(NUM_FROWS,1) = STRESS(1)/FCONV(1)
               ELSE
                  
               ENDIF
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'ELAS1   ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NDUM = 0
         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCELAS2, 2, SUBR_NAME )
         DO J=1,NELE                                       ! Write out ELAS2 engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:5) == 'ELAS2') THEN
               NUM_FROWS= NUM_FROWS+ 1
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               CALL CALC_ELEM_STRESSES ( NCELAS2, NDUM, NUM_FROWS, 'N', 'Y' )
               IF (FCONV(1) > 0.D0) THEN
                  FEMAP_EL_VECS(NUM_FROWS,1) = STRESS(1)/FCONV(1)
               ELSE
                  
               ENDIF
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'ELAS2   ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NDUM = 0
         NUM_FROWS= 0
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCELAS3, 2, SUBR_NAME )
         DO J=1,NELE                                       ! Write out ELAS3 engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:5) == 'ELAS3') THEN
               NUM_FROWS= NUM_FROWS+ 1
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               CALL CALC_ELEM_STRESSES ( NCELAS3, NDUM, NUM_FROWS, 'N', 'Y' )
               IF (FCONV(1) > 0.D0) THEN
                  FEMAP_EL_VECS(NUM_FROWS,1) = STRESS(1)/FCONV(1)
               ELSE
                  
               ENDIF
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'ELAS3   ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

         NDUM = 0
         NUM_FROWS= 0                                      ! 'N' in call to EMG means do not write to BUG file
         CALL ALLOCATE_FEMAP_DATA ( 'FEMAP ELEM ARRAYS', NCELAS4, 2, SUBR_NAME )
         DO J=1,NELE                                       ! Write out ELAS4 engineering forces
            EID   = EDAT(EPNT(J))
            TYPE  = ETYPE(J)
            IF (ETYPE(J)(1:5) == 'ELAS4') THEN
               NUM_FROWS= NUM_FROWS+ 1
               DO K=0,MBUG-1
                  WRT_BUG(K) = 0
               ENDDO
               PLY_NUM = 0
               CALL EMG ( J   , OPT, 'N', SUBR_NAME, 'N' ) ! 'N' in call to EMG means do not write to BUG file
               FEMAP_EL_NUMS(NUM_FROWS,1) = EID
               IF (NUM_EMG_FATAL_ERRS > 0) THEN
                  IERROR = IERROR + 1
                  CYCLE
               ENDIF
               CALL ELMDIS
               CALL ELEM_STRE_STRN_ARRAYS ( 1 )
               CALL CALC_ELEM_STRESSES ( NCELAS4, NDUM, NUM_FROWS, 'N', 'Y' )
               IF (FCONV(1) > 0.D0) THEN
                  FEMAP_EL_VECS(NUM_FROWS,1) = STRESS(1)/FCONV(1)
               ELSE
                  
               ENDIF
            ENDIF            
         ENDDO
         IF (NUM_FROWS > 0) THEN
            CALL WRITE_FEMAP_ELFO_VECS ( 'ELAS4   ', NUM_FROWS, FEMAP_SET_ID )
         ENDIF
         CALL DEALLOCATE_FEMAP_DATA

      ENDIF

      IF (IERROR > 0) THEN
         REQUEST = 'ELEMENT ENGINEERING FORCE'
         WRITE(ERR,9201) TYPE, REQUEST, EID
         WRITE(F06,9201) TYPE, REQUEST, EID
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  963 FORMAT(' *ERROR   946: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NO CODE FOR 1D ELEMENT TYPE "',A,'"')

 9192 FORMAT(I8,1X,A,A8,I8,4X,A20)

 9199 FORMAT(' ')

 9200 FORMAT(' *ERROR  9200: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ARRAY OGEL WAS ALLOCATED TO HAVE ',I12,' ROWS. ATTEMPT TO WRITE TO OGEL BEYOND THIS')
 
 9201 FORMAT(' *ERROR  9201: DUE TO ABOVE LISTED ERRORS, CANNOT CALCULATE ',A,' REQUESTS FOR ',A,' ELEMENT ID = ',I8)

! **********************************************************************************************************************************

      END SUBROUTINE OFP3_ELFE_1D
