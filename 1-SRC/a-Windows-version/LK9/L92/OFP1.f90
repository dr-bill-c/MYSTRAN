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

      SUBROUTINE OFP1 ( JVEC, WHAT, SC_OUT_REQ, FEMAP_SET_ID, ITG, OT4_GROW )

! Processes grid point accel, displ and applied force output requests for one subcase.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, OT4
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, GROUT_ACCE_BIT, GROUT_DISP_BIT, GROUT_OLOA_BIT, IBIT, INT_SC_NUM,&
                                         MELGP, MOGEL, NGRID, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  OFP1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  OTMSKIP, POST
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  ANY_ACCE_OUTPUT, ANY_DISP_OUTPUT, ANY_OLOA_OUTPUT, GROUT, GRID, GRID_ID
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, MAXREQ, OGEL
      USE COL_VECS, ONLY              :  UG_COL, UG0_COL, PG_COL, PHIXG_COL, PHIXN_COL
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ACCE, OTM_DISP, TXT_ACCE, TXT_DISP
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  ACCE_OUT, DISP_OUT, OLOA_OUT

      USE OFP1_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'OFP1'
      CHARACTER(LEN=*) , INTENT(IN)   :: WHAT              ! Indicator whether to process displ or force output requests
      CHARACTER( 1*BYTE)              :: ACCE_ALL_SAME_CID ! Indicator of whether all grids, for the output set, have the same
!                                                            global coord sys for accel output
      CHARACTER( 1*BYTE)              :: DISP_ALL_SAME_CID ! Indicator of whether all grids, for the output set, have the same
!                                                            global coord sys for displacement output
      CHARACTER( 1*BYTE)              :: OLOAD_ALL_SAME_CID! Indicator of whether all grids, for the output set, have the same
!                                                            global coord sys for applied load output
      CHARACTER(31*BYTE)              :: OT4_DESCRIPTOR    ! Descriptor for rows of OT4 file
      CHARACTER( 1*BYTE), PARAMETER   :: IHDR      = 'Y'   ! An input to subr WRITE_GRD_PRT_OUTPUTS, called herein
      CHARACTER( 1*BYTE)              :: WRITE_OGEL(NGRID) ! 'Y'/'N' as to whether to write OGEL for a grid (used to avoid writing
!                                                            constr forces in subr WRITE_GRD_PRT_OUTPUTS for grids with no force

      INTEGER(LONG), INTENT(IN)       :: FEMAP_SET_ID      ! Set ID for FEMAP output
      INTEGER(LONG), INTENT(IN)       :: ITG               ! Unit number for text files for OTM row descriptors 
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: SC_OUT_REQ        ! If > 0, then req1uests for WHAT are to be output
      INTEGER(LONG), INTENT(INOUT)    :: OT4_GROW          ! Row number in OT4 file for grid related OTM descriptors
      INTEGER(LONG)                   :: G_SET_COL         ! Col number in TDOF where the G-set DOF's exist
      INTEGER(LONG)                   :: GDOF              ! G-set DOF number
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IB                ! If > 0, there are displ or applied load output requests
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IROW_FILE         ! Row number in text file
      INTEGER(LONG)                   :: IROW_MAT          ! Row number in OTM's
      INTEGER(LONG)                   :: NREQ              ! Number of user requested outputs of displ/force
      INTEGER(LONG)                   :: NUM               ! Count of the number of rows added to array OGEL
      INTEGER(LONG)                   :: NUM_COMPS         ! Either 6 or 1 depending on whether grid is a physical grid or a SPOINT
      INTEGER(LONG)                   :: ROW_NUM_START     ! DOF number where TDOF data begins for a grid
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OFP1_BEGEND
      INTEGER(LONG)                   :: TDOF_ROW          ! Row no. in array TDOF to find GDOF DOF number

      INTRINSIC IAND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,MAXREQ
         DO J=1,MOGEL
            OGEL(I,J) = ZERO
         ENDDO 
      ENDDO   
 
! Initialize WRITE_OGEL

      DO I=1,NGRID
         WRITE_OGEL(I) = 'Y'
      ENDDO

! ---------------------------------------------------------------------------------------------------------------------------------
! Process acceleration output requests for CB sol. 

      IF (WHAT == 'ACCE') THEN

         IROW_FILE = 0
         IROW_MAT  = 0
         OT4_DESCRIPTOR = 'Acceleration'

         ACCE_ALL_SAME_CID = 'Y'                           ! Check if all grids, for which there will be output, have same coord sys
         DO I=1,NGRID-1
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_ACCE_BIT))
            IF (IB > 0) THEN
               IF (GRID(I+1,3) /= GRID(I,3)) THEN
                  ACCE_ALL_SAME_CID = 'N'
                  EXIT
               ENDIF
           ENDIF
         ENDDO

         NREQ = 0                                          ! Count the number of requests.
         DO I=1,NGRID
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_ACCE_BIT))
            IF (IB > 0) THEN
               NREQ = NREQ + 1
            ENDIF
         ENDDO

         NUM  = 0
         DO I=1,NGRID                                      ! Prepare OGEL so subr WRITE_GRD_PRT_OUTPUTS can output requested items
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_ACCE_BIT))
            IF (IB > 0) THEN
               NUM = NUM + 1
               GID_OUT_ARRAY(NUM,1)       = GRID(I,1)
               GID_OUT_ARRAY(NUM,2)       = GRID(I,3)
               GID_OUT_ARRAY(NUM,MELGP+1) = GRID(I,5)
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_ID(I), IGRID )
               ROW_NUM_START = TDOF_ROW_START(IGRID)
               CALL GET_GRID_NUM_COMPS ( GRID_ID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
                  TDOF_ROW = ROW_NUM_START + J - 1
                  GDOF = TDOF(TDOF_ROW,G_SET_COL)
                  OGEL(NUM,J) = PHIXG_COL(GDOF)
                  IROW_FILE = IROW_FILE + 1
                  IROW_MAT  = IROW_MAT  + 1
                  OTM_ACCE(IROW_MAT,JVEC) = PHIXG_COL(GDOF)
                  IF (JVEC == 1) THEN
                     WRITE(TXT_ACCE(IROW_FILE), 9191) IROW_MAT, OT4_DESCRIPTOR, GRID(I,1), J
                  ENDIF
               ENDDO
               IF ((NUM == NREQ) .AND. (SC_OUT_REQ > 0)) THEN

                  IF ((ACCE_OUT(1:5) == 'PUNCH') .OR. (ACCE_OUT(1:4) == 'BOTH')) THEN
                     CALL WRITE_GRD_PCH_OUTPUTS ( JVEC, NUM, WHAT )
                  ENDIF

                  IF ((ACCE_OUT(1:5) == 'PRINT') .OR. (ACCE_OUT(1:4) == 'BOTH')) THEN
                     CALL WRITE_GRD_PRT_OUTPUTS ( JVEC, NUM, WHAT, IHDR, ACCE_ALL_SAME_CID, WRITE_OGEL )
                  ENDIF

                  EXIT

               ENDIF
               IF ((SOL_NAME(1:12) == 'GEN CB MODEL') .AND. (JVEC == 1) .AND. (IROW_MAT >= 1)) THEN
                  DO J=1,OTMSKIP                           ! Write OTMSKIP blank separator lines
                     IROW_FILE = IROW_FILE + 1
                     WRITE(TXT_ACCE(IROW_FILE), 9199)
                  ENDDO
               ENDIF
            ENDIF
         ENDDO


! ---------------------------------------------------------------------------------------------------------------------------------
! Process displacement output requests. 

      ELSE IF (WHAT == 'DISP') THEN
         IROW_FILE = 0
         IROW_MAT  = 0
         OT4_DESCRIPTOR = 'Displacement'

         DISP_ALL_SAME_CID = 'Y'                           ! Check if all grids, for which there will be output, have same coord sys
         DO I=1,NGRID-1
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_DISP_BIT))
            IF (IB > 0) THEN
               IF (GRID(I+1,3) /= GRID(I,3)) THEN
                  DISP_ALL_SAME_CID = 'N'
                  EXIT
               ENDIF
           ENDIF
         ENDDO

         NREQ = 0                                          ! Count the number of requests.
         DO I=1,NGRID
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_DISP_BIT))
            IF (IB > 0) THEN
               NREQ = NREQ + 1
            ENDIF
         ENDDO

         NUM  = 0
         DO I=1,NGRID                                      ! Prepare OGEL so subr WRITE_GRD_PRT_OUTPUTS can output requested items
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_DISP_BIT))
            IF (IB > 0) THEN
               NUM = NUM + 1
               GID_OUT_ARRAY(NUM,1)       = GRID(I,1)
               GID_OUT_ARRAY(NUM,2)       = GRID(I,3)
               GID_OUT_ARRAY(NUM,MELGP+1) = GRID(I,5)
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_ID(I), IGRID )
               ROW_NUM_START = TDOF_ROW_START(IGRID)
               CALL GET_GRID_NUM_COMPS ( GRID_ID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
                  TDOF_ROW = ROW_NUM_START + J - 1
                  GDOF = TDOF(TDOF_ROW,G_SET_COL)
                  OGEL(NUM,J) = UG_COL(GDOF)
                  IF (SOL_NAME(1:12) == 'GEN CB MODEL') THEN
                     IROW_FILE = IROW_FILE + 1
                     IROW_MAT = IROW_MAT + 1
                     OTM_DISP(IROW_MAT,JVEC) = UG_COL(GDOF)
                     IF (JVEC == 1) THEN
                        WRITE(TXT_DISP(IROW_FILE), 9191) IROW_MAT, OT4_DESCRIPTOR, GRID(I,1), J
                     ENDIF
                  ENDIF
               ENDDO
               IF ((NUM == NREQ) .AND. (SC_OUT_REQ > 0)) THEN

                  IF ((DISP_OUT(1:5) == 'PUNCH') .OR. (DISP_OUT(1:4) == 'BOTH')) THEN
                     CALL WRITE_GRD_PCH_OUTPUTS ( JVEC, NUM, WHAT )
                  ENDIF

                  IF ((DISP_OUT(1:5) == 'PRINT') .OR. (DISP_OUT(1:4) == 'BOTH')) THEN
                     CALL WRITE_GRD_PRT_OUTPUTS ( JVEC, NUM, WHAT, IHDR, DISP_ALL_SAME_CID, WRITE_OGEL )
                  ENDIF

                  EXIT

               ENDIF
               IF ((SOL_NAME(1:12) == 'GEN CB MODEL') .AND. (JVEC == 1) .AND. (IROW_MAT >= 1)) THEN
                  DO J=1,OTMSKIP                           ! Write OTMSKIP blank separator lines
                     IROW_FILE = IROW_FILE + 1
                     WRITE(TXT_DISP(IROW_FILE), 9199)
                  ENDDO
               ENDIF
            ENDIF
         ENDDO

         IF ((POST /= 0) .AND. (ANY_DISP_OUTPUT > 0)) THEN
            CALL WRITE_FEMAP_GRID_VECS ( UG_COL, FEMAP_SET_ID, 'DISP' )
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
! Process applied load output requests. 

      ELSE IF (WHAT == 'OLOAD') THEN
         OLOAD_ALL_SAME_CID = 'Y'                          ! Check if all grids, for which there will be output, have same coord sys
         DO I=1,NGRID-1                                    ! If not, then we won't write OLOAD output totals
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_OLOA_BIT))
            IF (IB > 0) THEN
               IF (GRID(I+1,3) /= GRID(I,3)) THEN
                  OLOAD_ALL_SAME_CID = 'N'
                  EXIT
               ENDIF
           ENDIF
         ENDDO

         NREQ = 0                                          ! Count the number of requests.
         DO I=1,NGRID
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_OLOA_BIT))
            IF (IB > 0) THEN
               NREQ = NREQ + 1
            ENDIF
         ENDDO   

         NUM = 0
         DO I=1,NGRID                                      ! 
            IB = IAND(GROUT(I,INT_SC_NUM),IBIT(GROUT_OLOA_BIT))
            IF (IB > 0) THEN
               NUM = NUM + 1
               GID_OUT_ARRAY(NUM,1)       = GRID(I,1)
               GID_OUT_ARRAY(NUM,2)       = GRID(I,3)
               GID_OUT_ARRAY(NUM,MELGP+1) = GRID(I,5)
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_ID(I), IGRID )
               ROW_NUM_START = TDOF_ROW_START(IGRID)
               CALL GET_GRID_NUM_COMPS ( GRID_ID(I), NUM_COMPS, SUBR_NAME )
               DO J=1,NUM_COMPS
                  CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
                  TDOF_ROW = ROW_NUM_START + J - 1
                  GDOF = TDOF(TDOF_ROW,G_SET_COL)
                  OGEL(NUM,J) = PG_COL(GDOF)
               ENDDO   
               WRITE_OGEL(NUM) = 'N'
               DO J=1,NUM_COMPS
                  IF (OGEL(NUM,J) /= ZERO) THEN
                     WRITE_OGEL(NUM) = 'Y'
                  ENDIF
               ENDDO

               IF (NUM == NREQ) THEN

                  IF ((OLOA_OUT(1:5) == 'PUNCH') .OR. (OLOA_OUT(1:4) == 'BOTH')) THEN
                     CALL WRITE_GRD_PCH_OUTPUTS ( JVEC, NUM, WHAT )
                  ENDIF

                  IF ((OLOA_OUT(1:5) == 'PRINT') .OR. (OLOA_OUT(1:4) == 'BOTH')) THEN
                     CALL WRITE_GRD_PRT_OUTPUTS ( JVEC, NUM, WHAT, IHDR, OLOAD_ALL_SAME_CID, WRITE_OGEL )
                  ENDIF

                  EXIT

               ENDIF
            ENDIF
         ENDDO

         IF ((POST /= 0) .AND. (ANY_OLOA_OUTPUT > 0)) THEN  ! No need to transform PG_COL to basic for FEMAP (handles it as-is)
            CALL WRITE_FEMAP_GRID_VECS ( PG_COL, FEMAP_SET_ID, 'OLOA' )
         ENDIF

! ---------------------------------------------------------------------------------------------------------------------------------
! Coding error: illegal input WHAT

      ELSE

         WRITE(ERR,9100) SUBR_NAME, WHAT   
         WRITE(F06,9100) SUBR_NAME, WHAT   
         FATAL_ERR = FATAL_ERR + 1
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
 9100 FORMAT(' *ERROR  9100: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ILLEGAL INPUT FOR VARIABLE "WHAT" = ',A)

 9191 FORMAT(I8,1X,A,I8,I8)

 9199 FORMAT(' ')




! **********************************************************************************************************************************

      END SUBROUTINE OFP1
