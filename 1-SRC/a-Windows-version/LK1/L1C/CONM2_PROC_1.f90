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
 
      SUBROUTINE CONM2_PROC_1
 
! CONM2 Processor #1
!-------------------
! In the MYSTRAN input Bulk Data File, RCONM2 was in a local coord system at the mass.
! This subr converts those values to the basic coord system at the mass (NOT at the grid). This is needed for the Grid Point Weight
! Generator. A later subr (CONM2_PROC_2) will convert the mass terms to the global coord system at the grid point. Array RCONM2 is
! overwritten with the values in basic coords at the mass point at the close of this subr.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1Y
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DATA_NAM_LEN, FATAL_ERR, MCMASS, MCONM2, MPMASS, MRCONM2, MRPMASS, NCMASS,  &
                                         NCONM2, NCORD, NGRID, NPMASS, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  CONM2_PROC_1_BEGEND
      USE MODEL_STUF, ONLY            :  CMASS, CONM2, PMASS, RCONM2, RPMASS, GRID, GRID_ID, CORD
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      USE CONM2_PROC_1_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CONM2_PROC_1'
      CHARACTER(1*BYTE)               :: CORD_FND          ! = 'Y' if coord sys ID on CONM2 defined, 'N' otherwise
      CHARACTER(LEN=DATA_NAM_LEN)     :: DATA_SET_NAME     ! A data set name for output purposes
      CHARACTER(1*BYTE)               :: GRID_FND          ! = 'Y' if grid ID on CONM2 defined, 'N' otherwise
      CHARACTER(8*BYTE), PARAMETER    :: NAME      = 'CONM2   '
 
      INTEGER(LONG)                   :: ACID              ! Actual (external) coordinate system ID
      INTEGER(LONG)                   :: AGRID             ! Actual grid number where CONM2 is located  
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number in array GRID_ID where AGRID is found
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: ICORD             ! Internal coordinate system ID
      INTEGER(LONG)                   :: IERROR    = 0     ! Error count
      INTEGER(LONG)                   :: NCOLA             ! No. cols in a matrix. For subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG)                   :: NCOLB             ! No. cols in a matrix. For subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG)                   :: NROWA             ! No. rows in a matrix. For subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG)                   :: NUM_COMPS         ! No. displ components (1 for SPOINT, 6 for actual grid)
      INTEGER(LONG)                   :: NUM_RCONM2_RESET  ! No. RCONM2's reset to zero because they are connected to SPOINT's
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CONM2_PROC_1_BEGEND
 
      REAL(DOUBLE)                    :: DX_0              ! Offset of mass from grid in basic coord sys X direction
      REAL(DOUBLE)                    :: DY_0              ! Offset of mass from grid in basic coord sys Y direction
      REAL(DOUBLE)                    :: DZ_0              ! Offset of mass from grid in basic coord sys Z direction
      REAL(DOUBLE)                    :: D_0(3)            ! Array containing DX_0, DY_0, DZ_0

      REAL(DOUBLE)                    :: DX_CID            ! Offset of mass from grid in coord sys ACID X direction
      REAL(DOUBLE)                    :: DY_CID            ! Offset of mass from grid in coord sys ACID Y direction 
      REAL(DOUBLE)                    :: DZ_CID            ! Offset of mass from grid in coord sys ACID Z direction 
      REAL(DOUBLE)                    :: D_CID(3)          ! Array containing DX_CID, DY_CID, DZ_CID

      REAL(DOUBLE)                    :: DUM33(3,3)        ! Intermediate return from subr MATMULT_FFF

      REAL(DOUBLE)                    :: IXX_M_0           ! X-X MOI about the mass c.g. in basic coord sys
      REAL(DOUBLE)                    :: IYX_M_0           ! Y-X MOI about the mass c.g. in basic coord sys
      REAL(DOUBLE)                    :: IYY_M_0           ! Y-Y MOI about the mass c.g. in basic coord sys
      REAL(DOUBLE)                    :: IZX_M_0           ! Z-X MOI about the mass c.g. in basic coord sys
      REAL(DOUBLE)                    :: IZY_M_0           ! Z-Y MOI about the mass c.g. in basic coord sys
      REAL(DOUBLE)                    :: IZZ_M_0           ! Z-Z MOI about the mass c.g. in basic coord sys

      REAL(DOUBLE)                    :: IXX_M_CID         ! X-X MOI about the mass c.g. in ACID  coord sys
      REAL(DOUBLE)                    :: IYX_M_CID         ! Y-X MOI about the mass c.g. in ACID  coord sys
      REAL(DOUBLE)                    :: IYY_M_CID         ! Y-Y MOI about the mass c.g. in ACID  coord sys
      REAL(DOUBLE)                    :: IZX_M_CID         ! Z-X MOI about the mass c.g. in ACID  coord sys
      REAL(DOUBLE)                    :: IZY_M_CID         ! Z-Y MOI about the mass c.g. in ACID  coord sys
      REAL(DOUBLE)                    :: IZZ_M_CID         ! Z-Z MOI about the mass c.g. in ACID  coord sys

      REAL(DOUBLE)                    :: MOI_M_0(3,3)      ! Array of above MOI's, POI's in basic coord sys 
      REAL(DOUBLE)                    :: MOI_M_CID(3,3)    ! Array of above MOI's, POI's in ACID  coord sys

      REAL(DOUBLE)                    :: MASS              ! Mass (or weight, depending on input units)
      REAL(DOUBLE)                    :: PHID, THETAD      ! Outputs from subr GEN_T0L
      REAL(DOUBLE)                    :: T_0_CID(3,3)      ! Transformation matrix from coord ACID to basic
      REAL(DOUBLE)                    :: T_CID_0(3,3)      ! Transformation matrix from basic to coord ACID (transp of T_0_CID)
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! For all SPOINT's reset all RCONM2 values back to zero (so that scalar points will not use offset and MOI terms)

      IERROR = 0
      NUM_RCONM2_RESET = 0
      DO I=1,NCONM2
         AGRID = CONM2(I,2)
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, GRID_ID_ROW_NUM )
         IF (GRID_ID_ROW_NUM == -1) THEN
            GRID_FND = 'N'                                 ! Grid ID on CONM2 undefined
            IERROR = IERROR + 1
            WRITE(ERR,1822) 'GRID ', AGRID, NAME, CONM2(I,1)
            WRITE(F06,1822) 'GRID ', AGRID, NAME, CONM2(I,1)
         ENDIF
         IF (IERROR == 0) THEN
            CALL GET_GRID_NUM_COMPS ( AGRID, NUM_COMPS, SUBR_NAME )
            IF (NUM_COMPS == 1) THEN
               NUM_RCONM2_RESET = NUM_RCONM2_RESET + 1
               DO J=2,MRCONM2                                 ! Keep mass but reset offsets amd MOI's to zero for SPOINT's
                  RCONM2(I,J) = ZERO
               ENDDO
            ENDIF
         ENDIF
      ENDDO

      IF (IERROR > 0) THEN
         WRITE(ERR,9996) SUBR_NAME, IERROR
         WRITE(F06,9996) SUBR_NAME, IERROR
         CALL OUTA_HERE ( 'Y' )                            ! Quit due to undefined grids
      ENDIF
 
      IF (NUM_RCONM2_RESET > 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,9901) NUM_RCONM2_RESET
         IF (SUPWARN == 'N') THEN
            WRITE(F06,9901) NUM_RCONM2_RESET
         ENDIF
      ENDIF

outer:DO I=1,NCONM2

         IF (DEBUG(15) == 1) CALL CONM2_PROC_1_DEB ( '1' )

! Get MASS, MOI's about c.g. of mass, and dists from c.g. to grid pt, from RCONM2 (in local coord sys CID):
 
         MASS      = RCONM2(I,1)
         DX_CID    = RCONM2(I,2)
         DY_CID    = RCONM2(I,3)
         DZ_CID    = RCONM2(I,4)
         IXX_M_CID = RCONM2(I,5)
         IYX_M_CID = RCONM2(I,6)
         IYY_M_CID = RCONM2(I,7)
         IZX_M_CID = RCONM2(I,8)
         IZY_M_CID = RCONM2(I,9)
         IZZ_M_CID = RCONM2(I,10)
 
! Put terms into matrices for use when we get MATMULT_FFF
 
         D_CID(1)       = DX_CID
         D_CID(2)       = DY_CID
         D_CID(3)       = DZ_CID
         MOI_M_CID(1,1) = IXX_M_CID
         MOI_M_CID(2,1) = IYX_M_CID
         MOI_M_CID(2,2) = IYY_M_CID
         MOI_M_CID(3,1) = IZX_M_CID
         MOI_M_CID(3,2) = IZY_M_CID
         MOI_M_CID(3,3) = IZZ_M_CID
         MOI_M_CID(1,2) = MOI_M_CID(2,1)
         MOI_M_CID(1,3) = MOI_M_CID(3,1)
         MOI_M_CID(2,3) = MOI_M_CID(3,2) 
 
! Get actual grid pt no. (AGRID) that this CONM2 is attached to and internal value for it (GRID_ID_ROW_NUM).

         GRID_FND = 'Y'
         AGRID = CONM2(I,2)
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, GRID_ID_ROW_NUM )
         IF (GRID_ID_ROW_NUM == -1) THEN
            GRID_FND = 'N'                                 ! Grid ID on CONM2 undefined
            IERROR = IERROR + 1
            WRITE(ERR,1822) 'GRID ', AGRID, NAME, CONM2(I,1)
            WRITE(F06,1822) 'GRID ', AGRID, NAME, CONM2(I,1)
         ENDIF
 
! The local system that CONM2 is defined in is ACID (or X). If not basic, transform from local to basic
! (princ local since no grid point is used on CONM card). Write error message if couldn't find coord system
 
         CORD_FND = 'N'
         ACID = CONM2(I,3)
         IF (ACID /= 0) THEN

j_loop1:    DO J=1,NCORD
               IF (ACID == CORD(J,2)) THEN
                  CORD_FND = 'Y'
                  ICORD = J
                  EXIT j_loop1
               ENDIF
            ENDDO j_loop1

            IF (CORD_FND == 'N') THEN                      ! Coord sys ID on CONM2 undefined
               IERROR = IERROR + 1
               WRITE(ERR,1822) 'COORD SYSTEM ', ACID, NAME, CONM2(I,1)
               WRITE(F06,1822) 'COORD SYSTEM ', ACID, NAME, CONM2(I,1)
            ENDIF

            IF (( CORD_FND == 'N') .OR. (GRID_FND == 'N')) THEN
               CYCLE outer                                 ! Can't continue (GRID_ID_ROW_NUM or ACID not found),
!                                                            so CYCLE and read next CONM2
            ENDIF
 
! Get transformation matrix from coord ACID to basic, T_0_CID
 
            CALL GEN_T0L ( GRID_ID_ROW_NUM, ICORD, THETAD, PHID, T_0_CID )

! Transpose T_0_CID to T_CID_0
 
            DO J=1,3
               DO K=1,3
                  T_CID_0(J,K) = T_0_CID(K,J)
               ENDDO
            ENDDO
    
! Transform D_CID to basic
 
            NROWA  = 3
            NCOLA  = 3
            NCOLB  = 1
            CALL MATMULT_FFF (T_0_CID, D_CID, NROWA, NCOLA, NCOLB, D_0 )
 
! Transform MOI's to basic
 
            NROWA  = 3
            NCOLA  = 3
            NCOLB  = 3
            CALL MATMULT_FFF ( MOI_M_CID, T_CID_0, NROWA, NCOLA, NCOLB, DUM33 )
            NROWA  = 3
            NCOLA  = 3
            NCOLB  = 3
            CALL MATMULT_FFF_T ( T_CID_0, DUM33, NROWA, NCOLA, NCOLB, MOI_M_0 )
         ELSE
            DO J=1,3
               D_0(J) = D_CID(J)
            ENDDO
            DO J=1,3
               DO K=1,3
                  MOI_M_0(J,K) = MOI_M_CID(J,K)
               ENDDO
            ENDDO   
         ENDIF
         DX_0    = D_0(1)
         DY_0    = D_0(2)
         DZ_0    = D_0(3)
         IXX_M_0 = MOI_M_0(1,1)
         IYX_M_0 = MOI_M_0(2,1)
         IYY_M_0 = MOI_M_0(2,2)
         IZX_M_0 = MOI_M_0(3,1)
         IZY_M_0 = MOI_M_0(3,2)
         IZZ_M_0 = MOI_M_0(3,3)
 
! Rewrite RCONM2 with values in basic coord system at the mass point 
 
         RCONM2(I, 1) = MASS
         RCONM2(I, 2) = DX_0
         RCONM2(I, 3) = DY_0
         RCONM2(I, 4) = DZ_0
         RCONM2(I, 5) = IXX_M_0
         RCONM2(I, 6) = IYX_M_0
         RCONM2(I, 7) = IYY_M_0
         RCONM2(I, 8) = IZX_M_0
         RCONM2(I, 9) = IZY_M_0
         RCONM2(I,10) = IZZ_M_0

         IF (CONM2(I,3) /= 0) THEN                         ! Write out mass data in basic if diff than local
            IF (DEBUG(15) == 1) THEN
               CALL CONM2_PROC_1_DEB ( '2' )
            ENDIF
         ELSE
            IF (DEBUG(15) == 1) THEN
               CALL CONM2_PROC_1_DEB ( '3' )
            ENDIF
         ENDIF

      ENDDO outer
 
      IF (IERROR > 0) THEN
         WRITE(ERR,9996) SUBR_NAME, IERROR
         WRITE(F06,9996) SUBR_NAME, IERROR
         CALL OUTA_HERE ( 'Y' )                            ! Quit due to undefined grid and coord sys ID's
      ENDIF
 
! Write mass data to L1Y.
 
      DATA_SET_NAME = 'CONM2, RCONM2'
      WRITE(L1Y) DATA_SET_NAME
      WRITE(L1Y) NCONM2
      DO I=1,NCONM2
         DO J=1,MCONM2
            WRITE(L1Y) CONM2(I,J)
         ENDDO 
         DO J=1,MRCONM2
            WRITE(L1Y) RCONM2(I,J)
         ENDDO
      ENDDO
 
      DATA_SET_NAME = 'CMASS, PMASS, RPMASS'
      WRITE(L1Y) DATA_SET_NAME
      WRITE(L1Y) NCMASS
      DO I=1,NCMASS
         DO J=1,MCMASS
            WRITE(L1Y) CMASS(I,J)
         ENDDO
      ENDDO
      WRITE(L1Y) NPMASS
      DO I=1,NPMASS 
         DO J=1,MPMASS
            WRITE(L1Y) PMASS(I,J)
         ENDDO
         DO J=1,MRPMASS
            WRITE(L1Y) RPMASS(I,J)
         ENDDO
      ENDDO
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1822 FORMAT(' *ERROR  1822: ',A,I8,' ON ',A,I8,' IS UNDEFINED')

 9901 FORMAT(' *WARNING    : CONM2 OFFSETS AND MOI''s FOR ',I8,' CONM2''s CONNECTED TO SPOINT''s HAVE BEEN RESET TO ZERO')

 9996 FORMAT(/,' PROCESSING ABORTED IN SUBROUTINE ',A,' DUE TO ABOVE ',I8,' ERRORS')

! **********************************************************************************************************************************
 
! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE CONM2_PROC_1_DEB ( WHICH )

      CHARACTER( 1*BYTE)              :: WHICH                  ! Decides what to print out for this call to this subr

! **********************************************************************************************************************************
      IF      (WHICH == '1') THEN

         WRITE(F06,1101)
         WRITE(F06,101)
         WRITE(F06,102) CONM2(I,1),CONM2(I,2),CONM2(I,3),(RCONM2(I,J),J=1,10)

      ELSE IF (WHICH == '2') THEN

         WRITE(F06,103) CONM2(I,1),CONM2(I,2),(RCONM2(I,J),J=1,10)
         WRITE(F06,*)
         WRITE(F06,3102)

      ELSE IF (WHICH == '3') THEN

         WRITE(F06,*)
         WRITE(F06,3102)

      ENDIF

! **********************************************************************************************************************************
 1101 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' :::::::::::::::::::::::::::::::::::::::START DEBUG(15) OUTPUT FROM SUBROUTINE CONM2_PROC_1::::::::::::::::::::::::',&
              ':::::::::::::::::',/)

  101 FORMAT(63X,'CONCENTRATED MASS DATA FROM SUBR CONM2_PROC_1',/,18X,                                                            &
'(VALUES ARE AT THE C.G. OF THE MASS AND ARE GIVEN FOR THE BASIC COORD SYSTEM AS WELL AS FOR THE LOCAL CONM2 COORD SYSTEM, IF ',   &
'DIFFERENT)',//,50X,                                                                                                               &
'O F F S E T   D I S T A N C E S',30X,'M O M E N T S   O F   I N E R T I A',/,                                                     &
'  CONM2 ID      GRID  CORD SYS      MASS           X             Y             Z            ',                                    &
'I11           I21           I22           I31           I32           I33')

  102 FORMAT(3I10,10(1ES14.6))

  103 FORMAT(2I10,'         0',10(1ES14.6))

 3102 FORMAT(' ::::::::::::::::::::::::::::::::::::::::END DEBUG(15) OUTPUT FROM SUBROUTINE CONM2_PROC_1:::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE CONM2_PROC_1_DEB

      END SUBROUTINE CONM2_PROC_1
