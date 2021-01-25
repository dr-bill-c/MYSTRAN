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
 
      SUBROUTINE ELMGM1 ( INT_ELEM_ID, WRITE_WARN )
 
! Calculates and checks some elem geometry for ROD, BAR, BEAM, triangles and provides a transformation matrix (TE) to transform the
! element stiffness matrix in the element system to the basic coordinate system. Calculates grid point coords in local coord system.
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MELGP, MOFFSET, NCORD, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMGM1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  BGRID, BUSH_CID, BUSH_OCID, CAN_ELEM_TYPE_OFFSET, CORD, EID, ELEM_LEN_12, ELEM_LEN_AB,    &
                                         ELGP, NUM_EMG_FATAL_ERRS, EOFF, GRID, OFFDIS, RCORD, TE, TE_IDENT, TYPE, XEB, XEL
 
      USE ELMGM1_USE_IFs

      IMPLICIT NONE
 
      CHARACTER( 1*BYTE)              :: ID(3)              ! Used in deciding whether TE_IDENT = 'Y' or 'N'
      CHARACTER( 5*BYTE)              :: SORT_ORDER         ! Order in which the VX(i) have been sorted in subr CALC_VEC_SORT_ORDER
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMGM1'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN         ! If 'Y" write warning messages, otherwise do not
      CHARACTER(1*BYTE)               :: CORD_FND           ! = 'Y' if coord sys ID on CONM2 defined, 'N' otherwise
      CHARACTER(1*BYTE)               :: DO_IT              ! = 'Y' execute code that follows (see where DO_IT is initialized)

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID        ! Internal element ID for which
      INTEGER(LONG)                   :: ACID_G             ! Actual coordinate system ID
      INTEGER(LONG)                   :: I,J,K              ! DO loop indices
      INTEGER(LONG)                   :: I3_IN(3)           ! Integer array used in sorting VX. 

      INTEGER(LONG)                   :: I3_OUT(3)          ! Integer array giving order of VX comps. If VX is in the order with
!                                                             comp 2 smallest then comp 3 then comp 1 then I3_OUT is 2, 3, 1 

      INTEGER(LONG)                   :: ICID               ! Internal coord sys no. corresponding to an actual coord sys no. 
      INTEGER(LONG)                   :: ROWNUM             ! A row number in an array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMGM1_BEGEND
  
      REAL(DOUBLE)                    :: EPS1               ! A small number to compare to real zero
      REAL(DOUBLE)                    :: LX(3)              ! Distances
      REAL(DOUBLE)                    :: MAGY               ! Magnitude of vector VY
      REAL(DOUBLE)                    :: MAGZ               ! Magnitude of vector VZ
      REAL(DOUBLE)                    :: OFFDIS_B(MOFFSET,3)! Offset distances for this element in basic coords
      REAL(DOUBLE)                    :: OFFDIS_G(MOFFSET,3)! Offset distances for this element in global coords (needed for BUSH)
      REAL(DOUBLE)                    :: PHID, THETAD       ! Outputs from subr GEN_T0L
      REAL(DOUBLE)                    :: T0G(3,3)           ! Matrix to transform offsets from global to basic  coords 
      REAL(DOUBLE)                    :: TG0(3,3)           ! Matrix to transform offsets from basic  to global coords 
      REAL(DOUBLE)                    :: T0I(3,3)           ! Matrix to transform offsets from BUSH OCID to basic coords 
      REAL(DOUBLE)                    :: VX(3)              ! A vector in the elem x dir
      REAL(DOUBLE)                    :: VY(3)              ! A vector in the elem y dir
      REAL(DOUBLE)                    :: VZ(3)              ! A vector in the elem z dir
      REAL(DOUBLE)                    :: V13(3)             ! A vector from grid 1 to grid 3 (for BAR, BEAM or USER1 it is V vector)
      REAL(DOUBLE)                    :: XIB(2,3)           ! Coords at ends of BUSH elem: XIB(1,J) should = XIB(2,J) for 0 len elem
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)
 
! Initialize
  
      DO I=1,MELGP
        DO J=1,3
           XEL(I,J) = ZERO
        ENDDO 
      ENDDO 
 
      DO I=1,3
        DO J=1,3
           TE(I,J) = ZERO
        ENDDO 
      ENDDO 
 
      DO I=1,3
         VX(I) = ZERO
      ENDDO

      DO_IT = 'Y'                                          ! Used to decide if some blocks of code wil get executed or not
      IF (TYPE == 'BUSH    ') THEN                         ! DO_IT = 'Y' except when BUSH elem has a defined CID in field 9 of CBUSH
         IF (BUSH_CID >= 0) THEN                           ! in which case the TE matrix for BUSH is obtained from CID
            DO_IT = 'N'
         ENDIF
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Make sure the number of elem grids is not larger than OFFDIS is dimensioned for

      IF ((CAN_ELEM_TYPE_OFFSET == 'Y') .AND. (ELGP > MOFFSET)) THEN
         WRITE(ERR,1954) SUBR_NAME, MOFFSET, ELGP, TYPE
         WRITE(F06,1954) SUBR_NAME, MOFFSET, ELGP, TYPE
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
      ENDIF

! Init OFFDIS_B. Some elements will not require the offsets transformed to basic

      IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'BUSH    ') .OR. (TYPE == 'ROD     ')) THEN
         DO I=1,ELGP
            DO J=1,3
               OFFDIS_B(I,J) = ZERO
            ENDDO
         ENDDO
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! If this is a BUSH element and field 9 on the CBUSH (CID) is >=0, the TE coord transformation matrix is determined from:

      IF ((TYPE == 'BUSH    ') .AND. (BUSH_CID >= 0)) THEN

         IF (BUSH_CID > 0) THEN                            ! Need to get transformation of BUSH_CID to basic and put it into TE

            CORD_FND = 'N'
            ICID = 0
            DO J=1,NCORD
               IF (BUSH_CID == CORD(J,2)) THEN
                  CORD_FND = 'Y'
                  ICID = J
                  EXIT
               ENDIF
            ENDDO   
            IF (CORD_FND == 'N') THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'COORD SYSTEM ', BUSH_CID, TYPE, EID
               WRITE(F06,1822) 'COORD SYSTEM ', BUSH_CID, TYPE, EID
            ENDIF

            IF (CORD_FND == 'Y') THEN                      ! NOTE: TE transforms a vector in basic to elem coord system (E). This is
!                                                            (transpose of matrix in RCORD at row ICID)
               TE(1,1) = RCORD(ICID, 4)   ;   TE(1,2) = RCORD(ICID, 7)   ;   TE(1,3) = RCORD(ICID,10)
               TE(2,1) = RCORD(ICID, 5)   ;   TE(2,2) = RCORD(ICID, 8)   ;   TE(2,3) = RCORD(ICID,11)
               TE(3,1) = RCORD(ICID, 6)   ;   TE(3,2) = RCORD(ICID, 9)   ;   TE(3,3) = RCORD(ICID,12)

            ENDIF

         ELSE                                              ! BUSH_CID is basic so TE is the identity matrix

            DO I=1,3
               DO J=1,3
                  TE(I,J) = ZERO
               ENDDO
               TE(I,I) = ONE
            ENDDO

         ENDIF

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! If there are offsets that are specified in global coords (or some other for BUSH), transform them to basic.
! NOTE: This is NOT transforming the offsets to basic. It is merely calculating the value of the offsets in basic coords so that
!       they can be used below to find the element axes in basic coords

      IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN

         IF (EOFF(INT_ELEM_ID) == 'Y') THEN
            DO I=1,ELGP
               ACID_G = GRID(BGRID(I),3)                   ! Get global coord sys for this grid
               IF (ACID_G /= 0) THEN                       ! Need to transform offset vector from global to basic coords
                  ICID = 0
                  DO J=1,NCORD
                     IF (ACID_G == CORD(J,2)) THEN         ! ACID_G global coord system exists. It was checked in CORDP_PROC
                        ICID = J
                        EXIT
                     ENDIF
                  ENDDO   

                  CALL GEN_T0L ( BGRID(I), ICID, THETAD, PHID, T0G )
                  DO J=1,3
                     OFFDIS_B(I,J) = T0G(J,1)*OFFDIS(I,1) + T0G(J,2)*OFFDIS(I,2) + T0G(J,3)*OFFDIS(I,3) 
                  ENDDO   
               ELSE                                        ! Offset was in basic coords
                  DO J=1,3
                     OFFDIS_B(I,J) = OFFDIS(I,J)
                  ENDDO   
               ENDIF
            ENDDO
         ELSE                                              ! There are no offsets so set OFFDIS_B to zero
            DO I=1,ELGP
               DO J=1,3
                  OFFDIS_B(I,J) = ZERO
               ENDDO
            ENDDO
         ENDIF 
         
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Offsets for BUSH are specified in a unique coord system. It may or may not be basic or global. So we first transform the input
! offset values to basic and then transform them to global. This way, when we process the offsets in subr ELEM_TRANSFORM_LBG, 
! we can treat the final offset values for the BUSH the same as we do for BAR, BEAM or ROD.
! The coord system for BUSH offsets is BUSH_OCID which can be:
!  (1) -1 means use local elem coord sys (BUSH_CID) for offsets (in which case these BUSH offsets are handled later), or
!  (2)  a positive number indicating a coord system number

      IF (TYPE == 'BUSH    ') THEN

         IF (EOFF(INT_ELEM_ID) == 'Y') THEN

            CORD_FND = 'N'
            IF (BUSH_OCID > 0) THEN                        ! Don't transform offsets here when BUSH_OCID = -1 (local axes offsets)
               ICID = 0
               DO J=1,NCORD
                  IF (BUSH_OCID == CORD(J,2)) THEN         ! BUSH_OCID coord system exists. It was checked in CORDP_PROC
                     CORD_FND = 'Y'
                     ICID = J
                     EXIT
                  ENDIF
               ENDDO   
               IF (CORD_FND == 'N') THEN                   ! Coord sys ID on CONM2 undefined
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1822) 'COORD SYSTEM ', BUSH_OCID, TYPE, EID
                  WRITE(F06,1822) 'COORD SYSTEM ', BUSH_OCID, TYPE, EID
               ENDIF

               IF (CORD_FND == 'Y') THEN
                  CALL GEN_T0L ( BGRID(1), ICID, THETAD, PHID, T0I )
                  DO J=1,3
                     OFFDIS_B(1,J) = T0I(J,1)*OFFDIS(1,1) + T0I(J,2)*OFFDIS(1,2) + T0I(J,3)*OFFDIS(1,3) 
                  ENDDO
               ELSE
                  DO J=1,3
                     OFFDIS_B(1,J) = ZERO
                  ENDDO
               ENDIF   

            ELSE                                           ! Offset was in basic coords

               DO J=1,3
                  OFFDIS_B(1,J) = OFFDIS(1,J)
               ENDDO   

            ENDIF

            DO J=1,3                                       ! Basic coords at end A of the BUSH elem
               XIB(1,J) = XEB(1,J) + OFFDIS_B(1,J)
            ENDDO

            DO J=1,3
               XIB(2,J) = XIB(1,J)                         ! Zero length of BUSH element  
               OFFDIS_B(2,J) = XIB(2,J) - XEB(2,J)
            ENDDO
                                                           ! Transform offsets from basic to global and put result into OFFDIS
            DO I=1,ELGP
               ACID_G = GRID(BGRID(I),3)                   ! Get global coord sys for this grid
               IF (ACID_G /= 0) THEN                       ! Global is not basic so need to transform offset from basic to global
                  ICID = 0
                  DO J=1,NCORD
                     IF (ACID_G == CORD(J,2)) THEN
                        ICID = J
                        EXIT
                     ENDIF
                  ENDDO   
                  CALL GEN_T0L ( BGRID(I), ICID, THETAD, PHID, T0G )
                  DO J=1,3                                 ! We want transpose of T0G since we are transforming from basic to global
                     DO K=1,3
                        TG0(J,K) = T0G(K,J)
                     ENDDO
                  ENDDO
                  DO J=1,3
                     OFFDIS_G(I,J) = TG0(J,1)*OFFDIS_B(I,1) + TG0(J,2)*OFFDIS_B(I,2) + TG0(J,3)*OFFDIS_B(I,3) 
                  ENDDO   
               ELSE                                        ! Global was basic so no transformation of coords needed
                  DO J=1,3
                     OFFDIS_G(I,J) = OFFDIS_B(I,J)
                  ENDDO   
               ENDIF
            ENDDO
            DO I=1,ELGP                                    ! Put global offsets in OFFDIS since these are the ones needed when
               DO J=1,3                                    ! offsets are applied in subr ELEM_TRANSFORM_LBG
                  OFFDIS(I,J) = OFFDIS_G(I,J)
               ENDDO
            ENDDO

         ELSE                                              ! There are no offsets so set OFFDIS_B to zero for both grids

            DO I=1,ELGP
               DO J=1,3
                  OFFDIS_B(I,J) = ZERO
               ENDDO
            ENDDO

         ENDIF 

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
      IF ((TYPE == 'BUSH    ') .AND. (DEBUG(110) > 0)) THEN
         CALL DEBUG_ELMGM1_FOR_BUSH
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Calculate a vector between ends of the element in basic coords (not between grids if there are offsets). 
! NOTE: BUSH is a zero length element and the vector in the element x direction is determined from either:
!       (1) CID 

      IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'ROD     ')) THEN
         VX(1) = ( XEB(2,1) + OFFDIS_B(2,1) ) - ( XEB(1,1) + OFFDIS_B(1,1) )
         VX(2) = ( XEB(2,2) + OFFDIS_B(2,2) ) - ( XEB(1,2) + OFFDIS_B(1,2) )
         VX(3) = ( XEB(2,3) + OFFDIS_B(2,3) ) - ( XEB(1,3) + OFFDIS_B(1,3) )
      ELSE
         VX(1) = XEB(2,1) - XEB(1,1)
         VX(2) = XEB(2,2) - XEB(1,2)
         VX(3) = XEB(2,3) - XEB(1,3)
      ENDIF
      LX(1) = VX(1)
      LX(2) = VX(2)
      LX(3) = VX(3)
                                                           ! When there is no BUSH_CID the x axis is along line between the 2 grids
      IF (TYPE == 'BUSH    ') THEN
         VX(1) = XEB(2,1) - XEB(1,1)
         VX(2) = XEB(2,2) - XEB(1,2)
         VX(3) = XEB(2,3) - XEB(1,3)
         LX(1) = ( XEB(2,1) + OFFDIS_B(2,1) ) - ( XEB(1,1) + OFFDIS_B(1,1) )
         LX(2) = ( XEB(2,2) + OFFDIS_B(2,2) ) - ( XEB(1,2) + OFFDIS_B(1,2) )
         LX(3) = ( XEB(2,3) + OFFDIS_B(2,3) ) - ( XEB(1,3) + OFFDIS_B(1,3) )
      ENDIF

! Length of element between ends is:

      ELEM_LEN_AB = DSQRT( LX(1)*LX(1) + LX(2)*LX(2) + LX(3)*LX(3) )

! If ELEM_LEN_AB is equal to zero (for all but BUSH) then write error and return. If BUSH elem length NOT zero, then error also

      IF (TYPE /= 'BUSH    ') THEN
         IF (ELEM_LEN_AB <= EPS1) THEN
            WRITE(ERR,1904) TYPE, EID, ELEM_LEN_AB
            WRITE(F06,1904) TYPE, EID, ELEM_LEN_AB
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            RETURN
         ENDIF
      ELSE
         IF (ELEM_LEN_AB > .0001D0) THEN
            WRITE(ERR,1959) SUBR_NAME, TYPE, EID, ELEM_LEN_AB
            WRITE(F06,1959) SUBR_NAME, TYPE, EID, ELEM_LEN_AB
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            RETURN
         ENDIF
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Unit vector in element X direction except for BUSH element with CID >= 0 (i.e. when BUSH does not have a V vector)

      IF (DO_IT == 'Y') THEN
         IF (TYPE == 'BUSH    ') THEN
            DO I=1,3
               TE(1,I) = VX(I)/ELEM_LEN_12                 ! For BUSH use length between 2 grids as length between elem ends is zero
            ENDDO
         ELSE
            DO I=1,3
               TE(1,I) = VX(I)/ELEM_LEN_AB
            ENDDO
         ENDIF 
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
      IF (TYPE == 'ROD     ') THEN

         DO I=1,3
            I3_IN(I)   = I
            I3_OUT(I)  = I3_IN(I)
         ENDDO
         CALL CALC_VEC_SORT_ORDER ( VX, SORT_ORDER, I3_OUT)! Use this rather than SORT_INT1_REAL1 - didn't work for vec 10., 0., 0.
         IF (SORT_ORDER == '     ') THEN                   ! Subr CALC_VEC_SORT_ORDER did not find a sort order
            FATAL_ERR = FATAL_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            WRITE(ERR,1944) SUBR_NAME, TYPE, EID
            WRITE(F06,1944) SUBR_NAME, TYPE, EID
            RETURN
         ENDIF
                                                           ! See notes on "Some Basic Vector Operations In IDL" on web site:
                                                           ! http://fermi.jhuapl.edu/s1r/idl/s1rlib/vectors/v_basic.html
         VY(I3_OUT(1)) =  ZERO                             !  (a) Component of VY in direction of min VX is set to zero
         VY(I3_OUT(2)) =  VX(I3_OUT(3))                    !  (b) Other 2 VY(i) are corresponding VX(i) switched with one x(-1)
         VY(I3_OUT(3)) = -VX(I3_OUT(2))
         MAGY  = DSQRT(VY(1)*VY(1) + VY(2)*VY(2) + VY(3)*VY(3))


         IF (DABS(MAGY) < EPS1) THEN
            FATAL_ERR = FATAL_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            WRITE(ERR,1938) SUBR_NAME, 'Y', TYPE, EID, (VY(I),I=1,3)
            WRITE(F06,1938) SUBR_NAME, 'Y', TYPE, EID, (VY(I),I=1,3)
            RETURN
         ENDIF

         DO I=1,3
            TE(2,I) = VY(I)/MAGY
         ENDDO

         CALL CROSS ( VX, VY, VZ )

         MAGZ  = DSQRT(VZ(1)*VZ(1) + VZ(2)*VZ(2) + VZ(3)*VZ(3))

         IF (DABS(MAGZ) < EPS1) THEN
            FATAL_ERR = FATAL_ERR + 1
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            WRITE(ERR,1938) SUBR_NAME, 'Z', TYPE, EID, (VZ(I),I=1,3)
            WRITE(F06,1938) SUBR_NAME, 'Z', TYPE, EID, (VZ(I),I=1,3)
            RETURN
         ENDIF

         DO I=1,3
            TE(3,I) = VZ(I)/MAGZ
         ENDDO 

         TE_IDENT = 'N'
         DO I=1,3
            ID(I) = 'N'
         ENDDO
         DO I=1,3
            IF (DABS(TE(I,I) - ONE) < EPS1) THEN
               ID(I) = 'Y'
            ELSE
               ID(I) = 'N'
            ENDIF
         ENDDO 
         IF ((ID(1) == 'Y') .AND. (ID(2) == 'Y') .AND. (ID(3) == 'Y')) THEN
            TE_IDENT = 'Y'
         ENDIF

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Calculate remainder of TE for elements other than ROD
 
! Calculate V13, vector from G.P.-1 to G.P.-3. For BAR, BEAM, BUDH, USER1 the V13 vector is the v vector = XEB(ELGP+1,i)
 
      IF ((TYPE /= 'ROD     ') .AND. (DO_IT == 'Y')) THEN
    
         IF ((TYPE == 'BAR     ') .OR. (TYPE == 'BEAM    ') .OR. (TYPE == 'BUSH    ') .OR. (TYPE == 'USER1   ')) THEN
            ROWNUM = ELGP + 1
         ELSE      
            ROWNUM = 3
         ENDIF
         DO I=1,3  
            V13(I) = XEB(ROWNUM,I) - XEB(1,I)
         ENDDO 
 
! Calculate VX x V13 and unit vector in elem z dir. (Col. 3 of TE). If MAGZ is equal to zero, then vector from G.P. 1
! to G.P. 3 is parallel to vector from G.P.-1 to G.P.-2 so write error and quit.
 
         CALL CROSS ( VX, V13, VZ )
         MAGZ = DSQRT(VZ(1)*VZ(1) + VZ(2)*VZ(2) + VZ(3)*VZ(3))
         IF (MAGZ <=  EPS1) THEN
            IF ((TYPE == 'BAR     ')  .OR. (TYPE == 'BEAM    ')) THEN
               WRITE(ERR,1905) TRIM(TYPE), EID
               WRITE(F06,1905) TRIM(TYPE), EID
               NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
               FATAL_ERR = FATAL_ERR + 1
               RETURN
            ELSE 
               WRITE(ERR,1906) TYPE, EID
               WRITE(F06,1906) TYPE, EID
               NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
               FATAL_ERR = FATAL_ERR + 1
               RETURN
            ENDIF
         ENDIF
         DO I=1,3
            TE(3,I) = VZ(I)/MAGZ
         ENDDO 
 
! Calculate unit vector in elem. y dir. (from VZ (cross) VX): If MAGY equals zero then quit.
 
         CALL CROSS ( VZ, VX, VY )
         MAGY = DSQRT(VY(1)*VY(1) + VY(2)*VY(2) + VY(3)*VY(3))
         IF(MAGY <= EPS1) THEN
            WRITE(ERR,1912) EID,TYPE
            WRITE(F06,1912) EID,TYPE
            NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
            FATAL_ERR = FATAL_ERR + 1
            RETURN
         ENDIF
         DO I=1,3
            TE(2,I) = VY(I)/MAGY
         ENDDO 
 
! Now set TE_IDENT to be 'Y' if TE is an identity matrix. TE will be an identity matrix if the diag terms are unity.
 
         TE_IDENT = 'N'
         DO I=1,3
            ID(I) = 'N'
         ENDDO
         DO I=1,3
            IF (DABS(TE(I,I) - ONE) < EPS1) THEN
               ID(I) = 'Y'
            ELSE
               ID(I) = 'N'
            ENDIF
         ENDDO 
         IF ((ID(1) == 'Y') .AND. (ID(2) == 'Y') .AND. (ID(3) == 'Y')) THEN
            TE_IDENT = 'Y'
         ENDIF
 
      ENDIF
 

! ----------------------------------------------------------------------------------------------------------------------------------
! Use TE to get array of elem coords in local system.
 
      XEL(1,1) = ZERO
      XEL(1,2) = ZERO
      XEL(1,3) = ZERO
  
      DO I=2,ELGP
         DO J=1,3
            XEL(I,J) = ZERO
            DO K=1,3
               XEL(I,J) = XEL(I,J) + (XEB(I,K) - XEB(1,K))*TE(J,K)
            ENDDO 
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

 1904 FORMAT(' *ERROR  1904: ',A,I8,' HAS LENGTH BETWEEN ITS ELEMENT ENDS (INCL EFFECTS OF OFFSETS) = ',1ES9.2,'. TOO SMALL')

 1905 FORMAT(' *ERROR  1905: V VECTOR ON ',A,' ELEMENT ',I8,' IS PARALLEL TO VECTOR FROM END A TO END B')

 1906 FORMAT(' *ERROR  1906: ',A,I8,' HAS INTERNAL GRID 3 (FOR V VECTOR) TOO CLOSE TO LINE BETWEEN INTERNAL GRIDS 1 AND 2')

 1912 FORMAT(' *ERROR  1912: CANNOT CALCULATE VECTOR IN ELEMENT Y DIRECTION FOR ELEMENT ',I8,' TYPE ',A)

 1938 FORMAT(' *ERROR  1938: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CANNOT CALCULATE PROPER VECTOR IN ELEMENT LOCAL COORDINATE DIRECTION ',A,' FOR ',A,' ELEMENT ',I8,'.' &
                    ,/,14X,' THE VECTOR COMPONENTS CALCULATED WERE ',3(1ES14.6))

 1944 FORMAT(' *ERROR  1944: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE VX VECTOR FOR ',A,' ELEMENT ',I8,' WAS LEFT UNSORTED. IT MUST BE SORTED TO DETERMINE VY, VZ') 

 1954 FORMAT(' *ERROR  1954: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' DIMENSION OF ARRAYS OFFDIS, OFFSET ARE ONLY ',I8,' BUT MUST BE ',I8,' FOR ELEM TYPE ',A)

 1959 FORMAT(' *ERROR  1959: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,A,I8,' HAS LENGTH (INCL EFFECTS OF OFFSETS) = ',1ES9.2,'. SHOULD BE ZERO')










! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEBUG_ELMGM1_FOR_BUSH

      IMPLICIT NONE

! **********************************************************************************************************************************
      IF (BUSH_CID >= 0) THEN
         WRITE(F06,'(A,I8)') '    The element coordinate system will be BUSH_CID coord system   ',bush_cid
         WRITE(F06,*)
      ELSE
         WRITE(F06,'(A)') '    The element coordinate system will be determined from the 2 grids and the specified VVEC'
         WRITE(F06,*)
      ENDIF

      IF (EOFF(INT_ELEM_ID) == 'Y') THEN

         WRITE(F06,*) '   OFFDIS array of offsets based on input on CBUSH and ELEM_LEN_12 (only end A input is used)'
         WRITE(F06,'(A,3(1ES14.6))') '                                     End A  = ', (offdis(1,j),j=1,3)
         WRITE(F06,*)

         WRITE(F06,*) '   OFFDIS array of offsets in basic coords:'
         WRITE(F06,'(A,3(1ES14.6))') '                                     End A  = ', (offdis_b(1,j),j=1,3)
         WRITE(F06,'(A,3(1ES14.6))') '                                     End B  = ', (offdis_b(2,j),j=1,3)
         WRITE(F06,*)

         WRITE(F06,*) '   OFFDIS array of offsets transformed to global coords:'
         WRITE(F06,'(A,3(1ES14.6))') '                                     END A  = ', (OFFDIS(1,J),J=1,3)
         WRITE(F06,'(A,3(1ES14.6))') '                                     End B  = ', (offdis(2,j),j=1,3)
         WRITE(F06,*)

      ENDIF

      WRITE(F06,'(A,3(1ES14.6))') '    ELEM_LEN_AB                             = ',ELEM_LEN_AB
      WRITE(F06,'(A,3(1ES14.6))') '    ELEM_LEN_12                             = ',ELEM_LEN_12

! **********************************************************************************************************************************

      END SUBROUTINE DEBUG_ELMGM1_FOR_BUSH

      END SUBROUTINE ELMGM1
