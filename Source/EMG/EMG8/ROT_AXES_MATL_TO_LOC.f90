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

      SUBROUTINE ROT_AXES_MATL_TO_LOC ( WRITE_WARN )

! Rotates material and CTE matrices from the material axes (specified on connection entry) to element local axes

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEMATC, NCORD
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE MODEL_STUF, ONLY            :  ALPVEC, CORD, ISOLID, EB, EBM, EM, ES, ET, MTRL_TYPE, NUM_EMG_FATAL_ERRS, QUAD_DELTA,     &
                                         RCORD, TE, THETAM, TYPE
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  ROT_AXES_MATL_TO_LOC_BEGEND

      USE ROT_AXES_MATL_TO_LOC_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ROT_AXES_MATL_TO_LOC'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y' write warning messages, otherwise do not
      CHARACTER( 1*BYTE)              :: FOUND             ! If 'Y' we found something we were looking for

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ROT_AXES_MATL_TO_LOC_BEGEND
      INTEGER(LONG)                   :: CORDM             ! Actual coord system ID (CORDM on PSOLID Bulk Data entry)
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: ICORD             ! Internal coord system ID for CORDM
      INTEGER(LONG)                   :: K                 ! Counter


      REAL(DOUBLE)                    :: DUM22(2,2)        ! Intermediate matrix in calculating outputs
      REAL(DOUBLE)                    :: DUM33(3,3)        ! Intermediate matrix in calculating outputs
      REAL(DOUBLE)                    :: DUM64(6,4)        ! Intermediate matrix in calculating outputs
      REAL(DOUBLE)                    :: DUM66(6,6)        ! Intermediate matrix in calculating outputs
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: MATL_AXES_ROTATE  ! Angle to rotate material axes to coincide with local elem x axis
      REAL(DOUBLE)                    :: T0M(3,3)          ! Transform matrix from material coords to basic coords
      REAL(DOUBLE)                    :: T1(6,6)           ! Transforms 6 stress and 6x6 material mats from material to element axes
!                                                            (T1' transforms strains)
      REAL(DOUBLE)                    :: T1_MB(3,3)        ! Portion of T1: transforms 3x3 EM, EB, EMB from material to elem axes
      REAL(DOUBLE)                    :: T1_TS(2,2)        ! Portion of T1: transforms 3x3 ET from material to elem axes
      REAL(DOUBLE)                    :: TEM(3,3)          ! TE*T0M
      REAL(DOUBLE)                    :: TME(3,3)          ! Coord transf matrix which will rotate a vector in local element coord
!                                                            system to a vector in the material coord system (Um = TME*Ue)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

      IF ((TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'TRIA3')) THEN

         IF      (TYPE(1:5) == 'QUAD4') THEN

            MATL_AXES_ROTATE = THETAM - QUAD_DELTA         ! We need angle which would rotate local elem x axis to mat'l axis,
!                                                            NOT the opposite (since we want a transform matrix that will take
!                                                            a vector in elem coords and convert it into a vector in mat'l coords
         ELSE IF (TYPE(1:5) == 'TRIA3') THEN

            MATL_AXES_ROTATE = THETAM

         ENDIF

         IF (DABS(MATL_AXES_ROTATE) > EPS1) THEN

            CALL PLANE_COORD_TRANS_21 ( MATL_AXES_ROTATE, TME, SUBR_NAME )
            CALL GET_T1_TRANSFORM_MAT
                                                           ! T1_MB is for Sxx, Syy, Sxy which are rows and cols 1,2,4 from T1
            T1_MB(1,1) = T1(1,1)     ;     T1_MB(1,2) = T1(1,2)     ;     T1_MB(1,3) = T1(1,4)
            T1_MB(2,1) = T1(2,1)     ;     T1_MB(2,2) = T1(2,2)     ;     T1_MB(2,3) = T1(2,4)
            T1_MB(3,1) = T1(4,1)     ;     T1_MB(3,2) = T1(4,2)     ;     T1_MB(3,3) = T1(4,4)
                                                           ! T1_ET is for Syz, Szx which are rows and cols 5,6 from T1
            T1_TS(1,1) = T1(5,5)     ;     T1_TS(1,2) = T1(5,6)
            T1_TS(2,1) = T1(6,5)     ;     T1_TS(2,2) = T1(6,6)

            IF ((MTRL_TYPE(1) == 2) .OR. (MTRL_TYPE(1) == 8)) THEN
               CALL MATMULT_FFF   ( EM , T1_MB  , 3, 3, 3, DUM33 )
               CALL MATMULT_FFF_T ( T1_MB, DUM33, 3, 3, 3, EM    )
            ENDIF

            IF ((MTRL_TYPE(2) == 2) .OR. (MTRL_TYPE(2) == 8)) THEN
               CALL MATMULT_FFF   ( EB , T1_MB  , 3, 3, 3, DUM33 )
               CALL MATMULT_FFF_T ( T1_MB, DUM33, 3, 3, 3, EB    )
            ENDIF

            IF ((MTRL_TYPE(3) == 2) .OR. (MTRL_TYPE(3) == 8)) THEN
               CALL MATMULT_FFF   ( ET , T1_TS  , 2, 2, 2, DUM22 )
               CALL MATMULT_FFF_T ( T1_TS, DUM22, 2, 2, 2, ET    )
            ENDIF

            IF ((MTRL_TYPE(4) == 2) .OR. (MTRL_TYPE(4) == 8)) THEN
               CALL MATMULT_FFF   ( EBM, T1_MB  , 3, 3, 3, DUM33 )
               CALL MATMULT_FFF_T ( T1_MB, DUM33, 3, 3, 3, EBM    )
            ENDIF

            CALL MATMULT_FFF_T (T1, ALPVEC, 6, 6, MEMATC, DUM64 )
            DO I=1,6
               DO J=1,MEMATC
                  ALPVEC(I,J) = DUM64(I,J)
               ENDDO
            ENDDO

         ENDIF

      ELSE IF ((TYPE(1:4) == 'HEXA') .OR. (TYPE(1:5) == 'PENTA') .OR. (TYPE(1:5) == 'TETRA')) THEN

         IF (ISOLID(3) /= -1) THEN                         ! If -1 ES already in elem coords, else transf ES from basic to elem axes

            CORDM = ISOLID(3)                              ! Get transf matrix T0M. CORDM mtrl coord sys from PSOLID Bulk data entry

            DO I=1,3
               DO J=1,3
                  T0M(I,J) = ZERO
               ENDDO
            ENDDO

            IF (CORDM == 0) THEN                           ! CORDM was basic so set T0M to the identity matrix

               DO I=1,3
                  DO J=1,3
                     T0M(I,J) = ZERO
                  ENDDO
                  T0M(I,I) = ONE
               ENDDO

            ELSE                                           ! Need to transform ES mat'l matrix to basic coords

               FOUND = 'N'
               DO I=1,NCORD                                ! Get the internal coord system ID for CORDM
                  IF (CORDM == CORD(I,2)) THEN
                     ICORD = I
                     FOUND = 'Y'
                     EXIT
                  ENDIF
               ENDDO   

               IF (FOUND == 'Y') THEN
                  DO I=1,3
                     DO J=1,3
                        K = 3 + 3*(I-1) + J
                        T0M(I,J) = RCORD(ICORD,K)
                     ENDDO
                  ENDDO
               ELSE
                  WRITE(ERR,1822) 'COORD SYSTEM ', CORDM, 'PSOLID', ISOLID(1)
                  WRITE(F06,1822) 'COORD SYSTEM ', CORDM, 'PSOLID', ISOLID(1)
                  FATAL_ERR = FATAL_ERR + 1
                  NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
               ENDIF

            ENDIF

            CALL MATMULT_FFF ( TE , T0M  , 3, 3, 3, TEM )
            DO I=1,3                                       ! Transform TEM to TME
               DO J=1,3
                  TME(I,J) = TEM(J,I)
               ENDDO
            ENDDO

            CALL GET_T1_TRANSFORM_MAT
            CALL MATMULT_FFF   ( ES , T1   , 6, 6, 6, DUM66 )
            CALL MATMULT_FFF_T ( T1 , DUM66, 6, 6, 6, ES    )

            CALL MATMULT_FFF_T (T1, ALPVEC, 6, 6, MEMATC, DUM64 )
            DO I=1,6
               DO J=1,MEMATC
                  ALPVEC(I,J) = DUM64(I,J)
               ENDDO
            ENDDO

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
 1822 FORMAT(' *ERROR  1822: ',A,I8,' ON ',A,I8,' IS UNDEFINED')





! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE GET_T1_TRANSFORM_MAT

! Formulates 6x6 transformation matrix T1 from the coefficients in 3x3 coord transformation matrix A where A is the coord
! transformation matrix that transforms a unit vector in element coords (Ue) to a unit vector in material coords (Um):

!                                                  Um = Tme*Ue                                                         (1)
! A can be calculated in the following way:

!                                                  Ue = Te0*U0                                                         (2)

! where U0 is a unit vector in basic coords and Te0 is the coord transformation matrix stored as array TE (gen in subr ELMGMi).
! From subr CORD_PROC, we can get the coord transformation matrix, T0M, that transforms a unit vector in coord system CORDP
! (from PSOLID Bulk Data entry) to a unit vector in basic coords (T0M is stored in RCORD):

!                                                  U0 = T0m*Um                                                         (3)
! Substitute (3) into (2) to get:
!                                                  Ue = (Te0*T0m)*Um                                                   (4)

! Define                                          Tem = (Te0*T0m)                                                      (5)

! The transpose of Tem is Tme, the transformation matrix in eqn(1) we seek:

!                                                 Tme = Tem' = (Te0*T0m)'                                              (6)

! The 2nd order stress tensor is a 3x3 matrix of the 9 (6 independent) stresses:

!                                                       | Sxx  Sxy  Sxz |
!                                                   S = | Syx  Syy  Syz |
!                                                   ~   | Szx  Szy  Szz |

! where Sij = Sji and, for example Sxx is the normal stress in the x direction, Sxy is the shear stress in the xy plane, etc.

! Matrix Tme is used to transform this 2nd order stress tensor from material coords to element coords via the equation:

!                                                 See = Tme'*Smm*Tme = Tem*Smm*Tem'                                    (7)
!                                                 ~          ~
! The terms in this triple matrix product can be used to rewrite the stress transformation with the 6 independent stresses
! in a vector (instead of as the 3x3 stress tensor). To this end, define the vector:

!                                                       | Sxx | 
!                                                       | Syy |
!                                                   S = | Szz |                                                        (8)
!                                                       | Sxy |
!                                                       | Syz |
!                                                       | Szx |

! Then the relationship between S in material coords (Sm) and S in element coords (Se) can be written at:

!                                                  Se = T1*Sm                                                          (9)

! where T1 is a 6x6 matrix of coefficients from Tme (eqn (6)). The 9 terms in TME are denoted as Aij and the 36 terms in T1,
! developed from the Aij using eqn (7), are developed in the code below. Again, they are developed by expanding the triple
! matrix product (symbolically) in eqn (7) and then rewriting the 6 independent of the resulting 9 equations in the 6x6
! matrix form of (9).
 

      USE PENTIUM_II_KIND, ONLY       :  DOUBLE

      IMPLICIT NONE

      REAL(DOUBLE)                    :: A11,A12,A13       ! Coefficients from matrix TME
      REAL(DOUBLE)                    :: A21,A22,A23       ! Coefficients from matrix TME
      REAL(DOUBLE)                    :: A31,A32,A33       ! Coefficients from matrix TME

! **********************************************************************************************************************************
! Formulate 6x6 matrix T1 from terms in 3x3 matrix TME

      A11 = TME(1,1)     ;     A12 = TME(1,2)     ;     A13 = TME(1,3)
      A21 = TME(2,1)     ;     A22 = TME(2,2)     ;     A23 = TME(2,3)
      A31 = TME(3,1)     ;     A32 = TME(3,2)     ;     A33 = TME(3,3)

! Matrix T1 is formed from the coefficients in matrix TME (denoted as Aij coefficients)

                                                           ! Row 1 of T1:
      T1(1,1)= A11*A11           ;   T1(1,2)= A21*A21           ;   T1(1,3)= A31*A31; 
      T1(1,4)= 2*A11*A21         ;   T1(1,5)= 2*A21*A31         ;   T1(1,6)= 2*A11*A31

                                                           ! Row 2 of T1:
      T1(2,1)= A12*A12           ;   T1(2,2)= A22*A22           ;   T1(2,3)= A32*A32; 
      T1(2,4)= 2*A12*A22         ;   T1(2,5)= 2*A22*A32         ;   T1(2,6)= 2*A12*A32

                                                           ! Row 3 of T1:
      T1(3,1)= A13*A13           ;   T1(3,2)= A23*A23           ;   T1(3,3)= A33*A33; 
      T1(3,4)= 2*A13*A23         ;   T1(3,5)= 2*A23*A33         ;   T1(3,6)= 2*A13*A33

                                                           ! Row 4 of T1:
      T1(4,1)= A11*A12           ;   T1(4,2)= A21*A22           ;   T1(4,3)= A31*A32; 
      T1(4,4)= A11*A22+A21*A12   ;   T1(4,5)= A21*A32+A31*A22   ;   T1(4,6)= A11*A32+A31*A12

                                                           ! Row 5 of T1:
      T1(5,1)= A12*A13           ;   T1(5,2)= A22*A23           ;   T1(5,3)= A32*A33; 
      T1(5,4)= A12*A23+A22*A13   ;   T1(5,5)= A22*A33+A32*A23   ;   T1(5,6)= A12*A33+A32*A13

                                                           ! Row 6 of T1:
      T1(6,1)= A13*A11           ;   T1(6,2)= A23*A21           ;   T1(6,3)= A33*A31; 
      T1(6,4)= A13*A21+A23*A11   ;   T1(6,5)= A23*A31+A33*A21   ;   T1(6,6)= A13*A31+A33*A11

! **********************************************************************************************************************************

      END SUBROUTINE GET_T1_TRANSFORM_MAT

      END SUBROUTINE ROT_AXES_MATL_TO_LOC