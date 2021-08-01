!===================================================================================================================================
      SUBROUTINE WRITE_OP2_HEADER(POST)
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  OP2

      integer, intent(in)  :: POST
      character(len=28) :: TAPE_CODE
      character(len=8)  :: NASTRAN_VERSION
      IF (POST == -1) THEN
        !_write_markers(op2, op2_ascii, [3, 0, 7])
        WRITE(OP2) 3
        WRITE(OP2) 3, 24, 2021-2000  ! date
        !WRITE(OP2) 0
        WRITE(OP2) 7
        TAPE_CODE = 'NASTRAN FORT TAPE ID CODE - '
!       write(OP2) 1
!       write(OP2) 7
        WRITE(OP2) TAPE_CODE
!       fop2.write(pack(endian + b'7i 28s i', *[4, 1, 4,
!                                               4, 7, 4,
!                                               28, tape_code, 28]))

!       nastran_version = b'NX8.5   ' if obj.is_nx else b'XXXXXXXX'
        NASTRAN_VERSION = 'XXXXXXXX'
        WRITE(OP2) 2
        WRITE(OP2) NASTRAN_VERSION
!       write(OP2) 'XXXXXXXX'
!       fop2.write(pack(endian + b'4i 8s i', *[4, 2, 4,
!                                              #4, 2, 4,
!                                              #4, 1, 4,
!                                              #4, 8, 4,
!                                              8, nastran_version, 8]))
        WRITE(OP2) -1
        WRITE(OP2) 0
!       fop2.write(pack(endian + b'6i', *[4, -1, 4,
!                                         4, 0, 4,]))
      ELSE IF(POST == -2) THEN
!       write_markers(fop2_ascii, [2, 4])
        WRITE(OP2) 2
        WRITE(OP2) 4
      ENDIF
      END SUBROUTINE WRITE_OP2_HEADER

!===================================================================================================================================
      SUBROUTINE  END_OP2_TABLE(ITABLE)
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  ERR, OP2
      IMPLICIT NONE
      INTEGER(LONG) :: ITABLE
      WRITE(ERR,9114) ITABLE

 9114 FORMAT(" *DEBUG:       END_OP2_TABLE; ITABLE=", I8)
      WRITE(OP2) ITABLE
      WRITE(OP2) 1
      WRITE(OP2) 0
      WRITE(OP2) 0
      END SUBROUTINE END_OP2_TABLE

!===================================================================================================================================
      SUBROUTINE  END_OP2_TABLES()
!      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  ERR, OP2
      IMPLICIT NONE
 9115 FORMAT(" *DEBUG:       END_OP2_TABLES", A)
      WRITE(ERR,9115) " "
      WRITE(OP2) 0
      END SUBROUTINE END_OP2_TABLES
!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM()
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      USE IOUNT1, ONLY                :  ERR, OP2
      
      ! GEOM1 - GRID/COORDs
      USE SCONTR, ONLY                :  NGRID
      USE MODEL_STUF, ONLY            :  GRID_ID, GRID
      
      ! GEOM2 - elements
      !USE MODEL_STUF, ONLY : ELAS1, ELAS2, ELAS3, ELAS4, ROD, TETRA4, TETRA10
      USE SCONTR, ONLY : NCTETRA4, NCTETRA10, NCPENTA6, NCPENTA15, NCHEXA8, NCHEXA20
      USE SCONTR, ONLY : NCQUAD4, NCQUAD4K, NCSHEAR, NCTRIA3, NCTRIA3K
      USE SCONTR, ONLY : NCROD, NCBAR, NCBEAM, NCBUSH, NCELAS1, NCELAS2, NCELAS3, NCELAS4
      USE SCONTR, ONLY : NCMASS, NCONM2

      USE SCONTR, ONLY     : NELE, NEDAT
      USE MODEL_STUF, ONLY : ETYPE, EOFF, EPNT

      IMPLICIT NONE
      INTEGER(LONG)                    :: ITABLE                        ! the subtable counter
      INTEGER(LONG)                    :: SEID                          ! superelment id
      INTEGER(LONG)                    :: I,J                           ! counter
      CHARACTER(LEN=8*BYTE)            :: TABLE_NAME                    ! the current op2 table name
      INTEGER(LONG)                    :: NGRID_ACTUAL, NSPOINT_ACTUAL  ! number of GRID, SPOINTs; corrects for oversizing
      INTEGER(LONG), DIMENSION(NGRID)  :: GRID_INDEX, SPOINT_INDEX      ! oversized, but mehhh
      LOGICAL                          :: IS_GEOM1                      ! do we need to write the table
      LOGICAL                          :: IS_GEOM2                      ! do we need to write the table
      LOGICAL                          :: IS_GEOM3                      ! do we need to write the table
      LOGICAL                          :: IS_GEOM4                      ! do we need to write the table

      INTEGER(LONG)  :: NCTETRA, NCPENTA, NCHEXA
      INTEGER(LONG), DIMENSION(NCELAS1, 6)  :: CELAS1
      INTEGER(LONG), DIMENSION(NCELAS2, 6)  :: CELAS2
      INTEGER(LONG), DIMENSION(NCELAS3, 4)  :: CELAS3
      INTEGER(LONG), DIMENSION(NCELAS4, 4)  :: CELAS4
      INTEGER(LONG), DIMENSION(NCROD, 4)    :: CROD
      INTEGER(LONG), DIMENSION(NCTRIA3, 4)  :: CTRIA3
      INTEGER(LONG), DIMENSION(NCQUAD4, 4)  :: CQUAD4
      INTEGER(LONG), DIMENSION(NCSHEAR, 4)   :: CSHEAR
!      INTEGER(LONG), DIMENSION(:, :), allocatable :: CTETRA
!      INTEGER(LONG), DIMENSION(:, :), allocatable :: CPENTA
!      INTEGER(LONG), DIMENSION(:, :), allocatable  :: CHEXA

      ! initialize - GEOM1
      IS_GEOM1 = .FALSE.
      NGRID_ACTUAL = 0
      NSPOINT_ACTUAL = 0

      ! initialize - GEOM2
      IS_GEOM2 = .FALSE.
      
      ! initialize - GEOM2
      IS_GEOM3 = .FALSE.
      
      ! initialize - GEOM2
      IS_GEOM4 = .FALSE.
      
      !---------------------------------------------------------------------------------------------
      IF(NGRID > 0) THEN
        IS_GEOM1 = .TRUE.
      ENDIF

      IF((NCELAS1 > 0) .OR. (NCELAS2 > 0) .OR. (NCELAS3 > 0) .OR. (NCELAS4 > 0) .OR.        &
         (NCROD > 0) .OR. (NCBAR > 0) .OR. (NCBEAM > 0) .OR. (NCBUSH > 0) .OR.              &
         (NCSHEAR > 0) .OR. (NCTRIA3 > 0) .OR. (NCQUAD4 > 0) .OR.                           &
         (NCTETRA4 > 0) .OR. (NCTETRA10 > 0) .OR. (NCPENTA6 > 0) .OR. (NCPENTA15 > 0) .OR.  & 
         (NCHEXA8 > 0) .OR. (NCHEXA20 > 0)) THEN
        IS_GEOM2 = .TRUE.
      ENDIF

! Each row of GRID is for one grid point and contains:
!              (1) Grid point number    in col 1
!              (2) Input coord system   in col 2
!              (3) Global coord system  in col 3
!              (4) Permanent SPC's      in col 4
!              (5) Line break indicator in col 5 (put this many line breaks in F06 after this grid number)
!              (6) Num of comps         in col 6 (1 for SPOINT, 6 for actual grid)
!
!            The array is sorted in the following order:
!              (1) After the B.D. deck is read it is in GRID input order
!              (2) After subr GRID_PROC it is in grid point numerical order
!
! Each row of RGRID is for one grid point and contains:
!          After Bulk Data has been read: the 3 coords of the grid in the coord sys defined in col 2 of array GRID for this G.P.
!          After subr GRID_PROC has run : the 3 coords of the grid in the basic (0) coord sys of the model
 1    FORMAT("****DEBUG:   WRITE_OP2GEOM ngrid",i4)
      IF (IS_GEOM1) THEN
        WRITE(ERR,1) NGRID
      
        TABLE_NAME = "GEOM1"
        CALL WRITE_OP2_GEOM_HEADER(TABLE_NAME, ITABLE)

        ! identify the GRIDs vs. the SPOINTs
        DO I=1,NGRID
          IF (GRID(I,6) == 1) THEN
            NSPOINT_ACTUAL = NSPOINT_ACTUAL + 1
            SPOINT_INDEX(NSPOINT_ACTUAL) = I
          ELSE
            NGRID_ACTUAL = NGRID_ACTUAL + 1
            GRID_INDEX(NGRID_ACTUAL) = I
          ENDIF
        ENDDO

        CALL WRITE_OP2_GEOM1_GRID(ITABLE, NGRID_ACTUAL, GRID_INDEX)
        CALL WRITE_OP2_GEOM1_CORD(ITABLE)

        !DO I=1,NGRID
          ! is GRID_ID(I) the same as GRID(I,1)???
          !
          !(nid, cp, x1, x2, x3, cd, ps, seid)
          !                        nid        cp         x                   y                   z                  
        !  WRITE(ERR,2) GRID_ID(I), GRID(I,1), GRID(I,2), REAL(RGRID(I,1),4), REAL(RGRID(I,2),4), REAL(RGRID(I,3),4), &
          !            cd         ps         ndof
        !               GRID(I,3), GRID(I,4), GRID(I,6)
        !ENDDO
        CALL END_OP2_GEOM_TABLE(ITABLE)
      ENDIF
      
      !CHARACTER( 8*BYTE), ALLOCATABLE :: ETYPE(:)    ! NELE  x 1 array of elem types
      !CHARACTER( 1*BYTE), ALLOCATABLE :: EOFF(:)     ! NELE  x 1 array of 'Y' for elem offsets or 'N' if not

      !INTEGER(LONG)     , ALLOCATABLE :: EDAT(:)     ! NEDAT x 1 array of elem connection data
      !INTEGER(LONG)     , ALLOCATABLE :: EPNT(:)     ! NELE  x 1 array of pointers to EDAT where data begins for an elem
      
      !DO I=1,NEDAT
      !ENDDO

      NCTETRA = NCTETRA4 + NCTETRA10
      NCPENTA = NCPENTA6 + NCPENTA15
      NCHEXA = NCHEXA8 + NCHEXA20

      IS_GEOM2 = .FALSE.
      IF (IS_GEOM2) THEN
!        ALLOCATE ( CTETRA(NCTETRA,12) )      
!        ALLOCATE ( CPENTA(NCPENTA,17) )      
!        ALLOCATE ( CHEXA(NCHEXA,22) )      
        
        CALL GET_GEOM2(CELAS1, CELAS2, CELAS3, CELAS4, &
                       CROD,                           &
                       CTRIA3, CQUAD4, CSHEAR,         &
!                       CTETRA, CPENTA, CHEXA,          &
                       NCTETRA, NCPENTA, NCHEXA)

        TABLE_NAME = "GEOM2"
        CALL WRITE_OP2_GEOM_HEADER(TABLE_NAME, ITABLE)

        CALL WRITE_OP2_GEOM2_CROD(ITABLE, CROD)
        !CALL WRITE_OP2_GEOM2_CELAS1(ITABLE, CELAS1)
        !CALL WRITE_OP2_GEOM2_CELAS2(ITABLE, CELAS2)
        !CALL WRITE_OP2_GEOM2_CELAS3(ITABLE, CELAS3)
        !CALL WRITE_OP2_GEOM2_CELAS4(ITABLE, CELAS4)
        !CALL WRITE_OP2_GEOM2_CTETRA(ITABLE, CTETRA, NCTETRA)
      
!       CORD2S_INDEX(1000000)   ! intentional crash
        CALL END_OP2_GEOM_TABLE(ITABLE)
!        DEALLOCATE (CTETRA)
!        DEALLOCATE (CPENTA)
!        DEALLOCATE (CHEXA)
      ENDIF

      !  PELAS  = Array of integer data from PELAS Bulk Data entries. Each row is for one PELAS entry read in B.D. and contains:
      !             ( 1) Col  1: Property ID
      !
      !  RPELAS = Array of real data from PELAS Bulk Data entries. Each row is for one PELAS entry read in B.D. and contains:
      !             ( 1) Col  1: Spring rate, K
      !             ( 2) Col  2: Damping coefficient, GE
      !             ( 3) Col  3: Stress recovery coefficient, S


      !  PROD   = Array of integer data from PROD  Bulk Data entries. Each row is for one PROD  entry read in B.D. and contains:
      !             ( 1) Col  1: Property ID
      !             ( 2) Col  2: Material ID
      !
      !  RPROD  = Array of real data from PROD  Bulk Data entries. Each row is for one PROD  entry read in B.D. and contains:
      !             ( 1) Col  1: Cross sectional area, A
      !             ( 2) Col  2: Torsion constant, J
      !             ( 3) Col  3: Torsion stress recov. coeff, C
      !             ( 4) Col  4: Non structural mass, NSM
      END SUBROUTINE WRITE_OP2_GEOM

!===================================================================================================================================
      SUBROUTINE GET_GEOM2(CELAS1, CELAS2, CELAS3, CELAS4, &
                     CROD,                                 &
                     CTRIA3, CQUAD4, CSHEAR,               &
!                     CTETRA, CPENTA, CHEXA,                &
                     NCTETRA, NCPENTA, NCHEXA)
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      USE IOUNT1, ONLY                :  ERR, OP2
      USE SCONTR, ONLY : NCTETRA4, NCTETRA10, NCPENTA6, NCPENTA15, NCHEXA8, NCHEXA20
      USE SCONTR, ONLY : NCQUAD4, NCQUAD4K, NCSHEAR, NCTRIA3, NCTRIA3K
      USE SCONTR, ONLY : NCROD, NCBAR, NCBEAM, NCBUSH, NCELAS1, NCELAS2, NCELAS3, NCELAS4
      USE SCONTR, ONLY : NCMASS, NCONM2
      USE SCONTR, ONLY     : NELE, NEDAT
      USE MODEL_STUF, ONLY : ETYPE, EOFF, EPNT, EDAT

      IMPLICIT NONE
      INTEGER(LONG)  :: NCTETRA, NCPENTA, NCHEXA
      INTEGER(LONG) :: I = 0               ! counter
      INTEGER(LONG) :: J                   ! counter

      INTEGER(LONG) :: ICELAS1 = 0         ! loop counter
      INTEGER(LONG) :: ICELAS2 = 0         ! loop counter
      INTEGER(LONG) :: ICELAS3 = 0         ! loop counter
      INTEGER(LONG) :: ICELAS4 = 0         ! loop counter

      INTEGER(LONG) :: ICROD = 0           ! loop counter
      INTEGER(LONG) :: ICBAR = 0           ! loop counter
      INTEGER(LONG) :: ICBEAM = 0          ! loop counter

      INTEGER(LONG) :: ICQUAD4 = 0         ! loop counter
      INTEGER(LONG) :: ICTRIA3 = 0         ! loop counter
      INTEGER(LONG) :: ICSHEAR = 0         ! loop counter

      INTEGER(LONG) :: ICTETRA = 0         ! loop counter
      INTEGER(LONG) :: ICPENTA = 0         ! loop counter
      INTEGER(LONG) :: ICHEXA = 0          ! loop counter
      INTEGER(LONG)  :: EPNTK

      INTEGER(LONG), DIMENSION(NCELAS1, 6)  :: CELAS1
      INTEGER(LONG), DIMENSION(NCELAS2, 6)  :: CELAS2
      INTEGER(LONG), DIMENSION(NCELAS3, 4)  :: CELAS3
      INTEGER(LONG), DIMENSION(NCELAS4, 4)  :: CELAS4
      INTEGER(LONG), DIMENSION(NCROD, 4)    :: CROD

      INTEGER(LONG), DIMENSION(NCTRIA3, 4)  :: CTRIA3
      INTEGER(LONG), DIMENSION(NCQUAD4, 4)  :: CQUAD4
      INTEGER(LONG), DIMENSION(NCSHEAR, 4)   :: CSHEAR

!      INTEGER(LONG), DIMENSION(NCTETRA, 12) :: CTETRA
!      INTEGER(LONG), DIMENSION(NCPENTA, 17) :: CPENTA
!      INTEGER(LONG), DIMENSION(NCHEXA, 22)  :: CHEXA

 3    FORMAT(i8, "; ETYPE=", A, "; EOFF=",A, "; EPNT=", i8)
 4    FORMAT("  4 int fields: ", A, 4(" ", i4))
 6    FORMAT("  6 int fields: ", A, 6(" ", i4))
 8    FORMAT("  8 int fields: ", A, 8(" ", i4))
10    FORMAT(" 10 int fields: ", A, 10(" ", i4))
12    FORMAT(" 12 int fields: ", A, 12(" ", i4))
      DO I=1,NELE
        WRITE(ERR,3) I, ETYPE(I), EOFF(I), EPNT(I)
        EPNTK = EPNT(I)

        !EID  = EDAT(EPNTK)
        !PID  = EDAT(EPNTK+1)
        IF      (ETYPE(I)(1:5) .EQ. 'ELAS1') THEN  
          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,6)
          ! 6 fields
          ICELAS1 = ICELAS1 + 1
          DO J=1,5
            CELAS1(ICELAS1, J) = EDAT(EPNTK-1+J)
          ENDDO
        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS2') THEN
          ! 6 fields
          ICELAS2 = ICELAS2 + 1
          DO J=1,6
            CELAS2(ICELAS2, J) = EDAT(EPNTK-1+J)
          ENDDO
          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,6)

        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS3') THEN  
          ! 4 fields
          ICELAS3 = ICELAS3 + 1
          DO J=1,4
            CELAS3(ICELAS3, J) = EDAT(EPNTK-1+J)
          ENDDO
          WRITE(ERR,4) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,4)
        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS4') THEN  
          ! 4 fields
          ICELAS4 = ICELAS4 + 1
          DO J=1,4
            CELAS4(ICELAS4, J) = EDAT(EPNTK-1+J)
          ENDDO
          WRITE(ERR,4) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,4)

        ELSE IF (ETYPE(I)(1:3) .EQ. 'ROD') THEN  
          ! 4 fields
          ICROD = ICROD + 1
          DO J=1,4
            CROD(ICROD, J) = EDAT(EPNTK-1+J)
          ENDDO
          WRITE(ERR,4) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,4)

        ELSE IF (ETYPE(I)(1:5) .EQ. 'SHEAR') THEN  
          ! 6 fields
          ICSHEAR = ICSHEAR + 1
          DO J=1,6
            CSHEAR(ICSHEAR, J) = EDAT(EPNTK-1+J)
          ENDDO
          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,6)

!        ELSE IF (ETYPE(I)(1:5) .EQ. 'TETRA4') THEN  
!          ! 6 fields
!          ICTETRA = ICTETRA + 1
!          DO J=1,6
!            CTETRA(ICTETRA, J) = EDAT(EPNTK-1+J)
!          ENDDO
!          DO J=7,12
!            CTETRA(ICTETRA, J) = 0
!          ENDDO
!          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,12)
!        ELSE IF (ETYPE(I)(1:5) .EQ. 'TETRA10') THEN
!          ! 12 fields
!          ICTETRA = ICTETRA + 1
!          DO J=1,12
!            CTETRA(ICTETRA, J) = EDAT(EPNTK-1+J)
!          ENDDO
!          WRITE(ERR,12) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,12)  
        ENDIF
        !EPNT(I)
      ENDDO
      END SUBROUTINE GET_GEOM2


!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM_HEADER(TABLE_NAME, ITABLE)
      USE PENTIUM_II_KIND, ONLY         :  LONG, BYTE
      USE IOUNT1, ONLY                  :  OP2
      IMPLICIT NONE
      CHARACTER(LEN=8*BYTE),INTENT(IN)  :: TABLE_NAME
      INTEGER(LONG),INTENT(INOUT)       :: ITABLE

      CALL WRITE_TABLE_HEADER(TABLE_NAME)
      !WRITE(OP2) 7
      !WRITE(OP2) 1, 2, 3, 4, 5, 6, 7

      !WRITE(OP2) -2
      !WRITE(OP2) 1
      !WRITE(OP2) 0

      !WRITE(OP2) 2
      !WRITE(OP2) 1, 2

      WRITE(OP2) -3
      WRITE(OP2) 1
      WRITE(OP2) 0
      ITABLE = -3
      END SUBROUTINE WRITE_OP2_GEOM_HEADER

      
!===================================================================================================================================
      SUBROUTINE END_OP2_GEOM_TABLE( ITABLE )
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      IMPLICIT NONE
      INTEGER(LONG),INTENT(INOUT)       :: ITABLE

      WRITE(OP2) 3
      WRITE(OP2) 1, 2, 3
      ITABLE = ITABLE - 1

      WRITE(OP2) ITABLE
      WRITE(OP2) 1
      WRITE(OP2) 0

      WRITE(OP2) 0
      ITABLE = ITABLE - 1
      END SUBROUTINE END_OP2_GEOM_TABLE
