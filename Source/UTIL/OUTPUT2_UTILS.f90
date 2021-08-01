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
      USE SCONTR, ONLY                :  NGRID, NCORD
      USE MODEL_STUF, ONLY            :  GRID_ID, GRID, RGRID, CORD, RCORD
      
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
      INTEGER(LONG)                    :: NUM_WIDE, NVALUES             ! helper flags
      INTEGER(LONG) :: NCORD1R, NCORD1C, NCORD1S, NCORD2R, NCORD2C, NCORD2S
      INTEGER(LONG), DIMENSION(NCORD)  :: CORD1R_INDEX, CORD1C_INDEX, CORD1S_INDEX, CORD2R_INDEX, CORD2C_INDEX, CORD2S_INDEX      ! oversized, but mehhh

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
      SEID = 0
      NGRID_ACTUAL = 0
      NSPOINT_ACTUAL = 0
      NCORD1R = 0
      NCORD1C = 0
      NCORD1S = 0
      NCORD2R = 0
      NCORD2C = 0
      NCORD2S = 0

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
 2    FORMAT("****DEBUG:   GRID_ID=",i4, "; ID=",i4, "; cp=", i4, "; x=", f8.3, "; y=", f8.3, "; z=", f8.3, &
             "; cd=", i4, "; PS=", i4,"; NDOF=",i4)
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

        IF(NGRID_ACTUAL > 0) THEN
          ! if there are GRIDs
          NUM_WIDE = 8
          NVALUES = NUM_WIDE * NGRID_ACTUAL
          
          DO J=1,NGRID_ACTUAL
            ! nid, cp, x, y, z, cd, ps, seid
            I = GRID_INDEX(J)
            WRITE(ERR,2) GRID_ID(I), GRID(I,1), GRID(I,2), REAL(RGRID(I,1),4), REAL(RGRID(I,2),4), REAL(RGRID(I,3),4), &
                         GRID(I,3), GRID(I,4), GRID(I,6)
          ENDDO
          ! oh dear...
          !
          ! the core of what we want is...
          !WRITE(OP2) (GRID_ID(I), GRID(I,2),                 &
          !            RGRID(I,1), RGRID(I,2), RGRID(I,3),    &
          !            GRID(I,3), GRID(I,4), SEID, J=1,NGRID_ACTUAL)
          !
          ! We then sub in I = GRID_INDEX(J), which is required because
          ! mystran stores the GRIDs and SPOINTs in a single array.
          !
          ! Then we wrap any doubles in REAL(*,4) to cast it as a float.
          !
          ! Finally we tack on the magic code (4501,45,1).
          ! Thanks to that code, we have to add 3 the nvalues...
          !
          WRITE(OP2) NVALUES + 3
          !           nid, cp, x, y, z, cd, ps, seid
          WRITE(OP2) 4501, 45, 1,                                                                                   &
                      (GRID_ID(GRID_INDEX(J)),         GRID(GRID_INDEX(J),2),                                         &
                      REAL(RGRID(GRID_INDEX(J),1),4), REAL(RGRID(GRID_INDEX(J),2),4), REAL(RGRID(GRID_INDEX(J),3),4), &
                      GRID(GRID_INDEX(J),3), GRID(GRID_INDEX(J),4), SEID, J=1,NGRID_ACTUAL)
          
          ! -4
          ITABLE = ITABLE - 1
          WRITE(OP2) ITABLE
          WRITE(OP2) 1
          WRITE(OP2) 0
        ENDIF
        
        ! Each row of CORD, for a CORD1R,C,S has:
        !    Col. 1: Coord type
        !            11 for CORD1R
        !            12 for CORD1C
        !            13 for CORD1S
        !    Col. 2: CID = Coord system ID
        !    Col. 3: RID = Reference coord system ID for Grid A
        !    Col. 4: RID = Reference coord system ID for Grid B
        !    Col. 5: RID = Reference coord system ID for Grid C
        !
        ! Each row of CORD, for a CORD2R,C,S has the same as for CORD1 above but only needs 1 reference system:
        !    Col. 1: Coord type
        !            21 for CORD2R
        !            22 for CORD2C
        !            23 for CORD2S
        !    Col. 2: CID = Coord system ID
        !    Col. 3: RID = Reference coord system ID for this CORD2R,C,S
        !
        ! Each row of RCORD has:
        !   After Bulk Data has been read:
        !     Col.  1: X coord of Pt A in RID coord system 
        !     Col.  2: Y coord of Pt A in RID coord system  
        !     Col.  3: Z coord of Pt A in RID coord system  
        !     Col.  4: X coord of Pt B in RID coord system  
        !     Col.  5: Y coord of Pt B in RID coord system  
        !     Col.  6: Z coord of Pt B in RID coord system  
        !     Col.  7: X coord of Pt C in RID coord system  
        !     Col.  8: Y coord of Pt C in RID coord system  
        !     Col.  9: Z coord of Pt C in RID coord system  
        !     Col. 10: 0.
        !     Col. 11: 0.
        !     Col. 12: 0.
        !   
        !   After subr CORD_PROC has run:
        !     Col.  1: Location of CID origin in basic coord sys X dir
        !     Col.  2: Location of CID origin in basic coord sys Y dir 
        !     Col.  3: Location of CID origin in basic coord sys Z dir 
        !     Col.  4: TN(1,1,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
        !     Col.  5: TN(1,2,M) for M = row no. in RCORD (internal coord. sys. ID = M)  |- 1st row of TN in cols  4- 6 of RCORD
        !     Col.  6: TN(1,3,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
        !     Col.  7: TN(2,1,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
        !     Col.  8: TN(2,2,M) for M = row no. in RCORD (internal coord. sys. ID = M)  |- 2nd row of TN in cols  7- 9 of RCORD
        !     Col.  9: TN(2,3,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
        !     Col. 10: TN(3,1,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]
        !     Col. 11: TN(3,2,M) for M = row no. in RCORD (internal coord. sys. ID = M)  |- 3rd row of TN in cols 10-12 of RCORD
        !     Col. 12: TN(3,3,M) for M = row no. in RCORD (internal coord. sys. ID = M)  ]


        ! Each row of CORD, for a CORD1R,C,S has:
        !    Col. 1: Coord type
        !            11 for CORD1R
        !            12 for CORD1C
        !            13 for CORD1S
        !    Col. 2: CID = Coord system ID
        !    Col. 3: RID = Reference coord system ID for Grid A
        !    Col. 4: RID = Reference coord system ID for Grid B
        !    Col. 5: RID = Reference coord system ID for Grid C
        !
        ! Each row of CORD, for a CORD2R,C,S has the same as for CORD1 above but only needs 1 reference system:
        !    Col. 1: Coord type
        !            21 for CORD2R
        !            22 for CORD2C
        !            23 for CORD2S
        !    Col. 2: CID = Coord system ID
        !    Col. 3: RID = Reference coord system ID for this CORD2R,C,S
        !        ! identify the GRIDs vs. the SPOINTs
        
        DO I=1,NCORD
          IF (CORD(I,1) == 11) THEN
            NCORD1R = NCORD1R + 1
            CORD1R_INDEX(NCORD1R) = I
          ELSE IF (CORD(I,1) == 12) THEN
            NCORD1C = NCORD1C + 1
            CORD1C_INDEX(NCORD1C) = I
          ELSE IF (CORD(I,1) == 13) THEN
            NCORD1S = NCORD1S + 1
            CORD1S_INDEX(NCORD1S) = I

          ELSE IF (CORD(I,1) == 21) THEN
            NCORD2R = NCORD2R + 1
            CORD2R_INDEX(NCORD2R) = I
          ELSE IF (CORD(I,1) == 22) THEN
            NCORD2C = NCORD2C + 1
            CORD2C_INDEX(NCORD2C) = I 
          ELSE IF (CORD(I,1) == 23) THEN
            NCORD2S = NCORD2S + 1
            CORD2S_INDEX(NCORD2S) = I
          ENDIF
        ENDDO


        ! CORD1C : (1701, 17, 6)
        ! CORD1R : (1801, 18, 5)
        ! CORD1S : (1901, 19, 7)
        IF(NCORD1R > 0) THEN
            CALL WRITE_OP2_CORD1(ITABLE, NCORD1R, CORD1R_INDEX, 1, 1801, 18, 5)
        ENDIF
        IF(NCORD1C > 0) THEN
            CALL WRITE_OP2_CORD1(ITABLE, NCORD1C, CORD1C_INDEX, 2, 1701, 17, 6)
        ENDIF
        IF(NCORD1S > 0) THEN
            CALL WRITE_OP2_CORD1(ITABLE, NCORD1S, CORD1S_INDEX, 3, 1901, 19, 7)
        ENDIF

        ! CORD2C : (2001, 20, 9)
        ! CORD2R : (2101, 21, 8)
        ! CORD2S : (2201, 22, 10)
        IF(NCORD2R > 0) THEN
            CALL WRITE_OP2_CORD2(ITABLE, NCORD2R, CORD2R_INDEX, 1, 2101, 21, 8)
        ENDIF
        IF(NCORD2C > 0) THEN
            CALL WRITE_OP2_CORD2(ITABLE, NCORD2C, CORD2C_INDEX, 2, 2001, 20, 9)
        ENDIF
        IF(NCORD2S > 0) THEN
            CALL WRITE_OP2_CORD2(ITABLE, NCORD2S, CORD2S_INDEX, 3, 2201, 22, 10)
        ENDIF

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

        !IF (NCROD > 0) THEN
        !  NUM_WIDE = 4
        !  NVALUES = NUM_WIDE * NCROD
        !  WRITE(OP2) NVALUES + 3
        !
        !  ! ROD  4 words: (all read by call to subr ELEPO from subr BD_ROD1)
        !  !   1) Elem ID
        !  !   2) Prop ID
        !  !   3) Grid A
        !  !   4) Grid B
        !  WRITE(OP2) 3001, 30, 48, ((CROD(I,J), J=1,4), I=1,NCROD)
        !  ITABLE = ITABLE - 1
        !  WRITE(OP2) ITABLE
        !  WRITE(OP2) 1
        !  WRITE(OP2) 0
        !ENDIF
      
        IF (NCELAS1 > 0) THEN
          NUM_WIDE = 6
          NVALUES = NUM_WIDE * NCELAS1
          WRITE(OP2) NVALUES + 3
      
          ! ELAS1  6 words: (all read by call to subr ELEPO from subr BD_CELAS1)
          !   1) Elem ID
          !   2) Prop ID
          !   3) Grid A
          !   4) Grid B
          !   5) Components at Grid A
          !   6) Components at Grid B
          WRITE(OP2) 601, 6, 73, ((CELAS1(I,J), J=1,6), I=1,NCELAS1)
          ITABLE = ITABLE - 1
          WRITE(OP2) ITABLE
          WRITE(OP2) 1
          WRITE(OP2) 0
        ENDIF
      
        IF (NCELAS2 > 0) THEN
          NUM_WIDE = 6
          NVALUES = NUM_WIDE * NCELAS2
          WRITE(OP2) NVALUES + 3
      
          ! ELAS2  6 words: (all read by call to subr ELEPO from subr BD_CELAS2)
          !   1) Elem ID
          !   2) Prop ID which is set to -EID since real props are on the CELAS2 entry
          !   3) Grid A
          !   4) Grid B
          !   5) Components at Grid A
          !   6) Components at Grid B
          WRITE(OP2) 701, 7, 74, ((CELAS2(I,J), J=1,6), I=1,NCELAS2)
          ITABLE = ITABLE - 1
          WRITE(OP2) ITABLE
          WRITE(OP2) 1
          WRITE(OP2) 0
        ENDIF
      
        IF (NCELAS3 > 0) THEN
          NUM_WIDE = 4
          NVALUES = NUM_WIDE * NCELAS3
          WRITE(OP2) NVALUES + 3
      
          ! ELAS3  4 words: (all read by call to subr ELEPO from subr BD_CELAS3)
          !   1) Elem ID
          !   2) Prop ID
          !   3) Scalar point A
          !   4) Scalar point B
          WRITE(OP2) 801, 8, 75, ((CELAS3(I,J), J=1,4), I=1,NCELAS3)
          ITABLE = ITABLE - 1
          WRITE(OP2) ITABLE
          WRITE(OP2) 1
          WRITE(OP2) 0
        ENDIF
      
        IF (NCELAS4 > 0) THEN
          NUM_WIDE = 4
          NVALUES = NUM_WIDE * NCELAS4
          WRITE(OP2) NVALUES + 3
      
          ! ELAS4  4 words: (all read by call to subr ELEPO from subr BD_CELAS4)
          !   1) Elem ID
          !   2) Prop ID which is set to -EID since real props are on the CELAS2 entry
          !   3) Scalar point A
          !   4) Scalar point B
          WRITE(OP2) 901, 9, 76, ((CELAS4(I,J), J=1,4), I=1,NCELAS4)
          ITABLE = ITABLE - 1
          WRITE(OP2) ITABLE
          WRITE(OP2) 1
          WRITE(OP2) 0
        ENDIF
      
        !IF (NCTETRA4+NCTETRA10 > 0) THEN
        !  NUM_WIDE = 12
        !  NVALUES = NUM_WIDE * (NCTETRA4 + NCTETRA10)
        !  WRITE(OP2) NVALUES + 3
        !  
        !  ! TETRA4   6 words: (read by 1 or more calls to subr ELEPO from subr BD_TETRA)
        !  !   1) Elem ID
        !  !   2) Prop ID
        !  !   3) etc, Grids 1-4
        !
        !  ! TETRA10  2 words: (read by 1 or more calls to subr ELEPO from subr BD_TETRA)
        !  !   1) Elem ID
        !  !   2) Prop ID
        !  !   3) etc, Grids 1-10
        !  WRITE(OP2) 5508, 55, 217, ((CTETRA(I,J), J=1,12), I=1,NCTETRA10)
        !  !WRITE(OP2) 5508, 55, 217, & 
        !  !           ((TETRA4(I,J),  J=1,6), 0, 0, 0, 0, 0, 0, I=1,NCTETRA4), &
        !  !           ((TETRA10(I,J), J=1,12), I=1,NCTETRA10)
        !  ITABLE = ITABLE - 1
        !  WRITE(OP2) ITABLE
        !  WRITE(OP2) 1
        !  WRITE(OP2) 0
        !ENDIF
      
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
          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK+J), J=0,5)
          ! 6 fields
          ICELAS1 = ICELAS1 + 1
          DO J=1,5
            CELAS1(ICELAS1, I) = EDAT(EPNTK-1+J)
          ENDDO
        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS2') THEN
          ! 6 fields
          ICELAS2 = ICELAS2 + 1
          DO J=1,6
            CELAS2(ICELAS2, I) = EDAT(EPNTK-1+J)
          ENDDO
          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK+J), J=0,5)

        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS3') THEN  
          ! 4 fields
          ICELAS3 = ICELAS3 + 1
          DO J=1,4
            CELAS3(ICELAS3, I) = EDAT(EPNTK-1+J)
          ENDDO
          WRITE(ERR,4) ETYPE(I)(1:5), (EDAT(EPNTK+J), J=0,3)
        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS4') THEN  
          ! 4 fields
          ICELAS4 = ICELAS4 + 1
          DO J=1,4
            CELAS4(ICELAS4, I) = EDAT(EPNTK-1+J)
          ENDDO
          WRITE(ERR,4) ETYPE(I)(1:5), (EDAT(EPNTK+J), J=0,3)

!        ELSE IF (ETYPE(I)(1:3) .EQ. 'ROD') THEN  
!          ! 4 fields
!          ICROD = ICROD + 1
!          DO J=1,4
!            CROD(ICROD, I) = EDAT(EPNTK-1+J)
!          ENDDO
!          WRITE(ERR,4) ETYPE(I)(1:5), (EDAT(EPNTK+J), J=0,3)

        !ELSE IF (ETYPE(I)(1:5) .EQ. 'SHEAR') THEN  
        !  ! 6 fields
        !  ICSHEAR = ICSHEAR + 1
        !  DO J=1,4
        !    CSHEAR(ICSHEAR, I) = EDAT(EPNTK-1+J)
        !  ENDDO
        !  WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK+J), J=0,3)

!        ELSE IF (ETYPE(I)(1:5) .EQ. 'TETRA4') THEN  
!          ! 6 fields
!          ICTETRA = ICTETRA + 1
!          DO J=1,6
!            CTETRA(ICTETRA, I) = EDAT(EPNTK-1+J)
!          ENDDO
!          DO J=7,12
!            CTETRA(ICTETRA, I) = 0
!          ENDDO
!          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK+J), J=0,11)
!        ELSE IF (ETYPE(I)(1:5) .EQ. 'TETRA10') THEN
!          ! 12 fields
!          ICTETRA = ICTETRA + 1
!          DO J=1,12
!            CTETRA(ICTETRA, I) = EDAT(EPNTK-1+J)
!          ENDDO
!          WRITE(ERR,12) ETYPE(I)(1:5), (EDAT(EPNTK+J), J=0,11)  
        ENDIF
        !EPNT(I)
      ENDDO
      END SUBROUTINE GET_GEOM2

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_CORD1(ITABLE, NCORD1, CORD1_INDEX, RCS_INT, CODEA, CODEB, CODEC)
      !! TODO: this is probably very wrong...
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  OP2
      USE SCONTR, ONLY                :  NCORD
      USE MODEL_STUF, ONLY            :  CORD, RCORD
      IMPLICIT NONE
      INTEGER(LONG), INTENT(INOUT)                 :: ITABLE                        ! the subtable counter
      INTEGER(LONG), INTENT(IN)                    :: NCORD1                        ! the number of CORD1x cards
      INTEGER(LONG), DIMENSION(NCORD), INTENT(IN)  :: CORD1_INDEX                   ! oversized, but mehhh
      INTEGER(LONG), INTENT(IN)                    :: RCS_INT                       ! R, C, or S
      INTEGER(LONG), INTENT(IN)                    :: CODEA, CODEB, CODEC           ! the CORD2x codes
      INTEGER(LONG)                                :: I,J                           ! counter
      INTEGER(LONG)                                :: NUM_WIDE, NVALUES             ! helper flags
      INTEGER(LONG)                                :: COORD_INT                     ! CORD1x

      ! if there are NCORD1Rs
      NVALUES = 6 * NCORD1
      COORD_INT = 1
      WRITE(OP2) NVALUES + 3
!          Col. 1: Coord type
!                  11 for CORD1R
!                  12 for CORD1C
!                  13 for CORD1S
!          Col. 2: CID = Coord system ID
!          Col. 3: RID = Reference coord system ID for Grid A
!          Col. 4: RID = Reference coord system ID for Grid B
!          Col. 5: RID = Reference coord system ID for Grid C

      ! [cid, coord_rcs_int, coord_int, g1, g2, g3]
      ! TODO: is this g1 or g1.rid ???
      WRITE(OP2) CODEA, CODEB, CODEC,                                                      &
                  (CORD(CORD1_INDEX(J),2), RCS_INT, COORD_INT,                             &
                  CORD(CORD1_INDEX(J),3), CORD(CORD1_INDEX(J),3), CORD(CORD1_INDEX(J),3),  &
                  J=1,NCORD1)
      ! -4
      ITABLE = ITABLE - 1
      WRITE(OP2) ITABLE
      WRITE(OP2) 1
      WRITE(OP2) 0
      END SUBROUTINE WRITE_OP2_CORD1

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_CORD2(ITABLE, NCORD2, CORD2_INDEX, RCS_INT, CODEA, CODEB, CODEC)
      ! needs a docstring
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  OP2
      USE SCONTR, ONLY                :  NCORD
      USE MODEL_STUF, ONLY            :  CORD, RCORD
      IMPLICIT NONE
      INTEGER(LONG), INTENT(INOUT)                 :: ITABLE                        ! the subtable counter
      INTEGER(LONG), INTENT(IN)                    :: NCORD2                        ! the number of CORD2x cards
      INTEGER(LONG), DIMENSION(NCORD), INTENT(IN)  :: CORD2_INDEX                   ! oversized, but mehhh
      INTEGER(LONG), INTENT(IN)                    :: RCS_INT                       ! R, C, or S
      INTEGER(LONG), INTENT(IN)                    :: CODEA, CODEB, CODEC           ! the CORD2x codes
      INTEGER(LONG)                                :: I,J                           ! counter
      INTEGER(LONG)                                :: NUM_WIDE, NVALUES             ! helper flags
      INTEGER(LONG)                                :: COORD_INT                     ! CORD2x

      ! if there are CORD2Rs
      NVALUES = 13 * NCORD2
      COORD_INT = 2
      WRITE(OP2) NVALUES + 3
      ! [cid, coord_rcs_int, coord_int, rid, e1, e2, e3]
      !
      ! Col. 2: CID = Coord system ID
      ! Col. 3: RID = Reference coord system ID for this CORD2R,C,S
      ! Col.  1: X coord of Pt A in RID coord system 
      ! Col.  2: Y coord of Pt A in RID coord system  
      ! Col.  3: Z coord of Pt A in RID coord system  
      ! Col.  4: X coord of Pt B in RID coord system  
      ! Col.  5: Y coord of Pt B in RID coord system  
      ! Col.  6: Z coord of Pt B in RID coord system  
      ! Col.  7: X coord of Pt C in RID coord system  
      ! Col.  8: Y coord of Pt C in RID coord system  
      ! Col.  9: Z coord of Pt C in RID coord system
      
      !! TODO: we want this to be written before CORD_MAP or whatever is called...
      WRITE(OP2) CODEA, CODEB, CODEC,                                                                                &
                  (CORD(CORD2_INDEX(J),2), RCS_INT, COORD_INT, CORD(CORD2_INDEX(J),3),                               &
                  REAL(RCORD(CORD2_INDEX(J),1),4), REAL(RCORD(CORD2_INDEX(J),2),4), REAL(RCORD(CORD2_INDEX(J),3),4), &
                  REAL(RCORD(CORD2_INDEX(J),4),4), REAL(RCORD(CORD2_INDEX(J),5),4), REAL(RCORD(CORD2_INDEX(J),6),4), &
                  REAL(RCORD(CORD2_INDEX(J),7),4), REAL(RCORD(CORD2_INDEX(J),8),4), REAL(RCORD(CORD2_INDEX(J),9),4), &
                  J=1,NCORD2)
      ! -4
      ITABLE = ITABLE - 1
      WRITE(OP2) ITABLE
      WRITE(OP2) 1
      WRITE(OP2) 0
      END SUBROUTINE WRITE_OP2_CORD2


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
