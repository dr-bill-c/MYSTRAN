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
      USE SCONTR, ONLY                :  NGRID, NCORD
      USE MODEL_STUF, ONLY            :  GRID_ID, GRID, RGRID, CORD, RCORD
      IMPLICIT NONE
      INTEGER(LONG)                    :: ITABLE                        ! the subtable counter
      INTEGER(LONG)                    :: SEID                          ! superelment id
      INTEGER(LONG)                    :: I,J                           ! counter
      CHARACTER(LEN=8*BYTE)            :: TABLE_NAME                    ! the current op2 table name
      INTEGER(LONG)                    :: NGRID_ACTUAL, NSPOINT_ACTUAL  ! number of GRID, SPOINTs; corrects for oversizing
      INTEGER(LONG), DIMENSION(NGRID)  :: GRID_INDEX, SPOINT_INDEX      ! oversized, but mehhh
      LOGICAL                          :: IS_GEOM1                      ! do we need to write the table
      INTEGER(LONG)                    :: NUM_WIDE, NVALUES             ! helper flags
      INTEGER(LONG) :: NCORD1R, NCORD1C, NCORD1S, NCORD2R, NCORD2C, NCORD2S
      INTEGER(LONG), DIMENSION(NCORD)  :: CORD1R_INDEX, CORD1C_INDEX, CORD1S_INDEX, CORD2R_INDEX, CORD2C_INDEX, CORD2S_INDEX      ! oversized, but mehhh

      ! initialize
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
      
      IF(NGRID > 0) THEN
        IS_GEOM1 = .TRUE.
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
          WRITE(OP2) 4501, 45, 1,                                                                                     &
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
      END SUBROUTINE WRITE_OP2_GEOM

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
      WRITE(OP2) CODEA, CODEB, CODEC,                                                         &
                  (CORD(CORD1_INDEX(J),2), RCS_INT, COORD_INT,                               &
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
