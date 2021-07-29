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
      USE SCONTR, ONLY                :  NGRID
      USE MODEL_STUF, ONLY            :  GRID_ID, GRID, RGRID
      IMPLICIT NONE
      INTEGER(LONG)                    :: ITABLE                        ! the subtable counter
      INTEGER(LONG)                    :: SEID                          ! superelment id
      INTEGER(LONG)                    :: I,J                           ! counter
      CHARACTER(LEN=8*BYTE)            :: TABLE_NAME                    ! the current op2 table name
      INTEGER(LONG)                    :: NGRID_ACTUAL, NSPOINT_ACTUAL  ! number of GRID, SPOINTs; corrects for oversizing
      INTEGER(LONG), DIMENSION(NGRID)  :: GRID_INDEX, SPOINT_INDEX      ! oversized, but mehhh
      LOGICAL                          :: IS_GEOM1                      ! do we need to write the table
      INTEGER(LONG)                    :: NUM_WIDE, NVALUES             ! helper flags
 
      ! initialize
      IS_GEOM1 = .FALSE.
      SEID = 0
      NGRID_ACTUAL = 0
      NSPOINT_ACTUAL = 0
      
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

        IF(NSPOINT_ACTUAL > 0) THEN
          ! if there are SPOINTs
          !WRITE(OP2) NSPOINT_ACTUAL
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
