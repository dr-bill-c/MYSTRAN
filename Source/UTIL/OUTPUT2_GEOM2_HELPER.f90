!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CROD(ITABLE, CROD)
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  ERR,OP2
      USE SCONTR, ONLY : NCROD
      IMPLICIT NONE
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG), DIMENSION(NCROD, 4), INTENT(IN) :: CROD
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 4                 ! helper flag
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCROD > 0) THEN
        NVALUES = NUM_WIDE * NCROD
        WRITE(OP2) NVALUES + 3
      
        ! ROD  4 words: (all read by call to subr ELEPO from subr BD_ROD1)
        !   1) Elem ID
        !   2) Prop ID
        !   3) Grid A
        !   4) Grid B
        WRITE(OP2) 3001, 30, 48, ((CROD(I,J), J=1,4), I=1,NCROD)
 4      FORMAT(" **WRITE_OP2_GEOM2_CROD: ", 8(i4, " "))     
        DO I=1,NCROD
          WRITE(ERR,4) (CROD(I,J), J=1,4)
        ENDDO
        ITABLE = ITABLE - 1
        WRITE(OP2) ITABLE
        WRITE(OP2) 1
        WRITE(OP2) 0
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CROD


!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CELAS1(ITABLE, CELAS1)
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      USE SCONTR, ONLY : NCELAS1
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCELAS1, 6)           :: CELAS1
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 6                 ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCELAS1 > 0) THEN
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
      END SUBROUTINE WRITE_OP2_GEOM2_CELAS1
      
!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CELAS2(ITABLE, CELAS2)
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      USE SCONTR, ONLY : NCELAS2
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCELAS2, 6)           :: CELAS2
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 6                 ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCELAS2 > 0) THEN
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
      END SUBROUTINE WRITE_OP2_GEOM2_CELAS2

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CELAS3(ITABLE, CELAS3)
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      USE SCONTR, ONLY : NCELAS3
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCELAS3, 4)           :: CELAS3
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 4                 ! helper flag
      IF (NCELAS3 > 0) THEN
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
      END SUBROUTINE WRITE_OP2_GEOM2_CELAS3

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CELAS4(ITABLE, CELAS4)
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      USE SCONTR, ONLY : NCELAS4
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCELAS4, 4)           :: CELAS4
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 4                 ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCELAS4 > 0) THEN
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
      END SUBROUTINE WRITE_OP2_GEOM2_CELAS4
!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CTETRA(ITABLE, CTETRA, NCTETRA)
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCTETRA, 12)          :: CTETRA
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NCTETRA, NVALUES             ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 12                ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCTETRA > 0) THEN
        NVALUES = NUM_WIDE * NCTETRA
        WRITE(OP2) NVALUES + 3
        
        ! TETRA4   6 words: (read by 1 or more calls to subr ELEPO from subr BD_TETRA)
        !   1) Elem ID
        !   2) Prop ID
        !   3) etc, Grids 1-4
      
        ! TETRA10  2 words: (read by 1 or more calls to subr ELEPO from subr BD_TETRA)
        !   1) Elem ID
        !   2) Prop ID
        !   3) etc, Grids 1-10
        WRITE(OP2) 5508, 55, 217, ((CTETRA(I,J), J=1,12), I=1,NCTETRA)
        !WRITE(OP2) 5508, 55, 217, & 
        !           ((TETRA4(I,J),  J=1,6), 0, 0, 0, 0, 0, 0, I=1,NCTETRA4), &
        !           ((TETRA10(I,J), J=1,12), I=1,NCTETRA10)
        ITABLE = ITABLE - 1
        WRITE(OP2) ITABLE
        WRITE(OP2) 1
        WRITE(OP2) 0
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CTETRA
!===================================================================================================================================
