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
