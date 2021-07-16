

!==================================================================================================
!      SUBROUTINE WRITE_STRESS_TABLE_HEADER()
!      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
!      !INTEGER(LONG), INTENT(IN) :: ITABLE ! the subtable id
!      CHARACTER(8*BYTE) :: TABLE_NAME
!      TABLE_NAME = 'OES1    '
!      CALL WRITE_TABLE_HEADER(TABLE_NAME)
!      END SUBROUTINE WRITE_STRESS_TABLE_HEADER

!==================================================================================================
!      SUBROUTINE WRITE_STRAIN_TABLE_HEADER()
!      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      !INTEGER(LONG), INTENT(IN) :: ITABLE ! the subtable id
!      CHARACTER(8*BYTE) :: TABLE_NAME
!      TABLE_NAME = 'OSTR1   '
!      CALL WRITE_TABLE_HEADER(TABLE_NAME)
!      END SUBROUTINE WRITE_STRAIN_TABLE_HEADER

!==================================================================================================
      SUBROUTINE SET_OES_TABLE_NAME (ETYPE, TABLE_NAME, ITABLE)
      ! initializes the OES table name
      ! updates ITABLE and TABLE_NAME
      !
      ! TODO: mak sure the TABLE_NAME_NEW is oorrect...
      !
      USE PENTIUM_II_KIND, ONLY        :  BYTE, LONG
      USE IOUNT1, ONLY                 :  ERR

      CHARACTER(8*BYTE), INTENT(IN)    :: ETYPE          ! name of element type
      CHARACTER(8*BYTE), INTENT(INOUT) :: TABLE_NAME     ! name of the op2 table name
      CHARACTER(8*BYTE)                :: TABLE_NAME_NEW ! name of the op2 table name
      INTEGER(LONG)                    :: ITABLE         ! the subtable
 1    FORMAT("*DEBUG:      OUTPUT2_WRITE_STRESS: ", A)
 2    FORMAT("*DEBUG:      OUTPUT2_WRITE_STRESS: ", A, "ITABLE", I8)
 3    FORMAT("*DEBUG:      OUTPUT2_WRITE_STRESS: ", A, A)
      IF      ((ETYPE == 'BAR     ') .OR. (ETYPE == 'BEAM    ')) THEN
        TABLE_NAME_NEW= "OES1X   "
      ELSE IF ((ETYPE == 'ELAS1   ') .OR. (ETYPE == 'ELAS2   ') .OR. (ETYPE == 'ELAS3   ') .OR. (ETYPE == 'ELAS4   ') .OR.         &
               (ETYPE == 'TRIA3   ') .OR. (ETYPE == 'QUAD4   ') .OR. (ETYPE == 'SHEAR   ') .OR. (ETYPE == 'SHEAR   ') .OR.         &
               (ETYPE == 'HEXA8   ') .OR. (ETYPE == 'PENTA6  ') .OR. (ETYPE == 'TETRA4  ') .OR.                                    &
               (ETYPE == 'HEXA20  ') .OR. (ETYPE == 'PENTA15 ') .OR. (ETYPE == 'TETRA10 ') .OR.                                    &
               (ETYPE == 'BUSH    ') .OR. (ETYPE == 'QUAD4   ') .OR. (ETYPE == 'SHEAR   ')) THEN                                     
        TABLE_NAME_NEW= "OES1X1  "
        WRITE(ERR,1) "OES1X1 found"
      ELSE
        WRITE(ERR,1) "ERROR STATE"
        ! we're now in an error state
        ! also let's close the old table
        TABLE_NAME_NEW= "OES ERR "
        IF (ITABLE < -1) THEN
          WRITE(ERR,2) "closing stress table", ITABLE
          CALL END_OP2_TABLE(ITABLE)   ! close the previous
        ENDIF
        WRITE(ERR,2) "invalidating tableA",ITABLE
        ITABLE = 0
      ENDIF

      IF (TABLE_NAME /= TABLE_NAME_NEW) THEN
      ! first let's find out if we need to close off the previous table
        IF (ITABLE < -1) THEN
          WRITE(ERR,2) "closing stress table",ITABLE
          CALL END_OP2_TABLE(ITABLE)   ! close the previous
        ENDIF
        ! we're now at the beginning
        TABLE_NAME = TABLE_NAME_NEW
        ITABLE = -1
        WRITE(ERR,2) "will create stress table",ITABLE
      ENDIF

      ! if we started/restarted, we need to write the TABLE_NAME
      IF (ITABLE == -1) THEN
          WRITE(ERR,3) "creating stress table",TABLE_NAME
          CALL WRITE_TABLE_HEADER(TABLE_NAME)
          ITABLE = -3
      ENDIF
      WRITE(ERR,2) "writing itable",ITABLE
      CALL WRITE_ITABLE(ITABLE)  ! write the -3, -5, ... subtable header
      ITABLE = ITABLE - 1        ! flip it to -4, -6, ... so we don't have to do this later

      END SUBROUTINE SET_OES_TABLE_NAME


! ##################################################################################################################################
      SUBROUTINE WRITE_OES3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE)
!
!      Parameters
!      ==========
!      op2 : file int
!         the file pointer
!      itable : int
!         the subtable number
!      isubcase : int
!         the subcase id (from the bulk data deck)
!      etype : int
!         element type
!      num_wide : int
!         the number of fields in each element
!      stress_code : int
!         0 : stress???
!         1 : strain???
!
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      IMPLICIT NONE
      INTEGER(LONG), INTENT(INOUT) :: ITABLE
      INTEGER(LONG), INTENT(IN) :: ISUBCASE
      INTEGER(LONG), INTENT(IN) :: DEVICE_CODE
      INTEGER(LONG), INTENT(IN) :: ELEM_TYPE
      INTEGER(LONG), INTENT(IN) :: NUM_WIDE
      INTEGER(LONG), INTENT(IN) :: STRESS_CODE

      INTEGER(LONG) :: FIELD5, FIELD6, FIELD7
      INTEGER(LONG) :: FORMAT_CODE, ANALYSIS_CODE
!      we assumed static
      ANALYSIS_CODE = 1
      
      ! LSDVMN
      FIELD5 = 0

      FIELD6 = 0
      FIELD7 = 0
!      static is real
      FORMAT_CODE = 1
      CALL WRITE_OES3(ITABLE, ANALYSIS_CODE, ISUBCASE, DEVICE_CODE, FORMAT_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE)
      END SUBROUTINE WRITE_OES3_STATIC

! ##################################################################################################################################
      SUBROUTINE WRITE_OES3(ITABLE, ANALYSIS_CODE, ISUBCASE, DEVICE_CODE, FORMAT_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE)
!      Parameters
!      ==========
!      analysis_code
!        the solution type flag
!      approach_code
!        ???
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  OP2
      IMPLICIT NONE
      INTEGER(LONG), INTENT(INOUT) :: ITABLE
      INTEGER(LONG), INTENT(IN) :: ANALYSIS_CODE
      INTEGER(LONG), INTENT(IN) :: ISUBCASE
      INTEGER(LONG), INTENT(IN) :: DEVICE_CODE
      INTEGER(LONG), INTENT(IN) :: ELEM_TYPE
      INTEGER(LONG), INTENT(IN) :: FORMAT_CODE
      INTEGER(LONG), INTENT(IN) :: NUM_WIDE
      INTEGER(LONG), INTENT(IN) :: STRESS_CODE
      
      INTEGER(LONG) :: FIELD5, FIELD6, FIELD7, APPROACH_CODE
      INTEGER(LONG) :: TABLE_CODE, LOAD_SET, THERMAL, ACOUSTIC_FLAG
      CHARACTER(LEN=128) :: TITLE, LABEL, SUBTITLE
      
      WRITE(OP2) 146
      TITLE    = "Title"
      LABEL    = "Label"
      SUBTITLE = "Subtitle"
      ! stress/strain only
      TABLE_CODE = 5
      
      ! ???
      LOAD_SET = 1
      
      ! we're not doing acoustic
      ACOUSTIC_FLAG = 0
      
      ! not always 0 for stress, but for now
      THERMAL = 0

      APPROACH_CODE = ANALYSIS_CODE * 10 + DEVICE_CODE

      ! 584 bytes
      WRITE(OP2) APPROACH_CODE, TABLE_CODE, ELEM_TYPE, ISUBCASE, FIELD5, &
            FIELD6, FIELD7, LOAD_SET, FORMAT_CODE, NUM_WIDE, &
            STRESS_CODE, ACOUSTIC_FLAG, 0, 0, 0, &
            0, 0, 0, 0, 0, &
            0, 0, THERMAL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, &
            TITLE, SUBTITLE, LABEL

      ITABLE = ITABLE - 1
      CALL WRITE_ITABLE(ITABLE)
      END SUBROUTINE WRITE_OES3
