

!==================================================================================================
      SUBROUTINE WRITE_STRESS_TABLE_HEADER()
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      !INTEGER(LONG), INTENT(IN) :: ITABLE ! the subtable id
      CHARACTER(8*BYTE) :: TABLE_NAME
      TABLE_NAME = 'OES1    '
      CALL WRITE_TABLE_HEADER(TABLE_NAME)
      END SUBROUTINE WRITE_STRESS_TABLE_HEADER

!==================================================================================================
      SUBROUTINE WRITE_STRAIN_TABLE_HEADER()
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      !INTEGER(LONG), INTENT(IN) :: ITABLE ! the subtable id
      CHARACTER(8*BYTE) :: TABLE_NAME
      TABLE_NAME = 'OSTR1   '
      CALL WRITE_TABLE_HEADER(TABLE_NAME)
      END SUBROUTINE WRITE_STRAIN_TABLE_HEADER

! ##################################################################################################################################
      SUBROUTINE WRITE_OES3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE, NEW_RESULT)
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
      INTEGER(LONG), INTENT(IN) :: ITABLE
      INTEGER(LONG), INTENT(IN) :: ISUBCASE
      INTEGER(LONG), INTENT(IN) :: DEVICE_CODE
      INTEGER(LONG), INTENT(IN) :: ELEM_TYPE
      INTEGER(LONG), INTENT(IN) :: NUM_WIDE
      INTEGER(LONG), INTENT(IN) :: STRESS_CODE
      LOGICAL, INTENT(INOUT)    :: NEW_RESULT

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
      CALL WRITE_OES3(ITABLE, ANALYSIS_CODE, ISUBCASE, DEVICE_CODE, FORMAT_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE, NEW_RESULT)
      END SUBROUTINE WRITE_OES3_STATIC

! ##################################################################################################################################
      SUBROUTINE WRITE_OES3(ITABLE, ANALYSIS_CODE, ISUBCASE, DEVICE_CODE, FORMAT_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE, NEW_RESULT)
!      Parameters
!      ==========
!      analysis_code
!        the solution type flag
!      approach_code
!        ???
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  OP2
      IMPLICIT NONE
      INTEGER(LONG), INTENT(IN) :: ITABLE
      INTEGER(LONG), INTENT(IN) :: ANALYSIS_CODE
      INTEGER(LONG), INTENT(IN) :: ISUBCASE
      INTEGER(LONG), INTENT(IN) :: DEVICE_CODE
      INTEGER(LONG), INTENT(IN) :: ELEM_TYPE
      INTEGER(LONG), INTENT(IN) :: FORMAT_CODE
      INTEGER(LONG), INTENT(IN) :: NUM_WIDE
      INTEGER(LONG), INTENT(IN) :: STRESS_CODE
      LOGICAL, INTENT(INOUT)    :: NEW_RESULT
      
      INTEGER(LONG) :: FIELD5, FIELD6, FIELD7, APPROACH_CODE
      INTEGER(LONG) :: TABLE_CODE, LOAD_SET, THERMAL, ACOUSTIC_FLAG
      CHARACTER(LEN=128) :: TITLE, LABEL, SUBTITLE
      
      NEW_RESULT = .TRUE.
      IF(NEW_RESULT .AND. (ITABLE .NE. 3)) THEN
        WRITE(OP2) 146
        NEW_RESULT = .FALSE.
      ELSE
         WRITE(OP2) ITABLE
         WRITE(OP2) 1
         WRITE(OP2) 0
         WRITE(OP2) 146
      ENDIF
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

      CALL WRITE_ITABLE(ITABLE)
      END SUBROUTINE WRITE_OES3
