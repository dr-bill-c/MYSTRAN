! ##################################################################################################################################
      SUBROUTINE WRITE_OUG3_STATIC(ITABLE, ISUBCASE, DEVICE_CODE, ANALYSIS_CODE, TABLE_CODE, NEW_RESULT)
      USE PENTIUM_II_KIND, ONLY  :  BYTE, LONG, DOUBLE
      IMPLICIT NONE
      INTEGER(LONG), INTENT(IN) :: ITABLE, ISUBCASE, DEVICE_CODE, ANALYSIS_CODE, TABLE_CODE
      LOGICAL, INTENT(INOUT)    :: NEW_RESULT
      INTEGER(LONG) :: FORMAT_CODE, NUM_WIDE

!      1 = OP2 is the output file
!      DEVICE_CODE = 1

!     real
      FORMAT_CODE = 1
      NUM_WIDE = 8

      CALL WRITE_OUG3(ITABLE, ISUBCASE, FORMAT_CODE, DEVICE_CODE, ANALYSIS_CODE, TABLE_CODE, NUM_WIDE, NEW_RESULT)
      END SUBROUTINE WRITE_OUG3_STATIC

! ##################################################################################################################################
      SUBROUTINE WRITE_OUG3(ITABLE, ISUBCASE, FORMAT_CODE, DEVICE_CODE, ANALYSIS_CODE, TABLE_CODE, NUM_WIDE, NEW_RESULT)
!      Parameters
!      ==========
!      isubcase : int
!         the subcase id
      USE PENTIUM_II_KIND, ONLY  :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY           :  OP2
      IMPLICIT NONE
      INTEGER(LONG), INTENT(IN) :: ITABLE, ISUBCASE, FORMAT_CODE, DEVICE_CODE, ANALYSIS_CODE, TABLE_CODE, NUM_WIDE
      LOGICAL, INTENT(INOUT)    :: NEW_RESULT

      CHARACTER(LEN=128) :: TITLE, LABEL, SUBTITLE
      INTEGER(LONG) :: THERMAL, SORT_CODE, &
        RANDOM_CODE, ACOUSTIC_FLAG, OCODE, &
        APPROACH_CODE, TCODE
      INTEGER(LONG) :: FIELD5, FIELD6, FIELD7

      TITLE = "Title"
      LABEL = "Label"
      SUBTITLE = "Subtitle"

      write(*,*) NEW_RESULT, itable
      IF(NEW_RESULT .AND. (ITABLE .NE. -3)) THEN
!        header = [
!          4, 146, 4,
!        ]
        WRITE(OP2) 146
        NEW_RESULT = .FALSE.
      ELSE
!        header = [
!          4, itable, 4,
!          4, 1, 4,
!          4, 0, 4,
!          4, 146, 4,
!        ]
        CALL WRITE_ITABLE(ITABLE)
        !WRITE(OP2) ITABLE
        !WRITE(OP2) 1
        !WRITE(OP2) 0
        WRITE(OP2) 146
      ENDIF
      ! displacement only
      !if TABLE_NAME .EQ. "OUG1"
      !TABLE_CODE = 1
      THERMAL = 0

!       static
      !ANALYSIS_CODE = 1

!      sort1
!      should this be 1???
      SORT_CODE = 0

!      other minor assumptions
      RANDOM_CODE = 0
      ACOUSTIC_FLAG = 0
      OCODE = 0

!      table_code = tCode % 1000
!      sort_code = tCode // 1000
!      approach, tcode, int3, isubcase
      TCODE = SORT_CODE * 1000 + TABLE_CODE


      FIELD6 = 0
      FIELD7 = 0

!      if isinstance(acoustic_flag, float_types):
!          ftable3 = set_table3_field(ftable3, 12, b'f') # field 11

      IF(ANALYSIS_CODE == 1) THEN
!        what actually is this??
!        field5 = self.lsdvmns[itime]
        FIELD5 = 1
      ELSE
        print *, "analysis code not supported...stopping"
        stop
!        raise NotImplementedError(self.analysis_code)
      ENDIF

      APPROACH_CODE = ANALYSIS_CODE*10 + DEVICE_CODE
!      table3 = [
!          approach_code, table_code, 0, isubcase, field5,
!          field6, field7, random_code, format_code, num_wide,
!          oCode, acoustic_flag, 0, 0, 0,
!          0, 0, 0, 0, 0,
!          0, 0, thermal, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
!          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
!          0, 0, 0, 0,
!          title, subtitle, label,
!      ]
!      WRITE(OP2) 
      WRITE(OP2) APPROACH_CODE, TABLE_CODE, 0, ISUBCASE, FIELD5, &
          FIELD6, FIELD7, RANDOM_CODE, FORMAT_CODE, NUM_WIDE, &
          OCODE, ACOUSTIC_FLAG, 0, 0, 0, &
          0, 0, 0, 0, 0, &
          0, 0, THERMAL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, &
          TITLE, SUBTITLE, LABEL
!      assert table3[22] == thermal
!      print *, "finished OUGV1 table=3"

      CALL WRITE_ITABLE(ITABLE)
      END SUBROUTINE WRITE_OUG3

