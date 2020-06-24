! ##################################################################################################################################
 
      SUBROUTINE GET_MYSTRAN_DIR ( MYSTRAN_DIR, MYSTRAN_DIR_LEN )
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN

      USE GET_MYSTRAN_DIR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(FILE_NAM_MAXLEN*BYTE), INTENT(OUT) :: MYSTRAN_DIR       ! Directory where program executable (and INI file) exist

      INTEGER(LONG), INTENT(OUT)                   :: MYSTRAN_DIR_LEN   ! Length of MYSTRAN_DIR (not including trailing blanks)
      INTEGER(LONG)                                :: I                 ! DO loop index
 
      INTRINSIC                                    :: GET_ENVIRONMENT_VARIABLE

! **********************************************************************************************************************************
      CALL GET_ENVIRONMENT_VARIABLE ( 'MYSTRAN_directory', MYSTRAN_DIR, MYSTRAN_DIR_LEN  )

! **********************************************************************************************************************************
      END SUBROUTINE GET_MYSTRAN_DIR


