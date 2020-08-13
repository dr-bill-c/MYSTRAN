! ##################################################################################################################################
 
      SUBROUTINE GET_INI_FILNAM  ( MYSTRAN_DIR, MYSTRAN_DIR_LEN, INIFIL_NAME_LEN )
 
! Gets name (incl path) of the MYSTRAN.INI initialization file. This is the Linux version which uses '/' as a folder seperator 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, F04, INIFIL, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, PROG_NAME
      USE TIMDAT, ONLY                :  TSEC 

      USE GET_INI_FILNAM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(PROG_NAME)+4) :: FILNAM            ! File name for MYSTRAN initialization file (not including path)

                                                           ! Directory where MYSTRAN executable (and INI file) exist
      CHARACTER(FILE_NAM_MAXLEN*BYTE), INTENT(IN):: MYSTRAN_DIR

      INTEGER(LONG), INTENT(IN)       :: MYSTRAN_DIR_LEN   ! Length of MYSTRAN_DIR (not including trailing blanks)
      INTEGER(LONG), INTENT(OUT)      :: INIFIL_NAME_LEN   ! Length of INI file name (incl path)
 
! **********************************************************************************************************************************
      FILNAM( 1:LEN(PROG_NAME)) = PROG_NAME
      FILNAM(LEN(PROG_NAME)+1:LEN(PROG_NAME)+4) = '.INI'

      IF (MYSTRAN_DIR_LEN > 0) THEN
         INIFIL(1:MYSTRAN_DIR_LEN)  = MYSTRAN_DIR
         INIFIL(MYSTRAN_DIR_LEN+1:MYSTRAN_DIR_LEN+2)  = '/'
         INIFIL(MYSTRAN_DIR_LEN+2:) = FILNAM
         INIFIL_NAME_LEN = MYSTRAN_DIR_LEN +1 + LEN(PROG_NAME) + 4
      ELSE
         INIFIL = FILNAM
         INIFIL_NAME_LEN = LEN(PROG_NAME) + 4
      ENDIF

! **********************************************************************************************************************************

      END SUBROUTINE GET_INI_FILNAM

