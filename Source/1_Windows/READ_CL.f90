! ##################################################################################################################################
 
      SUBROUTINE READ_CL ( FILNAM, NC_FILNAM )
 
! Gets command line string, which is the name of a file (FILNAM), and counts the number (NC_FILNAM) of characters
! (leading blanks ignored) in the name. 
 
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN

      USE READ_CL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(OUT)   :: FILNAM            ! File name on command line
 
      INTEGER(LONG), INTENT(OUT)      :: NC_FILNAM         ! Length, in chars, of FILNAM (with leading blanks removed)
      INTEGER(LONG)                   :: STATUS            ! Status from GETARG. If /= -1, it is the length of the argument
 
      INTRINSIC                       :: GETARG

! **********************************************************************************************************************************
! Initialize outputs

      NC_FILNAM = 0
      FILNAM(1:FILE_NAM_MAXLEN) = ' '

! Get command line string which should contain the file name of the input data deck. 
 
      CALL GETARG ( 1, FILNAM, STATUS )
      IF (STATUS /= -1) THEN
         NC_FILNAM = STATUS
         ENDIF
      RETURN
 
      END SUBROUTINE READ_CL
