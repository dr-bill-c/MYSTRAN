! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail.com)                                              
                                                                                                         
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and      
! associated documentation files (the "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to   
! the following conditions:                                                                              
                                                                                                         
! The above copyright notice and this permission notice shall be included in all copies or substantial   
! portions of the Software and documentation.                                                                              
                                                                                                         
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS                                
! OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                            
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                            
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                                 
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,                          
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN                              
! THE SOFTWARE.                                                                                          
! _______________________________________________________________________________________________________
                                                                                                        
! End MIT license text.                                                                                      

      SUBROUTINE BD_SPC1 ( CARD, LARGE_FLD_INP, CC_SPC_FND )

! Processes SPC Bulk Data Cards. Reads and checks data and then write a record to file LINK1O for later processing.
! Each record in file LINK1O has:

!          SETID, COMPJ, GRIDJ, RSPCJ = 0., DOFSET 

! Note that RSPCJ is written to be compatible with the data written for Bulk Data card SPC

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1O
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LSPC1, NSPC1, NUM_SPC1_RECORDS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SPC1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN
      USE MODEL_STUF, ONLY            :  SPC1_SIDS, SPCSET

      USE BD_SPC1_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SPC1'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_SPC_FND        ! ='Y' if this SPC is a set requested in Case Control
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: DOFSET            ! 'SB' or 'SE' to denote the set that an SPC'd DOF belongs to
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! An output from subr TOKCHK called herein

      INTEGER(LONG)                   :: COMPJ     = 0     ! DOF's constrained at GRIDJ
      INTEGER(LONG)                   :: GRIDJ1    = 0     ! Grid ID on SPC card
      INTEGER(LONG)                   :: GRIDJ2    = 0     ! Grid ID on SPC card
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: SETID     = 0     ! SPC set ID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SPC1_BEGEND

      REAL(DOUBLE) , PARAMETER        :: RSPCJ     = ZERO  ! Enforced displ value (always zero on SPC1). Included for file LINK1O
!                                                            with SPC format.

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  SPC1 Bulk Data Card routine
 
!    FIELD   ITEM           ARRAY ELEMENT
!    -----   ------------   -------------
! Format #1:
!     2      Set ID         
!     3      Comp. numbers  
!     4-9    Grid ID's      
! Optional continuation cards
!     2-9    Grid ID's 

! Format #2:
!     2      Set ID 
!     3      Component numbers
!     4      Grid ID number 1
!     5      "THRU"
!     6      Grid ID number 2


! All SPC1 are SB set 

      DOFSET = 'SB'

! Make JCARD from CARD

      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

! Check for overflow

      NSPC1 = NSPC1 + 1
!xx   IF (NSPC1 > LSPC1) THEN                              ! Check for overflow
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LSPC1
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LSPC1
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF

! Check if SPC set ID matches a Case Control request

      CALL I4FLD ( JCARD(2), JF(2), SETID )                ! Read field 2: Set ID
      IF (IERRFL(2) == 'N') THEN
         IF (SETID == SPCSET) THEN
            CC_SPC_FND = 'Y'
         ENDIF
         SPC1_SIDS(NSPC1) = SETID
      ELSE
         JERR = JERR + 1
      ENDIF

! Get component numbers that are SPC'd

      CALL IP6CHK ( JCARD(3), JCARDO, IP6TYP, IDUM )       ! Read field 3: components
      IF (IERRFL(3) == 'N') THEN
         IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'ZERO    ') .OR. (IP6TYP == 'BLANK   ')) THEN
            CALL I4FLD ( JCARDO, JF(3), COMPJ )          
         ELSE
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1124) JF(3),JCARD(1),JCARD(2),JF(3),JCARD(3)
            WRITE(F06,1124) JF(3),JCARD(1),JCARD(2),JF(3),JCARD(3)
         ENDIF
      ELSE
         JERR = JERR + 1
      ENDIF

! Field 5 of SPC1 must have "THRU" or a grid pt number or blank.

      TOKEN = JCARD(5)(1:8)                                ! Only send the 1st 8 chars of this JCARD. It has been left justified
      CALL TOKCHK ( TOKEN, TOKTYP )                       ! TOKTYP must be THRU', 'INTEGER', or 'BLANK'

! **********************************************************************************************************************************
! Format # 2

      IF (TOKTYP == 'THRU    ') THEN                      
 
         JERR = 0

         IF (JCARD(4)(1:) /= ' ') THEN                     ! Get 1st Grid ID, GRIDJ1
            CALL I4FLD ( JCARD(4), JF(4), GRIDJ1 )
         ELSE            
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1125) 'GRID POINT', JF(4), JCARD(1)
            WRITE(F06,1125) 'GRID POINT', JF(4), JCARD(1)
         ENDIF
 
         IF (JCARD(6)(1:) /= ' ') THEN                     ! Get 2nd Grid ID, GRIDJ2
            CALL I4FLD ( JCARD(6), JF(6), GRIDJ2 )
         ELSE            
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1125) 'GRID POINT', JF(6), JCARD(1)
            WRITE(F06,1125) 'GRID POINT', JF(6), JCARD(1)
         ENDIF
 
         IF ((IERRFL(4)=='N') .AND. (IERRFL(6)=='N')) THEN ! Check GRIDJ2 > GRIDJ1 if there were no errors reading them
            IF (GRIDJ2 < GRIDJ1) THEN
               JERR      = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1128) JCARD(1), JCARD(2) 
               WRITE(F06,1128) JCARD(1), JCARD(2) 
            ENDIF
         ENDIF            
 
         CALL BD_IMBEDDED_BLANK ( JCARD,2,0,4,5,6,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2,4,5,6
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,7,8,9 )! Issue warning if fields 7,8,9 not blank
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

         IF (JERR == 0) THEN
            NUM_SPC1_RECORDS = NUM_SPC1_RECORDS + 1        ! Incr count of number of entries written to file LINK1O
            WRITE(L1O) SETID,COMPJ,GRIDJ1,GRIDJ2,RSPCJ,DOFSET
         ENDIF

! **********************************************************************************************************************************
! Format # 1

      ELSE IF ((TOKTYP == 'INTEGER ') .OR. (TOKTYP == 'BLANK   ')) THEN 
 
! Read and check data on parent card

         DO J=4,9                                          ! Read fields 4-9: Grid ID's.
            IF (JCARD(J)(1:) == ' ') THEN
               CYCLE
            ELSE
               CALL I4FLD ( JCARD(J), JF(J), GRIDJ1 )      ! Read another grid ID
               IF ((JERR == 0) .AND. (IERRFL(J) == 'N')) THEN 
                  NUM_SPC1_RECORDS = NUM_SPC1_RECORDS + 1  ! Incr count of number of entries written to file LINK1O
                  WRITE(L1O) SETID,COMPJ,GRIDJ1,GRIDJ1,RSPCJ,DOFSET
               ELSE
                  JERR = JERR + 1
               ENDIF
            ENDIF
         ENDDO   

         CALL BD_IMBEDDED_BLANK ( JCARD,2,0,4,5,6,7,8,9 )  ! Make sure there are no imbeded blanks fields 2-9, except 3 (components)
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

! Read and check data on optional continuation cards if JERR = 0 to this point

         IF (JERR == 0) THEN

            DO
               IF (LARGE_FLD_INP == 'N') THEN
                  CALL NEXTC  ( CARD, ICONT, IERR )
               ELSE
                  CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
                  CARD = CHILD
               ENDIF
               CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
               IF (ICONT == 1) THEN
                  DO J=2,9
                     IF (JCARD(J)(1:) == ' ') THEN
                        CYCLE
                     ELSE
                        CALL I4FLD ( JCARD(J), JF(J), GRIDJ1)! Read another grid ID
                        IF ((JERR == 0) .AND. (IERRFL(J) == 'N')) THEN
                           NUM_SPC1_RECORDS = NUM_SPC1_RECORDS + 1
                           WRITE(L1O) SETID,COMPJ,GRIDJ1,GRIDJ1,RSPCJ,DOFSET
                        ENDIF
                     ENDIF
                  ENDDO
                                                           ! Make sure that there are no imbedded blanks in fields 2-9
                  CALL BD_IMBEDDED_BLANK (JCARD,2,3,4,5,6,7,8,9 )
                  CALL CRDERR ( CARD )                     ! CRDERR prints errors found when reading fields

                  CYCLE
               ELSE
                  EXIT
               ENDIF

            ENDDO

         ENDIF

         IF (NUM_SPC1_RECORDS == 0) THEN                   ! No grids were specified on the SPC1 entry, so error
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1100) JCARD(1)
            WRITE(F06,1100) JCARD(1)
         ENDIF

      ELSE                                                 ! Error - Field 5 did not have "THRU", or an integer, or was blank 
 
         FATAL_ERR = FATAL_ERR+1
         WRITE(ERR,1127) JF(5), JCARD(1)
         WRITE(F06,1127) JF(5), JCARD(1)
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1100 FORMAT(' *ERROR  1100: ',A,' ENTRY MUST HAVE AT LEAST 1 GRID DEFINED (IN FIELDS 4-9 OF PARENT OR 2-9 OF CONTINUATION)')

 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')

 1125 FORMAT(' *ERROR  1125: NO ',A,' SPECIFIED IN FIELD',I4,' ON ',A,' CARD')

 1127 FORMAT(' *ERROR  1127: INVALID DATA IN FIELD ',I2,' OF ',A,' CARD. FIELD MUST HAVE THRU OR A GRID NUMBER OR BE BLANK')

 1128 FORMAT(' *ERROR  1128: ON ',A,A,' THE IDs MUST BE IN INCREASING ORDER FOR THRU OPTION')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************

      END SUBROUTINE BD_SPC1
