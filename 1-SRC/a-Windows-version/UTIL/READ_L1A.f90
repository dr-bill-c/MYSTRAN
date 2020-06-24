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
 
      SUBROUTINE READ_L1A ( CLOSE_STAT, WRITE_F04 )
 
! Reads in data that is in formatted file LINK1A, which is read by all LINK's after LINK1, as they begin. This text file contains
! the names of files opened for a run, the "counter" info (e.g. NGRID, number of grids, etc), solution number, PARAM's
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE

      USE IOUNT1, ONLY                :  MOT4,    MOU4,    WRT_ERR, WRT_LOG

      USE IOUNT1, ONLY                :  ANS,     BUG,     EIN,     ENF,     ERR,     F04,     F06,     IN0,     IN1,     INI,     &
                                         L1A,     NEU,     OT4,     PCH,     SEQ,     SPC,     SC1,                                &
                                         F21,     F22,     F23,     F24,     F25,                                                  &
                                         L1B,     L1C,     L1D,     L1E,     L1F,     L1G,     L1H,     L1I,     L1J,     L1K,     &
                                         L1L,     L1M,     L1N,     L1O,     L1P,     L1Q,     L1R,     L1S,     L1T,     L1U,     &
                                         L1V,     L1W,     L1X,     L1Y,     L1Z,                                                  &
                                         L2A,     L2B,     L2C,     L2D,     L2E,     L2F,     L2G,     L2H,     L2I,     L2J,     &
                                         L2K,     L2L,     L2M,     L2N,     L2O,     L2P,     L2Q,     L2R,     L2S,     L2T,     &
                                         L3A,     L4A,     L4B,     L4C,     L4D,     L5A,     L5B,     OU4

      USE IOUNT1, ONLY                :  ANSSTAT, BUGSTAT, EINSTAT, ENFSTAT, ERRSTAT, F04STAT, F06STAT, IN0STAT, IN1STAT, INISTAT, &
                                         L1ASTAT, NEUSTAT, OT4STAT, PCHSTAT, SEQSTAT, SPCSTAT,                                     &
                                         F21STAT, F22STAT, F23STAT, F24STAT, F25STAT,                                              &
                                         L1BSTAT, L1CSTAT, L1DSTAT, L1ESTAT, L1FSTAT, L1GSTAT, L1HSTAT, L1ISTAT, L1JSTAT, L1KSTAT, &
                                         L1LSTAT, L1MSTAT, L1NSTAT, L1OSTAT, L1PSTAT, L1QSTAT, L1RSTAT, L1SSTAT, L1TSTAT, L1USTAT, &
                                         L1VSTAT, L1WSTAT, L1XSTAT, L1YSTAT, L1ZSTAT,                                              &
                                         L2ASTAT, L2BSTAT, L2CSTAT, L2DSTAT, L2ESTAT, L2FSTAT, L2GSTAT, L2HSTAT, L2ISTAT, L2JSTAT, &
                                         L2KSTAT, L2LSTAT, L2MSTAT, L2NSTAT, L2OSTAT, L2PSTAT, L2QSTAT, L2RSTAT, L2SSTAT, L2TSTAT, &
                                         L3ASTAT, L4ASTAT, L4BSTAT, L4CSTAT, L4DSTAT, L5ASTAT, L5BSTAT, OU4STAT

      USE IOUNT1, ONLY                :  ANSFIL,  BUGFIL,  EINFIL,  ENFFIL,  ERRFIL,  F04FIL,  F06FIL,  IN0FIL,  INIFIL,  LINK1A,  &
                                         NEUFIL,  OT4FIL,  PCHFIL,  SEQFIL,  SPCFIL,  F21FIL,  F22FIL,  F23FIL,  F24FIL,  F25FIL,  &
                                         LINK1A,  LINK1B,  LINK1C,  LINK1D,  LINK1E,  LINK1F,  LINK1G,  LINK1H,  LINK1I,  LINK1J,  &
                                         LINK1K,  LINK1L,  LINK1M,  LINK1N,  LINK1O,  LINK1P,  LINK1Q,  LINK1R,  LINK1S,  LINK1T,  &
                                         LINK1U,  LINK1V,  LINK1W,  LINK1X,  LINK1Y,  LINK1Z,                                      &
                                         LINK2A,  LINK2B,  LINK2C,  LINK2D,  LINK2E,  LINK2F,  LINK2G,  LINK2H,  LINK2I,  LINK2J,  &
                                         LINK2K,  LINK2L,  LINK2M,  LINK2N,  LINK2O,  LINK2P,  LINK2Q,  LINK2R,  LINK2S,  LINK2T,  &
                                         LINK3A,  LINK4A,  LINK4B,  LINK4C,  LINK4D,  LINK5A,  LINK5B,  OU4FIL

      USE IOUNT1, ONLY                :  ANS_MSG, BUG_MSG, EIN_MSG, ENF_MSG, ERR_MSG, F04_MSG, F06_MSG, IN0_MSG, IN1_MSG, INI_MSG, &
                                         L1A_MSG, NEU_MSG, OT4_MSG, PCH_MSG, SEQ_MSG, SPC_MSG,                                     &
                                         F21_MSG, F22_MSG, F23_MSG, F24_MSG, F25_MSG,                                              &
                                         L1B_MSG, L1C_MSG, L1D_MSG, L1E_MSG, L1F_MSG, L1G_MSG, L1H_MSG, L1I_MSG, L1J_MSG, L1K_MSG, &
                                         L1L_MSG, L1M_MSG, L1N_MSG, L1O_MSG, L1P_MSG, L1Q_MSG, L1R_MSG, L1S_MSG, L1T_MSG, L1U_MSG, &
                                         L1V_MSG, L1W_MSG, L1X_MSG, L1Y_MSG, L1Z_MSG,                                              &
                                         L2A_MSG, L2B_MSG, L2C_MSG, L2D_MSG, L2E_MSG, L2F_MSG, L2G_MSG, L2H_MSG, L2I_MSG, L2J_MSG, &
                                         L2K_MSG, L2L_MSG, L2M_MSG, L2N_MSG, L2O_MSG, L2P_MSG, L2Q_MSG, L2R_MSG, L2S_MSG, L2T_MSG, &
                                         L3A_MSG, L4A_MSG, L4B_MSG, L4C_MSG, L4D_MSG, L5A_MSG, L5B_MSG, OU4_MSG,                   &
                                         MAX_FIL

      USE SCONTR
      USE PARAMS, ONLY                :  CBMIN3, CBMIN4, ELFORCEN, HEXAXIS, IORQ1B, IORQ1M, IORQ1S, IORQ2B, IORQ2T,&
                                         MATSPARS, MIN4TRED, QUAD4TYP, QUADAXIS, SPARSTOR


      USE TIMDAT, ONLY                :  STIME, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  READ_L1A_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      USE READ_L1A_USE_IFs


      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'READ_L1A'
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! STATUS when closing file LINK1A
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_F04         ! If 'Y' write subr begin/end times to F04 (if WRT_LOG >= SUBR_BEGEND)
      CHARACTER(80*BYTE)              :: MESSAG            ! File description. Used for error messaging
 
      INTEGER(LONG), PARAMETER        :: NUMIO      = 304  ! Number of terms in IOCHKI array
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHKI(NUMIO)     ! Array of IOSTAT error numbers when opening/reading a file
      INTEGER(LONG)                   :: JERR              ! Error count as records are read from file LINK1A
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to
      INTEGER(LONG)                   :: REC_NO            ! Indicator of record number when error encountered reading file
      INTEGER(LONG)                   :: XTIME             ! Time stamp read from file LINK1A
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = READ_L1A_BEGEND
 
! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Default units for writing errors the screen (until LINK1A is read)

      OUNT(1) = SC1
      OUNT(2) = SC1
 
! Initialize terms 
 
      REC_NO = 0
      JERR   = 0
 
! Open L1A file (file name read in calling routine)

      OPEN (L1A,FILE=LINK1A,STATUS='OLD',IOSTAT=IOCHKI(1),ACTION='READWRITE')
      IF (IOCHKI(1) /= 0) THEN
         CALL OPNERR ( IOCHKI(1), LINK1A, OUNT, WRITE_F04 )
         CALL FILERR ( OUNT, WRITE_F04 )
         CALL OUTA_HERE ( 'Y' )                                    ! Can't open file LINK1A, so quit
      ENDIF
 

! Read start time
 
      MESSAG = 'STIME'
      READ(L1A,110,IOSTAT=IOCHKI(1)) XTIME
      IF (IOCHKI(1) /= 0) THEN
         REC_NO = -99
         CALL READERR ( IOCHKI(1), LINK1A, MESSAG, REC_NO, OUNT, WRITE_F04 )
         JERR = JERR + 1
      ELSE
         IF (RESTART == 'N') THEN
            IF (XTIME /= STIME) THEN
               CALL STMERR ( XTIME, LINK1A, OUNT, WRITE_F04 )
               JERR = JERR +1
            ENDIF
         ELSE
            STIME = XTIME
         ENDIF
      ENDIF
 
! Read LINK number that was executing when file LINK1A was written

      MESSAG = ' LINK number executing when file LINK1A was written'
      READ(L1A,110,IOSTAT=IOCHKI(1)) LINKNO_L1A
      IF (IOCHKI(1) /= 0) THEN
         REC_NO = -99
         CALL READERR ( IOCHKI(1), LINK1A, MESSAG, REC_NO, OUNT, WRITE_F04 )
         JERR = JERR + 1
      ENDIF

! Read solution number

      MESSAG = 'SOLUTION NAME'
      READ(L1A,120,IOSTAT=IOCHKI(1)) SOL_NAME
      IF (IOCHKI(1) /= 0) THEN
         REC_NO = -99
         CALL READERR ( IOCHKI(1), LINK1A, MESSAG, REC_NO, OUNT, WRITE_F04 )
         JERR = JERR + 1
      ENDIF

! Read I/0 unit no's and names. First, null IOCHKI since it won't be checked until after all records, below, are read.
 
      DO I=1,NUMIO
         IOCHKI(I) = 0
      ENDDO
  
      MESSAG = 'I/O UNITS AND FILE NAMES  '
 
      READ(L1A,140,IOSTAT=IOCHKI(  1)) SC1

      READ(L1A,151,IOSTAT=IOCHKI(  2)) ANS,ANSSTAT,ANS_MSG,ANSFIL
      READ(L1A,151,IOSTAT=IOCHKI(  3)) BUG,BUGSTAT,BUG_MSG,BUGFIL
      READ(L1A,151,IOSTAT=IOCHKI(  4)) EIN,EINSTAT,EIN_MSG,EINFIL
      READ(L1A,151,IOSTAT=IOCHKI(  5)) ENF,ENFSTAT,ENF_MSG,ENFFIL
      READ(L1A,151,IOSTAT=IOCHKI(  6)) ERR,ERRSTAT,ERR_MSG,ERRFIL
      READ(L1A,151,IOSTAT=IOCHKI(  7)) F04,F04STAT,F04_MSG,F04FIL
      READ(L1A,151,IOSTAT=IOCHKI(  8)) F06,F06STAT,F06_MSG,F06FIL
      READ(L1A,151,IOSTAT=IOCHKI(  9)) IN0,IN0STAT,IN0_MSG,IN0FIL
      READ(L1A,151,IOSTAT=IOCHKI( 10)) L1A,L1ASTAT,L1A_MSG,LINK1A
      READ(L1A,151,IOSTAT=IOCHKI( 11)) NEU,NEUSTAT,NEU_MSG,NEUFIL
      READ(L1A,151,IOSTAT=IOCHKI( 12)) PCH,PCHSTAT,PCH_MSG,PCHFIL
      READ(L1A,151,IOSTAT=IOCHKI( 13)) SEQ,SEQSTAT,SEQ_MSG,SEQFIL
      READ(L1A,151,IOSTAT=IOCHKI( 14)) SPC,SPCSTAT,SPC_MSG,SPCFIL

      READ(L1A,151,IOSTAT=IOCHKI( 15)) F21,F21STAT,F21_MSG,F21FIL
      READ(L1A,151,IOSTAT=IOCHKI( 16)) F22,F22STAT,F22_MSG,F22FIL
      READ(L1A,151,IOSTAT=IOCHKI( 17)) F23,F23STAT,F23_MSG,F23FIL
      READ(L1A,151,IOSTAT=IOCHKI( 18)) F24,F24STAT,F24_MSG,F24FIL
      READ(L1A,151,IOSTAT=IOCHKI( 19)) F25,F25STAT,F25_MSG,F25FIL
      READ(L1A,151,IOSTAT=IOCHKI( 20)) L1B,L1BSTAT,L1B_MSG,LINK1B
      READ(L1A,151,IOSTAT=IOCHKI( 21)) L1C,L1CSTAT,L1C_MSG,LINK1C
      READ(L1A,151,IOSTAT=IOCHKI( 22)) L1D,L1DSTAT,L1D_MSG,LINK1D
      READ(L1A,151,IOSTAT=IOCHKI( 23)) L1E,L1ESTAT,L1E_MSG,LINK1E
      READ(L1A,151,IOSTAT=IOCHKI( 24)) L1F,L1FSTAT,L1F_MSG,LINK1F
      READ(L1A,151,IOSTAT=IOCHKI( 25)) L1G,L1GSTAT,L1G_MSG,LINK1G
      READ(L1A,151,IOSTAT=IOCHKI( 26)) L1H,L1HSTAT,L1H_MSG,LINK1H
      READ(L1A,151,IOSTAT=IOCHKI( 27)) L1I,L1ISTAT,L1I_MSG,LINK1I
      READ(L1A,151,IOSTAT=IOCHKI( 28)) L1J,L1JSTAT,L1J_MSG,LINK1J
      READ(L1A,151,IOSTAT=IOCHKI( 29)) L1K,L1KSTAT,L1K_MSG,LINK1K
      READ(L1A,151,IOSTAT=IOCHKI( 30)) L1L,L1LSTAT,L1L_MSG,LINK1L
      READ(L1A,151,IOSTAT=IOCHKI( 31)) L1M,L1MSTAT,L1M_MSG,LINK1M
      READ(L1A,151,IOSTAT=IOCHKI( 32)) L1N,L1NSTAT,L1N_MSG,LINK1N
      READ(L1A,151,IOSTAT=IOCHKI( 33)) L1O,L1OSTAT,L1O_MSG,LINK1O
      READ(L1A,151,IOSTAT=IOCHKI( 34)) L1P,L1PSTAT,L1P_MSG,LINK1P
      READ(L1A,151,IOSTAT=IOCHKI( 35)) L1Q,L1QSTAT,L1Q_MSG,LINK1Q
      READ(L1A,151,IOSTAT=IOCHKI( 36)) L1R,L1RSTAT,L1R_MSG,LINK1R
      READ(L1A,151,IOSTAT=IOCHKI( 37)) L1S,L1SSTAT,L1S_MSG,LINK1S
      READ(L1A,151,IOSTAT=IOCHKI( 38)) L1T,L1TSTAT,L1T_MSG,LINK1T
      READ(L1A,151,IOSTAT=IOCHKI( 39)) L1U,L1USTAT,L1U_MSG,LINK1U
      READ(L1A,151,IOSTAT=IOCHKI( 40)) L1V,L1VSTAT,L1V_MSG,LINK1V
      READ(L1A,151,IOSTAT=IOCHKI( 41)) L1W,L1WSTAT,L1W_MSG,LINK1W
      READ(L1A,151,IOSTAT=IOCHKI( 42)) L1X,L1XSTAT,L1X_MSG,LINK1X
      READ(L1A,151,IOSTAT=IOCHKI( 43)) L1Y,L1YSTAT,L1Y_MSG,LINK1Y
      READ(L1A,151,IOSTAT=IOCHKI( 44)) L1Z,L1ZSTAT,L1Z_MSG,LINK1Z
      READ(L1A,151,IOSTAT=IOCHKI( 45)) L2A,L2ASTAT,L2A_MSG,LINK2A
      READ(L1A,151,IOSTAT=IOCHKI( 46)) L2B,L2BSTAT,L2B_MSG,LINK2B
      READ(L1A,151,IOSTAT=IOCHKI( 47)) L2C,L2CSTAT,L2C_MSG,LINK2C
      READ(L1A,151,IOSTAT=IOCHKI( 48)) L2D,L2DSTAT,L2D_MSG,LINK2D
      READ(L1A,151,IOSTAT=IOCHKI( 49)) L2E,L2ESTAT,L2E_MSG,LINK2E
      READ(L1A,151,IOSTAT=IOCHKI( 50)) L2F,L2FSTAT,L2F_MSG,LINK2F
      READ(L1A,151,IOSTAT=IOCHKI( 51)) L2G,L2GSTAT,L2G_MSG,LINK2G
      READ(L1A,151,IOSTAT=IOCHKI( 52)) L2H,L2HSTAT,L2H_MSG,LINK2H
      READ(L1A,151,IOSTAT=IOCHKI( 53)) L2I,L2ISTAT,L2I_MSG,LINK2I
      READ(L1A,151,IOSTAT=IOCHKI( 54)) L2J,L2JSTAT,L2J_MSG,LINK2J
      READ(L1A,151,IOSTAT=IOCHKI( 55)) L2K,L2KSTAT,L2K_MSG,LINK2K
      READ(L1A,151,IOSTAT=IOCHKI( 56)) L2L,L2LSTAT,L2L_MSG,LINK2L
      READ(L1A,151,IOSTAT=IOCHKI( 57)) L2M,L2MSTAT,L2M_MSG,LINK2M
      READ(L1A,151,IOSTAT=IOCHKI( 58)) L2N,L2NSTAT,L2N_MSG,LINK2N
      READ(L1A,151,IOSTAT=IOCHKI( 59)) L2O,L2OSTAT,L2O_MSG,LINK2O
      READ(L1A,151,IOSTAT=IOCHKI( 60)) L2P,L2PSTAT,L2P_MSG,LINK2P
      READ(L1A,151,IOSTAT=IOCHKI( 61)) L2Q,L2QSTAT,L2Q_MSG,LINK2Q
      READ(L1A,151,IOSTAT=IOCHKI( 62)) L2R,L2RSTAT,L2R_MSG,LINK2R
      READ(L1A,151,IOSTAT=IOCHKI( 63)) L2S,L2SSTAT,L2S_MSG,LINK2S
      READ(L1A,151,IOSTAT=IOCHKI( 64)) L2T,L2TSTAT,L2T_MSG,LINK2T
      READ(L1A,151,IOSTAT=IOCHKI( 65)) L3A,L3ASTAT,L3A_MSG,LINK3A
      READ(L1A,151,IOSTAT=IOCHKI( 66)) L4A,L4ASTAT,L4A_MSG,LINK4A
      READ(L1A,151,IOSTAT=IOCHKI( 67)) L4B,L4BSTAT,L4B_MSG,LINK4B
      READ(L1A,151,IOSTAT=IOCHKI( 68)) L4C,L4CSTAT,L4C_MSG,LINK4C
      READ(L1A,151,IOSTAT=IOCHKI( 69)) L4D,L4DSTAT,L4D_MSG,LINK4D
      READ(L1A,151,IOSTAT=IOCHKI( 70)) L5A,L5ASTAT,L5A_MSG,LINK5A
      READ(L1A,151,IOSTAT=IOCHKI( 71)) L5B,L5BSTAT,L5B_MSG,LINK5B
      IF                        ( 71 > MAX_FIL) THEN
         WRITE(ERR,944) SUBR_NAME, MAX_FIL   
         WRITE(F06,944) SUBR_NAME, MAX_FIL   
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF 

      DO I=1,MOU4 
         READ(L1A,151,IOSTAT=IOCHKI(71+I)) OU4(I),OU4STAT(I),OU4FIL(I)
      ENDDO 
      DO I=1,MOT4 
         READ(L1A,151,IOSTAT=IOCHKI(71+MOU4+I)) OT4(I),OT4STAT(I),OT4FIL(I)
      ENDDO 
      DO I=1,71+MOT4+MOU4
         IF (IOCHKI(I) /= 0) THEN
            REC_NO = I
            CALL READERR ( IOCHKI(1), LINK1A, MESSAG, REC_NO, OUNT, WRITE_F04 )
            JERR = JERR + 1
         ENDIF
      ENDDO 

! Read counter info. First, null IOCHK since it won't be checked until after all records, below, are read
 
      DO I=1,NUMIO
         IOCHKI(I) = 0
      ENDDO

      MESSAG = 'VARIABLES FROM MODULE SCONTR'
      READ(L1A,160,IOSTAT=IOCHKI(  1)) LBAROFF             !   1 (From module SCONTR) 
      READ(L1A,160,IOSTAT=IOCHKI(  2)) LBUSHOFF            !   2 (From module SCONTR) 
      READ(L1A,160,IOSTAT=IOCHKI(  3)) LCMASS              !   3 (From module SCONTR) 
      READ(L1A,160,IOSTAT=IOCHKI(  4)) LCONM2              !   4 (From module SCONTR) 
      READ(L1A,160,IOSTAT=IOCHKI(  5)) LCORD               !   5 (From module SCONTR) 
      READ(L1A,160,IOSTAT=IOCHKI(  6)) LDOFG               !   6 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(  7)) LEDAT               !   7 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(  8)) LELE                !   8 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(  9)) LFORCE              !   9 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 10)) LGRAV               !  10 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 11)) LGRID               !  11 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 12)) LGUSERIN            !  12 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 13)) LIND_GRDS_MPCS      !  13 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 14)) LLOADC              !  14 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 15)) LLOADR              !  15 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 16)) LMATANGLE           !  16 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 17)) LMATL               !  17 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 18)) LMPC                !  18 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 19)) LMPCADDC            !  19 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 20)) LMPCADDR            !  20 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 21)) LPBAR               !  21 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 22)) LPBEAM              !  22 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 23)) LPBUSH              !  23 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 24)) LPCOMP              !  24 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 25)) LPCOMP_PLIES        !  25 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 26)) LPDAT               !  26 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 27)) LPELAS              !  27 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 28)) LPLATEOFF           !  28 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 29)) LPLATETHICK         !  29 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 30)) LPLOAD              !  30 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 31)) LPLOTEL             !  31 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 32)) LPMASS              !  32 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 33)) LPROD               !  33 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 34)) LPSHEAR             !  34 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 35)) LPSHEL              !  35 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 36)) LPSOLID             !  36 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 37)) LPUSER1             !  37 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 38)) LPUSERIN            !  38 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 39)) LRFORCE             !  39 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 40)) LRIGEL              !  40 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 41)) LSEQ                !  41 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 42)) LSETLN              !  42 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 43)) LSETS               !  43 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 44)) LSLOAD              !  44 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 45)) LSPC                !  45 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 46)) LSPC1               !  46 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 47)) LSPCADDC            !  47 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 48)) LSPCADDR            !  48 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 49)) LSUB                !  49 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 50)) LSUSERIN            !  50 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 51)) LTDAT               !  51 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 52)) LTERM_KGG           !  52 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 53)) LTERM_KGGD          !  53 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 54)) LTERM_MGGE          !  54 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 55)) LVVEC               !  55 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 56)) MAX_ELEM_DEGREE     !  56 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 57)) MAX_GAUSS_POINTS    !  57 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 58)) MAX_STRESS_POINTS   !  58 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 59)) MDT                 !  59 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 60)) MELGP               !  60 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 61)) MELDOF              !  61 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 62)) MID1_PCOMP_EQ       !  62 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 63)) MID2_PCOMP_EQ       !  63 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 64)) MID3_PCOMP_EQ       !  64 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 65)) MID4_PCOMP_EQ       !  65 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 66)) MLL_SDIA            !  66 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 67)) MMPC                !  67 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 68)) MOFFSET             !  68 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 69)) MRBE3               !  69 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 70)) MRSPLINE            !  70 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 71)) NAOCARD             !  71 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 72)) NBAROFF             !  72 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 73)) NBUSHOFF            !  73 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 74)) NBAROR              !  74 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 75)) NBEAMOR             !  75 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 76)) NCBAR               !  76 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 77)) NCBEAM              !  77 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 78)) NCBUSH              !  78 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 79)) NCELAS1             !  79 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 80)) NCELAS2             !  80 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 81)) NCELAS3             !  81 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 82)) NC_INFILE           !  82 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 83)) NCELAS4             !  83 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 84)) NCHEXA8             !  84 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 85)) NCHEXA20            !  85 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 86)) NCMASS              !  86 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 87)) NCONM2              !  87 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 88)) NCORD               !  88 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 89)) NCORD1              !  89 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 90)) NCORD2              !  90 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 91)) NCPENTA6            !  91 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 92)) NCPENTA15           !  92 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 93)) NCQUAD4             !  93 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 94)) NCQUAD4K            !  94 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 95)) NCROD               !  95 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 96)) NCSHEAR             !  96 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 97)) NCTETRA4            !  97 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 98)) NCTETRA10           !  98 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI( 99)) NCTRIA3             !  99 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(100)) NCTRIA3K            ! 100 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(101)) NCUSER1             ! 101 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(102)) NCUSERIN            ! 102 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(103)) NDOFA               ! 103 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(104)) NDOF_EIG            ! 104 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(105)) NDOFF               ! 105 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(106)) NDOFG               ! 106 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(107)) NDOFL               ! 107 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(108)) NDOFM               ! 108 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(109)) NDOFN               ! 109 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(110)) NDOFO               ! 110 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(111)) NDOFR               ! 111 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(112)) NDOFS               ! 112 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(113)) NDOFSA              ! 113 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(114)) NDOFSB              ! 114 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(115)) NDOFSE              ! 115 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(116)) NDOFSG              ! 116 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(117)) NDOFSZ              ! 117 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(118)) NEDAT               ! 118 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(119)) NELE                ! 119 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(120)) NFORCE              ! 120 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(121)) NGRAV               ! 121 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(122)) NGRDSET             ! 122 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(123)) NGRID               ! 123 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(124)) NIND_GRDS_MPCS      ! 124 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(125)) NLOAD               ! 125 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(126)) NMATANGLE           ! 126 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(127)) NMATL               ! 127 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(128)) NMPC                ! 128 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(129)) NMPCADD             ! 129 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(130)) NPBAR               ! 130 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(131)) NPBARL              ! 131 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(132)) NPBEAM              ! 132 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(133)) NPBUSH              ! 133 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(134)) NPCARD              ! 134 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(135)) NPCOMP              ! 135 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(136)) NPDAT               ! 136 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(137)) NPELAS              ! 137 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(138)) NPLATEOFF           ! 138 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(139)) NPLATETHICK         ! 139 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(140)) NPLOTEL             ! 140 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(141)) NPLOAD              ! 141 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(142)) NPLOAD4_3D          ! 142 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(143)) NPMASS              ! 143 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(144)) NPROD               ! 144 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(145)) NPSHEAR             ! 145 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(146)) NPSHEL              ! 146 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(147)) NPSOLID             ! 147 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(148)) NPUSER1             ! 148 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(149)) NPUSERIN            ! 149 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(150)) NRBAR               ! 150 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(151)) NRBE1               ! 151 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(152)) NRBE2               ! 152 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(153)) NRFORCE             ! 153 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(154)) NRIGEL              ! 154 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(155)) NRECARD             ! 155 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(156)) NROWS_OTM_ACCE      ! 156 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(157)) NROWS_OTM_DISP      ! 157 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(158)) NROWS_OTM_MPCF      ! 158 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(159)) NROWS_OTM_SPCF      ! 159 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(160)) NROWS_OTM_ELFE      ! 160 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(161)) NROWS_OTM_ELFN      ! 161 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(162)) NROWS_OTM_STRE      ! 162 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(163)) NROWS_OTM_STRN      ! 163 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(164)) NROWS_TXT_ACCE      ! 164 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(165)) NROWS_TXT_DISP      ! 165 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(166)) NROWS_TXT_MPCF      ! 166 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(167)) NROWS_TXT_SPCF      ! 167 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(168)) NROWS_TXT_ELFE      ! 168 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(169)) NROWS_TXT_ELFN      ! 169 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(170)) NROWS_TXT_STRE      ! 170 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(171)) NROWS_TXT_STRN      ! 171 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(172)) NRSPLINE            ! 172 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(173)) NSEQ                ! 173 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(174)) NSETS               ! 174 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(175)) NSLOAD              ! 175 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(176)) NSPC                ! 176 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(177)) NSPC1               ! 177 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(178)) NSPCADD             ! 178 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(179)) NSPOINT             ! 179 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(180)) NUM_SPCSIDS         ! 180 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(181)) NSUB                ! 181 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(182)) NTCARD              ! 182 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(183)) NTDAT               ! 183 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(184)) NTERM_ALL           ! 184 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(185)) NTERM_CG_LTM        ! 185 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(186)) NTERM_DLR           ! 186 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(187)) NTERM_GMN           ! 187 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(188)) NTERM_GOA           ! 188 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(189)) NTERM_HMN           ! 189 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(190)) NTERM_IF_LTM        ! 190 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(191)) NTERM_IRR           ! 191 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(192)) NTERM_KAA           ! 192 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(193)) NTERM_KAAD          ! 193 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(194)) NTERM_KAO           ! 194 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(195)) NTERM_KAOD          ! 195 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(196)) NTERM_KFF           ! 196 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(197)) NTERM_KFFD          ! 197 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(198)) NTERM_KFS           ! 198 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(199)) NTERM_KFSD          ! 199 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(200)) NTERM_KFSe          ! 200 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(201)) NTERM_KFSDe         ! 201 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(202)) NTERM_KGG           ! 202 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(203)) NTERM_KGGD          ! 203 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(204)) NTERM_KLL           ! 204 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(205)) NTERM_KLLD          ! 205 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(206)) NTERM_KLLDn         ! 206 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(207)) NTERM_KLLs          ! 207 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(208)) NTERM_KLLDs         ! 208 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(209)) NTERM_KMM           ! 209 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(210)) NTERM_KMMD          ! 210 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(211)) NTERM_KMSM          ! 211 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(212)) NTERM_KMSMn         ! 212 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(213)) NTERM_KMSMs         ! 213 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(214)) NTERM_KNM           ! 214 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(215)) NTERM_KNMD          ! 215 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(216)) NTERM_KNN           ! 216 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(217)) NTERM_KNND          ! 217 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(218)) NTERM_KOO           ! 218 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(219)) NTERM_KOOD          ! 219 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(220)) NTERM_KOOs          ! 220 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(221)) NTERM_KOODs         ! 221 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(222)) NTERM_KRL           ! 222 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(223)) NTERM_KRLD          ! 223 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(224)) NTERM_KRR           ! 224 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(225)) NTERM_KRRD          ! 225 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(226)) NTERM_KRRcb         ! 226 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(227)) NTERM_KRRcbs        ! 227 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(228)) NTERM_KRRcbn        ! 228 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(229)) NTERM_KXX           ! 229 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(230)) NTERM_KSS           ! 230 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(231)) NTERM_KSSD          ! 231 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(232)) NTERM_KSSe          ! 232 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(233)) NTERM_KSSDe         ! 233 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(234)) NTERM_LMN           ! 234 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(235)) NTERM_LTM           ! 235 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(236)) NTERM_MAA           ! 236 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(237)) NTERM_MAO           ! 237 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(238)) NTERM_MFF           ! 238 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(239)) NTERM_MFS           ! 239 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(240)) NTERM_MGG           ! 240 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(241)) NTERM_MGGC          ! 241 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(242)) NTERM_MGGE          ! 242 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(243)) NTERM_MGGS          ! 243 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(244)) NTERM_MLL           ! 244 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(245)) NTERM_MLLn          ! 245 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(246)) NTERM_MLLs          ! 246 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(247)) NTERM_MLR           ! 247 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(248)) NTERM_MMM           ! 248 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(249)) NTERM_MNM           ! 249 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(250)) NTERM_MNN           ! 250 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(251)) NTERM_MOO           ! 251 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(252)) NTERM_MPF0          ! 252 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(253)) NTERM_MRL           ! 253 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(254)) NTERM_MRN           ! 254 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(255)) NTERM_MRR           ! 255 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(256)) NTERM_MRRcb         ! 256 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(257)) NTERM_MRRcbn        ! 257 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(258)) NTERM_MXX           ! 258 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(259)) NTERM_MXXn          ! 259 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(260)) NTERM_MSS           ! 260 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(261)) NTERM_PA            ! 261 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(262)) NTERM_PF            ! 262 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(263)) NTERM_PFYS          ! 263 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(264)) NTERM_PG            ! 264 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(265)) NTERM_PHIXA         ! 265 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(266)) NTERM_PHIXG         ! 266 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(267)) NTERM_PHIZG         ! 267 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(268)) NTERM_PHIZL         ! 268 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(269)) NTERM_PHIZL1        ! 269 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(270)) NTERM_PHIZL2        ! 270 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(271)) NTERM_PL            ! 271 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(272)) NTERM_PM            ! 272 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(273)) NTERM_PN            ! 273 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(274)) NTERM_PO            ! 274 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(275)) NTERM_PR            ! 275 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(276)) NTERM_PS            ! 276 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(277)) NTERM_QM            ! 277 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(278)) NTERM_QS            ! 278 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(279)) NTERM_QSYS          ! 279 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(280)) NTERM_RMG           ! 280 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(281)) NTERM_RMM           ! 281 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(282)) NTERM_RMN           ! 282 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(283)) NTERM_ULL           ! 283 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(284)) NTERM_ULLI          ! 284 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(285)) NTSUB               ! 285 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(286)) NUM_CB_DOFS         ! 286 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(287)) NUM_EIGENS          ! 287 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(288)) NUM_KLLD_DIAG_ZEROS ! 288 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(289)) NUM_MLL_DIAG_ZEROS  ! 289 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(290)) NUM_MPCSIDS         ! 290 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(291)) NUM_PARTVEC_RECORDS ! 291 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(292)) NUM_PCHD_SPC1       ! 292 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(293)) NUM_SPC_RECORDS     ! 293 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(294)) NUM_SPC1_RECORDS    ! 294 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(295)) NUM_SUPT_CARDS      ! 295 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(296)) NUM_USET_RECORDS    ! 296 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(297)) NUM_USETSTR         ! 297 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(298)) NUM_USET            ! 298 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(299)) NUM_USET_U1         ! 299 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(300)) NUM_USET_U2         ! 300 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(301)) NVEC                ! 301 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(302)) NVVEC               ! 302 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(303)) PCH_LINE_NUM        ! 303 (From module SCONTR)
      READ(L1A,160,IOSTAT=IOCHKI(304)) SETLEN              ! 304 (From module SCONTR)

      DO I=1,NUMIO
         IF (IOCHKI(I) /= 0) THEN
            REC_NO = I
            CALL READERR ( IOCHKI(I), LINK1A, MESSAG, REC_NO, OUNT, WRITE_F04 )
            JERR = JERR + 1
         ENDIF
      ENDDO 
 
! Read PARAM's (some need to stay the same in a restart)

      MESSAG = 'PARAMS THAT NEED TO STAY SAME IN A RESTART'

      DO I=1,14
         IOCHKI(I) = 0
      ENDDO

      READ(L1A,191,IOSTAT=IOCHKI( 1)) ELFORCEN
      READ(L1A,191,IOSTAT=IOCHKI( 2)) HEXAXIS
      READ(L1A,191,IOSTAT=IOCHKI( 3)) MATSPARS
      READ(L1A,191,IOSTAT=IOCHKI( 4)) MIN4TRED
      READ(L1A,191,IOSTAT=IOCHKI( 5)) QUAD4TYP
      READ(L1A,191,IOSTAT=IOCHKI( 6)) QUADAXIS
      READ(L1A,191,IOSTAT=IOCHKI( 7)) SPARSTOR
      READ(L1A,192,IOSTAT=IOCHKI( 8)) IORQ1M
      READ(L1A,192,IOSTAT=IOCHKI( 9)) IORQ1S
      READ(L1A,192,IOSTAT=IOCHKI(10)) IORQ1B
      READ(L1A,192,IOSTAT=IOCHKI(11)) IORQ2B
      READ(L1A,192,IOSTAT=IOCHKI(12)) IORQ2T
      READ(L1A,193,IOSTAT=IOCHKI(13)) CBMIN3
      READ(L1A,193,IOSTAT=IOCHKI(14)) CBMIN4

      DO I=1,14
         IF (IOCHKI(I) /= 0) THEN
            REC_NO = I
            CALL READERR ( IOCHKI(I), LINK1A, MESSAG, REC_NO, OUNT, WRITE_F04 )
            JERR = JERR + 1
         ENDIF
      ENDDO 
 
! Read COMM

      MESSAG = 'COMM                    '
      READ(L1A,103,IOSTAT=IOCHKI(1)) (COMM(I),I=0,49)
      IF (IOCHKI(1) /= 0) THEN
         REC_NO = -99
         CALL READERR ( IOCHKI(1), LINK1A, MESSAG, REC_NO, OUNT, WRITE_F04 )
         JERR = JERR + 1
      ENDIF

! Finished reading L1A, so close:
 
      CALL FILE_CLOSE ( L1A, LINK1A, CLOSE_STAT, WRITE_F04 )
 
! Check JERR and stop if > 0
 
      IF (JERR > 0) THEN
         WRITE(SC1,911) JERR, LINK1A
         CALL OUTA_HERE ( 'Y' )                                    ! Errors reading file LINK1A, so quit
      ENDIF
 
! **********************************************************************************************************************************
      IF ((WRT_LOG >= SUBR_BEGEND) .AND. (WRITE_F04 == 'Y')) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  103 FORMAT(1X,50A1)

  110 FORMAT(1X,I14)

  120 FORMAT(1X,A)

  140 FORMAT(1X,I14)

  151 FORMAT(1X,I14,1X,A,1X,A,1X,A)

  160 FORMAT(I15)

  191 FORMAT(6X,A9)

  192 FORMAT(7X,I8)

  193 FORMAT(1X,1ES14.6)

  911 FORMAT(/,' PROCESSING TERMINATED DUE TO ABOVE ',I8,' ERRORS READING FILE ',/,A)
 
  944 FORMAT(' *ERROR  944: PROGRAMMING ERROR IN SUBROUTINE ',A,/,                                                                 &
                     /,14X,'ATTEMPT TO EXCEED MAX_FIL = ',I4,' NUMBER OF FILES IN WRITING FILE STATUS')

! **********************************************************************************************************************************
 
      END SUBROUTINE READ_L1A
