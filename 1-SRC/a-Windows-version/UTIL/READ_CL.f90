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
 
      SUBROUTINE READ_CL ( FILNAM, NC_FILNAM )
 
! Calls GET_COMMAND_LINE to get command line string, when MYSTRAN is invoked, (which should be the name of the input file),
! and counts the number (NC_FILNAM) of characters in the name (leading blanks ignored). 
 
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN

      USE READ_CL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(OUT)   :: FILNAM            ! File name on command line
 
      INTEGER(LONG), INTENT(OUT)      :: NC_FILNAM         ! Length, in chars, of FILNAM (with leading blanks removed)
      INTEGER(LONG)                   :: I                 ! DO loop index
 
      INTRINSIC                       :: GET_COMMAND_ARGUMENT

! **********************************************************************************************************************************
! Initialize outputs

      NC_FILNAM = FILE_NAM_MAXLEN

      FILNAM(1:FILE_NAM_MAXLEN) = ' '
 
      CALL GET_COMMAND_ARGUMENT (1, FILNAM )
 
      DO I=FILE_NAM_MAXLEN,1,-1
         IF (FILNAM(I:I) /= ' ') THEN
            EXIT
         ELSE
            NC_FILNAM = NC_FILNAM - 1
         ENDIF
      ENDDO   
 
      RETURN
 
      END SUBROUTINE READ_CL
