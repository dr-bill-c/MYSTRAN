! ###############################################################################################################################
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

   MODULE GET_INI_FILNAM_Interface

   INTERFACE

      SUBROUTINE GET_INI_FILNAM  ( MYSTRAN_DIR, MYSTRAN_DIR_LEN, INIFIL_NAME_LEN )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, F04, INIFIL, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, PROG_NAME
      USE TIMDAT, ONLY                :  TSEC 

      IMPLICIT NONE
 
      CHARACTER(FILE_NAM_MAXLEN*BYTE), INTENT(IN):: MYSTRAN_DIR

      INTEGER(LONG), INTENT(IN)       :: MYSTRAN_DIR_LEN   ! Length of MYSTRAN_DIR (not including trailing blanks)
      INTEGER(LONG), INTENT(OUT)      :: INIFIL_NAME_LEN   ! Length of INI file name (incl path)
 
      END SUBROUTINE GET_INI_FILNAM

   END INTERFACE

   END MODULE GET_INI_FILNAM_Interface

