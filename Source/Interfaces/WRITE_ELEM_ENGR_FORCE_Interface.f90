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

   MODULE WRITE_ELEM_ENGR_FORCE_Interface

   INTERFACE

      SUBROUTINE WRITE_ELEM_ENGR_FORCE ( JSUB, NUM, IHDR, ITABLE )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, INT_SC_NUM, NDOFR, NUM_CB_DOFS, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, OGEL
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ELEM_ENGR_FORCE_BEGEND
      USE MODEL_STUF, ONLY            :  ELEM_ONAME, LABEL, SCNUM, STITLE, TITLE, TYPE
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: IHDR              ! Indicator of whether to write an output header
  
      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG), INTENT(INOUT)    :: ITABLE            ! the current op2 subtable, should be -3, -5, ...
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ELEM_ENGR_FORCE_BEGEND
  
      END SUBROUTINE WRITE_ELEM_ENGR_FORCE

   END INTERFACE

   END MODULE WRITE_ELEM_ENGR_FORCE_Interface

