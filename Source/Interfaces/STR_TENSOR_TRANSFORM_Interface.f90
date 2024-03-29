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

   MODULE STR_TENSOR_TRANSFORM_Interface

   INTERFACE

      SUBROUTINE STR_TENSOR_TRANSFORM ( STRESS_TENSOR, STRESS_CORD_SYS )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NCORD
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  CORD, RCORD, TE
      USE SUBR_BEGEND_LEVELS, ONLY    :  STR_TENSOR_TRANSFORM_BEGEND

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: STRESS_CORD_SYS   ! Actual coord system ID for stress/strain/engr force output
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = STR_TENSOR_TRANSFORM_BEGEND

      REAL(DOUBLE), INTENT(INOUT)     :: STRESS_TENSOR(3,3)! 2D stress tensor (eqn 6 above)
      REAL(DOUBLE)                    :: DUM33(3,3)        ! Intermediate array used in calc outputs

      END SUBROUTINE STR_TENSOR_TRANSFORM

   END INTERFACE

   END MODULE STR_TENSOR_TRANSFORM_Interface

