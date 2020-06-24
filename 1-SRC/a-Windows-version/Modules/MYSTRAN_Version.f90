! Change log (changes following completion of Version 2.02 on 11/12/04)

! 12/24/04: New module

! ----------------------------------------------------------------------------------------------------------------------------------
! Change log (changes following completion of Version 4.05 on 04/16/07)

! Add MYSTRAN_COMMENT (I may use this to tell user that this is a beta version)

! ----------------------------------------------------------------------------------------------------------------------------------
! Change log (changes following completion of Version 7.01 updated on 11/09/14)

! 08/30/15 Change MYSTRAN_VER_AUTH to MYSTRAN_AUTHOR

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

      MODULE MYSTRAN_Version

! Data used for writing the MYSTRAN version and related data to the F06 file

      USE PENTIUM_II_KIND, ONLY      :  BYTE, LONG, DOUBLE
  
      IMPLICIT NONE

      SAVE

      CHARACTER(256*BYTE)            :: MYSTRAN_COMMENT  = '*** Please report any problems to the author at dbcase29@gmail.com ***'
      CHARACTER(  8*BYTE), PARAMETER :: MYSTRAN_VER_NUM  = '11.0'
      CHARACTER(  3*BYTE), PARAMETER :: MYSTRAN_VER_MONTH= 'Jun'
      CHARACTER(  2*BYTE), PARAMETER :: MYSTRAN_VER_DAY  = '22'
      CHARACTER(  4*BYTE), PARAMETER :: MYSTRAN_VER_YEAR = '2020'
      CHARACTER( 33*BYTE), PARAMETER :: MYSTRAN_AUTHOR   = 'MYSTRAN developed by Dr Bill Case'

      END MODULE MYSTRAN_Version
