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

      MODULE LINK5_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE LINK5

      USE TIME_INIT_Interface
      USE OURDAT_Interface
      USE OURTIM_Interface
      USE READ_L1A_Interface
      USE OUTA_HERE_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE ALLOCATE_COL_VEC_Interface
      USE READ_MATRIX_1_Interface
      USE FILE_OPEN_Interface
      USE READERR_Interface
      USE FILE_CLOSE_Interface
      USE ALLOCATE_EIGEN1_MAT_Interface
      USE READ_L1M_Interface
      USE ALLOCATE_FULL_MAT_Interface
      USE TDOF_COL_NUM_Interface
      USE BUILD_A_LR_Interface
      USE DEALLOCATE_COL_VEC_Interface
      USE BUILD_F_AO_Interface
      USE BUILD_N_FS_Interface
      USE BUILD_G_NM_Interface
      USE DEALLOCATE_MISC_MAT_Interface
      USE ALLOCATE_MISC_MAT_Interface
      USE GET_UG_123_IN_GRD_ORD_Interface
      USE RENORM_Interface
      USE WRITE_L1M_Interface
      USE EXPAND_PHIXA_TO_PHIXG_Interface
      USE DEALLOCATE_SPARSE_MAT_Interface
      USE GET_GRID_NUM_COMPS_Interface
      USE EIG_SUMMARY_Interface
      USE OUTPUT4_PROC_Interface
      USE DEALLOCATE_EIGEN1_MAT_Interface
      USE WRITE_L1A_Interface
      USE CHK_ARRAY_ALLOC_STAT_Interface
      USE WRITE_ALLOC_MEM_TABLE_Interface
      USE FILE_INQUIRE_Interface
      USE WRITE_FILNAM_Interface
      USE I4FLD_Interface

      END MODULE LINK5_USE_IFs
