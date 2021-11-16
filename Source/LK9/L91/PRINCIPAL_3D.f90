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
 
      SUBROUTINE PRINCIPAL_3D ( STR, PRINCIPAL_STR, MEAN, VONMISES, SIG_OCT, TAU_OCT )
 
! Calculates principal stresses or stains in solid elems:

!     (a) If STR input vector is stress then outputs are stress. 
!     (b) If STR input vector is strain then outputs are strain 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, TWO, THREE
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  PRINCIPAL_3D_BEGEND
 
      USE PRINCIPAL_3D_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PRINCIPAL_3D'

      INTEGER(LONG)                   :: I,J         = 0    ! DO loop indices
      INTEGER(LONG)                   :: INFO        = 0    ! An output from subr ROOTS_3D, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PRINCIPAL_3D_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: STR(6)             ! Stress or strain vector
      REAL(DOUBLE), INTENT(OUT)       :: MEAN               ! Mean stresses or strains
      REAL(DOUBLE), INTENT(OUT)       :: PRINCIPAL_STR(3)   ! Principal stresses or strains
      REAL(DOUBLE), INTENT(OUT)       :: SIG_OCT            ! Octrahedral normal stress or strain
      REAL(DOUBLE), INTENT(OUT)       :: TAU_OCT            ! Octrahedral shear  stress or strain
      REAL(DOUBLE), INTENT(OUT)       :: VONMISES           ! Octahedral stress or strain
      REAL(DOUBLE)                    :: SIG1,SIG2,SIG3     ! Principal stresses or strains
!xx   REAL(DOUBLE)                    :: I1,I2,I3           ! Invariants of Cauchy stress or strain tensor      
      REAL(DOUBLE)                    :: Q(3,3)             ! Output from subr ROOTS_3D, called herein (transform to princ dir's)
      REAL(DOUBLE)                    :: STR_TENSOR(3,3)    ! Principal stresses or strains in a 3x3 matrix
      REAL(DOUBLE)                    :: TAU12,TAU23,TAU13  ! Max shear stresses or strains
 
      EXTERNAL                        :: DGEMM

      INTRINSIC                       :: DABS, DATAN2, DSQRT
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!xx   I1 = STR(1) + STR(2) + STR(3)

!xx   I2 = STR(1)*STR(2) + STR(2)*STR(3) + STR(3)*STR(1) - STR(4)*STR(4) - STR(5)*STR(5) - STR(6)*STR(6)

!xx   I3 = STR(1)*STR(2)*STR(3) + TWO*STR(4)*STR(5)*STR(6) - STR(1)*STR(5)*STR(5) - STR(2)*STR(6)*STR(6) - STR(3)*STR(4)*STR(4)

      STR_TENSOR(1,1) = STR(1)
      STR_TENSOR(2,1) = STR(4)   ;   STR_TENSOR(2,2) = STR(2)
      STR_TENSOR(3,1) = STR(6)   ;   STR_TENSOR(3,2) = STR(5)   ;   STR_TENSOR(3,3) = STR(3)
      DO I=1,2
         DO J=I+1,3
            STR_TENSOR(I,J) = STR_TENSOR(J,I)
         ENDDO
      ENDDO

      CALL ROOTS_3D ( STR_TENSOR, Q, INFO )

      SIG1  = STR_TENSOR(3,3)                           ! Reverse order to get principal stresses/strains going from high to low
      SIG2  = STR_TENSOR(2,2)
      SIG3  = STR_TENSOR(1,1)

      TAU12 = HALF*(SIG1 - SIG2)
      TAU23 = HALF*(SIG2 - SIG3)
      TAU13 = HALF*(SIG1 - SIG3)

      PRINCIPAL_STR(1) = SIG1
      PRINCIPAL_STR(2) = SIG2
      PRINCIPAL_STR(3) = SIG3
      VONMISES = DSQRT(HALF*((SIG1 - SIG2)*(SIG1 - SIG2) + (SIG2 - SIG3)*(SIG2 - SIG3) + (SIG3 - SIG1)*(SIG3 - SIG1)))
      SIG_OCT  = (SIG1 + SIG2 + SIG3)/THREE
      TAU_OCT  = TWO*DSQRT((TAU12*TAU12 + TAU23*TAU23 + TAU13*TAU13))/THREE
      MEAN     = SIG_OCT

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################
 
      SUBROUTINE ROOTS_3D ( STR_TENSOR, Q, INFO )
 
! Jacobi solution for 3x3 eigenvalue problem used in finding principal moments of inertia
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  PRINCIPAL_3D_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE LAPACK_STD_EIG_1
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ROOTS_3D'
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: CALLED_SUBR = ' ' ! Name of a called subr (for output error purposes)
      CHARACTER( 1*BYTE), PARAMETER   :: JOBZ      = 'V'   ! Indicates to solve for eigenvalues and vectors in LAPACK subr DSYEV
      CHARACTER( 1*BYTE)              :: TRANSA            ! Transpose indicator for subr DGEMM
      CHARACTER( 1*BYTE)              :: TRANSB            ! Transpose indicator for subr DGEMM
      CHARACTER( 1*BYTE), PARAMETER   :: UPLO      = 'U'   ! Indicates array KBAND is the upper triangular part of KAA

      INTEGER(LONG), INTENT(OUT)      :: INFO              ! = 0:  successful exit
!                                                            < 0:  if INFO = -i, the i-th argument had an illegal value
!                                                            > 0:  if INFO = i, the algorithm failed to converge; i off_diag
!                                                            elems of an intermediate tridiag form did not converge to zero
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: N         = 3     ! Order of matrix STR_TENSOR
      INTEGER(LONG), PARAMETER        :: LWORK     = 3*N-1 ! Size of array WORK
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PRINCIPAL_3D_BEGEND + 1

      REAL(DOUBLE) , INTENT(INOUT)    :: STR_TENSOR(N,N)   ! On entry, the stress or strain tensor
!                                                            On exit , the principal stresses or strains (if INFO = 0)
      REAL(DOUBLE) , INTENT(OUT)      :: Q(N,N)            ! Transformation from basic to principal directions
!                                                            Q = Z'. That is;
!                                                            U(principal) = Q*U(basic), where U is a displ vector 
      REAL(DOUBLE)                    :: Z(N,N)            ! When subr DSYEV is called, Z = input STR_TENSOR
!                                                            When subr DSYEV returns, Z = eigenvecs: PMOI = Z'*STR_TENSOR*Z
      REAL(DOUBLE)                    :: DUM(N,N)          ! Intermediate result in calculating Z'*STR_TENSOR*Z 
      REAL(DOUBLE) , PARAMETER        :: ALPHA     = ONE   ! Scalar multiplier for subr DGEMM
      REAL(DOUBLE) , PARAMETER        :: BETA      = ZERO  ! Scalar multiplier for subr DGEMM
      REAL(DOUBLE)                    :: PSTR(N)           ! Principal stresses or strains in a 1-D array
      REAL(DOUBLE)                    :: WORK(LWORK)       ! Workspace
 
!xx   EXTERNAL                        :: DGEMM

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      INFO = 0

      DO I=1,N
         DO J=1,N
            Q(I,J) = ZERO
         ENDDO
      ENDDO

! Use LAPACK driver DSYEV to get all eigenvalues (the principal stresses/strains) and eigenvectors (returned in Z)

      DO I=1,N
         DO J=1,N
            Z(I,J) = STR_TENSOR(I,J)
            Q(I,J) = ZERO
         ENDDO
      ENDDO
 
      CALL DSYEV ( JOBZ, UPLO, N, Z, N, PSTR, WORK, LWORK, INFO )

! Set STR_TENSOR matrix to zero for off-diag terms and to STR_TENSOR for diag terms. Set Q = Z' (' = transpose) 

      CALLED_SUBR = 'DSTEQR'      
      IF      (INFO < 0) THEN                              ! LAPACK subr XERBLA should have reported error on an illegal argument
!                                                            in a called LAPACK subr, so we should not have gotten here
         WRITE(ERR,993) SUBR_NAME, CALLED_SUBR
         WRITE(F06,993) SUBR_NAME, CALLED_SUBR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ELSE IF (INFO > 0) THEN  ! No convergence in subr DSYEV

        WARN_ERR = WARN_ERR + 1
         WRITE(ERR,1001) CALLED_SUBR, SUBR_NAME
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1001) CALLED_SUBR, SUBR_NAME
         ENDIF
         RETURN

      ELSE                                                 ! INFO=0, so no error

         DO I=1,N                                          ! Set Q = Z'
            DO J=1,N
               Q(J,I) = Z(I,J)
            ENDDO
         ENDDO

         TRANSA = 'N'                                      ! STR_TENSOR <-- Z'*STR_TENSOR*Z, diags should be PSTR's,
         TRANSB = 'N'                                      !  off-diags should be zero
         CALL DGEMM ( TRANSA, TRANSB, N, N, N, ALPHA, STR_TENSOR, N, Z,   N, BETA, DUM,  N )
         TRANSA = 'T'
         TRANSB = 'N'
         CALL DGEMM ( TRANSA, TRANSB, N, N, N, ALPHA, Z,    N, DUM, N, BETA, STR_TENSOR, N )

      ENDIF  
      

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  993 FORMAT(' *ERROR   993: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' LAPACK SUBR XERBLA SHOULD HAVE REPORTED AN ERROR ON AN ILLEGAL ARGUMENT IN A CALL TO LAPACK SUBR '    &
                    ,/,15X,A,' (OR A SUBR CALLED BY IT) AND THEN ABORTED')

 1001 FORMAT(' *WARNING    : LAPACK SUBR ',A8,' CALLED BY SUBROUTINE ',A                                                           &
                    ,/,14X,' CANNOT CONVERGE IN ATTEMPTING TO FIND PRINCIPAL STRESSES/STRAINS'                                     &
                    ,/,14X,' THE ALGORITHM HAS FAILED TO FIND ALL THE EIGENVALUES (PRINCIPAL STRESSES/STRAINS) IN 90 ITERATIONS'   &
                    ,/,14X,' PRINCIPAL STRESSES/STRAINS AND PRINCIPAL DIRECTIONS CANNOT BE FOUND')
 
! **********************************************************************************************************************************

      END SUBROUTINE ROOTS_3D

      END SUBROUTINE PRINCIPAL_3D
