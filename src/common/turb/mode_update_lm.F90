!MNH_LIC Copyright 2006-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
      MODULE MODE_UPDATE_LM
IMPLICIT NONE
CONTAINS
SUBROUTINE UPDATE_LM(D,HLBCX,HLBCY,PLM,PLEPS)
!     #################################################################
!
!!****  *UPDATE_LM* - routine to set external points for mixing length
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------   
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!        
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine UPDATE_LM)
!!
!!    AUTHOR
!!    ------
!!      V. Masson        * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    april 2006
!!       V.Masson : Exchange of East and North sides
!!   J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!         
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
USE MODD_PARAMETERS
USE MODD_DIMPHYEX,   ONLY: DIMPHYEX_t
!
USE MODE_ll
!
IMPLICIT NONE
!
!
!*       0.1   declarations of arguments
!
TYPE(DIMPHYEX_t),       INTENT(IN)    :: D
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X boundary type
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y boundary type
!
REAL, DIMENSION(D%NIT,D%NJT,D%NKT), INTENT(INOUT) :: PLM   ! mixing length
REAL, DIMENSION(D%NIT,D%NJT,D%NKT), INTENT(INOUT) :: PLEPS ! dissipative length
!
!*       0.2   declarations of local variables
!
INTEGER             :: IIB      ! First physical index in x direction
INTEGER             :: IJB      ! First physical index in y direction
INTEGER             :: IIE      ! last  physical index in x direction
INTEGER             :: IJE      ! last  physical index in y direction
INTEGER             :: JI       ! loop index
!
LOGICAL                :: GNORTH, GSOUTH, GWEST, GEAST
TYPE(LIST_ll), POINTER :: TZLM_ll   ! list of fields to exchange
INTEGER                :: IINFO_ll       ! return code of parallel routine
!
!-------------------------------------------------------------------------------

!$acc data present_crm(PLM,PLEPS)
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS :
!              ----------------------------
IIB = D%NIB
IIE = D%NIE
IJB = D%NJB
IJE = D%NJE
NULLIFY(TZLM_ll)
!
GWEST  = ( HLBCX(1) /= 'CYCL' .AND. LWEST_ll() )
GEAST  = ( HLBCX(2) /= 'CYCL' .AND. LEAST_ll() )
GSOUTH = ( HLBCY(1) /= 'CYCL' .AND. LSOUTH_ll() )
GNORTH = ( HLBCY(2) /= 'CYCL' .AND. LNORTH_ll() )

!-------------------------------------------------------------------------------
!
!*       2.  UPDATE HALOs :
!            -------------
!
!
!!$IF(NHALO == 1) THEN
!$acc update_crm self(PLM,PLEPS)
  CALL ADD3DFIELD_ll( TZLM_ll, PLM,   'UPDATE_LM::PLM'   )
  CALL ADD3DFIELD_ll( TZLM_ll, PLEPS, 'UPDATE_LM::PLEPS' )
  CALL UPDATE_HALO_ll(TZLM_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZLM_ll)
!$acc update_crm device(PLM,PLEPS)
!!$END IF
!
!-------------------------------------------------------------------------------
!
!*       3.  UPDATE EXTERNAL POINTS OF GLOBAL DOMAIN:
!            ---------------------------------------
!

IF ( GWEST ) THEN
  !$acc kernels async
  PLM  (IIB-1,:,:) = PLM  (IIB,:,:)
  PLEPS(IIB-1,:,:) = PLEPS(IIB,:,:)
  !$acc end kernels
END IF
IF ( GEAST ) THEN
  !$acc kernels async
  PLM  (IIE+1,:,:) = PLM  (IIE,:,:)
  PLEPS(IIE+1,:,:) = PLEPS(IIE,:,:)
  !$acc end kernels
END IF
IF ( GSOUTH ) THEN
  !$acc kernels async 
  DO JI=1,SIZE(PLM,1)
    PLM  (JI,IJB-1,:) = PLM  (JI,IJB,:)
    PLEPS(JI,IJB-1,:) = PLEPS(JI,IJB,:)
  END DO
  !$acc end kernels
END IF
IF ( GNORTH ) THEN
  !$acc kernels async
  DO JI=1,SIZE(PLM,1)
    PLM  (JI,IJE+1,:) = PLM  (JI,IJE,:)
    PLEPS(JI,IJE+1,:) = PLEPS(JI,IJE,:)
 END DO
 !$acc end kernels
END IF
!$acc wait

!$acc end data

!-----------------------------------------------------------------------------
END SUBROUTINE UPDATE_LM
END MODULE MODE_UPDATE_LM
