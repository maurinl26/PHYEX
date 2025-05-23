!MNH_LIC Copyright 1994-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
MODULE MODE_SBL_DEPTH
IMPLICIT NONE
CONTAINS
!     ######spl
      SUBROUTINE SBL_DEPTH(D,CSTURB,PZZ,PFLXU,PFLXV,PWTHV,PLMO,PSBL_DEPTH)
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
!     #################################################################
!
!
!!****  *SBL_DEPTH* - computes SBL depth
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    SBL is defined as the layer where momentum flux is equal to XSBL_FRAC of its surface value
!!    
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson  * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         nov. 2005
!!      26/02/2020   T.Nagel Correction of SBL depth computation in neutral stratification
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CTURB, ONLY: CSTURB_t
USE MODD_DIMPHYEX, ONLY: DIMPHYEX_t
USE MODD_PARAMETERS, ONLY : XUNDEF
!
USE MODE_BL_DEPTH_DIAG, ONLY : BL_DEPTH_DIAG
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
TYPE(DIMPHYEX_t),       INTENT(IN)    :: D
TYPE(CSTURB_t),         INTENT(IN)    :: CSTURB
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PZZ       ! altitude of flux levels
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PFLXU     ! u'w'
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PFLXV     ! v'w'
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PWTHV     ! buoyancy flux
REAL, DIMENSION(D%NIJT),   INTENT(IN)    :: PLMO      ! Monin-Obukhov length
REAL, DIMENSION(D%NIJT),   INTENT(INOUT) :: PSBL_DEPTH! boundary layer height
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
!
INTEGER                                  :: JLOOP,JIJ,JK    ! loop counter
INTEGER :: IKB,IKE,IIJB,IIJE,IKT   ! index value for the Beginning
REAL, DIMENSION(D%NIJT) :: ZQ0      ! surface buoyancy flux
REAL, DIMENSION(D%NIJT) :: ZWU      ! surface friction u'w'
REAL, DIMENSION(D%NIJT) :: ZWV      ! surface friction v'w'
REAL, DIMENSION(D%NIJT) :: ZUSTAR2  ! surface friction
REAL, DIMENSION(D%NIJT) :: ZSBL_DYN ! SBL wih dynamical criteria
REAL, DIMENSION(D%NIJT,D%NKT) :: ZWIND
                                         ! intermediate wind for SBL calculation
REAL, DIMENSION(D%NIJT) :: ZSBL_THER! SBL wih thermal   criteria
REAL, DIMENSION(D%NIJT) :: ZA       ! ponderation coefficient
!----------------------------------------------------------------------------
!
!* initialisations
!
!
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('SBL_DEPTH',0,ZHOOK_HANDLE)
!
IKB=D%NKTB
IKE=D%NKTE
IIJE=D%NIJE
IIJB=D%NIJB
IKT=D%NKT
!
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZWU(IIJB:IIJE) = PFLXU(IIJB:IIJE,IKB)
ZWV(IIJB:IIJE) = PFLXV(IIJB:IIJE,IKB)
ZQ0(IIJB:IIJE) = PWTHV(IIJB:IIJE,IKB)
!
ZUSTAR2(IIJB:IIJE) = SQRT(ZWU(IIJB:IIJE)**2+ZWV(IIJB:IIJE)**2)
!
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
!----------------------------------------------------------------------------
!
!* BL and SBL diagnosed with friction criteria
!
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
ZWIND(IIJB:IIJE,1:IKT)=SQRT(PFLXU(IIJB:IIJE,1:IKT)**2+PFLXV(IIJB:IIJE,1:IKT)**2)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
CALL BL_DEPTH_DIAG(D,D%NKT,ZUSTAR2,PZZ(:,IKB),ZWIND,PZZ,CSTURB%XFTOP_O_FSURF,ZSBL_DYN)
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZSBL_DYN(IIJB:IIJE) = CSTURB%XSBL_O_BL * ZSBL_DYN(IIJB:IIJE)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
!
!----------------------------------------------------------------------------
!
!* BL and SBL diagnosed with buoyancy flux criteria
!
CALL BL_DEPTH_DIAG(D,D%NKT,ZQ0,PZZ(:,IKB),PWTHV,PZZ,CSTURB%XFTOP_O_FSURF,ZSBL_THER)
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZSBL_THER(IIJB:IIJE)= CSTURB%XSBL_O_BL * ZSBL_THER(IIJB:IIJE)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
!
!----------------------------------------------------------------------------
!
!* SBL depth
!
PSBL_DEPTH(:) = 0.
!$mnh_expand_where(JIJ=IIJB:IIJE)
WHERE (ZSBL_THER(IIJB:IIJE)> 0. .AND. ZSBL_DYN(IIJB:IIJE)> 0.) 
  PSBL_DEPTH(IIJB:IIJE) = MIN(ZSBL_THER(IIJB:IIJE),ZSBL_DYN(IIJB:IIJE))
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE)
!
!$mnh_expand_where(JIJ=IIJB:IIJE)
WHERE (ZSBL_THER(IIJB:IIJE)> 0. .AND. ZSBL_DYN(IIJB:IIJE)==0.) 
  PSBL_DEPTH(IIJB:IIJE) = ZSBL_THER(IIJB:IIJE)
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE)
!
!$mnh_expand_where(JIJ=IIJB:IIJE)
WHERE (ZSBL_THER(IIJB:IIJE)==0. .AND. ZSBL_DYN(IIJB:IIJE)> 0.) 
  PSBL_DEPTH(IIJB:IIJE) = ZSBL_DYN(IIJB:IIJE)
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE)
!
DO JLOOP=1,5
  !$mnh_expand_where(JIJ=IIJB:IIJE)
  WHERE (PLMO(IIJB:IIJE)/=XUNDEF .AND. ABS(PLMO(IIJB:IIJE))>=0.01 )
    ZA(IIJB:IIJE) = TANH(2.*PSBL_DEPTH(IIJB:IIJE)/PLMO(IIJB:IIJE))**2
    PSBL_DEPTH(IIJB:IIJE) = 0.2 * PSBL_DEPTH(IIJB:IIJE) + 0.8 * ((1.-ZA(IIJB:IIJE)) &
                                * ZSBL_DYN(IIJB:IIJE) + ZA(IIJB:IIJE) * ZSBL_THER(IIJB:IIJE) )
  END WHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE)
END DO
!$mnh_expand_where(JIJ=IIJB:IIJE)
WHERE (ABS(PLMO(IIJB:IIJE))<=0.01 ) 
  PSBL_DEPTH(IIJB:IIJE) = ZSBL_THER(IIJB:IIJE)
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE)
!$mnh_expand_where(JIJ=IIJB:IIJE)
WHERE (PLMO(IIJB:IIJE)==XUNDEF)
  PSBL_DEPTH(IIJB:IIJE) = ZSBL_DYN(IIJB:IIJE)
END WHERE
!$mnh_end_expand_where(JIJ=IIJB:IIJE)
!
!----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SBL_DEPTH',1,ZHOOK_HANDLE)
END SUBROUTINE SBL_DEPTH
END MODULE MODE_SBL_DEPTH
