!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###############
MODULE MODD_CST
!     ###############
!
!!****  *MODD_CST* - declaration of Physic constants
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the
!     Physics constants.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_CST)
!!
!!    AUTHOR
!!    ------
!!      V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    16/05/94
!!      J. Stein    02/01/95  add xrholw
!!      J.-P. Pinty 13/12/95  add XALPI,XBETAI,XGAMI
!!      J. Stein    25/07/97  add XTH00
!!      V. Masson   05/10/98  add XRHOLI
!!      C. Mari     31/10/00  add NDAYSEC
!!      V. Masson   01/03/03  add conductivity of ice
!!      R. El Khatib 04/08/14 add pre-computed quantities
!!      J.Escobar : 10/2017 : for real*4 , add XMNH_HUGE_12_LOG
!  J.L. Redelsperger 03/2021: add constants for ocean penetrating solar
!  S. Riette      01/2022: introduction of a structure
!  P. Wautelet 20/05/2022: add RASTA cloud radar wavelength
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
   IMPLICIT NONE

   SAVE

   TYPE CST_t
      !
      !*       1.     FUNDAMENTAL CONSTANTS
      !                ---------------------
      REAL :: XPI                !< Pi
      REAL :: XKARMAN            !< von karman constant
      REAL :: XLIGHTSPEED        !< light speed
      REAL :: XPLANCK            !< Planck constant
      REAL :: XBOLTZ             !< Boltzman constant
      REAL :: XAVOGADRO          !< Avogadro number
      !
      !*       2.     ASTRONOMICAL CONSTANTS
      !                ----------------------
      REAL :: XDAY, XSIYEA, XSIDAY !< day duration, sideral year duration, sideral day duration
      INTEGER :: NDAYSEC         !< Number of seconds in a day
      REAL :: XOMEGA             !< Earth rotation
      !
      !*       3.     TERRESTRIAL GEOIDE CONSTANTS
      !                ----------------------------
      REAL :: XRADIUS            !< Earth radius
      REAL :: XG                 !< Gravity constant
      !
      !*       4.     REFERENCE PRESSURE
      !                -------------------
      REAL :: XP00               !< Reference pressure
      REAL :: XP00OCEAN          !< Reference pressure for ocean model
      REAL :: XRH00OCEAN         !< Reference density for ocean model
      REAL :: XTH00              !< reference value  for the potential temperature
      REAL :: XTH00OCEAN         !< Ref value for pot temp in ocean model
      REAL :: XSA00OCEAN         !< Ref value for SAlinity in ocean model
      !
      !*       5.     RADIATION CONSTANTS
      !                -------------------
      REAL :: XSTEFAN, XI0        !< Stefan-Boltzman constant, solar constant
      !
      !*       6.     THERMODYNAMIC CONSTANTS
      !                -----------------------
      REAL :: XMD, XMV            !< Molar mass of dry air and molar mass of vapor
      REAL :: XRD, XRV            !< Gaz constant for dry air, gaz constant for vapor
      REAL :: XEPSILO            !< XMV/XMD
      REAL :: XCPD, XCPV          !< Cpd (dry air), Cpv (vapor)
      REAL :: XRHOLW             !< Volumic mass of liquid water
      REAL :: XCL, XCI            !< Cl (liquid), Ci (ice)
      REAL :: XTT                !< Triple point temperature
      REAL :: XLVTT              !< Vaporization heat constant
      REAL :: XLSTT              !< Sublimation heat constant
      REAL :: XLMTT              !< Melting heat constant
      REAL :: XESTT              !< Saturation vapor pressure  at triple point temperature
      REAL :: XALPW, XBETAW, XGAMW !< Constants for saturation vapor pressure function
      REAL :: XALPI, XBETAI, XGAMI !< Constants for saturation vapor pressure function over solid ice
      REAL :: XCONDI             !< thermal conductivity of ice (W m-1 K-1)
      REAL :: XALPHAOC           !< thermal expansion coefficient for ocean (K-1)
      REAL :: XBETAOC            !< Haline contraction coeff for ocean (S-1)
     !  REAL :: XROC = 0.69          !< coeff for SW penetration in  Ocean (Hoecker et al)
     !  REAL :: XD1 = 1.1            !< coeff for SW penetration in  Ocean (Hoecker et al)
     !  REAL :: XD2 = 23.            !< coeff for SW penetration in  Ocean (Hoecker et al)
      ! Values used in SURFEX CMO
      !REAL :: XROC=0.58
      !REAL :: XD1=0.35
      !REAL :: XD2=23.
      REAL :: XRHOLI             !< Volumic mass of ice
      !
      !*       7.     PRECOMPUTED CONSTANTS
      !                ---------------------
      REAL :: RDSRV              !< XRD/XRV
      REAL :: RDSCPD             !< XRD/XCPD
      REAL :: RINVXP00           !< 1./XP00
      !
      !*       8.     MACHINE PRECISION VALUE DEPENDING of REAL4/8 USE
      !                ---------------------
      REAL :: XMNH_TINY          !< minimum real on this machine
      REAL :: XMNH_TINY_12       !< sqrt(minimum real on this machine)
      REAL :: XMNH_EPSILON       !< minimum space with 1.0
      REAL :: XMNH_HUGE          !< maximum real on this machine
      REAL :: XMNH_HUGE_12_LOG   !< maximum log(sqrt(real)) on this machine
      REAL :: XEPS_DT            !< default value for DT test
      REAL :: XRES_FLAT_CART     !< default     flat&cart residual tolerance
      REAL :: XRES_OTHER         !< default not flat&cart residual tolerance
      REAL :: XRES_PREP          !< default     prep      residual tolerance
   END TYPE CST_t

!
      !*       1.     FUNDAMENTAL CONSTANTS
      !                ---------------------
   REAL:: XPI                !< Pi
   REAL:: XKARMAN            !< von karman constant
   REAL:: XLIGHTSPEED        !< light speed
   REAL:: XPLANCK            !< Planck constant
   REAL:: XBOLTZ             !< Boltzman constant
   REAL:: XAVOGADRO          !< Avogadro number
   !
   !*       2.     ASTRONOMICAL CONSTANTS
   !                ----------------------
   REAL:: XDAY, XSIYEA, XSIDAY !< day duration, sideral year duration, sideral day duration
   INTEGER :: NDAYSEC         !< Number of seconds in a day
   REAL:: XOMEGA             !< Earth rotation
   !
   !*       3.     TERRESTRIAL GEOIDE CONSTANTS
   !                ----------------------------
   REAL:: XRADIUS            !< Earth radius
   REAL:: XG                 !< Gravity constant
   !
   !*       4.     REFERENCE PRESSURE
   !                -------------------
   REAL:: XP00               !< Reference pressure
   REAL:: XP00OCEAN          !< Reference pressure for ocean model
   REAL:: XRH00OCEAN         !< Reference density for ocean model
   REAL:: XTH00              !< reference value  for the potential temperature
   REAL:: XTH00OCEAN         !< Ref value for pot temp in ocean model
   REAL:: XSA00OCEAN         !< Ref value for SAlinity in ocean model
   !
   !*       5.     RADIATION CONSTANTS
   !                -------------------
   REAL:: XSTEFAN, XI0        !< Stefan-Boltzman constant, solar constant
   !
   !*       6.     THERMODYNAMIC CONSTANTS
   !                -----------------------
   REAL:: XMD, XMV            !< Molar mass of dry air and molar mass of vapor
   REAL:: XRD, XRV            !< Gaz constant for dry air, gaz constant for vapor
   REAL:: XEPSILO            !< XMV/XMD
   REAL:: XCPD, XCPV          !< Cpd (dry air), Cpv (vapor)
   REAL:: XRHOLW             !< Volumic mass of liquid water
   REAL:: XCL, XCI            !< Cl (liquid), Ci (ice)
   REAL:: XTT                !< Triple point temperature
   REAL:: XLVTT              !< Vaporization heat constant
   REAL:: XLSTT              !< Sublimation heat constant
   REAL:: XLMTT              !< Melting heat constant
   REAL:: XESTT              !< Saturation vapor pressure  at triple point temperature
   REAL:: XALPW, XBETAW, XGAMW !< Constants for saturation vapor pressure function
   REAL:: XALPI, XBETAI, XGAMI !< Constants for saturation vapor pressure function over solid ice
   REAL:: XCONDI             !< thermal conductivity of ice (W m-1 K-1)
   REAL:: XALPHAOC           !< thermal expansion coefficient for ocean (K-1)
   REAL:: XBETAOC            !< Haline contraction coeff for ocean (S-1)
  !  REAL:: XROC = 0.69          !< coeff for SW penetration in  Ocean (Hoecker et al)
  !  REAL:: XD1 = 1.1            !< coeff for SW penetration in  Ocean (Hoecker et al)
  !  REAL:: XD2 = 23.            !< coeff for SW penetration in  Ocean (Hoecker et al)
   ! Values used in SURFEX CMO
   !REAL:: XROC=0.58
   !REAL:: XD1=0.35
   !REAL:: XD2=23.
   REAL:: XRHOLI             !< Volumic mass of ice
   !
   !*       7.     PRECOMPUTED CONSTANTS
   !                ---------------------
   REAL:: RDSRV              !< XRD/XRV
   REAL:: RDSCPD             !< XRD/XCPD
   REAL:: RINVXP00           !< 1./XP00
   !
   !*       8.     MACHINE PRECISION VALUE DEPENDING of REAL, PROTECTED4/8 USE
   !                ---------------------
   REAL:: XMNH_TINY          !< minimum REALon this machine
   REAL:: XMNH_TINY_12       !< sqrt(minimum REALon this machine)
   REAL:: XMNH_EPSILON       !< minimum space with 1.0
   REAL:: XMNH_HUGE          !< maximum REALon this machine
   REAL:: XMNH_HUGE_12_LOG   !< maximum log(sqrt(REAL, PROTECTED)) on this machine
   REAL:: XEPS_DT            !< default value for DT test
   REAL:: XRES_FLAT_CART     !< default     flat&cart residual tolerance
   REAL:: XRES_OTHER         !< default not flat&cart residual tolerance
   REAL :: XRES_PREP          !< default     prep      residual tolerance
!
TYPE(CST_t), target :: RCST
 !  
CONTAINS

   SUBROUTINE CST_INIT(CST)
     
      IMPLICIT NONE

      TYPE(CST_t), intent(in) :: CST

      XPI = CST%XPI
      XDAY = CST%XDAY
      XSIYEA = CST%XSIYEA
      XSIDAY = CST%XSIDAY
      XKARMAN = CST%XKARMAN
      XLIGHTSPEED = CST%XLIGHTSPEED
      XPLANCK = CST%XPLANCK
      XBOLTZ = CST%XBOLTZ
      XAVOGADRO = CST%XAVOGADRO
      XRADIUS = CST%XRADIUS
      XOMEGA = CST%XOMEGA
      XG = CST%XG
      XP00 = CST%XP00
      XP00OCEAN = CST%XP00OCEAN
      XRH00OCEAN = CST%XRH00OCEAN
      XSTEFAN = CST%XSTEFAN
      XI0 = CST%XI0
      XMD = CST%XMD
      XMV = CST%XMV
      XRD = CST%XRD
      XRV = CST%XRV
      XEPSILO = CST%XEPSILO
      XCPD = CST%XCPD
      XCPV = CST%XCPV
      XRHOLW = CST%XRHOLW
      XCL = CST%XCL
      XCI = CST%XCI
      XTT = CST%XTT
      XLVTT = CST%XLVTT
      XLSTT = CST%XLSTT
      XLMTT = CST%XLMTT
      XESTT = CST%XESTT
      XALPW = CST%XALPW
      XBETAW = CST%XBETAW
      XGAMW = CST%XGAMW
      XALPI = CST%XALPI
      XBETAI = CST%XBETAI
      XGAMI = CST%XGAMI
      XCONDI = CST%XCONDI
      XALPHAOC = CST%XALPHAOC
      XBETAOC = CST%XBETAOC
      XTH00 = CST%XTH00
      XTH00OCEAN = CST%XTH00OCEAN
      XSA00OCEAN = CST%XSA00OCEAN
      XRHOLI = CST%XRHOLI
      NDAYSEC = CST%NDAYSEC
      RDSRV = CST%RDSRV
      RDSCPD = CST%RDSCPD
      RINVXP00 = CST%RINVXP00
      XMNH_TINY = CST%XMNH_TINY
      XMNH_TINY_12 = CST%XMNH_TINY_12
      XMNH_EPSILON = CST%XMNH_EPSILON
      XMNH_HUGE = CST%XMNH_HUGE
      XMNH_HUGE_12_LOG = CST%XMNH_HUGE_12_LOG
      XEPS_DT = CST%XEPS_DT
      XRES_FLAT_CART = CST%XRES_FLAT_CART
      XRES_OTHER = CST%XRES_OTHER
      XRES_PREP = CST%XRES_PREP
   END SUBROUTINE CST_INIT
!
   SUBROUTINE SETUP_CST()
      !     ##################
      !
     !!****  *INI_CST * - routine to initialize the module MODD_CST
     !!
     !!    PURPOSE
     !!    -------
      !       The purpose of this routine is to initialize  the physical constants
      !     stored in  module MODD_CST.
      !
      !
     !!**  METHOD
     !!    ------
     !!      The physical constants are set to their numerical values
     !!
     !!
     !!    EXTERNAL
     !!    --------
     !!
     !!    IMPLICIT ARGUMENTS
     !!    ------------------
     !!      Module MODD_CST     : contains physical constants
     !!
     !!    REFERENCE
     !!    ---------
     !!      Book2 of the documentation (module MODD_CST, routine INI_CST)
     !!
     !!
     !!    AUTHOR
     !!    ------
     !!          V. Ducrocq       * Meteo France *
     !!
     !!    MODIFICATIONS
     !!    -------------
     !!      Original    18/05/94
     !!      J. Stein    02/01/95  add the volumic mass of liquid water
     !!      J.-P. Pinty 13/12/95  add the water vapor pressure over solid ice
     !!      J. Stein    29/06/97  add XTH00
     !!      V. Masson   05/10/98  add XRHOLI
     !!      C. Mari     31/10/00  add NDAYSEC
     !!      V. Masson   01/03/03  add XCONDI
     !!      J. Escobar  28/03/2014 for pb with emissivity/aerosol reset XMNH_TINY=1.0e-80 in real8 case
     !!      R. El Khatib 04/08/14 add pre-computed quantities
     !!      P. Marguinaud 04/10/16 Port to single precision
     !!      J.Escobar : 10/2017 : for real*4 , add XMNH_HUGE_12_LOG
     !!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
     !!      J.Escobar : 5/10/2018 : for real*4 ,higher value for XEPS_DT = 1.5e-4
     !!
      !-------------------------------------------------------------------------------
      !
      !*       0.    DECLARATIONS
      !              ------------
      !
      USE MODD_PRECISION, ONLY: MNHREAL, MNHREAL32, MNHREAL64
      USE MODE_MSG, ONLY: PRINT_MSG, NVERB_FATAL
      USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
      !
      IMPLICIT NONE
      !
      TYPE(CST_t) :: CST
      !
      REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
      !-------------------------------------------------------------------------------
      IF (LHOOK) CALL DR_HOOK('INI_CST', 0, ZHOOK_HANDLE)
      print *, "debug : mode_ini_cst.F90 - call cst_associate"

      associate(XPI => CST%XPI, &
        & XDAY => CST%XDAY, &
        & XSIYEA => CST%XSIYEA, &
        & XSIDAY => CST%XSIDAY, &
        & XKARMAN => CST%XKARMAN, &
        & XLIGHTSPEED => CST%XLIGHTSPEED, &
        & XPLANCK => CST%XPLANCK, &
        & XBOLTZ => CST%XBOLTZ, &
        & XAVOGADRO => CST%XAVOGADRO, &
        & XRADIUS => CST%XRADIUS, &
        & XOMEGA => CST%XOMEGA, &
        & XG => CST%XG, &
        & XP00 => CST%XP00, &
        & XP00OCEAN => CST%XP00OCEAN, &
        & XRH00OCEAN => CST%XRH00OCEAN, &
        & XSTEFAN => CST%XSTEFAN, &
        & XI0 => CST%XI0, &
        & XMD => CST%XMD, &
        & XMV => CST%XMV, &
        & XRD => CST%XRD, &
        & XRV => CST%XRV, &
        & XEPSILO => CST%XEPSILO, &
        & XCPD => CST%XCPD, &
        & XCPV => CST%XCPV, &
        & XRHOLW => CST%XRHOLW, &
        & XCL => CST%XCL, &
        & XCI => CST%XCI, &
        & XTT => CST%XTT, &
        & XLVTT => CST%XLVTT, &
        & XLSTT => CST%XLSTT, &
        & XLMTT => CST%XLMTT, &
        & XESTT => CST%XESTT, &
        & XALPW => CST%XALPW, &
        & XBETAW => CST%XBETAW, &
        & XGAMW => CST%XGAMW, &
        & XALPI => CST%XALPI, &
        & XBETAI => CST%XBETAI, &
        & XGAMI => CST%XGAMI, &
        & XCONDI => CST%XCONDI, &
        & XALPHAOC => CST%XALPHAOC, &
        & XBETAOC => CST%XBETAOC, &
        & XTH00 => CST%XTH00, &
        & XTH00OCEAN => CST%XTH00OCEAN, &
        & XSA00OCEAN => CST%XSA00OCEAN, &
        & XRHOLI => CST%XRHOLI, &
        & NDAYSEC => CST%NDAYSEC, &
        & RDSRV => CST%RDSRV, &
        & RDSCPD => CST%RDSCPD, &
        & RINVXP00 => CST%RINVXP00, &
        & XMNH_TINY => CST%XMNH_TINY, &
        & XMNH_TINY_12 => CST%XMNH_TINY_12, &
        & XMNH_EPSILON => CST%XMNH_EPSILON, &
        & XMNH_HUGE => CST%XMNH_HUGE, &
        & XMNH_HUGE_12_LOG => CST%XMNH_HUGE_12_LOG, &
        & XEPS_DT => CST%XEPS_DT, &
        & XRES_FLAT_CART => CST%XRES_FLAT_CART, &
        & XRES_OTHER => CST%XRES_OTHER, &
        & XRES_PREP => CST%XRES_PREP)

         !
         !*       1.     FUNDAMENTAL CONSTANTS
         !                ---------------------
         !
         XPI = 2.*ASIN(1.)
         XKARMAN = 0.4
         XLIGHTSPEED = 299792458.
         XPLANCK = 6.6260755E-34
         XBOLTZ = 1.380658E-23
         XAVOGADRO = 6.0221367E+23
         !
         !-------------------------------------------------------------------------------
         !
         !*       2.     ASTRONOMICAL CONSTANTS
         !                ----------------------
         !
         XDAY = 86400.
         XSIYEA = 365.25*XDAY*2.*XPI/6.283076
         XSIDAY = XDAY/(1.+XDAY/XSIYEA)
         XOMEGA = 2.*XPI/XSIDAY
         NDAYSEC = 24*3600 ! Number of seconds in a day
         !
         !-------------------------------------------------------------------------------!
         !
         !
         !*       3.     TERRESTRIAL GEOIDE CONSTANTS
         !                ----------------------------
         !
         XRADIUS = 6371229.
         XG = 9.80665
         !
         !-------------------------------------------------------------------------------
         !
         !*       4.     REFERENCE PRESSURE
         !                -------------------
         !
         ! Ocean model cst same as in 1D/CMO SURFEX
         ! values used in ini_cst to overwrite XP00 and XTH00
         XRH00OCEAN = 1024.
         XTH00OCEAN = 286.65
         XSA00OCEAN = 32.6
         XP00OCEAN = 201.E5
         !Atmospheric model
         XP00 = 1.E5
         XTH00 = 300.
         !-------------------------------------------------------------------------------
         !
         !*       5.     RADIATION CONSTANTS
         !                -------------------
         !
         ! Original: XSTEFAN = 2.* XPI**5 * XBOLTZ**4 / (15.* XLIGHTSPEED**2 * XPLANCK**3)
         ! Juan: XSTEFAN = ( 2.* XPI**5 / 15. ) * ( (XBOLTZ / XPLANCK) * XBOLTZ ) * (XBOLTZ/(XLIGHTSPEED*XPLANCK))**2
         ! Philippe Marguinaud: XSTEFAN = REAL (2._8* REAL (XPI, 8)**5 * REAL (XBOLTZ, 8)**4 / (15._8* REAL (XLIGHTSPEED, 8)**2 * REAL (XPLANCK, 8)**3))
         XSTEFAN = REAL(2._MNHREAL64*REAL(XPI, MNHREAL64)**5*REAL(XBOLTZ, MNHREAL64)**4/ &
                 & (15._MNHREAL64*REAL(XLIGHTSPEED, MNHREAL64)**2*REAL(XPLANCK, MNHREAL64)**3))
         XI0 = 1370.
         !
         !-------------------------------------------------------------------------------
         !
         !*       6.     THERMODYNAMIC CONSTANTS
         !                -----------------------
         !
         XMD = 28.9644E-3
         XMV = 18.0153E-3
         XRD = XAVOGADRO*XBOLTZ/XMD
         XRV = XAVOGADRO*XBOLTZ/XMV
         XEPSILO = XMV/XMD
         XCPD = 7.*XRD/2.
         XCPV = 4.*XRV
         XRHOLW = 1000.
         XRHOLI = 900.
         XCONDI = 2.22
         XCL = 4.218E+3
         XCI = 2.106E+3
         XTT = 273.16
         XLVTT = 2.5008E+6
         XLSTT = 2.8345E+6
         XLMTT = XLSTT - XLVTT
         XESTT = 611.14
         XGAMW = (XCL - XCPV)/XRV
         XBETAW = (XLVTT/XRV) + (XGAMW*XTT)
         XALPW = LOG(XESTT) + (XBETAW/XTT) + (XGAMW*LOG(XTT))
         XGAMI = (XCI - XCPV)/XRV
         XBETAI = (XLSTT/XRV) + (XGAMI*XTT)
         XALPI = LOG(XESTT) + (XBETAI/XTT) + (XGAMI*LOG(XTT))
         ! Values identical to ones used in CMO1D in SURFEX /could be modified
         ! Coefficient of thermal expansion of water (K-1)
         XALPHAOC = 1.9E-4
         ! Coeff of Haline contraction coeff (S-1)
         XBETAOC = 7.7475E-4
         !
         !*       7.     PRECOMPUTED CONSTANTS
         !                ---------------------
         !
         RDSRV = XRD/XRV
         RDSCPD = XRD/XCPD
         RINVXP00 = 1./XP00
         !
         !*       8.     MACHINE PRECISION VALUE DEPENDING of REAL4/8 USE
         !                ---------------------
         !
         XMNH_EPSILON = EPSILON(XMNH_EPSILON)
         XMNH_HUGE = HUGE(XMNH_HUGE)
         XMNH_HUGE_12_LOG = LOG(SQRT(XMNH_HUGE))

         IF (MNHREAL == MNHREAL64) THEN
            XMNH_TINY = 1.0e-80_MNHREAL
            XEPS_DT = 1.0e-5_MNHREAL
            XRES_FLAT_CART = 1.0e-12_MNHREAL
            XRES_OTHER = 1.0e-9_MNHREAL
            XRES_PREP = 1.0e-8_MNHREAL
         ELSEIF (MNHREAL == MNHREAL32) THEN
            XMNH_TINY = TINY(XMNH_TINY)
            XEPS_DT = 1.5e-4_MNHREAL
            XRES_FLAT_CART = 1.0e-12_MNHREAL
            XRES_OTHER = 1.0e-7_MNHREAL
            XRES_PREP = 1.0e-4_MNHREAL
         ELSE
            CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'INI_CST', 'Invalid MNH_REAL')
         END IF
         XMNH_TINY_12 = SQRT(XMNH_TINY)
         !
         !-------------------------------------------------------------------------------
         !
         IF (LHOOK) CALL DR_HOOK('INI_CST', 1, ZHOOK_HANDLE)

         RCST = CST


      END ASSOCIATE

      
      CALL CST_INIT(CST)
   END SUBROUTINE SETUP_CST
!
   SUBROUTINE PRINT_CST(KULOUT)
      INTEGER, INTENT(IN) :: KULOUT

      WRITE (UNIT=KULOUT, FMT='('' MODD_CST: FUNDAMENTAL CONSTANTS '')')
      WRITE (UNIT=KULOUT, FMT='('' XPI = '',E10.4,'' XKARMAN = '',E10.4,'' XLIGHTSPEED = '',E10.4,/, &
           &'' XPLANCK = '',E10.4,'' XBOLTZ = '',E10.4,'' XAVOGADRO = '',E10.4)')&
           &XPI, XKARMAN, XLIGHTSPEED,&
           &XPLANCK, XBOLTZ, XAVOGADRO

      WRITE (UNIT=KULOUT, FMT='('' MODD_CST: ASTRONOMICAL CONSTANTS '')')
      WRITE (UNIT=KULOUT, FMT='('' XDAY = '',E10.4,'' XSIYEA = '',E10.4,'' XSIDAY = '',E10.4,/,&
           &'' XOMEGA = '',E10.4,'' NDAYSEC = '', I6)')&
           &XDAY, XSIYEA, XSIDAY,&
           &XOMEGA, NDAYSEC

      WRITE (UNIT=KULOUT, FMT='('' MODD_CST: TERRESTRIAL GEOIDE CONSTANTS '')')
      WRITE (UNIT=KULOUT, FMT='('' XRADIUS = '',E10.4,'' XG = '',E10.4)')&
           &XRADIUS, XG

      WRITE (UNIT=KULOUT, FMT='('' MODD_CST: REFERENCE '')')
      WRITE (UNIT=KULOUT, FMT='('' XRH00OCEAN = '',E10.4,'' XTH00OCEAN = '',E10.4,'' XSA00OCEAN = '',E10.4,/,&
           &'' XP00OCEAN = '',E10.4,'' XP00 = '',E10.4,'' XTH00 = '',E10.4)')&
           &XRH00OCEAN, XTH00OCEAN, XSA00OCEAN,&
           &XP00OCEAN, XP00, XTH00

      WRITE (UNIT=KULOUT, FMT='('' MODD_CST: RADIATION CONSTANTS '')')
      WRITE (UNIT=KULOUT, FMT='('' XSTEFAN = '',E10.4,'' XIO = '',E10.4)')&
           &XSTEFAN, XI0

      WRITE (UNIT=KULOUT, FMT='('' MODD_CST: THERMODYNAMIC CONSTANTS '')')
      WRITE (UNIT=KULOUT, FMT='('' XMD = '',E10.4,'' XMV = '',E10.4,'' XRD = '',E10.4,/,&
           &'' XRV = '',E10.4,'' XEPSILO = '',E10.4,'' XCPD = '',E10.4,/,&
           &'' XCPV = '',E10.4,'' XRHOLW = '',E10.4,'' XRHOLI = '',E10.4,/,&
           &'' XCONDI = '',E10.4,'' XCL = '',E10.4,'' XCI = '',E10.4,/,&
           &'' XTT = '',E10.4,'' XLVTT = '',E10.4,'' XLSTT = '',E10.4,/,&
           &'' XLMTT = '',E10.4,'' XESTT = '',E10.4,'' XGAMW = '',E10.4,/,&
           &'' XBETAW = '',E10.4,'' XALPW = '',E10.4,'' XGAMI = '',E10.4,/,&
           &'' XBETAI = '',E10.4,'' XALPI = '',E10.4,'' XALPHAOC = '',E10.4,/,&
           &'' XBETAOC = '',E10.4)')&
           &XMD, XMV, XRD,&
           &XRV, XEPSILO, XCPD,&
           &XCPV, XRHOLW, XRHOLI,&
           &XCONDI, XCL, XCI,&
           &XTT, XLVTT, XLSTT,&
           &XLMTT, XESTT, XGAMW,&
           &XBETAW, XALPW, XGAMI,&
           &XBETAI, XALPI, XALPHAOC,&
           &XBETAOC

      WRITE (UNIT=KULOUT, FMT='('' MODD_CST: PRECOMPUTED CONSTANTS '')')
      WRITE (UNIT=KULOUT, FMT='('' RDSRV = '',E10.4,'' RDSCPD = '',E10.4,'' RINVXP00 = '',E10.4)')&
           &RDSRV, RDSCPD, RINVXP00

      WRITE (UNIT=KULOUT, FMT='('' MODD_CST: MACHINE PRECISION VALUE DEPENDING of REAL4/8 USE '')')
      WRITE (UNIT=KULOUT, FMT='('' XMNH_EPSILON = '',E10.4,'' XMNH_HUGE = '',E10.4,'' XMNH_HUGE_12_LOG = '',E10.4,/,&
           &'' XMNH_TINY = '',E10.4,'' XEPS_DT '',E10.4,'' XRES_FLAT_CART = '',E10.4,/,&
           &'' XRES_OTHER = '',E10.4,'' XRES_PREP = '',E10.4,'' XMNH_TINY_12 = '',E10.4)')&
           &XMNH_EPSILON, XMNH_HUGE, XMNH_HUGE_12_LOG,&
           &XMNH_TINY, XEPS_DT, XRES_FLAT_CART,&
           &XRES_OTHER, XRES_PREP, XMNH_TINY_12
!
   END SUBROUTINE PRINT_CST
!
END MODULE MODD_CST

