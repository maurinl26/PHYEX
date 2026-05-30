! Typed scheme selectors for the PHYEX bridge.
!
! PHYEX dispatches its schemes on 4-character string codes (CMICRO='ICE3',
! CSCONV='NONE', CTURB='TKEL', ...). Those magic strings are error-prone: a typo
! is not a compile error, it silently selects the wrong (or no) scheme deep in
! the Fortran. We keep the legacy strings as the wire format INI_PHYEX expects,
! but everything *we* write refers to schemes by these named integer constants
! and converts to the string in exactly one place (the *_name functions below).
!
! ABI/SYNC CONTRACT: the integer ids here mirror phyex/enums.py on the Python
! side. If you add or renumber a scheme, change it in both places in the same
! commit. The *_name mapping is the single source of truth for the legacy codes.
MODULE phyex_bridge_enums
    USE ISO_C_BINDING, ONLY : C_INT
    IMPLICIT NONE

    ! Microphysics / cloud scheme (INI_PHYEX CMICRO)
    INTEGER(C_INT), PARAMETER :: PHYEX_MICRO_NONE = 0
    INTEGER(C_INT), PARAMETER :: PHYEX_MICRO_ICE3 = 1
    INTEGER(C_INT), PARAMETER :: PHYEX_MICRO_ICE4 = 2
    INTEGER(C_INT), PARAMETER :: PHYEX_MICRO_LIMA = 3

    ! Shallow convection scheme (INI_PHYEX CSCONV)
    INTEGER(C_INT), PARAMETER :: PHYEX_SCONV_NONE = 0
    INTEGER(C_INT), PARAMETER :: PHYEX_SCONV_EDKF = 1

    ! Turbulence scheme (INI_PHYEX CTURB)
    INTEGER(C_INT), PARAMETER :: PHYEX_TURB_NONE = 0
    INTEGER(C_INT), PARAMETER :: PHYEX_TURB_TKEL = 1

CONTAINS

    ! Map a microphysics id to the legacy CMICRO code. '????' for an unknown id
    ! makes INI_PHYEX reject it loudly rather than running a wrong scheme.
    PURE FUNCTION phyex_micro_name(id) RESULT(name)
        INTEGER(C_INT), INTENT(IN) :: id
        CHARACTER(LEN=4) :: name
        SELECT CASE (id)
        CASE (PHYEX_MICRO_NONE); name = 'NONE'
        CASE (PHYEX_MICRO_ICE3); name = 'ICE3'
        CASE (PHYEX_MICRO_ICE4); name = 'ICE4'
        CASE (PHYEX_MICRO_LIMA); name = 'LIMA'
        CASE DEFAULT;            name = '????'
        END SELECT
    END FUNCTION phyex_micro_name

    PURE FUNCTION phyex_sconv_name(id) RESULT(name)
        INTEGER(C_INT), INTENT(IN) :: id
        CHARACTER(LEN=4) :: name
        SELECT CASE (id)
        CASE (PHYEX_SCONV_NONE); name = 'NONE'
        CASE (PHYEX_SCONV_EDKF); name = 'EDKF'
        CASE DEFAULT;            name = '????'
        END SELECT
    END FUNCTION phyex_sconv_name

    PURE FUNCTION phyex_turb_name(id) RESULT(name)
        INTEGER(C_INT), INTENT(IN) :: id
        CHARACTER(LEN=4) :: name
        SELECT CASE (id)
        CASE (PHYEX_TURB_NONE); name = 'NONE'
        CASE (PHYEX_TURB_TKEL); name = 'TKEL'
        CASE DEFAULT;           name = '????'
        END SELECT
    END FUNCTION phyex_turb_name

END MODULE phyex_bridge_enums


MODULE phyex_bridge
    USE ISO_C_BINDING
    ! Import the original routines and required modules
    USE MODI_ICE_ADJUST, ONLY : ICE_ADJUST
    USE MODI_RAIN_ICE, ONLY : RAIN_ICE
    USE MODI_SHALLOW_CONVECTION, ONLY : SHALLOW_CONVECTION
    USE MODI_TURB, ONLY : TURB
    USE PARKIND1, ONLY : JPIM, JPRB
    USE MODD_DIMPHYEX, ONLY : DIMPHYEX_t
    USE MODD_CST, ONLY : CST_t, CST
    USE MODD_RAIN_ICE_PARAM_n
    USE MODD_RAIN_ICE_DESCR_n
    USE MODD_PARAM_ICE_n
    USE MODD_NEB_n, ONLY : NEB_t
    USE MODD_TURB_n, ONLY : TURB_t
    USE MODD_BUDGET, ONLY : TBUDGETCONF_t, TBUDGETDATA_PTR
    USE MODD_CONVPAR, ONLY : CONVPAR_t
    USE MODD_CONVPAR_SHAL, ONLY : CONVPAR_SHAL
    USE MODD_NSV, ONLY : NSV_t
    USE MODD_CTURB, ONLY : CSTURB_t, CSTURB
    USE MODD_LES, ONLY : TLES_t
    USE MODD_IO, ONLY : TFILEDATA
    USE MODD_ELEC_PARAM, ONLY : ELEC_PARAM_t
    USE MODD_ELEC_DESCR, ONLY : ELEC_DESCR_t
    USE MODE_INI_CST, ONLY : INI_CST
    USE MODI_INI_PHYEX, ONLY : INI_PHYEX
    USE MODD_PHYEX, ONLY : PHYEX_t
    USE phyex_bridge_enums, ONLY : phyex_micro_name, phyex_sconv_name, phyex_turb_name, &
                                   PHYEX_MICRO_ICE3, PHYEX_SCONV_NONE, PHYEX_TURB_TKEL

    IMPLICIT NONE

    ! Define working precision based on JPRB (adjusts to sp or dp build)
#ifdef SINGLE_PRECISION
    INTEGER, PARAMETER :: WP = C_FLOAT
#else
    INTEGER, PARAMETER :: WP = C_DOUBLE
#endif

    ! PHYEX configuration is initialized once per process: INI_PHYEX allocates
    ! module-global state (e.g. RAIN_ICE_DESCR) that must not be re-allocated.
    TYPE(PHYEX_t), SAVE :: G_PHYEX
    LOGICAL, SAVE       :: G_PHYEX_INIT = .FALSE.
    ! Scheme ids the process was initialized with (-1 = not yet initialized).
    ! INI_PHYEX allocates module-global state, so the scheme is fixed for the
    ! life of the process; these let the Python layer report/guard against an
    ! attempt to switch scheme after the first call.
    INTEGER(C_INT), SAVE :: G_MICRO_ID = -1
    INTEGER(C_INT), SAVE :: G_SCONV_ID = -1
    INTEGER(C_INT), SAVE :: G_TURB_ID  = -1

CONTAINS

    ! Initialize the full PHYEX configuration exactly once (idempotent). The
    ! scheme ids select CMICRO/CSCONV/CTURB; the first call wins (INI_PHYEX
    ! allocates global state and must not be re-run), so later calls with
    ! different ids are ignored here — the Python layer guards against that.
    SUBROUTINE ensure_phyex_init(timestep, micro_id, sconv_id, turb_id)
        REAL(WP), INTENT(IN) :: timestep
        INTEGER(C_INT), INTENT(IN) :: micro_id, sconv_id, turb_id
        TYPE(TFILEDATA) :: TPFILE
        INTEGER :: IULOUT
        REAL :: ZDZMIN
        IF (G_PHYEX_INIT) RETURN
        ! Send INI_PHYEX's informational output to a scratch unit instead of
        ! stdout, so importing/using the library doesn't dump constants.
        OPEN(NEWUNIT=IULOUT, STATUS='SCRATCH')
        ZDZMIN = 20.0
        TPFILE%NLU = 0
        ! Scheme selectors go through the typed enum -> legacy-string mapping
        ! (single source of truth) instead of inline magic strings.
        CALL INI_PHYEX('AROME ', TPFILE, .TRUE., IULOUT, 0, 1,            &
            REAL(timestep), ZDZMIN,                                       &
            phyex_micro_name(micro_id),                                  &
            phyex_sconv_name(sconv_id),                                  &
            phyex_turb_name(turb_id),                                    &
            LDDEFAULTVAL=.TRUE., LDREADNAM=.FALSE., LDCHECK=.FALSE.,      &
            KPRINT=0, LDINIT=.FALSE., PHYEX_OUT=G_PHYEX)
        G_PHYEX%MISC%LMFCONV      = .FALSE.
        G_PHYEX%MISC%OCOMPUTE_SRC = .TRUE.
        G_PHYEX%PARAM_ICEN%LWARM  = .TRUE.
        G_PHYEX%NEBN%LSUBG_COND   = .FALSE.   ! all-or-nothing adjustment
        G_PHYEX%NEBN%LSIGMAS      = .TRUE.
        G_PHYEX%NEBN%CFRAC_ICE_ADJUST = 'S'
        CALL INI_PHYEX('AROME ', TPFILE, .TRUE., IULOUT, 0, 1,           &
            REAL(timestep), ZDZMIN,                                      &
            phyex_micro_name(micro_id),                                 &
            phyex_sconv_name(sconv_id),                                 &
            phyex_turb_name(turb_id),                                   &
            LDDEFAULTVAL=.FALSE., LDREADNAM=.FALSE., LDCHECK=.FALSE.,    &
            KPRINT=0, LDINIT=.TRUE., PHYEX_IN=G_PHYEX, PHYEX_OUT=G_PHYEX)
        G_MICRO_ID = micro_id
        G_SCONV_ID = sconv_id
        G_TURB_ID  = turb_id
        G_PHYEX_INIT = .TRUE.
    END SUBROUTINE ensure_phyex_init

    ! C-callable: configure the process schemes (delegates to ensure_phyex_init).
    SUBROUTINE c_phyex_configure(timestep, micro_id, sconv_id, turb_id) &
            BIND(C, name="c_phyex_configure")
        REAL(WP), VALUE, INTENT(IN) :: timestep
        INTEGER(C_INT), VALUE, INTENT(IN) :: micro_id, sconv_id, turb_id
        CALL ensure_phyex_init(timestep, micro_id, sconv_id, turb_id)
    END SUBROUTINE c_phyex_configure

    ! C-callable: id of the microphysics scheme the process was initialized with
    ! (-1 if not yet initialized). Lets Python guard against scheme switching.
    FUNCTION c_phyex_active_micro() RESULT(id) BIND(C, name="c_phyex_active_micro")
        INTEGER(C_INT) :: id
        id = G_MICRO_ID
    END FUNCTION c_phyex_active_micro

    ! C-callable wrapper for ICE_ADJUST
    SUBROUTINE c_ice_adjust_wrap(                                          &
        nlon, nlev, krr, timestep,                                         &
        ptr_sigqsat, ptr_pabs, ptr_sigs, ptr_th, ptr_exn, ptr_exn_ref,    &
        ptr_rho_dry_ref, ptr_rv, ptr_rc, ptr_ri, ptr_rr, ptr_rs, ptr_rg,  &
        ptr_cf_mf, ptr_rc_mf, ptr_ri_mf,                                   &
        ptr_rvs, ptr_rcs, ptr_ris, ptr_ths,                                &
        ptr_cldfr, ptr_icldfr, ptr_wcldfr                                  &
    ) BIND(C, name="c_ice_adjust")
    
        ! C-compatible arguments (using WP for working precision)
        INTEGER(C_INT), VALUE, INTENT(IN) :: nlon, nlev, krr
        REAL(WP), VALUE, INTENT(IN) :: timestep
        
        ! C pointers for input arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_sigqsat      ! 1D: (nlon)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pabs         ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_sigs         ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_th           ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_exn          ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_exn_ref      ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rho_dry_ref  ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rv           ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rc           ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ri           ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rr           ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rs           ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rg           ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_cf_mf        ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rc_mf        ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ri_mf        ! 2D: (nlon, nlev)
        
        ! C pointers for input/output tendency arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rvs          ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rcs          ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ris          ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ths          ! 2D: (nlon, nlev)
        
        ! C pointers for output arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_cldfr        ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_icldfr       ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_wcldfr       ! 2D: (nlon, nlev)

        ! Fortran pointers to map C data (using WP for working precision)
        REAL(KIND=WP), POINTER, DIMENSION(:) :: f_sigqsat
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_pabs, f_sigs, f_th, f_exn, f_exn_ref
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_rho_dry_ref, f_rv, f_rc, f_ri
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_rr, f_rs, f_rg
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_cf_mf, f_rc_mf, f_ri_mf
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_rvs, f_rcs, f_ris, f_ths
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_cldfr, f_icldfr, f_wcldfr
        
        ! Local variables for PHYEX structures
        TYPE(DIMPHYEX_t) :: D
        TYPE(TBUDGETDATA_PTR), DIMENSION(0) :: TBUDGETS
        
        ! Additional required arrays (using WP)
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: PRHODJ, PZZ
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: PMFCONV
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: PWEIGHT_MF_CLOUD
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: PSSIO, PSSIU, PIFR
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: PSRCS
        LOGICAL :: LMFCONV, OCOMPUTE_SRC
        
        ! Convert C pointers to Fortran arrays
        CALL C_F_POINTER(ptr_sigqsat, f_sigqsat, [nlon])
        CALL C_F_POINTER(ptr_pabs, f_pabs, [nlon, nlev])
        CALL C_F_POINTER(ptr_sigs, f_sigs, [nlon, nlev])
        CALL C_F_POINTER(ptr_th, f_th, [nlon, nlev])
        CALL C_F_POINTER(ptr_exn, f_exn, [nlon, nlev])
        CALL C_F_POINTER(ptr_exn_ref, f_exn_ref, [nlon, nlev])
        CALL C_F_POINTER(ptr_rho_dry_ref, f_rho_dry_ref, [nlon, nlev])
        CALL C_F_POINTER(ptr_rv, f_rv, [nlon, nlev])
        CALL C_F_POINTER(ptr_rc, f_rc, [nlon, nlev])
        CALL C_F_POINTER(ptr_ri, f_ri, [nlon, nlev])
        CALL C_F_POINTER(ptr_rr, f_rr, [nlon, nlev])
        CALL C_F_POINTER(ptr_rs, f_rs, [nlon, nlev])
        CALL C_F_POINTER(ptr_rg, f_rg, [nlon, nlev])
        CALL C_F_POINTER(ptr_cf_mf, f_cf_mf, [nlon, nlev])
        CALL C_F_POINTER(ptr_rc_mf, f_rc_mf, [nlon, nlev])
        CALL C_F_POINTER(ptr_ri_mf, f_ri_mf, [nlon, nlev])
        CALL C_F_POINTER(ptr_rvs, f_rvs, [nlon, nlev])
        CALL C_F_POINTER(ptr_rcs, f_rcs, [nlon, nlev])
        CALL C_F_POINTER(ptr_ris, f_ris, [nlon, nlev])
        CALL C_F_POINTER(ptr_ths, f_ths, [nlon, nlev])
        CALL C_F_POINTER(ptr_cldfr, f_cldfr, [nlon, nlev])
        CALL C_F_POINTER(ptr_icldfr, f_icldfr, [nlon, nlev])
        CALL C_F_POINTER(ptr_wcldfr, f_wcldfr, [nlon, nlev])
        
        ! Initialize DIMPHYEX structure
        D%NIT = nlon
        D%NIB = 1
        D%NIE = nlon
        D%NJT = 1
        D%NJB = 1
        D%NJE = 1
        D%NKT = nlev
        D%NKL = 1        ! Ground to space ordering
        D%NKA = 1
        D%NKU = nlev
        D%NKB = 1
        D%NKE = nlev
        D%NKTB = 1
        D%NKTE = nlev
        D%NIBC = 1
        D%NJBC = 1
        D%NIEC = nlon
        D%NJEC = 1
        D%NIJT = nlon
        D%NIJB = 1
        D%NIJE = nlon
        D%NKLES = nlev
        D%NLESMASK = 0
        D%NLES_TIMES = 0
        
        ! Fully initialize the PHYEX configuration once per process.
        CALL ensure_phyex_init(timestep, PHYEX_MICRO_ICE3, PHYEX_SCONV_NONE, PHYEX_TURB_TKEL)
        
        ! Allocate and initialize additional required arrays
        ALLOCATE(PRHODJ(nlon, nlev))
        ALLOCATE(PZZ(nlon, nlev))
        ALLOCATE(PMFCONV(nlon, nlev))
        ALLOCATE(PWEIGHT_MF_CLOUD(nlon, nlev))
        ALLOCATE(PSSIO(nlon, nlev))
        ALLOCATE(PSSIU(nlon, nlev))
        ALLOCATE(PIFR(nlon, nlev))
        ALLOCATE(PSRCS(nlon, nlev))
        
        ! Compute PRHODJ from density and assume unit Jacobian
        PRHODJ = f_rho_dry_ref
        
        ! Set height field (simplified - could be passed as parameter)
        PZZ = 0.0_WP
        
        ! Initialize mass flux arrays
        PMFCONV = 0.0_WP
        PWEIGHT_MF_CLOUD = 0.0_WP
        LMFCONV = .FALSE.
        
        ! Initialize output arrays
        PSSIO = 0.0_WP
        PSSIU = 0.0_WP
        PIFR = 0.0_WP
        PSRCS = 0.0_WP
        OCOMPUTE_SRC = .TRUE.

        ! OpenACC data region for GPU execution
        !$acc data create(PRHODJ, PZZ, PMFCONV, PWEIGHT_MF_CLOUD, PSSIO, PSSIU, PIFR, PSRCS) &
        !$acc&     deviceptr(f_sigqsat, f_pabs, f_sigs, f_th, f_exn, f_exn_ref, f_rho_dry_ref) &
        !$acc&     deviceptr(f_rv, f_rc, f_ri, f_rr, f_rs, f_rg) &
        !$acc&     deviceptr(f_cf_mf, f_rc_mf, f_ri_mf) &
        !$acc&     deviceptr(f_rvs, f_rcs, f_ris, f_ths) &
        !$acc&     deviceptr(f_cldfr, f_icldfr, f_wcldfr)

        ! Call the actual ICE_ADJUST routine with the INI_PHYEX-populated config
        CALL ICE_ADJUST(                                                       &
            D, G_PHYEX%CST, G_PHYEX%RAIN_ICE_PARAMN, G_PHYEX%NEBN, G_PHYEX%TURBN, &
            G_PHYEX%PARAM_ICEN, G_PHYEX%MISC%TBUCONF, krr,                     &
            'BRID',                                                        &
            timestep, f_sigqsat,                                           &
            PRHODJ, f_exn_ref, f_rho_dry_ref, f_sigs, LMFCONV, PMFCONV,   &
            f_pabs, PZZ,                                                   &
            f_exn, f_cf_mf, f_rc_mf, f_ri_mf, PWEIGHT_MF_CLOUD,            &
            f_icldfr, f_wcldfr, PSSIO, PSSIU, PIFR,                        &
            f_rv, f_rc, f_rvs, f_rcs, f_th, f_ths,                         &
            OCOMPUTE_SRC, PSRCS, f_cldfr,                                  &
            f_rr, f_ri, f_ris, f_rs, f_rg, TBUDGETS, 0                     &
        )

        !$acc end data

        ! Cleanup
        DEALLOCATE(PRHODJ, PZZ, PMFCONV, PWEIGHT_MF_CLOUD)
        DEALLOCATE(PSSIO, PSSIU, PIFR, PSRCS)

    END SUBROUTINE c_ice_adjust_wrap

    ! C-callable wrapper for RAIN_ICE
    SUBROUTINE c_rain_ice_wrap(                                            &
        nlon, nlev, krr, timestep,                                         &
        ptr_exn, ptr_dzz, ptr_rhodj, ptr_rhodref, ptr_exnref, ptr_pabs,   &
        ptr_cldfr, ptr_icldfr, ptr_ssio, ptr_ssiu, ptr_ifr,               &
        ptr_tht, ptr_rvt, ptr_rct, ptr_rrt, ptr_rit, ptr_rst, ptr_rgt,    &
        ptr_sigs,                                                          &
        ptr_cit,                                                           &
        ptr_hlc_hrc, ptr_hlc_hcf, ptr_hli_hri, ptr_hli_hcf,               &
        ptr_ths, ptr_rvs, ptr_rcs, ptr_rrs, ptr_ris, ptr_rss, ptr_rgs,    &
        ptr_evap3d, ptr_rainfr,                                            &
        ptr_inprc, ptr_inprr, ptr_inprs, ptr_inprg, ptr_indep             &
    ) BIND(C, name="c_rain_ice")
    
        ! C-compatible arguments (using WP for working precision)
        INTEGER(C_INT), VALUE, INTENT(IN) :: nlon, nlev, krr
        REAL(WP), VALUE, INTENT(IN) :: timestep
        
        ! C pointers for 2D input arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_exn, ptr_dzz, ptr_rhodj
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rhodref, ptr_exnref, ptr_pabs
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_cldfr, ptr_icldfr
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ssio, ptr_ssiu, ptr_ifr
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_tht, ptr_rvt, ptr_rct
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rrt, ptr_rit, ptr_rst, ptr_rgt
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_sigs
        
        ! C pointers for 2D input/output arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_cit
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_hlc_hrc, ptr_hlc_hcf
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_hli_hri, ptr_hli_hcf
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ths, ptr_rvs, ptr_rcs
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_rrs, ptr_ris, ptr_rss, ptr_rgs
        
        ! C pointers for 2D output arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_evap3d, ptr_rainfr
        
        ! C pointers for 1D output arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_inprc, ptr_inprr
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_inprs, ptr_inprg, ptr_indep

        ! Fortran pointers to map C data (using WP for working precision)
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_exn, f_dzz, f_rhodj
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_rhodref, f_exnref, f_pabs
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_cldfr, f_icldfr
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_ssio, f_ssiu, f_ifr
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_tht, f_rvt, f_rct
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_rrt, f_rit, f_rst, f_rgt
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_sigs
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_cit
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_hlc_hrc, f_hlc_hcf
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_hli_hri, f_hli_hcf
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_ths, f_rvs, f_rcs
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_rrs, f_ris, f_rss, f_rgs
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_evap3d, f_rainfr
        REAL(KIND=WP), POINTER, DIMENSION(:) :: f_inprc, f_inprr
        REAL(KIND=WP), POINTER, DIMENSION(:) :: f_inprs, f_inprg, f_indep

        ! Local variables for PHYEX structures
        TYPE(DIMPHYEX_t) :: D
        TYPE(TBUDGETDATA_PTR), DIMENSION(0) :: TBUDGETS
        LOGICAL :: OELEC, OSEDIM_BEARD
        REAL(KIND=WP) :: PTHVREFZIKB

        ! Convert C pointers to Fortran arrays
        CALL C_F_POINTER(ptr_exn, f_exn, [nlon, nlev])
        CALL C_F_POINTER(ptr_dzz, f_dzz, [nlon, nlev])
        CALL C_F_POINTER(ptr_rhodj, f_rhodj, [nlon, nlev])
        CALL C_F_POINTER(ptr_rhodref, f_rhodref, [nlon, nlev])
        CALL C_F_POINTER(ptr_exnref, f_exnref, [nlon, nlev])
        CALL C_F_POINTER(ptr_pabs, f_pabs, [nlon, nlev])
        CALL C_F_POINTER(ptr_cldfr, f_cldfr, [nlon, nlev])
        CALL C_F_POINTER(ptr_icldfr, f_icldfr, [nlon, nlev])
        CALL C_F_POINTER(ptr_ssio, f_ssio, [nlon, nlev])
        CALL C_F_POINTER(ptr_ssiu, f_ssiu, [nlon, nlev])
        CALL C_F_POINTER(ptr_ifr, f_ifr, [nlon, nlev])
        CALL C_F_POINTER(ptr_tht, f_tht, [nlon, nlev])
        CALL C_F_POINTER(ptr_rvt, f_rvt, [nlon, nlev])
        CALL C_F_POINTER(ptr_rct, f_rct, [nlon, nlev])
        CALL C_F_POINTER(ptr_rrt, f_rrt, [nlon, nlev])
        CALL C_F_POINTER(ptr_rit, f_rit, [nlon, nlev])
        CALL C_F_POINTER(ptr_rst, f_rst, [nlon, nlev])
        CALL C_F_POINTER(ptr_rgt, f_rgt, [nlon, nlev])
        CALL C_F_POINTER(ptr_sigs, f_sigs, [nlon, nlev])
        CALL C_F_POINTER(ptr_cit, f_cit, [nlon, nlev])
        CALL C_F_POINTER(ptr_hlc_hrc, f_hlc_hrc, [nlon, nlev])
        CALL C_F_POINTER(ptr_hlc_hcf, f_hlc_hcf, [nlon, nlev])
        CALL C_F_POINTER(ptr_hli_hri, f_hli_hri, [nlon, nlev])
        CALL C_F_POINTER(ptr_hli_hcf, f_hli_hcf, [nlon, nlev])
        CALL C_F_POINTER(ptr_ths, f_ths, [nlon, nlev])
        CALL C_F_POINTER(ptr_rvs, f_rvs, [nlon, nlev])
        CALL C_F_POINTER(ptr_rcs, f_rcs, [nlon, nlev])
        CALL C_F_POINTER(ptr_rrs, f_rrs, [nlon, nlev])
        CALL C_F_POINTER(ptr_ris, f_ris, [nlon, nlev])
        CALL C_F_POINTER(ptr_rss, f_rss, [nlon, nlev])
        CALL C_F_POINTER(ptr_rgs, f_rgs, [nlon, nlev])
        CALL C_F_POINTER(ptr_evap3d, f_evap3d, [nlon, nlev])
        CALL C_F_POINTER(ptr_rainfr, f_rainfr, [nlon, nlev])
        CALL C_F_POINTER(ptr_inprc, f_inprc, [nlon])
        CALL C_F_POINTER(ptr_inprr, f_inprr, [nlon])
        CALL C_F_POINTER(ptr_inprs, f_inprs, [nlon])
        CALL C_F_POINTER(ptr_inprg, f_inprg, [nlon])
        CALL C_F_POINTER(ptr_indep, f_indep, [nlon])
        
        ! Initialize DIMPHYEX structure
        D%NIT = nlon
        D%NIB = 1
        D%NIE = nlon
        D%NJT = 1
        D%NJB = 1
        D%NJE = 1
        D%NKT = nlev
        D%NKL = 1
        D%NKA = 1
        D%NKU = nlev
        D%NKB = 1
        D%NKE = nlev
        D%NKTB = 1
        D%NKTE = nlev
        D%NIBC = 1
        D%NJBC = 1
        D%NIEC = nlon
        D%NJEC = 1
        D%NIJT = nlon
        D%NIJB = 1
        D%NIJE = nlon
        D%NKLES = nlev
        D%NLESMASK = 0
        D%NLES_TIMES = 0
        
        ! Fully initialize the PHYEX configuration once per process. This
        ! populates CST, PARAM_ICEN and the RAIN_ICE_PARAMN/RAIN_ICE_DESCRN
        ! microphysical constants via INI_PHYEX (HCLOUD='ICE3'), so RAIN_ICE
        ! no longer relies on caller-supplied or default-initialized structures.
        CALL ensure_phyex_init(timestep, PHYEX_MICRO_ICE3, PHYEX_SCONV_NONE, PHYEX_TURB_TKEL)

        ! Electrical scheme disabled (ELEC structures unused when OELEC=.FALSE.)
        OELEC = .FALSE.
        OSEDIM_BEARD = .FALSE.
        PTHVREFZIKB = 0.0_WP

        ! OpenACC data region for GPU execution
        !$acc data deviceptr(f_exn, f_dzz, f_rhodj, f_rhodref, f_exnref, f_pabs) &
        !$acc&     deviceptr(f_cit, f_cldfr, f_icldfr, f_ssio, f_ssiu, f_ifr) &
        !$acc&     deviceptr(f_hlc_hrc, f_hlc_hcf, f_hli_hri, f_hli_hcf) &
        !$acc&     deviceptr(f_tht, f_rvt, f_rct, f_rrt, f_rit, f_rst, f_rgt) &
        !$acc&     deviceptr(f_ths, f_rvs, f_rcs, f_rrs, f_ris, f_rss, f_rgs) &
        !$acc&     deviceptr(f_inprc, f_inprr, f_evap3d, f_inprs, f_inprg, f_indep) &
        !$acc&     deviceptr(f_rainfr, f_sigs)

        ! Call the actual RAIN_ICE routine with the INI_PHYEX-populated config
        CALL RAIN_ICE(                                                     &
            D, G_PHYEX%CST, G_PHYEX%PARAM_ICEN,                            &
            G_PHYEX%RAIN_ICE_PARAMN, G_PHYEX%RAIN_ICE_DESCRN,             &
            G_PHYEX%ELEC_PARAM, G_PHYEX%ELEC_DESCR, G_PHYEX%MISC%TBUCONF, &
            OELEC, OSEDIM_BEARD, PTHVREFZIKB,                              &
            timestep, krr, f_exn,                                          &
            f_dzz, f_rhodj, f_rhodref, f_exnref, f_pabs, f_cit, f_cldfr,   &
            f_icldfr, f_ssio, f_ssiu, f_ifr,                               &
            f_hlc_hrc, f_hlc_hcf, f_hli_hri, f_hli_hcf,                    &
            f_tht, f_rvt, f_rct, f_rrt, f_rit, f_rst,                      &
            f_rgt, f_ths, f_rvs, f_rcs, f_rrs, f_ris, f_rss, f_rgs,        &
            f_inprc, f_inprr, f_evap3d,                                    &
            f_inprs, f_inprg, f_indep, f_rainfr, f_sigs,                   &
            TBUDGETS, 0                                                    &
        )

        !$acc end data

    END SUBROUTINE c_rain_ice_wrap

    ! C-callable wrapper for SHALLOW_CONVECTION
    SUBROUTINE c_shallow_convection_wrap(                                     &
        nlon, nlev, kice, kbdia, ktdia,                                       &
        osettadj_int, ptadjs, och1conv_int, kch1,                             &
        ptr_ppabst, ptr_pzz, ptr_ptkecls, ptr_ptt, ptr_prvt, ptr_prct,       &
        ptr_prit, ptr_pwt, ptr_ptten, ptr_prvten, ptr_prcten, ptr_priten,    &
        ptr_kcltop, ptr_kclbas, ptr_pumf, ptr_pch1, ptr_pch1ten              &
    ) BIND(C, name="c_shallow_convection")

        ! C-compatible arguments
        INTEGER(C_INT), VALUE, INTENT(IN) :: nlon, nlev, kice, kbdia, ktdia
        INTEGER(C_INT), VALUE, INTENT(IN) :: osettadj_int, och1conv_int, kch1
        REAL(WP), VALUE, INTENT(IN) :: ptadjs

        ! C pointers for 1D input arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ptkecls     ! 1D: (nlon)

        ! C pointers for 2D input arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ppabst      ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pzz         ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ptt         ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prvt        ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prct        ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prit        ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pwt         ! 2D: (nlon, nlev)

        ! C pointers for 2D input/output arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ptten       ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prvten      ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prcten      ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_priten      ! 2D: (nlon, nlev)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pumf        ! 2D: (nlon, nlev)

        ! C pointers for 1D input/output arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_kcltop      ! 1D: (nlon)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_kclbas      ! 1D: (nlon)

        ! C pointers for 3D chemical tracer arrays
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pch1        ! 3D: (nlon, nlev, kch1)
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pch1ten     ! 3D: (nlon, nlev, kch1)

        ! Fortran pointers to map C data
        REAL(KIND=WP), POINTER, DIMENSION(:) :: f_ptkecls
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_ppabst, f_pzz, f_ptt
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_prvt, f_prct, f_prit, f_pwt
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_ptten, f_prvten, f_prcten, f_priten
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_pumf
        INTEGER(KIND=C_INT), POINTER, DIMENSION(:) :: f_kcltop, f_kclbas
        REAL(KIND=WP), POINTER, DIMENSION(:,:,:) :: f_pch1, f_pch1ten

        ! Local variables for PHYEX structures
        TYPE(DIMPHYEX_t) :: D
        TYPE(NSV_t) :: NSV
        TYPE(CONVPAR_t) :: CONVPAR
        TYPE(CONVPAR_SHAL) :: CVP_SHAL
        LOGICAL :: LOSETTADJ, LOCH1CONV

        ! Convert C integers to Fortran logicals
        LOSETTADJ = (osettadj_int /= 0)
        LOCH1CONV = (och1conv_int /= 0)

        ! Convert C pointers to Fortran arrays
        CALL C_F_POINTER(ptr_ptkecls, f_ptkecls, [nlon])
        CALL C_F_POINTER(ptr_ppabst, f_ppabst, [nlon, nlev])
        CALL C_F_POINTER(ptr_pzz, f_pzz, [nlon, nlev])
        CALL C_F_POINTER(ptr_ptt, f_ptt, [nlon, nlev])
        CALL C_F_POINTER(ptr_prvt, f_prvt, [nlon, nlev])
        CALL C_F_POINTER(ptr_prct, f_prct, [nlon, nlev])
        CALL C_F_POINTER(ptr_prit, f_prit, [nlon, nlev])
        CALL C_F_POINTER(ptr_pwt, f_pwt, [nlon, nlev])
        CALL C_F_POINTER(ptr_ptten, f_ptten, [nlon, nlev])
        CALL C_F_POINTER(ptr_prvten, f_prvten, [nlon, nlev])
        CALL C_F_POINTER(ptr_prcten, f_prcten, [nlon, nlev])
        CALL C_F_POINTER(ptr_priten, f_priten, [nlon, nlev])
        CALL C_F_POINTER(ptr_kcltop, f_kcltop, [nlon])
        CALL C_F_POINTER(ptr_kclbas, f_kclbas, [nlon])
        CALL C_F_POINTER(ptr_pumf, f_pumf, [nlon, nlev])
        CALL C_F_POINTER(ptr_pch1, f_pch1, [nlon, nlev, kch1])
        CALL C_F_POINTER(ptr_pch1ten, f_pch1ten, [nlon, nlev, kch1])

        ! Initialize DIMPHYEX structure
        D%NIT = nlon
        D%NIB = 1
        D%NIE = nlon
        D%NJT = 1
        D%NJB = 1
        D%NJE = 1
        D%NKT = nlev
        D%NKL = 1
        D%NKA = 1
        D%NKU = nlev
        D%NKB = 1
        D%NKE = nlev
        D%NKTB = 1
        D%NKTE = nlev
        D%NIBC = 1
        D%NJBC = 1
        D%NIEC = nlon
        D%NJEC = 1
        D%NIJT = nlon
        D%NIJB = 1
        D%NIJE = nlon
        D%NKLES = nlev
        D%NLESMASK = 0
        D%NLES_TIMES = 0

        ! Initialize NSV structure (tracers)
        NSV%NSV_USER = 0
        NSV%NSV_C2R2BEG = 0
        NSV%NSV_C2R2END = 0
        NSV%NSV_C1R3BEG = 0
        NSV%NSV_C1R3END = 0
        NSV%NSV_ELECBEG = 0
        NSV%NSV_ELECEND = 0
        NSV%NSV_LNOXBEG = 0
        NSV%NSV_LNOXEND = 0
        NSV%NSV_DSTBEG = 0
        NSV%NSV_DSTEND = 0
        NSV%NSV_SLTBEG = 0
        NSV%NSV_SLTEND = 0
        NSV%NSV_PPBEG = 0
        NSV%NSV_PPEND = 0
        NSV%NSV_CSBEG = 0
        NSV%NSV_CSEND = 0
        NSV%NSV_AERBEG = 0
        NSV%NSV_AEREND = 0
        NSV%NSV_SNWBEG = 0
        NSV%NSV_SNWEND = 0
        NSV%NSV_CHEMBEG = 0
        NSV%NSV_CHEMEND = 0

        ! Initialize CONVPAR structure (deep convection parameters)
        CONVPAR%XA25 = 625.0E6_WP     ! Reference grid area (25km)^2
        CONVPAR%XCRAD = 1500.0_WP     ! Cloud radius (m)
        CONVPAR%XCDEPTH = 3000.0_WP   ! Minimum necessary cloud depth
        CONVPAR%XENTR = 0.03_WP       ! Entrainment constant
        CONVPAR%XZLCL = 3500.0_WP     ! Max LCL height
        CONVPAR%XZPBL = 6000.0_WP     ! Minimum PBL height
        CONVPAR%XWTRIG = 6.0_WP       ! Trigger vertical velocity
        CONVPAR%XNHGAM = 1.3333_WP    ! Non-hydrostatic pressure factor
        CONVPAR%XTFRZ1 = 268.16_WP    ! Freezing interval begin
        CONVPAR%XTFRZ2 = 248.16_WP    ! Freezing interval end
        CONVPAR%XRHDBC = 0.9_WP       ! Relative humidity below cloud
        CONVPAR%XRCONV = 0.015_WP     ! Precipitation conversion constant
        CONVPAR%XSTABT = 0.75_WP      ! Stability in fractional time integration
        CONVPAR%XSTABC = 0.95_WP      ! Stability in CAPE adjustment
        CONVPAR%XUSRDPTH = 16500.0_WP ! Pressure thickness for updraft moisture
        CONVPAR%XMELDPTH = 10000.0_WP ! Layer for precipitation melt
        CONVPAR%XUVDP = 0.7_WP        ! Pressure perturbation in momentum transport

        ! Initialize CVP_SHAL structure (shallow convection parameters)
        CVP_SHAL%XA25 = 625.0E6_WP       ! Reference grid area
        CVP_SHAL%XCRAD = 1500.0_WP       ! Cloud radius
        CVP_SHAL%XCTIME_SHAL = 10800.0_WP ! Convective adjustment time
        CVP_SHAL%XCDEPTH = 2500.0_WP     ! Minimum cloud depth
        CVP_SHAL%XCDEPTH_D = 3000.0_WP   ! Maximum cloud thickness
        CVP_SHAL%XDTPERT = 1.0_WP        ! Temperature perturbation at LCL
        CVP_SHAL%XATPERT = 0.0_WP        ! Parameter for temp perturbation
        CVP_SHAL%XBTPERT = 0.0_WP        ! Parameter for temp perturbation
        CVP_SHAL%XENTR = 0.03_WP         ! Entrainment constant
        CVP_SHAL%XZLCL = 3500.0_WP       ! Max LCL height
        CVP_SHAL%XZPBL = 6000.0_WP       ! Minimum PBL height
        CVP_SHAL%XWTRIG = 6.0_WP         ! Trigger vertical velocity
        CVP_SHAL%XNHGAM = 1.3333_WP      ! Non-hydrostatic pressure factor
        CVP_SHAL%XTFRZ1 = 268.16_WP      ! Freezing interval begin
        CVP_SHAL%XTFRZ2 = 248.16_WP      ! Freezing interval end
        CVP_SHAL%XSTABT = 0.75_WP        ! Stability factor
        CVP_SHAL%XSTABC = 0.95_WP        ! Stability in CAPE adjustment
        CVP_SHAL%XAW = 1.0_WP            ! WLCL parameter A
        CVP_SHAL%XBW = 0.0_WP            ! WLCL parameter B
        CVP_SHAL%LLSMOOTH = .TRUE.            ! Smoothing flag

        ! Initialize the shared PHYEX config once per process so the physical
        ! constants come from INI_PHYEX (G_PHYEX%CST), consistent with the other
        ! routines. The Kain-Fritsch CONVPAR_SHAL/CONVPAR/NSV structures are NOT
        ! part of PHYEX_t, so they stay hand-rolled above. ptadjs is the natural
        ! timescale argument here (CST init is timestep-independent).
        CALL ensure_phyex_init(ptadjs, PHYEX_MICRO_ICE3, PHYEX_SCONV_NONE, PHYEX_TURB_TKEL)

        ! OpenACC data region for GPU execution
        !$acc data deviceptr(f_ppabst, f_pzz, f_ptkecls, f_ptt, f_prvt, f_prct, f_prit, f_pwt) &
        !$acc&     deviceptr(f_ptten, f_prvten, f_prcten, f_priten, f_pumf) &
        !$acc&     deviceptr(f_kcltop, f_kclbas, f_pch1, f_pch1ten)

        ! Call the actual SHALLOW_CONVECTION routine with the shared constants
        CALL SHALLOW_CONVECTION(                                               &
            CVP_SHAL, G_PHYEX%CST, D, NSV, CONVPAR, kbdia, ktdia,              &
            kice, LOSETTADJ, ptadjs, f_ppabst, f_pzz,                          &
            f_ptkecls, f_ptt, f_prvt, f_prct, f_prit, f_pwt,                   &
            f_ptten, f_prvten, f_prcten, f_priten,                             &
            f_kcltop, f_kclbas, f_pumf, LOCH1CONV, kch1,                       &
            f_pch1, f_pch1ten                                                  &
        )

        !$acc end data

    END SUBROUTINE c_shallow_convection_wrap

    ! C-callable wrapper for TURB (operational path, mirrors src/offline main_turb).
    !
    ! Python passes (nlon, nlev) fields with level 1 = model top and level nlev =
    ! surface (matches NKL=-1). TURB needs a 2-level vertical halo, so we work
    ! internally on (nlon, nlev+2) arrays: Python levels map to interior levels
    ! 2..nlev+1, with halo at level 1 (above top) and nlev+2 (below surface).
    ! Config comes from INI_PHYEX (G_PHYEX) so CSTURB/TURBN/NEBN are valid; the
    ! flags and the OUT-array shapes are kept consistent with the schemes'
    ! MERGE(D%NKT,0,<flag>) shape contracts (OCOMPUTE_SRC, OCLOUDMODIFLM, OFLYER).
    SUBROUTINE c_turb_wrap(                                                    &
        nlon, nlev, krr, ptstep,                                               &
        ptr_pdxx, ptr_pdyy, ptr_pdzz, ptr_pzz,                                 &
        ptr_prhodj, ptr_pthvref, ptr_psfth, ptr_psfrv,                         &
        ptr_ppabst, ptr_put, ptr_pvt, ptr_pwt, ptr_ptket,                      &
        ptr_pthlt, ptr_prt,                                                    &
        ptr_prus, ptr_prvs, ptr_prws, ptr_prthls, ptr_prrs, ptr_prtkes        &
    ) BIND(C, name="c_turb")

        INTEGER(C_INT), VALUE, INTENT(IN) :: nlon, nlev, krr
        REAL(WP), VALUE, INTENT(IN) :: ptstep

        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pdxx, ptr_pdyy, ptr_pdzz
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pzz, ptr_prhodj, ptr_pthvref
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_ppabst
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_put, ptr_pvt, ptr_pwt, ptr_ptket
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_pthlt
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_psfth, ptr_psfrv
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prt
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prus, ptr_prvs, ptr_prws
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prthls, ptr_prtkes
        TYPE(C_PTR), VALUE, INTENT(IN) :: ptr_prrs

        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_pdxx, f_pdyy, f_pdzz
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_pzz, f_prhodj, f_pthvref
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_ppabst
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_put, f_pvt, f_pwt, f_ptket
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_pthlt
        REAL(KIND=WP), POINTER, DIMENSION(:) :: f_psfth, f_psfrv
        REAL(KIND=WP), POINTER, DIMENSION(:,:,:) :: f_prt
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_prus, f_prvs, f_prws
        REAL(KIND=WP), POINTER, DIMENSION(:,:) :: f_prthls, f_prtkes
        REAL(KIND=WP), POINTER, DIMENSION(:,:,:) :: f_prrs

        TYPE(DIMPHYEX_t) :: D
        TYPE(TURB_t)     :: TURBN
        TYPE(NEB_t)      :: NEBN
        TYPE(TLES_t)     :: TLES
        TYPE(TFILEDATA)  :: TPFILE
        TYPE(TBUDGETDATA_PTR), DIMENSION(0) :: TBUDGETS

        INTEGER :: nk
        ! Halo'd internal fields (nlon, nlev+2)
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zdxx, zdyy, zdzz, zzz, zdzx, zdzy
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zrhodj, zthvref, zpabst
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zut, zvt, zwt, ztket, zthlt
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zlengthm, zlengthh, zmfmoist
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zsrct, zsigs, zcei
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zflxzthvmf, zflxzumf, zflxzvmf
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zrus, zrvs, zrws, zrthls, zrtkes
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zwth, zwrc, zdp, ztp, ztdiff, ztdiss, zedr
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:,:) :: zrt, zrrs, zsvt, zrsvs, zwsv
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:,:) :: zhgradleo, zhgradgog
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:) :: zdircosxw, zdircosyw, zdircoszw
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:) :: zcosslope, zsinslope, zzs
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:) :: zsfth, zsfrv, zsfu, zsfv
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:) :: zbl_depth, zsbl_depth
        REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:) :: zsfsv

        CHARACTER(LEN=4), DIMENSION(2) :: HLBCX, HLBCY
        CHARACTER(LEN=4) :: HCLOUD, HELEC
        LOGICAL :: O2D, ONOMIXLG, OFLAT, OCOUPLES, OBLOWSNOW, OIBM, OFLYER
        LOGICAL :: OCOMPUTE_SRC, OOCEAN, ODEEPOC, ODIAG_IN_RUN, OCLOUDMODIFLM
        INTEGER :: KSPLIT, KSV, KSV_LGBEG, KSV_LGEND, KHALO
        INTEGER :: KSV_LIMA_NR, KSV_LIMA_NS, KSV_LIMA_NG, KSV_LIMA_NH
        INTEGER :: KGRADIENTSLEO, KGRADIENTSGOG, KRRL, KRRI
        REAL(KIND=WP) :: PRSNOW

        ! Map C pointers (Python arrays, nlon x nlev)
        CALL C_F_POINTER(ptr_pdxx, f_pdxx, [nlon, nlev])
        CALL C_F_POINTER(ptr_pdyy, f_pdyy, [nlon, nlev])
        CALL C_F_POINTER(ptr_pdzz, f_pdzz, [nlon, nlev])
        CALL C_F_POINTER(ptr_pzz, f_pzz, [nlon, nlev])
        CALL C_F_POINTER(ptr_prhodj, f_prhodj, [nlon, nlev])
        CALL C_F_POINTER(ptr_pthvref, f_pthvref, [nlon, nlev])
        CALL C_F_POINTER(ptr_psfth, f_psfth, [nlon])
        CALL C_F_POINTER(ptr_psfrv, f_psfrv, [nlon])
        CALL C_F_POINTER(ptr_ppabst, f_ppabst, [nlon, nlev])
        CALL C_F_POINTER(ptr_put, f_put, [nlon, nlev])
        CALL C_F_POINTER(ptr_pvt, f_pvt, [nlon, nlev])
        CALL C_F_POINTER(ptr_pwt, f_pwt, [nlon, nlev])
        CALL C_F_POINTER(ptr_ptket, f_ptket, [nlon, nlev])
        CALL C_F_POINTER(ptr_pthlt, f_pthlt, [nlon, nlev])
        CALL C_F_POINTER(ptr_prt, f_prt, [nlon, nlev, krr])
        CALL C_F_POINTER(ptr_prus, f_prus, [nlon, nlev])
        CALL C_F_POINTER(ptr_prvs, f_prvs, [nlon, nlev])
        CALL C_F_POINTER(ptr_prws, f_prws, [nlon, nlev])
        CALL C_F_POINTER(ptr_prthls, f_prthls, [nlon, nlev])
        CALL C_F_POINTER(ptr_prrs, f_prrs, [nlon, nlev, krr])
        CALL C_F_POINTER(ptr_prtkes, f_prtkes, [nlon, nlev])

        ! Shared, validated PHYEX config (CST, CSTURB, TURBN, NEBN, TBUCONF).
        CALL ensure_phyex_init(ptstep, PHYEX_MICRO_ICE3, PHYEX_SCONV_NONE, PHYEX_TURB_TKEL)

        ! Local config: force BL89 (self-computed mixing length, no external
        ! HARATU lengths needed) and enable subgrid condensation, consistent with
        ! OCOMPUTE_SRC + the full-size PSIGS/PSRCT below.
        TURBN = G_PHYEX%TURBN
        TURBN%LHARAT   = .FALSE.
        TURBN%CTURBLEN = 'BL89'
        NEBN = G_PHYEX%NEBN
        NEBN%LSUBG_COND = .TRUE.
        TLES%LLES = .FALSE.
        TLES%LLES_CALL = .FALSE.
        TPFILE%LOPENED = .FALSE.
        TPFILE%NLU = 0

        nk = nlev + 2

        ! DIMPHYEX with a 2-level vertical halo (interior 2..nk-1), NKL=-1.
        D%NIT = nlon; D%NIB = 1; D%NIE = nlon
        D%NJT = 1; D%NJB = 1; D%NJE = 1
        D%NIJT = nlon; D%NIJB = 1; D%NIJE = nlon
        D%NIBC = 1; D%NJBC = 1; D%NIEC = nlon; D%NJEC = 1
        D%NKL = -1
        D%NKT = nk
        D%NKA = nk
        D%NKU = 1
        D%NKB = nk - 1
        D%NKE = 2
        D%NKTB = 2
        D%NKTE = nk - 1
        D%NKLES = nk
        D%NLESMASK = 0
        D%NLES_TIMES = 0

        ! Operational flags (mirror the offline reference driver).
        KRRL = 0
        IF (krr >= 2) KRRL = 1
        IF (krr >= 3) KRRL = 2
        KRRI = 0
        IF (krr >= 4) KRRI = 1
        IF (krr >= 5) KRRI = 2
        IF (krr >= 6) KRRI = 3
        IF (krr >= 7) KRRI = 4
        KSV = 0; KSV_LGBEG = 0; KSV_LGEND = 0
        KSV_LIMA_NR = 0; KSV_LIMA_NS = 0; KSV_LIMA_NG = 0; KSV_LIMA_NH = 0
        KGRADIENTSLEO = 0; KGRADIENTSGOG = 0; KHALO = 1
        KSPLIT = MAX(1, G_PHYEX%TURBN%NTURBSPLIT)
        HLBCX = 'CYCL'; HLBCY = 'CYCL'
        HCLOUD = 'ICE3'; HELEC = 'NONE'
        O2D = .FALSE.; ONOMIXLG = .FALSE.; OFLAT = .FALSE.
        OCOUPLES = .FALSE.; OBLOWSNOW = .FALSE.; OIBM = .FALSE.
        OFLYER = .TRUE.; OCOMPUTE_SRC = .TRUE.
        OOCEAN = .FALSE.; ODEEPOC = .FALSE.; ODIAG_IN_RUN = .FALSE.
        OCLOUDMODIFLM = .FALSE.
        PRSNOW = 1.0_WP

        ! Allocate internal arrays (full-size; sizes honour the MERGE contracts).
        ALLOCATE(zdxx(nlon,nk), zdyy(nlon,nk), zdzz(nlon,nk), zzz(nlon,nk))
        ALLOCATE(zdzx(nlon,nk), zdzy(nlon,nk))
        ALLOCATE(zrhodj(nlon,nk), zthvref(nlon,nk), zpabst(nlon,nk))
        ALLOCATE(zut(nlon,nk), zvt(nlon,nk), zwt(nlon,nk), ztket(nlon,nk), zthlt(nlon,nk))
        ALLOCATE(zlengthm(nlon,nk), zlengthh(nlon,nk), zmfmoist(nlon,nk))
        ALLOCATE(zsrct(nlon,nk), zsigs(nlon,nk))
        ALLOCATE(zcei(nlon,0))
        ALLOCATE(zflxzthvmf(nlon,nk), zflxzumf(nlon,nk), zflxzvmf(nlon,nk))
        ALLOCATE(zrus(nlon,nk), zrvs(nlon,nk), zrws(nlon,nk), zrthls(nlon,nk), zrtkes(nlon,nk))
        ALLOCATE(zwth(nlon,nk), zwrc(nlon,nk), zdp(nlon,nk), ztp(nlon,nk))
        ALLOCATE(ztdiff(nlon,nk), ztdiss(nlon,nk), zedr(nlon,nk))
        ALLOCATE(zrt(nlon,nk,krr), zrrs(nlon,nk,krr))
        ALLOCATE(zsvt(nlon,nk,0), zrsvs(nlon,nk,0), zwsv(nlon,nk,0))
        ALLOCATE(zhgradleo(nlon,nk,0), zhgradgog(nlon,nk,0))
        ALLOCATE(zdircosxw(nlon), zdircosyw(nlon), zdircoszw(nlon))
        ALLOCATE(zcosslope(nlon), zsinslope(nlon), zzs(nlon))
        ALLOCATE(zsfth(nlon), zsfrv(nlon), zsfu(nlon), zsfv(nlon))
        ALLOCATE(zbl_depth(nlon), zsbl_depth(nlon), zsfsv(nlon,0))

        ! Copy Python fields into the halo'd interior (zero-gradient halos; PZZ
        ! extrapolated so the boundary layer thicknesses stay non-zero).
        CALL cin(f_pdxx, zdxx)
        CALL cin(f_pdyy, zdyy)
        CALL cin(f_pdzz, zdzz)
        CALL cin_z(f_pzz, zzz)
        CALL cin(f_prhodj, zrhodj)
        CALL cin(f_pthvref, zthvref)
        CALL cin(f_ppabst, zpabst)
        CALL cin(f_put, zut)
        CALL cin(f_pvt, zvt)
        CALL cin(f_pwt, zwt)
        CALL cin(f_ptket, ztket)
        CALL cin(f_pthlt, zthlt)
        CALL cin3(f_prt, zrt)
        CALL cin(f_prus, zrus)
        CALL cin(f_prvs, zrvs)
        CALL cin(f_prws, zrws)
        CALL cin(f_prthls, zrthls)
        CALL cin(f_prtkes, zrtkes)
        CALL cin3(f_prrs, zrrs)

        zdzx = 0.0_WP; zdzy = 0.0_WP
        zlengthm = 0.0_WP; zlengthh = 0.0_WP; zmfmoist = 0.0_WP
        zsrct = 0.0_WP; zsigs = 0.0_WP
        zflxzthvmf = 0.0_WP; zflxzumf = 0.0_WP; zflxzvmf = 0.0_WP
        zdircosxw = 0.0_WP; zdircosyw = 0.0_WP; zdircoszw = 1.0_WP
        zcosslope = 1.0_WP; zsinslope = 0.0_WP; zzs = 0.0_WP
        zsfth = f_psfth; zsfrv = f_psfrv; zsfu = 0.0_WP; zsfv = 0.0_WP
        zbl_depth = 0.0_WP; zsbl_depth = 0.0_WP

        ! Call TURB with the offline reference's argument order.
        CALL TURB(G_PHYEX%CST, G_PHYEX%CSTURB, G_PHYEX%MISC%TBUCONF, TURBN, NEBN, D, TLES, &
            krr, KRRL, KRRI, HLBCX, HLBCY, KGRADIENTSLEO, KGRADIENTSGOG, KHALO,            &
            KSPLIT, OCLOUDMODIFLM, KSV, KSV_LGBEG, KSV_LGEND,                              &
            KSV_LIMA_NR, KSV_LIMA_NS, KSV_LIMA_NG, KSV_LIMA_NH,                            &
            O2D, ONOMIXLG, OFLAT, OCOUPLES, OBLOWSNOW, OIBM, OFLYER, OCOMPUTE_SRC, PRSNOW, &
            OOCEAN, ODEEPOC, ODIAG_IN_RUN,                                                 &
            TURBN%CTURBLEN_CLOUD, HCLOUD, HELEC,                                           &
            ptstep, TPFILE,                                                                &
            zdxx, zdyy, zdzz, zdzx, zdzy, zzz,                                             &
            zdircosxw, zdircosyw, zdircoszw, zcosslope, zsinslope,                         &
            zrhodj, zthvref, zhgradleo, zhgradgog, zzs,                                    &
            zsfth, zsfrv, zsfsv, zsfu, zsfv,                                               &
            zpabst, zut, zvt, zwt, ztket, zsvt, zsrct,                                     &
            zlengthm, zlengthh, zmfmoist,                                                  &
            zbl_depth, zsbl_depth,                                                         &
            zcei, TURBN%XCEI_MIN, TURBN%XCEI_MAX, TURBN%XCOEF_AMPL_SAT,                    &
            zthlt, zrt,                                                                    &
            zrus, zrvs, zrws, zrthls, zrrs, zrsvs, zrtkes,                                 &
            zsigs,                                                                         &
            zflxzthvmf, zflxzumf, zflxzvmf,                                                &
            zwth, zwrc, zwsv, zdp, ztp, ztdiff, ztdiss,                                    &
            TBUDGETS, KBUDGETS=0, PEDR=zedr)

        ! Copy interior tendencies back to the Python arrays.
        CALL cout(zrus, f_prus)
        CALL cout(zrvs, f_prvs)
        CALL cout(zrws, f_prws)
        CALL cout(zrthls, f_prthls)
        CALL cout(zrtkes, f_prtkes)
        CALL cout3(zrrs, f_prrs)

        DEALLOCATE(zdxx, zdyy, zdzz, zzz, zdzx, zdzy, zrhodj, zthvref, zpabst)
        DEALLOCATE(zut, zvt, zwt, ztket, zthlt, zlengthm, zlengthh, zmfmoist)
        DEALLOCATE(zsrct, zsigs, zcei, zflxzthvmf, zflxzumf, zflxzvmf)
        DEALLOCATE(zrus, zrvs, zrws, zrthls, zrtkes)
        DEALLOCATE(zwth, zwrc, zdp, ztp, ztdiff, ztdiss, zedr)
        DEALLOCATE(zrt, zrrs, zsvt, zrsvs, zwsv, zhgradleo, zhgradgog)
        DEALLOCATE(zdircosxw, zdircosyw, zdircoszw, zcosslope, zsinslope, zzs)
        DEALLOCATE(zsfth, zsfrv, zsfu, zsfv, zbl_depth, zsbl_depth, zsfsv)

    CONTAINS

        ! Copy a (nlon,nlev) field into a (nlon,nk) array: interior 2..nk-1,
        ! zero-gradient halos at 1 and nk.
        SUBROUTINE cin(src, dst)
            REAL(KIND=WP), INTENT(IN)  :: src(:,:)
            REAL(KIND=WP), INTENT(OUT) :: dst(:,:)
            dst(:,2:nk-1) = src(:,1:nlev)
            dst(:,1)  = src(:,1)
            dst(:,nk) = src(:,nlev)
        END SUBROUTINE cin

        ! Like cin but linearly extrapolates the halos (for heights, so layer
        ! thicknesses across the boundary stay finite and non-zero).
        SUBROUTINE cin_z(src, dst)
            REAL(KIND=WP), INTENT(IN)  :: src(:,:)
            REAL(KIND=WP), INTENT(OUT) :: dst(:,:)
            dst(:,2:nk-1) = src(:,1:nlev)
            dst(:,1)  = 2.0_WP*src(:,1)    - src(:,2)
            dst(:,nk) = 2.0_WP*src(:,nlev) - src(:,nlev-1)
        END SUBROUTINE cin_z

        SUBROUTINE cin3(src, dst)
            REAL(KIND=WP), INTENT(IN)  :: src(:,:,:)
            REAL(KIND=WP), INTENT(OUT) :: dst(:,:,:)
            dst(:,2:nk-1,:) = src(:,1:nlev,:)
            dst(:,1,:)  = src(:,1,:)
            dst(:,nk,:) = src(:,nlev,:)
        END SUBROUTINE cin3

        ! Copy the interior of a (nlon,nk) array back to a (nlon,nlev) field.
        SUBROUTINE cout(src, dst)
            REAL(KIND=WP), INTENT(IN)  :: src(:,:)
            REAL(KIND=WP), INTENT(OUT) :: dst(:,:)
            dst(:,1:nlev) = src(:,2:nk-1)
        END SUBROUTINE cout

        SUBROUTINE cout3(src, dst)
            REAL(KIND=WP), INTENT(IN)  :: src(:,:,:)
            REAL(KIND=WP), INTENT(OUT) :: dst(:,:,:)
            dst(:,1:nlev,:) = src(:,2:nk-1,:)
        END SUBROUTINE cout3

    END SUBROUTINE c_turb_wrap

END MODULE phyex_bridge