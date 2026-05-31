! Independent golden-reference oracle for ICE_ADJUST.
!
! This program calls ICE_ADJUST *directly* (USE MODI_ICE_ADJUST) — it does NOT
! go through the Cython/C bridge. It reads input arrays from a binary file
! written by Python and writes the post-call output arrays back to a binary
! file. The Python golden test then runs phyex.ice_adjust on the *same* inputs
! and asserts the binding reproduces these outputs to round-off. Any divergence
! means the binding's plumbing (pointer mapping, array order/contiguity, in-place
! semantics, config) altered the physics relative to a native PHYEX call.
!
! The INI_PHYEX configuration here mirrors ensure_phyex_init in the bridge, so
! the two share the same physics config by construction and should agree exactly.
!
! Usage: oracle_ice_adjust_dp.exe <input.bin> <output.bin>
! Binary layout (native endianness, stream access):
!   int32  nlon, nlev, krr
!   real8  timestep
!   real8  sigqsat(nlon)
!   real8  <22 arrays (nlon,nlev), column-major> in the order below
PROGRAM ORACLE_ICE_ADJUST
    USE MODD_DIMPHYEX,   ONLY: DIMPHYEX_t
    USE MODD_PHYEX,      ONLY: PHYEX_t
    USE MODD_BUDGET,     ONLY: TBUDGETDATA_PTR
    USE MODD_IO,         ONLY: TFILEDATA
    USE MODI_INI_PHYEX,  ONLY: INI_PHYEX
    USE MODI_ICE_ADJUST, ONLY: ICE_ADJUST

    IMPLICIT NONE

    INTEGER(KIND=4) :: nlon, nlev, krr
    REAL :: timestep
    CHARACTER(LEN=512) :: infile, outfile
    INTEGER :: iu

    TYPE(PHYEX_t), SAVE :: G
    TYPE(DIMPHYEX_t) :: D
    TYPE(TBUDGETDATA_PTR), DIMENSION(0) :: TBUDGETS
    TYPE(TFILEDATA) :: TPFILE
    INTEGER :: IULOUT
    REAL :: ZDZMIN
    LOGICAL :: LMFCONV, OCOMPUTE_SRC

    REAL, ALLOCATABLE :: sigqsat(:)
    REAL, ALLOCATABLE :: pabs(:,:), sigs(:,:), th(:,:), exn(:,:), exn_ref(:,:)
    REAL, ALLOCATABLE :: rho_dry_ref(:,:), rv(:,:), rc(:,:), ri(:,:), rr(:,:), rs(:,:), rg(:,:)
    REAL, ALLOCATABLE :: cf_mf(:,:), rc_mf(:,:), ri_mf(:,:)
    REAL, ALLOCATABLE :: rvs(:,:), rcs(:,:), ris(:,:), ths(:,:)
    REAL, ALLOCATABLE :: cldfr(:,:), icldfr(:,:), wcldfr(:,:)
    REAL, ALLOCATABLE :: PRHODJ(:,:), PZZ(:,:), PMFCONV(:,:), PWEIGHT_MF_CLOUD(:,:)
    REAL, ALLOCATABLE :: PSSIO(:,:), PSSIU(:,:), PIFR(:,:), PSRCS(:,:)

    CALL GET_COMMAND_ARGUMENT(1, infile)
    CALL GET_COMMAND_ARGUMENT(2, outfile)

    ! --- read inputs ---
    OPEN(NEWUNIT=iu, FILE=TRIM(infile), FORM='UNFORMATTED', ACCESS='STREAM', STATUS='OLD')
    READ(iu) nlon, nlev, krr
    ALLOCATE(sigqsat(nlon))
    ALLOCATE(pabs(nlon,nlev), sigs(nlon,nlev), th(nlon,nlev), exn(nlon,nlev), exn_ref(nlon,nlev))
    ALLOCATE(rho_dry_ref(nlon,nlev), rv(nlon,nlev), rc(nlon,nlev), ri(nlon,nlev))
    ALLOCATE(rr(nlon,nlev), rs(nlon,nlev), rg(nlon,nlev))
    ALLOCATE(cf_mf(nlon,nlev), rc_mf(nlon,nlev), ri_mf(nlon,nlev))
    ALLOCATE(rvs(nlon,nlev), rcs(nlon,nlev), ris(nlon,nlev), ths(nlon,nlev))
    ALLOCATE(cldfr(nlon,nlev), icldfr(nlon,nlev), wcldfr(nlon,nlev))
    READ(iu) timestep
    READ(iu) sigqsat
    READ(iu) pabs, sigs, th, exn, exn_ref, rho_dry_ref, rv, rc, ri, rr, rs, rg
    READ(iu) cf_mf, rc_mf, ri_mf
    READ(iu) rvs, rcs, ris, ths
    READ(iu) cldfr, icldfr, wcldfr
    CLOSE(iu)

    ! --- configuration (mirrors ensure_phyex_init in the bridge) ---
    OPEN(NEWUNIT=IULOUT, STATUS='SCRATCH')
    ZDZMIN = 20.0
    TPFILE%NLU = 0
    CALL INI_PHYEX('AROME ', TPFILE, .TRUE., IULOUT, 0, 1, &
        REAL(timestep), ZDZMIN, 'ICE3', 'NONE', 'TKEL',   &
        LDDEFAULTVAL=.TRUE., LDREADNAM=.FALSE., LDCHECK=.FALSE., &
        KPRINT=0, LDINIT=.FALSE., PHYEX_OUT=G)
    G%MISC%LMFCONV      = .FALSE.
    G%MISC%OCOMPUTE_SRC = .TRUE.
    G%PARAM_ICEN%LWARM  = .TRUE.
    G%NEBN%LSUBG_COND   = .FALSE.
    G%NEBN%LSIGMAS      = .TRUE.
    G%NEBN%CFRAC_ICE_ADJUST = 'S'
    CALL INI_PHYEX('AROME ', TPFILE, .TRUE., IULOUT, 0, 1, &
        REAL(timestep), ZDZMIN, 'ICE3', 'NONE', 'TKEL',   &
        LDDEFAULTVAL=.FALSE., LDREADNAM=.FALSE., LDCHECK=.FALSE., &
        KPRINT=0, LDINIT=.TRUE., PHYEX_IN=G, PHYEX_OUT=G)

    ! --- DIMPHYEX (same as the bridge: no halo, ground-to-space) ---
    D%NIT = nlon; D%NIB = 1; D%NIE = nlon
    D%NJT = 1; D%NJB = 1; D%NJE = 1
    D%NKT = nlev; D%NKL = 1; D%NKA = 1; D%NKU = nlev
    D%NKB = 1; D%NKE = nlev; D%NKTB = 1; D%NKTE = nlev
    D%NIBC = 1; D%NJBC = 1; D%NIEC = nlon; D%NJEC = 1
    D%NIJT = nlon; D%NIJB = 1; D%NIJE = nlon
    D%NKLES = nlev; D%NLESMASK = 0; D%NLES_TIMES = 0

    ALLOCATE(PRHODJ(nlon,nlev), PZZ(nlon,nlev), PMFCONV(nlon,nlev))
    ALLOCATE(PWEIGHT_MF_CLOUD(nlon,nlev), PSSIO(nlon,nlev), PSSIU(nlon,nlev))
    ALLOCATE(PIFR(nlon,nlev), PSRCS(nlon,nlev))
    PRHODJ = rho_dry_ref
    PZZ = 0.0; PMFCONV = 0.0; PWEIGHT_MF_CLOUD = 0.0
    PSSIO = 0.0; PSSIU = 0.0; PIFR = 0.0; PSRCS = 0.0
    LMFCONV = .FALSE.
    OCOMPUTE_SRC = .TRUE.

    CALL ICE_ADJUST(D, G%CST, G%RAIN_ICE_PARAMN, G%NEBN, G%TURBN,        &
        G%PARAM_ICEN, G%MISC%TBUCONF, krr, 'BRID',                       &
        timestep, sigqsat,                                               &
        PRHODJ, exn_ref, rho_dry_ref, sigs, LMFCONV, PMFCONV,            &
        pabs, PZZ,                                                       &
        exn, cf_mf, rc_mf, ri_mf, PWEIGHT_MF_CLOUD,                      &
        icldfr, wcldfr, PSSIO, PSSIU, PIFR,                             &
        rv, rc, rvs, rcs, th, ths,                                      &
        OCOMPUTE_SRC, PSRCS, cldfr,                                      &
        rr, ri, ris, rs, rg, TBUDGETS, 0)

    ! --- write outputs (same order the Python test expects) ---
    OPEN(NEWUNIT=iu, FILE=TRIM(outfile), FORM='UNFORMATTED', ACCESS='STREAM', STATUS='REPLACE')
    WRITE(iu) cldfr, icldfr, wcldfr
    WRITE(iu) ths, rvs, rcs, ris
    CLOSE(iu)

END PROGRAM ORACLE_ICE_ADJUST
