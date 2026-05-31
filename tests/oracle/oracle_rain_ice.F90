! Independent golden-reference oracle for RAIN_ICE.
!
! Calls RAIN_ICE *directly* (USE MODI_RAIN_ICE) — not through the Cython/C bridge.
! Reads the (nlon,nlev) inputs the Python binding takes from a binary file and
! writes back the post-call in/out + output arrays. The Python golden test runs
! phyex.rain_ice on the same inputs and asserts agreement to round-off.
!
! Mirrors c_rain_ice_wrap: no-halo DIMPHYEX, config from INI_PHYEX (G), electrical
! scheme disabled.
!
! Usage: oracle_rain_ice_dp.exe <input.bin> <output.bin>
PROGRAM ORACLE_RAIN_ICE
    USE MODD_DIMPHYEX,  ONLY: DIMPHYEX_t
    USE MODD_PHYEX,     ONLY: PHYEX_t
    USE MODD_IO,        ONLY: TFILEDATA
    USE MODD_BUDGET,    ONLY: TBUDGETDATA_PTR
    USE MODI_INI_PHYEX, ONLY: INI_PHYEX
    USE MODI_RAIN_ICE,  ONLY: RAIN_ICE

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
    LOGICAL :: OELEC, OSEDIM_BEARD
    REAL :: PTHVREFZIKB

    REAL, ALLOCATABLE :: exn(:,:), dzz(:,:), rhodj(:,:), rhodref(:,:), exnref(:,:), pabs(:,:)
    REAL, ALLOCATABLE :: cldfr(:,:), icldfr(:,:), ssio(:,:), ssiu(:,:), ifr(:,:)
    REAL, ALLOCATABLE :: tht(:,:), rvt(:,:), rct(:,:), rrt(:,:), rit(:,:), rst(:,:), rgt(:,:), sigs(:,:)
    REAL, ALLOCATABLE :: cit(:,:), hlc_hrc(:,:), hlc_hcf(:,:), hli_hri(:,:), hli_hcf(:,:)
    REAL, ALLOCATABLE :: ths(:,:), rvs(:,:), rcs(:,:), rrs(:,:), ris(:,:), rss(:,:), rgs(:,:)
    REAL, ALLOCATABLE :: evap3d(:,:), rainfr(:,:)
    REAL, ALLOCATABLE :: inprc(:), inprr(:), inprs(:), inprg(:), indep(:)

    CALL GET_COMMAND_ARGUMENT(1, infile)
    CALL GET_COMMAND_ARGUMENT(2, outfile)

    OPEN(NEWUNIT=iu, FILE=TRIM(infile), FORM='UNFORMATTED', ACCESS='STREAM', STATUS='OLD')
    READ(iu) nlon, nlev, krr
    ALLOCATE(exn(nlon,nlev), dzz(nlon,nlev), rhodj(nlon,nlev), rhodref(nlon,nlev), exnref(nlon,nlev), pabs(nlon,nlev))
    ALLOCATE(cldfr(nlon,nlev), icldfr(nlon,nlev), ssio(nlon,nlev), ssiu(nlon,nlev), ifr(nlon,nlev))
    ALLOCATE(tht(nlon,nlev), rvt(nlon,nlev), rct(nlon,nlev), rrt(nlon,nlev), rit(nlon,nlev), rst(nlon,nlev), rgt(nlon,nlev), sigs(nlon,nlev))
    ALLOCATE(cit(nlon,nlev), hlc_hrc(nlon,nlev), hlc_hcf(nlon,nlev), hli_hri(nlon,nlev), hli_hcf(nlon,nlev))
    ALLOCATE(ths(nlon,nlev), rvs(nlon,nlev), rcs(nlon,nlev), rrs(nlon,nlev), ris(nlon,nlev), rss(nlon,nlev), rgs(nlon,nlev))
    ALLOCATE(evap3d(nlon,nlev), rainfr(nlon,nlev))
    ALLOCATE(inprc(nlon), inprr(nlon), inprs(nlon), inprg(nlon), indep(nlon))
    READ(iu) timestep
    READ(iu) exn, dzz, rhodj, rhodref, exnref, pabs, cldfr, icldfr, ssio, ssiu, ifr
    READ(iu) tht, rvt, rct, rrt, rit, rst, rgt, sigs
    READ(iu) cit, hlc_hrc, hlc_hcf, hli_hri, hli_hcf
    READ(iu) ths, rvs, rcs, rrs, ris, rss, rgs
    READ(iu) evap3d, rainfr
    READ(iu) inprc, inprr, inprs, inprg, indep
    CLOSE(iu)

    ! Config (mirrors ensure_phyex_init).
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

    D%NIT = nlon; D%NIB = 1; D%NIE = nlon
    D%NJT = 1; D%NJB = 1; D%NJE = 1
    D%NKT = nlev; D%NKL = 1; D%NKA = 1; D%NKU = nlev
    D%NKB = 1; D%NKE = nlev; D%NKTB = 1; D%NKTE = nlev
    D%NIBC = 1; D%NJBC = 1; D%NIEC = nlon; D%NJEC = 1
    D%NIJT = nlon; D%NIJB = 1; D%NIJE = nlon
    D%NKLES = nlev; D%NLESMASK = 0; D%NLES_TIMES = 0

    OELEC = .FALSE.
    OSEDIM_BEARD = .FALSE.
    PTHVREFZIKB = 0.0

    CALL RAIN_ICE(D, G%CST, G%PARAM_ICEN, G%RAIN_ICE_PARAMN, G%RAIN_ICE_DESCRN, &
        G%ELEC_PARAM, G%ELEC_DESCR, G%MISC%TBUCONF,                             &
        OELEC, OSEDIM_BEARD, PTHVREFZIKB,                                       &
        timestep, krr, exn,                                                     &
        dzz, rhodj, rhodref, exnref, pabs, cit, cldfr,                          &
        icldfr, ssio, ssiu, ifr,                                                &
        hlc_hrc, hlc_hcf, hli_hri, hli_hcf,                                     &
        tht, rvt, rct, rrt, rit, rst,                                           &
        rgt, ths, rvs, rcs, rrs, ris, rss, rgs,                                 &
        inprc, inprr, evap3d,                                                   &
        inprs, inprg, indep, rainfr, sigs,                                      &
        TBUDGETS, 0)

    OPEN(NEWUNIT=iu, FILE=TRIM(outfile), FORM='UNFORMATTED', ACCESS='STREAM', STATUS='REPLACE')
    WRITE(iu) cit, hlc_hrc, hlc_hcf, hli_hri, hli_hcf
    WRITE(iu) ths, rvs, rcs, rrs, ris, rss, rgs
    WRITE(iu) evap3d, rainfr
    WRITE(iu) inprc, inprr, inprs, inprg, indep
    CLOSE(iu)

END PROGRAM ORACLE_RAIN_ICE
