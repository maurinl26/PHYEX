! Independent golden-reference oracle for TURB.
!
! Calls TURB *directly* (USE MODI_TURB) — not through the Cython/C bridge. It is
! a fresh standalone reimplementation of the operational setup (config from
! INI_PHYEX, the nlev+2 vertical halo, the flag-consistent OUT-array shapes), so
! a transcription/indexing bug differs between it and c_turb_wrap and is caught
! when the binding and this oracle are compared.
!
! Reads (nlon, nlev) inputs (the same arrays the Python binding takes; level 1 =
! model top, level nlev = surface) from a binary file and writes back the post-
! call tendency arrays. The Python golden test runs phyex.turb on the same inputs
! and asserts agreement to round-off.
!
! Usage: oracle_turb_dp.exe <input.bin> <output.bin>
PROGRAM ORACLE_TURB
    USE MODD_DIMPHYEX, ONLY: DIMPHYEX_t
    USE MODD_PHYEX,    ONLY: PHYEX_t
    USE MODD_TURB_n,   ONLY: TURB_t
    USE MODD_NEB_n,    ONLY: NEB_t
    USE MODD_LES,      ONLY: TLES_t
    USE MODD_IO,       ONLY: TFILEDATA
    USE MODD_BUDGET,   ONLY: TBUDGETDATA_PTR
    USE MODI_INI_PHYEX, ONLY: INI_PHYEX
    USE MODI_TURB,      ONLY: TURB

    IMPLICIT NONE

    INTEGER(KIND=4) :: nlon, nlev, krr
    REAL :: ptstep
    CHARACTER(LEN=512) :: infile, outfile
    INTEGER :: iu, nk

    TYPE(PHYEX_t), SAVE :: G
    TYPE(DIMPHYEX_t) :: D
    TYPE(TURB_t)     :: TURBN
    TYPE(NEB_t)      :: NEBN
    TYPE(TLES_t)     :: TLES
    TYPE(TFILEDATA)  :: TPFILE
    TYPE(TBUDGETDATA_PTR), DIMENSION(0) :: TBUDGETS
    INTEGER :: IULOUT
    REAL :: ZDZMIN

    ! Python-side (nlon,nlev) inputs
    REAL, ALLOCATABLE :: pdxx(:,:), pdyy(:,:), pdzz(:,:), pzz(:,:)
    REAL, ALLOCATABLE :: prhodj(:,:), pthvref(:,:), ppabst(:,:)
    REAL, ALLOCATABLE :: put(:,:), pvt(:,:), pwt(:,:), ptket(:,:), pthlt(:,:)
    REAL, ALLOCATABLE :: psfth(:), psfrv(:)
    REAL, ALLOCATABLE :: prt(:,:,:)
    REAL, ALLOCATABLE :: prus(:,:), prvs(:,:), prws(:,:), prthls(:,:), prtkes(:,:)
    REAL, ALLOCATABLE :: prrs(:,:,:)

    ! Internal halo'd arrays (nlon, nlev+2)
    REAL, ALLOCATABLE :: zdxx(:,:), zdyy(:,:), zdzz(:,:), zzz(:,:), zdzx(:,:), zdzy(:,:)
    REAL, ALLOCATABLE :: zrhodj(:,:), zthvref(:,:), zpabst(:,:)
    REAL, ALLOCATABLE :: zut(:,:), zvt(:,:), zwt(:,:), ztket(:,:), zthlt(:,:)
    REAL, ALLOCATABLE :: zlengthm(:,:), zlengthh(:,:), zmfmoist(:,:)
    REAL, ALLOCATABLE :: zsrct(:,:), zsigs(:,:), zcei(:,:)
    REAL, ALLOCATABLE :: zflxzthvmf(:,:), zflxzumf(:,:), zflxzvmf(:,:)
    REAL, ALLOCATABLE :: zrus(:,:), zrvs(:,:), zrws(:,:), zrthls(:,:), zrtkes(:,:)
    REAL, ALLOCATABLE :: zwth(:,:), zwrc(:,:), zdp(:,:), ztp(:,:), ztdiff(:,:), ztdiss(:,:), zedr(:,:)
    REAL, ALLOCATABLE :: zrt(:,:,:), zrrs(:,:,:), zsvt(:,:,:), zrsvs(:,:,:), zwsv(:,:,:)
    REAL, ALLOCATABLE :: zhgradleo(:,:,:), zhgradgog(:,:,:)
    REAL, ALLOCATABLE :: zdircosxw(:), zdircosyw(:), zdircoszw(:)
    REAL, ALLOCATABLE :: zcosslope(:), zsinslope(:), zzs(:)
    REAL, ALLOCATABLE :: zsfth(:), zsfrv(:), zsfu(:), zsfv(:), zbl_depth(:), zsbl_depth(:)
    REAL, ALLOCATABLE :: zsfsv(:,:)

    CHARACTER(LEN=4), DIMENSION(2) :: HLBCX, HLBCY
    CHARACTER(LEN=4) :: HCLOUD, HELEC
    LOGICAL :: O2D, ONOMIXLG, OFLAT, OCOUPLES, OBLOWSNOW, OIBM, OFLYER
    LOGICAL :: OCOMPUTE_SRC, OOCEAN, ODEEPOC, ODIAG_IN_RUN, OCLOUDMODIFLM
    INTEGER :: KSPLIT, KSV, KSV_LGBEG, KSV_LGEND, KHALO
    INTEGER :: KSV_LIMA_NR, KSV_LIMA_NS, KSV_LIMA_NG, KSV_LIMA_NH
    INTEGER :: KGRADIENTSLEO, KGRADIENTSGOG, KRRL, KRRI
    REAL :: PRSNOW

    CALL GET_COMMAND_ARGUMENT(1, infile)
    CALL GET_COMMAND_ARGUMENT(2, outfile)

    OPEN(NEWUNIT=iu, FILE=TRIM(infile), FORM='UNFORMATTED', ACCESS='STREAM', STATUS='OLD')
    READ(iu) nlon, nlev, krr
    ALLOCATE(pdxx(nlon,nlev), pdyy(nlon,nlev), pdzz(nlon,nlev), pzz(nlon,nlev))
    ALLOCATE(prhodj(nlon,nlev), pthvref(nlon,nlev), ppabst(nlon,nlev))
    ALLOCATE(put(nlon,nlev), pvt(nlon,nlev), pwt(nlon,nlev), ptket(nlon,nlev), pthlt(nlon,nlev))
    ALLOCATE(psfth(nlon), psfrv(nlon))
    ALLOCATE(prt(nlon,nlev,krr))
    ALLOCATE(prus(nlon,nlev), prvs(nlon,nlev), prws(nlon,nlev), prthls(nlon,nlev), prtkes(nlon,nlev))
    ALLOCATE(prrs(nlon,nlev,krr))
    READ(iu) ptstep
    READ(iu) psfth, psfrv
    READ(iu) pdxx, pdyy, pdzz, pzz, prhodj, pthvref, ppabst, put, pvt, pwt, ptket, pthlt
    READ(iu) prt
    READ(iu) prus, prvs, prws, prthls, prtkes
    READ(iu) prrs
    CLOSE(iu)

    ! Config (mirrors ensure_phyex_init + c_turb_wrap's local overrides).
    OPEN(NEWUNIT=IULOUT, STATUS='SCRATCH')
    ZDZMIN = 20.0
    TPFILE%NLU = 0
    CALL INI_PHYEX('AROME ', TPFILE, .TRUE., IULOUT, 0, 1, &
        REAL(ptstep), ZDZMIN, 'ICE3', 'NONE', 'TKEL',     &
        LDDEFAULTVAL=.TRUE., LDREADNAM=.FALSE., LDCHECK=.FALSE., &
        KPRINT=0, LDINIT=.FALSE., PHYEX_OUT=G)
    G%MISC%LMFCONV      = .FALSE.
    G%MISC%OCOMPUTE_SRC = .TRUE.
    G%PARAM_ICEN%LWARM  = .TRUE.
    G%NEBN%LSUBG_COND   = .FALSE.
    G%NEBN%LSIGMAS      = .TRUE.
    G%NEBN%CFRAC_ICE_ADJUST = 'S'
    CALL INI_PHYEX('AROME ', TPFILE, .TRUE., IULOUT, 0, 1, &
        REAL(ptstep), ZDZMIN, 'ICE3', 'NONE', 'TKEL',     &
        LDDEFAULTVAL=.FALSE., LDREADNAM=.FALSE., LDCHECK=.FALSE., &
        KPRINT=0, LDINIT=.TRUE., PHYEX_IN=G, PHYEX_OUT=G)

    TURBN = G%TURBN
    TURBN%LHARAT   = .FALSE.
    TURBN%CTURBLEN = 'BL89'
    NEBN = G%NEBN
    NEBN%LSUBG_COND = .TRUE.
    TLES%LLES = .FALSE.
    TLES%LLES_CALL = .FALSE.
    TPFILE%LOPENED = .FALSE.

    nk = nlev + 2

    D%NIT = nlon; D%NIB = 1; D%NIE = nlon
    D%NJT = 1; D%NJB = 1; D%NJE = 1
    D%NIJT = nlon; D%NIJB = 1; D%NIJE = nlon
    D%NIBC = 1; D%NJBC = 1; D%NIEC = nlon; D%NJEC = 1
    D%NKL = -1; D%NKT = nk; D%NKA = nk; D%NKU = 1
    D%NKB = nk - 1; D%NKE = 2; D%NKTB = 2; D%NKTE = nk - 1
    D%NKLES = nk; D%NLESMASK = 0; D%NLES_TIMES = 0

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
    KSPLIT = MAX(1, G%TURBN%NTURBSPLIT)
    HLBCX = 'CYCL'; HLBCY = 'CYCL'
    HCLOUD = 'ICE3'; HELEC = 'NONE'
    O2D = .FALSE.; ONOMIXLG = .FALSE.; OFLAT = .FALSE.
    OCOUPLES = .FALSE.; OBLOWSNOW = .FALSE.; OIBM = .FALSE.
    OFLYER = .TRUE.; OCOMPUTE_SRC = .TRUE.
    OOCEAN = .FALSE.; ODEEPOC = .FALSE.; ODIAG_IN_RUN = .FALSE.
    OCLOUDMODIFLM = .FALSE.
    PRSNOW = 1.0

    ALLOCATE(zdxx(nlon,nk), zdyy(nlon,nk), zdzz(nlon,nk), zzz(nlon,nk), zdzx(nlon,nk), zdzy(nlon,nk))
    ALLOCATE(zrhodj(nlon,nk), zthvref(nlon,nk), zpabst(nlon,nk))
    ALLOCATE(zut(nlon,nk), zvt(nlon,nk), zwt(nlon,nk), ztket(nlon,nk), zthlt(nlon,nk))
    ALLOCATE(zlengthm(nlon,nk), zlengthh(nlon,nk), zmfmoist(nlon,nk))
    ALLOCATE(zsrct(nlon,nk), zsigs(nlon,nk), zcei(nlon,0))
    ALLOCATE(zflxzthvmf(nlon,nk), zflxzumf(nlon,nk), zflxzvmf(nlon,nk))
    ALLOCATE(zrus(nlon,nk), zrvs(nlon,nk), zrws(nlon,nk), zrthls(nlon,nk), zrtkes(nlon,nk))
    ALLOCATE(zwth(nlon,nk), zwrc(nlon,nk), zdp(nlon,nk), ztp(nlon,nk), ztdiff(nlon,nk), ztdiss(nlon,nk), zedr(nlon,nk))
    ALLOCATE(zrt(nlon,nk,krr), zrrs(nlon,nk,krr))
    ALLOCATE(zsvt(nlon,nk,0), zrsvs(nlon,nk,0), zwsv(nlon,nk,0))
    ALLOCATE(zhgradleo(nlon,nk,0), zhgradgog(nlon,nk,0))
    ALLOCATE(zdircosxw(nlon), zdircosyw(nlon), zdircoszw(nlon))
    ALLOCATE(zcosslope(nlon), zsinslope(nlon), zzs(nlon))
    ALLOCATE(zsfth(nlon), zsfrv(nlon), zsfu(nlon), zsfv(nlon), zbl_depth(nlon), zsbl_depth(nlon))
    ALLOCATE(zsfsv(nlon,0))

    CALL cin(pdxx, zdxx); CALL cin(pdyy, zdyy); CALL cin(pdzz, zdzz)
    CALL cin_z(pzz, zzz)
    CALL cin(prhodj, zrhodj); CALL cin(pthvref, zthvref); CALL cin(ppabst, zpabst)
    CALL cin(put, zut); CALL cin(pvt, zvt); CALL cin(pwt, zwt)
    CALL cin(ptket, ztket); CALL cin(pthlt, zthlt)
    CALL cin3(prt, zrt)
    CALL cin(prus, zrus); CALL cin(prvs, zrvs); CALL cin(prws, zrws)
    CALL cin(prthls, zrthls); CALL cin(prtkes, zrtkes)
    CALL cin3(prrs, zrrs)

    zdzx = 0.0; zdzy = 0.0
    zlengthm = 0.0; zlengthh = 0.0; zmfmoist = 0.0
    zsrct = 0.0; zsigs = 0.0
    zflxzthvmf = 0.0; zflxzumf = 0.0; zflxzvmf = 0.0
    zdircosxw = 0.0; zdircosyw = 0.0; zdircoszw = 1.0
    zcosslope = 1.0; zsinslope = 0.0; zzs = 0.0
    zsfth = psfth; zsfrv = psfrv; zsfu = 0.0; zsfv = 0.0
    zbl_depth = 0.0; zsbl_depth = 0.0

    CALL TURB(G%CST, G%CSTURB, G%MISC%TBUCONF, TURBN, NEBN, D, TLES, &
        krr, KRRL, KRRI, HLBCX, HLBCY, KGRADIENTSLEO, KGRADIENTSGOG, KHALO, &
        KSPLIT, OCLOUDMODIFLM, KSV, KSV_LGBEG, KSV_LGEND, &
        KSV_LIMA_NR, KSV_LIMA_NS, KSV_LIMA_NG, KSV_LIMA_NH, &
        O2D, ONOMIXLG, OFLAT, OCOUPLES, OBLOWSNOW, OIBM, OFLYER, OCOMPUTE_SRC, PRSNOW, &
        OOCEAN, ODEEPOC, ODIAG_IN_RUN, &
        TURBN%CTURBLEN_CLOUD, HCLOUD, HELEC, &
        ptstep, TPFILE, &
        zdxx, zdyy, zdzz, zdzx, zdzy, zzz, &
        zdircosxw, zdircosyw, zdircoszw, zcosslope, zsinslope, &
        zrhodj, zthvref, zhgradleo, zhgradgog, zzs, &
        zsfth, zsfrv, zsfsv, zsfu, zsfv, &
        zpabst, zut, zvt, zwt, ztket, zsvt, zsrct, &
        zlengthm, zlengthh, zmfmoist, &
        zbl_depth, zsbl_depth, &
        zcei, TURBN%XCEI_MIN, TURBN%XCEI_MAX, TURBN%XCOEF_AMPL_SAT, &
        zthlt, zrt, &
        zrus, zrvs, zrws, zrthls, zrrs, zrsvs, zrtkes, &
        zsigs, &
        zflxzthvmf, zflxzumf, zflxzvmf, &
        zwth, zwrc, zwsv, zdp, ztp, ztdiff, ztdiss, &
        TBUDGETS, KBUDGETS=0, PEDR=zedr)

    prus   = zrus(:,2:nk-1)
    prvs   = zrvs(:,2:nk-1)
    prws   = zrws(:,2:nk-1)
    prthls = zrthls(:,2:nk-1)
    prtkes = zrtkes(:,2:nk-1)
    prrs   = zrrs(:,2:nk-1,:)

    OPEN(NEWUNIT=iu, FILE=TRIM(outfile), FORM='UNFORMATTED', ACCESS='STREAM', STATUS='REPLACE')
    WRITE(iu) prus, prvs, prws, prthls, prtkes
    WRITE(iu) prrs
    CLOSE(iu)

CONTAINS

    SUBROUTINE cin(src, dst)
        REAL, INTENT(IN)  :: src(:,:)
        REAL, INTENT(OUT) :: dst(:,:)
        dst(:,2:nk-1) = src(:,1:nlev)
        dst(:,1)  = src(:,1)
        dst(:,nk) = src(:,nlev)
    END SUBROUTINE cin

    SUBROUTINE cin_z(src, dst)
        REAL, INTENT(IN)  :: src(:,:)
        REAL, INTENT(OUT) :: dst(:,:)
        dst(:,2:nk-1) = src(:,1:nlev)
        dst(:,1)  = 2.0*src(:,1)    - src(:,2)
        dst(:,nk) = 2.0*src(:,nlev) - src(:,nlev-1)
    END SUBROUTINE cin_z

    SUBROUTINE cin3(src, dst)
        REAL, INTENT(IN)  :: src(:,:,:)
        REAL, INTENT(OUT) :: dst(:,:,:)
        dst(:,2:nk-1,:) = src(:,1:nlev,:)
        dst(:,1,:)  = src(:,1,:)
        dst(:,nk,:) = src(:,nlev,:)
    END SUBROUTINE cin3

END PROGRAM ORACLE_TURB
