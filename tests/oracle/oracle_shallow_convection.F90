! Independent golden-reference oracle for SHALLOW_CONVECTION.
!
! Calls SHALLOW_CONVECTION *directly* (USE MODI_SHALLOW_CONVECTION) — not through
! the Cython/C bridge. Mirrors c_shallow_convection_wrap: no-halo DIMPHYEX,
! zeroed NSV, hand-rolled CONVPAR/CONVPAR_SHAL (Kain-Fritsch, not in PHYEX_t),
! CST from INI_PHYEX. Reads inputs / writes outputs as stream binary.
!
! Usage: oracle_shallow_convection_dp.exe <input.bin> <output.bin>
PROGRAM ORACLE_SHALLOW_CONVECTION
    USE MODD_DIMPHYEX,    ONLY: DIMPHYEX_t
    USE MODD_PHYEX,       ONLY: PHYEX_t
    USE MODD_NSV,         ONLY: NSV_t
    USE MODD_CONVPAR,     ONLY: CONVPAR_t
    USE MODD_CONVPAR_SHAL, ONLY: CONVPAR_SHAL
    USE MODD_IO,          ONLY: TFILEDATA
    USE MODI_INI_PHYEX,   ONLY: INI_PHYEX
    USE MODI_SHALLOW_CONVECTION, ONLY: SHALLOW_CONVECTION

    IMPLICIT NONE

    INTEGER(KIND=4) :: nlon, nlev, kice, kbdia, ktdia, iosettadj, ioch1conv, kch1
    REAL :: ptadjs
    CHARACTER(LEN=512) :: infile, outfile
    INTEGER :: iu

    TYPE(PHYEX_t), SAVE :: G
    TYPE(DIMPHYEX_t) :: D
    TYPE(NSV_t)      :: NSV
    TYPE(CONVPAR_t)  :: CONVPAR
    TYPE(CONVPAR_SHAL) :: CVP_SHAL
    TYPE(TFILEDATA)  :: TPFILE
    INTEGER :: IULOUT
    REAL :: ZDZMIN
    LOGICAL :: LOSETTADJ, LOCH1CONV

    REAL,    ALLOCATABLE :: ptkecls(:)
    REAL,    ALLOCATABLE :: ppabst(:,:), pzz(:,:), ptt(:,:), prvt(:,:), prct(:,:), prit(:,:), pwt(:,:)
    REAL,    ALLOCATABLE :: ptten(:,:), prvten(:,:), prcten(:,:), priten(:,:), pumf(:,:)
    INTEGER, ALLOCATABLE :: kcltop(:), kclbas(:)
    REAL,    ALLOCATABLE :: pch1(:,:,:), pch1ten(:,:,:)

    ! Two-level top padding so CONVECT_UPDRAFT_SHAL's KCTL+2 detrainment writes
    ! stay in-bounds; mirrors the c_shallow_convection bridge (issue #8).
    INTEGER :: nk, jbuf, ikt_eff
    REAL, ALLOCATABLE :: z_ppabst(:,:), z_pzz(:,:), z_ptt(:,:), z_prvt(:,:), z_prct(:,:), z_prit(:,:), z_pwt(:,:)
    REAL, ALLOCATABLE :: z_ptten(:,:), z_prvten(:,:), z_prcten(:,:), z_priten(:,:), z_pumf(:,:)
    REAL, ALLOCATABLE :: z_pch1(:,:,:), z_pch1ten(:,:,:)

    CALL GET_COMMAND_ARGUMENT(1, infile)
    CALL GET_COMMAND_ARGUMENT(2, outfile)

    OPEN(NEWUNIT=iu, FILE=TRIM(infile), FORM='UNFORMATTED', ACCESS='STREAM', STATUS='OLD')
    READ(iu) nlon, nlev, kice, kbdia, ktdia, iosettadj, ioch1conv, kch1
    ALLOCATE(ptkecls(nlon))
    ALLOCATE(ppabst(nlon,nlev), pzz(nlon,nlev), ptt(nlon,nlev), prvt(nlon,nlev))
    ALLOCATE(prct(nlon,nlev), prit(nlon,nlev), pwt(nlon,nlev))
    ALLOCATE(ptten(nlon,nlev), prvten(nlon,nlev), prcten(nlon,nlev), priten(nlon,nlev), pumf(nlon,nlev))
    ALLOCATE(kcltop(nlon), kclbas(nlon))
    ALLOCATE(pch1(nlon,nlev,kch1), pch1ten(nlon,nlev,kch1))
    READ(iu) ptadjs
    READ(iu) ptkecls
    READ(iu) ppabst, pzz, ptt, prvt, prct, prit, pwt
    READ(iu) ptten, prvten, prcten, priten, pumf
    READ(iu) kcltop, kclbas
    READ(iu) pch1, pch1ten
    CLOSE(iu)

    LOSETTADJ = (iosettadj /= 0)
    LOCH1CONV = (ioch1conv /= 0)

    ! Physical constants from INI_PHYEX (mirrors ensure_phyex_init); only CST is
    ! used by SHALLOW_CONVECTION (CONVPAR/CVP_SHAL/NSV are hand-rolled below).
    OPEN(NEWUNIT=IULOUT, STATUS='SCRATCH')
    ZDZMIN = 20.0
    TPFILE%NLU = 0
    CALL INI_PHYEX('AROME ', TPFILE, .TRUE., IULOUT, 0, 1, &
        REAL(ptadjs), ZDZMIN, 'ICE3', 'NONE', 'TKEL',     &
        LDDEFAULTVAL=.TRUE., LDREADNAM=.FALSE., LDCHECK=.FALSE., &
        KPRINT=0, LDINIT=.FALSE., PHYEX_OUT=G)
    G%MISC%LMFCONV      = .FALSE.
    G%MISC%OCOMPUTE_SRC = .TRUE.
    G%PARAM_ICEN%LWARM  = .TRUE.
    G%NEBN%LSUBG_COND   = .FALSE.
    G%NEBN%LSIGMAS      = .TRUE.
    G%NEBN%CFRAC_ICE_ADJUST = 'S'
    CALL INI_PHYEX('AROME ', TPFILE, .TRUE., IULOUT, 0, 1, &
        REAL(ptadjs), ZDZMIN, 'ICE3', 'NONE', 'TKEL',     &
        LDDEFAULTVAL=.FALSE., LDREADNAM=.FALSE., LDCHECK=.FALSE., &
        KPRINT=0, LDINIT=.TRUE., PHYEX_IN=G, PHYEX_OUT=G)

    nk = nlev + 2
    D%NIT = nlon; D%NIB = 1; D%NIE = nlon
    D%NJT = 1; D%NJB = 1; D%NJE = 1
    D%NKT = nk; D%NKL = 1; D%NKA = 1; D%NKU = nk
    D%NKB = 1; D%NKE = nlev; D%NKTB = 1; D%NKTE = nlev
    D%NIBC = 1; D%NJBC = 1; D%NIEC = nlon; D%NJEC = 1
    D%NIJT = nlon; D%NIJB = 1; D%NIJE = nlon
    D%NKLES = nk; D%NLESMASK = 0; D%NLES_TIMES = 0

    NSV%NSV_USER = 0
    NSV%NSV_C2R2BEG = 0; NSV%NSV_C2R2END = 0
    NSV%NSV_C1R3BEG = 0; NSV%NSV_C1R3END = 0
    NSV%NSV_ELECBEG = 0; NSV%NSV_ELECEND = 0
    NSV%NSV_LNOXBEG = 0; NSV%NSV_LNOXEND = 0
    NSV%NSV_DSTBEG = 0; NSV%NSV_DSTEND = 0
    NSV%NSV_SLTBEG = 0; NSV%NSV_SLTEND = 0
    NSV%NSV_PPBEG = 0; NSV%NSV_PPEND = 0
    NSV%NSV_CSBEG = 0; NSV%NSV_CSEND = 0
    NSV%NSV_AERBEG = 0; NSV%NSV_AEREND = 0
    NSV%NSV_SNWBEG = 0; NSV%NSV_SNWEND = 0
    NSV%NSV_CHEMBEG = 0; NSV%NSV_CHEMEND = 0

    CONVPAR%XA25 = 625.0E6; CONVPAR%XCRAD = 1500.0; CONVPAR%XCDEPTH = 3000.0
    CONVPAR%XENTR = 0.03; CONVPAR%XZLCL = 3500.0; CONVPAR%XZPBL = 6000.0
    CONVPAR%XWTRIG = 6.0; CONVPAR%XNHGAM = 1.3333; CONVPAR%XTFRZ1 = 268.16
    CONVPAR%XTFRZ2 = 248.16; CONVPAR%XRHDBC = 0.9; CONVPAR%XRCONV = 0.015
    CONVPAR%XSTABT = 0.75; CONVPAR%XSTABC = 0.95; CONVPAR%XUSRDPTH = 16500.0
    CONVPAR%XMELDPTH = 10000.0; CONVPAR%XUVDP = 0.7

    CVP_SHAL%XA25 = 625.0E6; CVP_SHAL%XCRAD = 1500.0; CVP_SHAL%XCTIME_SHAL = 10800.0
    CVP_SHAL%XCDEPTH = 2500.0; CVP_SHAL%XCDEPTH_D = 3000.0; CVP_SHAL%XDTPERT = 1.0
    CVP_SHAL%XATPERT = 0.0; CVP_SHAL%XBTPERT = 0.0; CVP_SHAL%XENTR = 0.03
    CVP_SHAL%XZLCL = 3500.0; CVP_SHAL%XZPBL = 6000.0; CVP_SHAL%XWTRIG = 6.0
    CVP_SHAL%XNHGAM = 1.3333; CVP_SHAL%XTFRZ1 = 268.16; CVP_SHAL%XTFRZ2 = 248.16
    CVP_SHAL%XSTABT = 0.75; CVP_SHAL%XSTABC = 0.95; CVP_SHAL%XAW = 1.0
    CVP_SHAL%XBW = 0.0; CVP_SHAL%LLSMOOTH = .TRUE.

    ! Pad with two buffer levels above the physical top; pass KTDIA+2 so the
    ! scheme reserves JCVEXT=2 boundary levels and KCTL+2 lands in the buffer.
    ALLOCATE(z_ppabst(nlon,nk), z_pzz(nlon,nk), z_ptt(nlon,nk))
    ALLOCATE(z_prvt(nlon,nk), z_prct(nlon,nk), z_prit(nlon,nk), z_pwt(nlon,nk))
    ALLOCATE(z_ptten(nlon,nk), z_prvten(nlon,nk), z_prcten(nlon,nk), z_priten(nlon,nk), z_pumf(nlon,nk))
    ALLOCATE(z_pch1(nlon,nk,kch1), z_pch1ten(nlon,nk,kch1))
    z_ppabst(:,1:nlev)=ppabst; z_pzz(:,1:nlev)=pzz; z_ptt(:,1:nlev)=ptt
    z_prvt(:,1:nlev)=prvt; z_prct(:,1:nlev)=prct; z_prit(:,1:nlev)=prit; z_pwt(:,1:nlev)=pwt
    z_ptten(:,1:nlev)=ptten; z_prvten(:,1:nlev)=prvten; z_prcten(:,1:nlev)=prcten
    z_priten(:,1:nlev)=priten; z_pumf(:,1:nlev)=pumf
    z_pch1(:,1:nlev,:)=pch1; z_pch1ten(:,1:nlev,:)=pch1ten
    DO jbuf = nlev+1, nk
        z_pzz(:,jbuf)    = 2.0*z_pzz(:,jbuf-1)    - z_pzz(:,jbuf-2)
        z_ppabst(:,jbuf) = MAX(1.0, 2.0*z_ppabst(:,jbuf-1) - z_ppabst(:,jbuf-2))
        z_ptt(:,jbuf)=z_ptt(:,nlev); z_prvt(:,jbuf)=z_prvt(:,nlev); z_prct(:,jbuf)=z_prct(:,nlev)
        z_prit(:,jbuf)=z_prit(:,nlev); z_pwt(:,jbuf)=z_pwt(:,nlev)
        z_ptten(:,jbuf)=0.; z_prvten(:,jbuf)=0.; z_prcten(:,jbuf)=0.; z_priten(:,jbuf)=0.; z_pumf(:,jbuf)=0.
        z_pch1(:,jbuf,:)=z_pch1(:,nlev,:); z_pch1ten(:,jbuf,:)=0.
    END DO
    ikt_eff = ktdia + 2

    CALL SHALLOW_CONVECTION(CVP_SHAL, G%CST, D, NSV, CONVPAR, kbdia, ikt_eff, &
        kice, LOSETTADJ, ptadjs, z_ppabst, z_pzz,                           &
        ptkecls, z_ptt, z_prvt, z_prct, z_prit, z_pwt,                      &
        z_ptten, z_prvten, z_prcten, z_priten,                             &
        kcltop, kclbas, z_pumf, LOCH1CONV, kch1,                            &
        z_pch1, z_pch1ten)

    ptten=z_ptten(:,1:nlev); prvten=z_prvten(:,1:nlev); prcten=z_prcten(:,1:nlev)
    priten=z_priten(:,1:nlev); pumf=z_pumf(:,1:nlev); pch1ten=z_pch1ten(:,1:nlev,:)

    OPEN(NEWUNIT=iu, FILE=TRIM(outfile), FORM='UNFORMATTED', ACCESS='STREAM', STATUS='REPLACE')
    WRITE(iu) ptten, prvten, prcten, priten, pumf
    WRITE(iu) kcltop, kclbas
    WRITE(iu) pch1ten
    CLOSE(iu)

END PROGRAM ORACLE_SHALLOW_CONVECTION
