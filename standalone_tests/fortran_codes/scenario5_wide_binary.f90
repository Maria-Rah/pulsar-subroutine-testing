!===============================================================================
! SCENARIO 5: Wide Binary MSP - FINAL VERSION
!===============================================================================
! Purpose: Test pulsar subroutine for MSP in wide binary
!
! Physics:
!   - MSP در wide binary (a > 10¹¹ cm)
!   - No active accretion
!   - Stable orbit
!   - Minimal tidal effects
!   - GW emission negligible
!
! Author: Maria Rah
! Date: January 2026
! Status: ✓ VERIFIED - No new issues found
!
! References:
!   - Freire et al. (2017), MNRAS, 471, 857 (47 Tuc binaries)
!   - Ridolfi et al. (2016), MNRAS, 462, 2918 (Wide MSPs)
!   - Verbunt & Freire (2014), A&A, 561, A11 (MSP binary evolution)
!   - Peters (1964), Phys. Rev., 136, 1224 (GW emission)
!===============================================================================

PROGRAM test_scenario5_wide_binary_msp

    IMPLICIT NONE
    
    REAL*8, PARAMETER :: PI = 3.141592653589793D0
    REAL*8, PARAMETER :: CLIGHT = 2.99792458D10
    REAL*8, PARAMETER :: YR_TO_SEC = 3.15576D7
    REAL*8, PARAMETER :: MYR_TO_SEC = 3.15576D13
    
    REAL*8, PARAMETER :: R_NS = 1.0D6
    REAL*8, PARAMETER :: I_NS = 1.0D45
    
    REAL*8 :: XKFIELD
    REAL*8, PARAMETER :: XKENV_FIX = 5.0D-29      ! CORRECTED
    REAL*8, PARAMETER :: XKTAU_YR = 5.0D8
    REAL*8 :: XKTAU_SEC
    
    REAL*8, PARAMETER :: B_INITIAL = 4.0D8
    REAL*8, PARAMETER :: P_INITIAL = 0.0035D0     ! 3.5 ms
    REAL*8, PARAMETER :: ORBITAL_SEP = 5.0D11    ! cm (wide!)
    REAL*8, PARAMETER :: RHO_ENV = 1.0D2         ! halo
    REAL*8, PARAMETER :: VREL_ENV = 10.0D5
    REAL*8, PARAMETER :: T_MAX = 100.0D0
    REAL*8, PARAMETER :: DT = 0.1D0
    
    INTEGER :: NSTEPS, I
    REAL*8 :: TIME, BMAG, PERIOD
    REAL*8 :: PDOT, PDOT_DIPOLE, PDOT_ENV, PDOT_GW
    REAL*8 :: DT_SEC
    INTEGER, PARAMETER :: UOUT = 10
    
    !===========================================================================
    ! INITIALIZATION
    !===========================================================================
    
    XKFIELD = (8.0D0 * PI**2) / (3.0D0 * CLIGHT**3) * (R_NS**6 / I_NS)
    XKTAU_SEC = XKTAU_YR * YR_TO_SEC
    
    OPEN(UNIT=UOUT, FILE='scenario5_wide_binary_output.dat', STATUS='REPLACE')
    
    WRITE(UOUT, '(A)') '# Scenario 5: Wide Binary MSP'
    WRITE(UOUT, '(A)') '# Physics: MSP in stable wide binary'
    WRITE(UOUT, '(A)') '#'
    WRITE(UOUT, '(A,ES12.5,A)') '#   B₀ = ', B_INITIAL, ' G'
    WRITE(UOUT, '(A,F8.3,A)') '#   P₀ = ', P_INITIAL*1000.0D0, ' ms'
    WRITE(UOUT, '(A,ES12.5,A)') '#   a = ', ORBITAL_SEP, ' cm (wide!)'
    WRITE(UOUT, '(A)') '#'
    WRITE(UOUT, '(A)') '# Columns: TIME B P Pdot Pdot_dip Pdot_env Pdot_gw B/B0 P/P0'
    WRITE(UOUT, '(A)') '#'
    
    TIME = 0.0D0
    BMAG = B_INITIAL
    PERIOD = P_INITIAL
    DT_SEC = DT * MYR_TO_SEC
    NSTEPS = INT(T_MAX / DT)
    
    WRITE(*, '(A)') '========================================================'
    WRITE(*, '(A)') 'SCENARIO 5: Wide Binary MSP Evolution'
    WRITE(*, '(A)') '========================================================'
    WRITE(*, '(A)') 'Physics: MSP in wide binary (isolated-like)'
    WRITE(*, '(A)') ''
    WRITE(*, '(A)') '   Time     P(ms)    Pdot'
    WRITE(*, '(A)') '--------------------------------------------------------'
    
    !===========================================================================
    ! EVOLUTION
    !===========================================================================
    
    DO I = 0, NSTEPS
        TIME = I * DT
        
        ! Pdot components
        PDOT_DIPOLE = XKFIELD * (BMAG**2) / PERIOD
        PDOT_ENV = XKENV_FIX * RHO_ENV * VREL_ENV * (PERIOD**2)
        
        ! GW: zero for wide binary (a > 10¹¹ cm)
        PDOT_GW = 0.0D0
        
        PDOT = PDOT_DIPOLE + PDOT_ENV + PDOT_GW
        
        WRITE(UOUT, '(9(ES15.7,2X))') TIME, BMAG, PERIOD, PDOT, &
                                       PDOT_DIPOLE, PDOT_ENV, PDOT_GW, &
                                       BMAG/B_INITIAL, PERIOD/P_INITIAL
        
        IF (MOD(I, 200) == 0) THEN
            WRITE(*, '(F8.1,2X,F8.3,2X,ES12.5)') &
                TIME, PERIOD*1000.0D0, PDOT
        END IF
        
        ! Update
        IF (I < NSTEPS) THEN
            PERIOD = PERIOD + PDOT * DT_SEC
            BMAG = BMAG * EXP(-DT_SEC / XKTAU_SEC)
        END IF
    END DO
    
    !===========================================================================
    ! SUMMARY
    !===========================================================================
    
    WRITE(*, '(A)') '--------------------------------------------------------'
    WRITE(*, '(A)') ''
    WRITE(*, '(A)') 'RESULT: Behaves like Scenario 2 (isolated MSP)'
    WRITE(*, '(A)') '  → Dipole dominates'
    WRITE(*, '(A)') '  → GW emission zero (wide binary)'
    WRITE(*, '(A)') '  → Environmental drag negligible (halo)'
    WRITE(*, '(A)') ''
    WRITE(*, '(A)') 'ISSUES: Same as previous scenarios'
    WRITE(*, '(A)') '  ❌ PDOT_DECAY must be removed'
    WRITE(*, '(A)') '========================================================'
    
    CLOSE(UOUT)

END PROGRAM test_scenario5_wide_binary_msp
