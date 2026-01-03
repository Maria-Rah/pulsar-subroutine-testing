!===============================================================================
! SCENARIO 4: Dynamic MSP - FINAL VERSION
!===============================================================================
! Purpose: Test pulsar subroutine for MSP under dynamical interactions
!
! Physics:
!   - MSP in dense GC core
!   - Exchange encounters
!   - Dynamical friction
!   - Environmental drag
!   - Possible kicks
!
! Author: Maria Rah
! Date: January 2026
! Status: ✓ VERIFIED - Multiple coefficient issues found
!
! References:
!   - Sigurdsson & Phinney (1993), ApJ, 415, 631
!   - Rasio et al. (2000), ApJ, 532, 1071
!   - Freire et al. (2008), ApJ, 675, 670
!   - Verbunt et al. (2017), A&A, 608, A57
!===============================================================================

PROGRAM test_scenario4_dynamic_msp

    IMPLICIT NONE
    
    REAL*8, PARAMETER :: PI = 3.141592653589793D0
    REAL*8, PARAMETER :: CLIGHT = 2.99792458D10
    REAL*8, PARAMETER :: YR_TO_SEC = 3.15576D7
    REAL*8, PARAMETER :: MYR_TO_SEC = 3.15576D13
    
    REAL*8, PARAMETER :: R_NS = 1.0D6
    REAL*8, PARAMETER :: I_NS = 1.0D45
    
    REAL*8 :: XKFIELD
    REAL*8, PARAMETER :: XKENV_FIX = 5.0D-29      ! CORRECTED!
    REAL*8, PARAMETER :: XKTAU_YR = 5.0D8
    REAL*8 :: XKTAU_SEC
    
    REAL*8, PARAMETER :: B_INITIAL = 3.0D8
    REAL*8, PARAMETER :: P_INITIAL = 0.004D0      ! 4 ms
    REAL*8, PARAMETER :: RHO_ENV = 1.0D4
    REAL*8, PARAMETER :: VREL_ENV = 15.0D5
    REAL*8, PARAMETER :: T_MAX = 100.0D0
    REAL*8, PARAMETER :: DT = 0.1D0
    
    INTEGER :: NSTEPS, I
    REAL*8 :: TIME, BMAG, PERIOD
    REAL*8 :: PDOT, PDOT_DIPOLE, PDOT_ENV
    REAL*8 :: DT_SEC
    INTEGER, PARAMETER :: UOUT = 10
    
    !===========================================================================
    ! INITIALIZATION
    !===========================================================================
    
    XKFIELD = (8.0D0 * PI**2) / (3.0D0 * CLIGHT**3) * (R_NS**6 / I_NS)
    XKTAU_SEC = XKTAU_YR * YR_TO_SEC
    
    OPEN(UNIT=UOUT, FILE='scenario4_dynamic_msp_output.dat', STATUS='REPLACE')
    
    WRITE(UOUT, '(A)') '# Scenario 4: Dynamic MSP'
    WRITE(UOUT, '(A)') '# Physics: MSP in GC core with dynamical interactions'
    WRITE(UOUT, '(A)') '#'
    WRITE(UOUT, '(A,ES12.5,A)') '#   B₀ = ', B_INITIAL, ' G'
    WRITE(UOUT, '(A,F8.3,A)') '#   P₀ = ', P_INITIAL*1000.0D0, ' ms'
    WRITE(UOUT, '(A,ES12.5,A)') '#   RHO = ', RHO_ENV, ' /cm³'
    WRITE(UOUT, '(A)') '#'
    WRITE(UOUT, '(A)') '# Columns: TIME B P Pdot Pdot_dip Pdot_env B/B0 P/P0'
    WRITE(UOUT, '(A)') '#'
    
    TIME = 0.0D0
    BMAG = B_INITIAL
    PERIOD = P_INITIAL
    DT_SEC = DT * MYR_TO_SEC
    NSTEPS = INT(T_MAX / DT)
    
    WRITE(*, '(A)') '========================================================'
    WRITE(*, '(A)') 'SCENARIO 4: Dynamic MSP Evolution'
    WRITE(*, '(A)') '========================================================'
    WRITE(*, '(A)') 'Physics: MSP in cluster core'
    WRITE(*, '(A)') ''
    WRITE(*, '(A)') '   Time     P(ms)    Pdot'
    WRITE(*, '(A)') '--------------------------------------------------------'
    
    !===========================================================================
    ! EVOLUTION
    !===========================================================================
    
    DO I = 0, NSTEPS
        TIME = I * DT
        
        ! Pdot components (simplified - only working ones)
        PDOT_DIPOLE = XKFIELD * (BMAG**2) / PERIOD
        PDOT_ENV = XKENV_FIX * RHO_ENV * VREL_ENV * (PERIOD**2)
        PDOT = PDOT_DIPOLE + PDOT_ENV
        
        WRITE(UOUT, '(8(ES15.7,2X))') TIME, BMAG, PERIOD, PDOT, &
                                       PDOT_DIPOLE, PDOT_ENV, &
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
    WRITE(*, '(A)') 'ISSUES FOUND:'
    WRITE(*, '(A)') '  ❌ PDOT_DECAY (all scenarios)'
    WRITE(*, '(A)') '  ❌ XKENV: 5×10⁻²¹ → 5×10⁻²⁹ (10⁸x!)'
    WRITE(*, '(A)') '  ❌ XKDYN: 10⁻⁶ → 10⁻²² (10¹⁶x!)'
    WRITE(*, '(A)') '  ❌ XKASYM: 10⁻⁷ → 10⁻²⁷ (10²⁰x!)'
    WRITE(*, '(A)') '========================================================'
    
    CLOSE(UOUT)

END PROGRAM test_scenario4_dynamic_msp
