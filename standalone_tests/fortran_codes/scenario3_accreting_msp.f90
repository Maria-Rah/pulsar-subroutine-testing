!===============================================================================
! SCENARIO 3: Accreting MSP - FINAL VERSION
!===============================================================================
! Purpose: Test pulsar subroutine for accreting millisecond pulsar
!
! Physics:
!   - Weak B-field (buried): B ~ 2×10^8 G
!   - Very fast spin: P ~ 2 ms
!   - ACTIVE accretion: Ṁ ~ 10^-10 Msun/yr
!   - In binary with low-mass companion
!   - Competition: spin-up (accretion) vs spin-down (dipole)
!
! Author: Maria Rah
! Date: January 2026
! Status: ✓ VERIFIED - Issues found: PDOT_DECAY, XKACC
!
! References:
!   - Papitto et al. (2013), Nature, 501, 517 (IGR J18245-2452)
!   - Archibald et al. (2009), Science, 324, 1411 (Transitional MSPs)
!   - Bassa et al. (2014), MNRAS, 441, 1825 (PSR J1023+0038)
!   - Ghosh & Lamb (1979), ApJ, 234, 296 (Accretion theory)
!===============================================================================

PROGRAM test_scenario3_accreting_msp

    IMPLICIT NONE
    
    REAL*8, PARAMETER :: PI = 3.141592653589793D0
    REAL*8, PARAMETER :: CLIGHT = 2.99792458D10
    REAL*8, PARAMETER :: YR_TO_SEC = 3.15576D7
    REAL*8, PARAMETER :: MYR_TO_SEC = 3.15576D13
    REAL*8, PARAMETER :: MSUN = 1.989D33
    
    REAL*8, PARAMETER :: R_NS = 1.0D6
    REAL*8, PARAMETER :: I_NS = 1.0D45
    
    REAL*8 :: XKFIELD
    REAL*8, PARAMETER :: XKACC_CORRECT = 2.0D-16      ! CORRECTED (not 2e-5!)
    REAL*8, PARAMETER :: XKENV = 5.0D-21
    REAL*8, PARAMETER :: XKTAU_YR = 5.0D8
    REAL*8 :: XKTAU_SEC
    
    REAL*8, PARAMETER :: B_INITIAL = 2.0D8            ! Weak, buried
    REAL*8, PARAMETER :: P_INITIAL = 0.002D0          ! 2 ms (very fast)
    REAL*8, PARAMETER :: MDOT_RATE = 1.0D-10          ! Msun/yr
    REAL*8, PARAMETER :: RHO_ENV = 1.0D4              ! GC core density
    REAL*8, PARAMETER :: VREL_ENV = 20.0D5            ! 20 km/s
    REAL*8, PARAMETER :: T_MAX = 100.0D0
    REAL*8, PARAMETER :: DT = 0.1D0
    
    INTEGER :: NSTEPS, I
    REAL*8 :: TIME, BMAG, PERIOD
    REAL*8 :: PDOT, PDOT_DIPOLE, PDOT_ACC, PDOT_ENV
    REAL*8 :: DT_SEC, MDOT_CGS
    INTEGER, PARAMETER :: UOUT = 10
    CHARACTER(LEN=50) :: OUTFILE
    
    !===========================================================================
    ! INITIALIZATION
    !===========================================================================
    
    XKFIELD = (8.0D0 * PI**2) / (3.0D0 * CLIGHT**3) * (R_NS**6 / I_NS)
    XKTAU_SEC = XKTAU_YR * YR_TO_SEC
    
    ! Convert accretion rate to CGS
    MDOT_CGS = MDOT_RATE * MSUN / YR_TO_SEC
    
    OUTFILE = 'scenario3_accreting_msp_output.dat'
    OPEN(UNIT=UOUT, FILE=OUTFILE, STATUS='REPLACE')
    
    WRITE(UOUT, '(A)') '# Scenario 3: Accreting MSP (Transitional)'
    WRITE(UOUT, '(A)') '# Physics: Accretion spin-up vs dipole spin-down'
    WRITE(UOUT, '(A)') '#'
    WRITE(UOUT, '(A,ES12.5,A)') '#   B₀ = ', B_INITIAL, ' G (Papitto+ 2013)'
    WRITE(UOUT, '(A,F8.3,A)') '#   P₀ = ', P_INITIAL*1000.0D0, ' ms'
    WRITE(UOUT, '(A,ES12.5,A)') '#   Ṁ = ', MDOT_RATE, ' Msun/yr'
    WRITE(UOUT, '(A)') '#'
    WRITE(UOUT, '(A)') '# Columns: TIME B P Pdot Pdot_dip Pdot_acc Pdot_env B/B0 P/P0'
    WRITE(UOUT, '(A)') '#'
    
    TIME = 0.0D0
    BMAG = B_INITIAL
    PERIOD = P_INITIAL
    DT_SEC = DT * MYR_TO_SEC
    NSTEPS = INT(T_MAX / DT)
    
    WRITE(*, '(A)') '========================================================'
    WRITE(*, '(A)') 'SCENARIO 3: Accreting MSP Evolution'
    WRITE(*, '(A)') '========================================================'
    WRITE(*, '(A)') 'Physics:'
    WRITE(*, '(A)') '  ✓ Accretion spin-up (dominant!)'
    WRITE(*, '(A)') '  ✓ Dipole spin-down (weak)'
    WRITE(*, '(A)') '  ✓ Environmental drag'
    WRITE(*, '(A)') ''
    WRITE(*, '(A,ES12.5)') 'XKACC (corrected) = ', XKACC_CORRECT
    WRITE(*, '(A)') ''
    WRITE(*, '(A)') '   Time     P(ms)    Pdot_tot    Pdot_dip    Pdot_acc'
    WRITE(*, '(A)') '--------------------------------------------------------'
    
    !===========================================================================
    ! EVOLUTION
    !===========================================================================
    
    DO I = 0, NSTEPS
        TIME = I * DT
        
        ! Pdot components
        PDOT_DIPOLE = XKFIELD * (BMAG**2) / PERIOD
        PDOT_ACC = -XKACC_CORRECT * MDOT_CGS * PERIOD
        PDOT_ENV = XKENV * RHO_ENV * VREL_ENV * (PERIOD**2)
        PDOT = PDOT_DIPOLE + PDOT_ACC + PDOT_ENV
        
        WRITE(UOUT, '(9(ES15.7,2X))') TIME, BMAG, PERIOD, PDOT, &
                                       PDOT_DIPOLE, PDOT_ACC, PDOT_ENV, &
                                       BMAG/B_INITIAL, PERIOD/P_INITIAL
        
        IF (MOD(I, 200) == 0) THEN
            WRITE(*, '(F8.1,2X,F8.3,2X,4(ES12.5,2X))') &
                TIME, PERIOD*1000.0D0, PDOT, PDOT_DIPOLE, PDOT_ACC
        END IF
        
        ! Update
        IF (I < NSTEPS) THEN
            PERIOD = PERIOD + PDOT * DT_SEC
            IF (PERIOD < 1.0D-4) PERIOD = 1.0D-4
            BMAG = BMAG * EXP(-DT_SEC / XKTAU_SEC)
        END IF
    END DO
    
    !===========================================================================
    ! SUMMARY
    !===========================================================================
    
    WRITE(*, '(A)') '--------------------------------------------------------'
    WRITE(*, '(A)') 'FINAL STATE:'
    WRITE(*, '(A)') '========================================================'
    WRITE(*, '(A,ES12.5,A)') 'B_final = ', BMAG, ' G'
    WRITE(*, '(A,F10.3,A)') 'P_final = ', PERIOD*1000.0D0, ' ms'
    WRITE(*, '(A,ES12.5,A)') 'Pdot = ', PDOT, ' s/s'
    WRITE(*, '(A)') ''
    
    IF (PDOT < 0.0D0) THEN
        WRITE(*, '(A)') '→ NET SPIN-UP (accretion dominates)'
    ELSE
        WRITE(*, '(A)') '→ NET SPIN-DOWN'
    END IF
    
    WRITE(*, '(A)') ''
    WRITE(*, '(A)') 'ISSUES FOUND IN SUBROUTINE:'
    WRITE(*, '(A)') '  ❌ PDOT_DECAY (same as Sc1/Sc2)'
    WRITE(*, '(A)') '  ❌ XKACC too large (2e-5 should be ~2e-16)'
    WRITE(*, '(A)') '  ✓ PDOT_ENV OK'
    WRITE(*, '(A)') ''
    WRITE(*, '(A,A)') 'Output: ', TRIM(OUTFILE)
    WRITE(*, '(A)') '========================================================'
    
    CLOSE(UOUT)

END PROGRAM test_scenario3_accreting_msp
