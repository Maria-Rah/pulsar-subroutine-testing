! ============================================================================
! SCENARIO 6: NS-NS MERGER (PARTIAL TEST)
! ============================================================================
PROGRAM test_scenario6
    IMPLICIT NONE
    
    REAL*8, PARAMETER :: PI = 3.141592653589793D0
    REAL*8, PARAMETER :: CLIGHT = 2.99792458D10
    REAL*8, PARAMETER :: MYR_TO_SEC = 3.15576D13
    REAL*8, PARAMETER :: R_NS = 1.0D6
    REAL*8, PARAMETER :: I_NS = 1.0D45
    
    REAL*8 :: XKFIELD, XKENV, XKASYM, XKTAU_SEC
    REAL*8 :: B0, P0, B, P, Pdot
    REAL*8 :: Pdot_dipole, Pdot_env, Pdot_asym
    REAL*8 :: RHO, VREL, VKICK
    REAL*8 :: TMAX, DT, DT_SEC, TIME
    INTEGER :: N, I
    
    REAL*8, ALLOCATABLE :: TIME_ARR(:), B_ARR(:), P_ARR(:), PDOT_ARR(:)
    REAL*8, ALLOCATABLE :: PDOT_DIP(:), PDOT_ENV_ARR(:), PDOT_ASYM_ARR(:)
    REAL*8, ALLOCATABLE :: B_NORM(:), P_NORM(:)
    
    WRITE(*,*) "================================================================"
    WRITE(*,*) "SCENARIO 6: NS-NS MERGER (PARTIAL TEST)"
    WRITE(*,*) "GW and MERGE components disabled (coefficient errors)"
    WRITE(*,*) "================================================================"
    
    ! Coefficients (CORRECTED)
    XKFIELD = (8.0D0 * PI**2) / (3.0D0 * CLIGHT**3) * (R_NS**6 / I_NS)
    XKENV = 5.0D-29
    XKASYM = 1.0D-27
    XKTAU_SEC = 5.0D8 * 3.15576D7
    
    ! Initial conditions
    B0 = 5.0D11
    P0 = 0.010D0
    RHO = 1.0D4
    VREL = 15.0D5
    VKICK = 300.0D5
    
    WRITE(*,'(A,ES12.5,A)') "  B0 = ", B0, " G"
    WRITE(*,'(A,F8.3,A)') "  P0 = ", P0*1000.0D0, " ms"
    WRITE(*,'(A,F8.1,A)') "  V_kick = ", VKICK/1.0D5, " km/s"
    
    ! Time
    TMAX = 100.0D0
    DT = 0.1D0
    DT_SEC = DT * MYR_TO_SEC
    N = INT(TMAX/DT) + 1
    
    ALLOCATE(TIME_ARR(N), B_ARR(N), P_ARR(N), PDOT_ARR(N))
    ALLOCATE(PDOT_DIP(N), PDOT_ENV_ARR(N), PDOT_ASYM_ARR(N))
    ALLOCATE(B_NORM(N), P_NORM(N))
    
    B = B0
    P = P0
    
    DO I = 1, N
        TIME = (I-1) * DT
        
        ! Pdot components (GW and MERGE disabled)
        Pdot_dipole = XKFIELD * (B**2) / P
        Pdot_env = XKENV * RHO * VREL * (P**2)
        
        IF (VKICK > 200.0D5) THEN
            Pdot_asym = XKASYM * VKICK * P
        ELSE
            Pdot_asym = 0.0D0
        END IF
        
        Pdot = Pdot_dipole + Pdot_env + Pdot_asym
        
        TIME_ARR(I) = TIME
        B_ARR(I) = B
        P_ARR(I) = P
        PDOT_ARR(I) = Pdot
        PDOT_DIP(I) = Pdot_dipole
        PDOT_ENV_ARR(I) = Pdot_env
        PDOT_ASYM_ARR(I) = Pdot_asym
        B_NORM(I) = B / B0
        P_NORM(I) = P / P0
        
        IF (MOD(I-1, 200) == 0) THEN
            WRITE(*,'(F8.1,ES12.4,F10.3,ES14.4)') TIME, B, P*1000.0D0, Pdot
        END IF
        
        IF (I < N) THEN
            P = P + Pdot * DT_SEC
            B = B * EXP(-DT_SEC / XKTAU_SEC)
        END IF
    END DO
    
    WRITE(*,*)
    WRITE(*,'(A,ES12.4,A)') "  B_final = ", B_ARR(N), " G"
    WRITE(*,'(A,F10.3,A)') "  P_final = ", P_ARR(N)*1000.0D0, " ms"
    WRITE(*,'(A,ES12.4,A)') "  Pdot_final = ", PDOT_ARR(N), " s/s"
    WRITE(*,*) "  Status: PARTIAL TEST (Dipole+Env+Asym only)"
    
    OPEN(UNIT=10, FILE='scenario6_output.dat', STATUS='REPLACE')
    WRITE(10,'(A)') '# TIME B P Pdot Pdot_dip Pdot_env Pdot_asym B/B0 P/P0'
    DO I = 1, N
        WRITE(10,'(9ES16.7)') TIME_ARR(I), B_ARR(I), P_ARR(I), PDOT_ARR(I), &
                               PDOT_DIP(I), PDOT_ENV_ARR(I), PDOT_ASYM_ARR(I), &
                               B_NORM(I), P_NORM(I)
    END DO
    CLOSE(10)
    
    WRITE(*,*) "✓ Output: scenario6_output.dat"
    
    DEALLOCATE(TIME_ARR, B_ARR, P_ARR, PDOT_ARR)
    DEALLOCATE(PDOT_DIP, PDOT_ENV_ARR, PDOT_ASYM_ARR, B_NORM, P_NORM)
    
END PROGRAM test_scenario6
