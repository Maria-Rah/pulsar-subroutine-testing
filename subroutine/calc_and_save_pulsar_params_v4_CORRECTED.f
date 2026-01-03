C***********************************************************************
C     FILE: calc_and_save_pulsar_params.f (VERSION 4.0 - CORRECTED)
C     PURPOSE: Calculate and save pulsar parameters with internal
C              physics-based scenario detection
C     AUTHOR: Maria Rah
C     DATE: January 2026
C     
C     CHANGES FROM V3.0:
C       - Output file renamed: ns.33_* → pulsar.55_*
C       - Unit number matches extension (UNIT=55 → pulsar.55_*)
C       - Ready for common6.h variable declarations
C       - Ready for file_init.F integration with KZ check
C     
C     CORRECTIONS FROM STANDALONE SCENARIO TESTING (Jan 2026):
C       - PDOT_DECAY removed (double counting with B-field exponential decay)
C       - Coefficient values verified via 7 scenario tests
C       - XKACC, XKENV, XKDYN, XKASYM tested and working
C       - XKGW and XKMERGE need verification (Scenarios 6,7)
C     
C     COEFFICIENT VALUES (in common6.h - VERIFIED VIA TESTING):
C       XKFIELD: from dipole radiation formula (auto-calculated)
C       XKACC = 2.0D-16 (Ghosh & Lamb 1979)
C       XKENV = 5.0D-29 (environmental drag)
C       XKDYN = 1.0D-22 (dynamical friction)
C       XKASYM = 1.0D-27 (asymmetric kick effects)
C       XKGW needs verification from Peters (1964)
C       XKMERGE needs verification from literature
C     
C     KEY FEATURES:
C       - Detects scenarios using NBODY6++GPU internal physics
C       - Sets flags internally based on physical conditions
C       - Uses only common6.h data (simplified arguments)
C       - Compatible with Rainer Spurzem's NBODY6++GPU design
C       - Generates P-Pdot, P-B, P-E data for plotting
C***********************************************************************
      SUBROUTINE calc_and_save_pulsar_params(I)
C***********************************************************************
C     Main subroutine for pulsar parameter calculation and tracking
C
C     INPUT:
C       I - Pulsar index
C
C     All other data comes from common6.h:
C       - Positions, velocities, masses
C       - Binary information
C       - Stellar types
C       - Physical constants from Namelist
C
C     OUTPUT:
C       - Updates PERIOD(I), PDOT(I), BMAG(I)
C       - Sets scenario flags
C       - Writes to pulsar.55_* output file
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'common6.h'
C
C     Input
      INTEGER I
C
C     Local variables
      INTEGER J, I_COMP, K, SCENARIO_ID, NEIGHBOR_COUNT
      INTEGER KW_COMP
      REAL*8 DT, DM, DM_DT, DR_DT, DE_DT
      REAL*8 VKICK, VKICK_SQ, VREL, VMEAN(3)
      REAL*8 ORBITAL_SEP, Q_RATIO, R_LOBE, R_COMP, ECC
      REAL*8 DIST, DIST_CENTER, LAT, LON, ALPHA
      REAL*8 E_KINETIC, E_POTENTIAL, E_TOTAL, E_BINDING
      REAL*8 E_BIND_NOW, E_CHANGE
      REAL*8 GW_POWER, T_MERGE, TIDAL_RADIUS
      REAL*8 M1, M2
      REAL*8 PDOT_DIPOLE, PDOT_DECAY, PDOT_ENV, PDOT_ACC
      REAL*8 PDOT_MERGE, PDOT_ASYM, PDOT_DYN, PDOT_GW
      LOGICAL IS_BINARY, IS_SINGLE, WAS_IN_BINARY, WAS_SINGLE
      LOGICAL COMP_IS_NS, COMP_IS_BH, COMP_IS_WD
      LOGICAL IS_CLOSE_ENOUGH, IS_DONOR_EVOLVING, IS_RLOF
      LOGICAL IS_ACCRETING, NO_ACCRETION, IS_WIDE
      LOGICAL SUDDEN_MASS_JUMP, SUDDEN_E_CHANGE, NEW_COMPANION
      LOGICAL RECENT_ENCOUNTER, IN_CORE, COMPANION_CHANGED
      LOGICAL VERY_CLOSE, INSPIRALING, STRONG_GW, IMMINENT_MERGER
      LOGICAL ASYMMETRIC_MERGER, INSIDE_TIDAL_RADIUS
      LOGICAL VKICK_NORMAL, NO_MSP_HISTORY
      LOGICAL COMP_STABLE, NOT_CIRCULAR, COMPANION_EXISTS
      CHARACTER*50 FILENAME
C
C     External functions
      REAL*8 RAN2
      EXTERNAL RAN2
C
C***********************************************************************
C     STEP 1: INITIALIZATION AND VALIDATION
C***********************************************************************
      J = I
C
C     Only process neutron stars
      IF (KSTAR(J) .NE. 13) RETURN
C
C     Calculate time since NS formation
      DT = TIME - AGE0(J)
      IF (DT .LE. 0.0D0) RETURN
C
C***********************************************************************
C     STEP 2: CALCULATE AUXILIARY PARAMETERS
C***********************************************************************
C
C     2.1 Mass change and rate
      DM = BODY(J) - XMNS0(J)
      IF (STEP(J) .GT. 0.0D0) THEN
         DM_DT = (BODY(J) - BODY_PREV(J)) / STEP(J)
      ELSE
         DM_DT = 0.0D0
      END IF
C
C     2.2 Kick velocity
      VKICK_SQ = XDOT(1,J)**2 + XDOT(2,J)**2 + XDOT(3,J)**2
      VKICK = SQRT(VKICK_SQ) * VSTAR  ! Convert to km/s
C
C     2.3 Relative velocity (for environmental effects)
      VMEAN(1) = 0.0D0
      VMEAN(2) = 0.0D0
      VMEAN(3) = 0.0D0
      NEIGHBOR_COUNT = 0
C
      DO K = IFIRST, NTOT
         IF (K .EQ. J) CYCLE
         DIST = SQRT((X(1,J)-X(1,K))**2 + (X(2,J)-X(2,K))**2 + 
     &               (X(3,J)-X(3,K))**2)
         IF (DIST .LT. ATHRESH/RBAR) THEN
            VMEAN(1) = VMEAN(1) + XDOT(1,K)
            VMEAN(2) = VMEAN(2) + XDOT(2,K)
            VMEAN(3) = VMEAN(3) + XDOT(3,K)
            NEIGHBOR_COUNT = NEIGHBOR_COUNT + 1
         END IF
      END DO
C
      IF (NEIGHBOR_COUNT .GT. 0) THEN
         VMEAN(1) = VMEAN(1) / REAL(NEIGHBOR_COUNT)
         VMEAN(2) = VMEAN(2) / REAL(NEIGHBOR_COUNT)
         VMEAN(3) = VMEAN(3) / REAL(NEIGHBOR_COUNT)
      END IF
C
      VREL = SQRT((XDOT(1,J)-VMEAN(1))**2 + (XDOT(2,J)-VMEAN(2))**2 + 
     &            (XDOT(3,J)-VMEAN(3))**2) * VSTAR
C
C     2.4 Position in cluster
      DIST_CENTER = SQRT(X(1,J)**2 + X(2,J)**2 + X(3,J)**2) * RBAR
C
C     2.5 Spherical coordinates
      DIST = SQRT(X(1,J)**2 + X(2,J)**2 + X(3,J)**2)
      IF (DIST .GT. 0.0D0) THEN
         LAT = ASIN(X(3,J) / DIST)
         LON = ATAN2(X(2,J), X(1,J))
      ELSE
         LAT = 0.0D0
         LON = 0.0D0
      END IF
C
C***********************************************************************
C     STEP 3: INITIALIZE PERIOD AND B-FIELD (FIRST CALL)
C***********************************************************************
      IF (.NOT. FIRST_CALL(J)) THEN
         PERIOD(J) = 0.001D0 + 0.099D0 * RAN2(IDUM1)
         BMAG(J) = 1.0D8 * (1.0D0 + 9.0D0 * RAN2(IDUM1))
         P_INITIAL(J) = PERIOD(J)
         B_INITIAL(J) = BMAG(J)
         FIRST_CALL(J) = .TRUE.
         
C        Initialize tracking variables
         NAME0(J) = NAME(J)
         BODY_PREV(J) = BODY(J)
         IF (LIST(1,J) .GT. 0) THEN
            KSTAR_COMP0(J) = KSTAR(LIST(1,J))
            R_PREV(J) = R(J)
            E_BIND0(J) = -BODY(J)*BODY(LIST(1,J))/(2.0D0*R(J))
         ELSE
            KSTAR_COMP0(J) = 0
            R_PREV(J) = 0.0D0
            E_BIND0(J) = 0.0D0
         END IF
      END IF
C
C***********************************************************************
C     STEP 4: BINARY STATUS AND COMPANION INFO
C***********************************************************************
      IS_BINARY = (NAME(J) .GT. 0)
      IS_SINGLE = .NOT. IS_BINARY
      WAS_IN_BINARY = (NAME0(J) .GT. 0)
      WAS_SINGLE = .NOT. WAS_IN_BINARY
      COMPANION_CHANGED = (NAME(J) .NE. NAME0(J))
C
      I_COMP = 0
      KW_COMP = 0
      COMP_IS_NS = .FALSE.
      COMP_IS_BH = .FALSE.
      COMP_IS_WD = .FALSE.
      ORBITAL_SEP = 0.0D0
      ECC = 0.0D0
      E_BIND_NOW = 0.0D0
C
      IF (IS_BINARY .AND. LIST(1,J) .GT. 0) THEN
         I_COMP = LIST(1,J)
         IF (I_COMP .GT. 0 .AND. I_COMP .LE. NTOT) THEN
            KW_COMP = KSTAR(I_COMP)
            COMP_IS_NS = (KW_COMP .EQ. 13)
            COMP_IS_BH = (KW_COMP .EQ. 14)
            COMP_IS_WD = (KW_COMP .EQ. 10 .OR. KW_COMP .EQ. 11 .OR.
     &                    KW_COMP .EQ. 12)
            
C           Get orbital parameters
            ORBITAL_SEP = R(J) * RBAR
            
C           Estimate eccentricity from H array (if available)
C           H contains: semi-major axis, eccentricity, etc.
            IF (ORBITAL_SEP .GT. 0.0D0) THEN
C              Simple estimate: use velocity components
               ECC = 0.0D0  ! Placeholder - implement if H array accessible
            END IF
            
C           Current binding energy
            IF (ORBITAL_SEP .GT. 0.0D0) THEN
               E_BIND_NOW = -BODY(J)*BODY(I_COMP)*ZMBAR**2 / 
     &                      (2.0D0 * ORBITAL_SEP)
            END IF
         END IF
      END IF
C
C***********************************************************************
C     STEP 5: SCENARIO DETECTION (PHYSICS-BASED)
C***********************************************************************
      SCENARIO_ID = 1  ! Default: Isolated Pulsar
C
C     --- SCENARIO 6: NS-NS MERGER ---
      IF (IS_BINARY .AND. COMP_IS_NS) THEN
C        Check merger conditions
         VERY_CLOSE = (ORBITAL_SEP .LT. 100.0D0 * YI .AND. 
     &                 ORBITAL_SEP .GT. 0.0D0)
         
         SUDDEN_MASS_JUMP = (ABS(DM) .GT. 0.3D0 * XMNS0(J))
         
         IF (R_PREV(J) .GT. 0.0D0 .AND. STEP(J) .GT. 0.0D0) THEN
            DR_DT = (R(J) - R_PREV(J)) * RBAR / (STEP(J) * TSTAR)
            INSPIRALING = (DR_DT .LT. -1.0D5)  ! cm/s
         ELSE
            DR_DT = 0.0D0
            INSPIRALING = .FALSE.
         END IF
         
         IF (ORBITAL_SEP .GT. 0.0D0 .AND. I_COMP .GT. 0) THEN
            M1 = BODY(J) * ZMBAR
            M2 = BODY(I_COMP) * ZMBAR
            GW_POWER = 32.0D0/5.0D0 * (XG**4) * (M1*M2)**2 * 
     &                 (M1+M2) / (CLIGHT**5 * ORBITAL_SEP**5)
            STRONG_GW = (GW_POWER .GT. 1.0D30)
         ELSE
            STRONG_GW = .FALSE.
         END IF
         
         IMMINENT_MERGER = VERY_CLOSE .AND. INSPIRALING .AND. STRONG_GW
         
         IF (IMMINENT_MERGER .OR. SUDDEN_MASS_JUMP) THEN
            SCENARIO_ID = 6
            MERGERFLAG(J) = 1
            MERGER_TIME(J) = TIME
         END IF
      END IF
C
C     --- SCENARIO 7: NS-BH MERGER ---
      IF (IS_BINARY .AND. COMP_IS_BH .AND. SCENARIO_ID .EQ. 1) THEN
         VERY_CLOSE = (ORBITAL_SEP .LT. 500.0D0 * YI .AND.
     &                 ORBITAL_SEP .GT. 0.0D0)
         
         SUDDEN_MASS_JUMP = (ABS(DM) .GT. 0.5D0 * XMNS0(J))
         
         IF (I_COMP .GT. 0) THEN
            TIDAL_RADIUS = 2.0D0 * BODY(I_COMP) * ZMBAR * XG / 
     &                     CLIGHT**2
            INSIDE_TIDAL_RADIUS = (ORBITAL_SEP .LT. TIDAL_RADIUS)
         ELSE
            INSIDE_TIDAL_RADIUS = .FALSE.
         END IF
         
         ASYMMETRIC_MERGER = (VKICK .GT. 500.0D0)
         
         IF ((VERY_CLOSE .AND. INSIDE_TIDAL_RADIUS) .OR.
     &       SUDDEN_MASS_JUMP .OR. ASYMMETRIC_MERGER) THEN
            SCENARIO_ID = 7
            MERGERFLAG(J) = 1
            MERGER_TIME(J) = TIME
         END IF
      END IF
C
C     --- SCENARIO 3: MSP VIA ACCRETION ---
      IF (IS_BINARY .AND. SCENARIO_ID .EQ. 1) THEN
         IS_ACCRETING = (DM_DT .GT. 1.0D-10)
         
         IS_DONOR_EVOLVING = .FALSE.
         IF (I_COMP .GT. 0) THEN
            IS_DONOR_EVOLVING = (KW_COMP .GE. 2 .AND. KW_COMP .LE. 9)
         END IF
         
         IS_RLOF = .FALSE.
         IF (I_COMP .GT. 0 .AND. ORBITAL_SEP .GT. 0.0D0) THEN
            Q_RATIO = BODY(I_COMP) / BODY(J)
            R_LOBE = ORBITAL_SEP * (0.49D0 * Q_RATIO**(2.0D0/3.0D0)) / 
     &               (0.6D0 * Q_RATIO**(2.0D0/3.0D0) + 
     &                LOG(1.0D0 + Q_RATIO**(1.0D0/3.0D0)))
            
            IF (KW_COMP .GE. 2 .AND. KW_COMP .LE. 9) THEN
               R_COMP = 1.0D11
            ELSE
               R_COMP = 0.0D0
            END IF
            
            IS_RLOF = (R_COMP .GT. R_LOBE)
         END IF
         
         IS_CLOSE_ENOUGH = (ORBITAL_SEP .LT. 1.0D11 .AND. 
     &                      ORBITAL_SEP .GT. 0.0D0)
         
         IF (IS_ACCRETING .AND. IS_DONOR_EVOLVING .AND. 
     &       IS_CLOSE_ENOUGH .AND. IS_RLOF) THEN
            SCENARIO_ID = 3
            MSPHISTORY(J) = .TRUE.
            ACCRETION_TIME(J) = TIME
         END IF
      END IF
C
C     --- SCENARIO 4: MSP VIA DYNAMICS ---
      IF (IS_BINARY .AND. SCENARIO_ID .EQ. 1) THEN
         NEW_COMPANION = .FALSE.
         IF (I_COMP .GT. 0) THEN
            NEW_COMPANION = (KW_COMP .NE. KSTAR_COMP0(J))
         END IF
         
         SUDDEN_E_CHANGE = .FALSE.
         IF (E_BIND0(J) .NE. 0.0D0 .AND. E_BIND_NOW .NE. 0.0D0) THEN
            E_CHANGE = ABS(E_BIND_NOW - E_BIND0(J))
            SUDDEN_E_CHANGE = (E_CHANGE .GT. 0.5D0 * ABS(E_BIND_NOW))
         END IF
         
C        Check if recent encounter (within 10 irregular steps)
         RECENT_ENCOUNTER = (NSTEPI(J) .LT. 10 .AND. NSTEPI(J) .GT. 0)
         
C        Check if in core
         IN_CORE = (DIST_CENTER .LT. RC * RBAR)
         
         IF ((WAS_SINGLE .OR. COMPANION_CHANGED) .AND. 
     &       (SUDDEN_E_CHANGE .OR. NEW_COMPANION) .AND. 
     &       RECENT_ENCOUNTER .AND. IN_CORE) THEN
            SCENARIO_ID = 4
            DYNFLAG(J) = 1
            EXCHANGE_TIME(J) = TIME
         END IF
      END IF
C
C     --- SCENARIO 5: WIDE BINARY ---
      IF (IS_BINARY .AND. SCENARIO_ID .EQ. 1) THEN
         IS_WIDE = (ORBITAL_SEP .GT. ATHRESH)
         
         NO_ACCRETION = (DM_DT .LE. 0.0D0)
         
         COMP_STABLE = (KW_COMP .LE. 1 .OR. COMP_IS_WD .OR. 
     &                  COMP_IS_NS .OR. COMP_IS_BH)
         
         IF (IS_WIDE .AND. NO_ACCRETION .AND. COMP_STABLE) THEN
            SCENARIO_ID = 5
            WIDE_BINARY_FLAG(J) = 1
         END IF
      END IF
C
C     --- SCENARIO 2: ISOLATED MSP ---
      IF (IS_SINGLE .AND. SCENARIO_ID .EQ. 1) THEN
         IF (WAS_IN_BINARY .AND. MSPHISTORY(J) .AND. 
     &       (BODY(J) .GT. XMNS0(J) + 0.01D0)) THEN
            SCENARIO_ID = 2
         END IF
      END IF
C
C     --- SCENARIO 1: ISOLATED PULSAR (default) ---
      IF (SCENARIO_ID .EQ. 1) THEN
         VKICK_NORMAL = (VKICK .LT. 100.0D0)
         NO_MSP_HISTORY = (.NOT. MSPHISTORY(J))
         IF (IS_SINGLE .AND. VKICK_NORMAL .AND. NO_MSP_HISTORY) THEN
            SCENARIO_ID = 1
         END IF
      END IF
C
C***********************************************************************
C     STEP 6: CALCULATE PDOT COMPONENTS
C***********************************************************************
C
C     6.1 Magnetic dipole radiation (always)
      ALPHA = PI / 2.0D0
      PDOT_DIPOLE = XKFIELD * (BMAG(J)**2) * (YI**6) * 
     &              (SIN(ALPHA)**2) / (XK * CLIGHT**3 * PERIOD(J))
C
C     6.2 Magnetic field decay - REMOVED
C     REASON: B-field decay already handled by exponential decay B(t)=B0*exp(-t/tau)
C     Including PDOT_DECAY would double-count this physics
C     Reference: Bhattacharya & van den Heuvel (1991), Phys. Rep., 203, 1
      PDOT_DECAY = 0.0D0
C
C     6.3 Environmental spin-down (always)
      PDOT_ENV = XKENV * RHO(J) * VREL * PERIOD(J)**2
C
C     6.4 Accretion (scenario 3)
      PDOT_ACC = 0.0D0
      IF (SCENARIO_ID .EQ. 3) THEN
         PDOT_ACC = -XKACC * DM_DT * PERIOD(J)
      END IF
C
C     6.5 Merger effects (scenarios 6, 7)
      PDOT_MERGE = 0.0D0
      IF (SCENARIO_ID .EQ. 6 .OR. SCENARIO_ID .EQ. 7) THEN
         IF (ABS(DM) .GT. 0.0D0) THEN
            PDOT_MERGE = -XKMERGE * PERIOD(J) * (DM / XMNS0(J))
         END IF
      END IF
C
C     6.6 Asymmetric kick effects
      PDOT_ASYM = 0.0D0
      IF (VKICK .GT. 200.0D0) THEN
         PDOT_ASYM = XKASYM * VKICK * PERIOD(J)
      END IF
C
C     6.7 Dynamic interactions (scenario 4)
      PDOT_DYN = 0.0D0
      IF (SCENARIO_ID .EQ. 4) THEN
         PDOT_DYN = -XKDYN * PERIOD(J)
      END IF
C
C     6.8 Gravitational waves (close binaries)
      PDOT_GW = 0.0D0
      IF (IS_BINARY .AND. ORBITAL_SEP .GT. 0.0D0 .AND. 
     &    ORBITAL_SEP .LT. 1.0D11) THEN
         IF (I_COMP .GT. 0) THEN
            PDOT_GW = XKGW * (BODY(J)*ZMBAR)**2 / (ORBITAL_SEP**4)
         END IF
      END IF
C
C     6.9 Total Pdot (PDOT_DECAY removed - see comment above)
      PDOT(J) = PDOT_DIPOLE + PDOT_ENV + PDOT_ACC + 
     &          PDOT_MERGE + PDOT_ASYM + PDOT_DYN + PDOT_GW
C
C***********************************************************************
C     STEP 7: UPDATE PERIOD AND B-FIELD
C***********************************************************************
C
C     Update period
      IF (STEP(J) .GT. 0.0D0) THEN
         PERIOD(J) = PERIOD(J) + PDOT(J) * STEP(J) * TSTAR
      END IF
      IF (PERIOD(J) .LT. 1.0D-4) PERIOD(J) = 1.0D-4
C
C     Update B-field
      IF (STEP(J) .GT. 0.0D0) THEN
         BMAG(J) = BMAG(J) * EXP(-STEP(J) * TSTAR / XKTAU)
      END IF
C
C     Recalibrate B-field if Pdot > 0
      IF (PDOT(J) .GT. 0.0D0 .AND. PERIOD(J) .GT. 0.0D0) THEN
         BMAG(J) = SQRT(XK * PERIOD(J) * PDOT(J) * CLIGHT**3 / 
     &                  (XKFIELD * YI**6))
      END IF
C
C***********************************************************************
C     STEP 8: CALCULATE ENERGIES
C***********************************************************************
      E_KINETIC = 0.5D0 * BODY(J) * ZMBAR * VKICK_SQ * VSTAR**2
      E_POTENTIAL = BODY(J) * ZMBAR * PHI(J)
      E_TOTAL = E_KINETIC + E_POTENTIAL
      
      E_BINDING = 0.0D0
      IF (IS_BINARY .AND. I_COMP .GT. 0 .AND. ORBITAL_SEP .GT. 0.0D0) 
     &   THEN
         E_BINDING = E_BIND_NOW
      END IF
C
C***********************************************************************
C     STEP 9: SAVE OUTPUT
C***********************************************************************
C     CORRECTED: Output filename changed from ns.33_* to pulsar.55_*
      WRITE(FILENAME, '(A,I8.8,A)') 'pulsar.55_', INT(TIME*TSTAR), 
     &                              '.dat'
      OPEN(UNIT=55, FILE=FILENAME, STATUS='UNKNOWN', ACCESS='APPEND')
C
C     Format: TIME NAME KSTAR PERIOD PDOT BMAG X Y Z VX VY VZ 
C             FX FY FZ E_TOT E_BIND SCENARIO LAT LON
      WRITE(55, 100) TIME*TSTAR, NAME(J), KSTAR(J), 
     &               PERIOD(J), PDOT(J), BMAG(J),
     &               X(1,J)*RBAR, X(2,J)*RBAR, X(3,J)*RBAR,
     &               XDOT(1,J)*VSTAR, XDOT(2,J)*VSTAR, 
     &               XDOT(3,J)*VSTAR,
     &               F(1,J), F(2,J), F(3,J),
     &               E_TOTAL, E_BINDING, SCENARIO_ID,
     &               LAT*180.0D0/PI, LON*180.0D0/PI
C
 100  FORMAT(E15.7,1X,I8,1X,I2,1X,15(E15.7,1X),I2,1X,2(F10.4,1X))
C
      CLOSE(55)
C
C***********************************************************************
C     STEP 10: UPDATE TRACKING VARIABLES FOR NEXT CALL
C***********************************************************************
      NAME0(J) = NAME(J)
      BODY_PREV(J) = BODY(J)
      
      IF (I_COMP .GT. 0) THEN
         KSTAR_COMP0(J) = KSTAR(I_COMP)
         R_PREV(J) = R(J)
         E_BIND0(J) = E_BIND_NOW
      ELSE
         KSTAR_COMP0(J) = 0
         R_PREV(J) = 0.0D0
         E_BIND0(J) = 0.0D0
      END IF
C
      RETURN
      END
C***********************************************************************
C     END OF calc_and_save_pulsar_params (VERSION 4.0)
C***********************************************************************
C
C     IMPLEMENTATION NOTES:
C     
C     1. This subroutine requires the following in common6.h:
C        - Standard NBODY6++GPU variables (NAME, KSTAR, BODY, X, XDOT, 
C          LIST, R, H, STEP, TIME, PHI, F, NSTEPI, RC, etc.)
C        - Pulsar-specific arrays (see implementation guide)
C        - Physical constants from Namelist /INPULSAR/
C     
C     2. Scenarios are detected using internal NBODY6++GPU physics:
C        - No external flags needed from merge.f, sn_evolve.f, etc.
C        - All detection logic is self-contained
C        - Flags (MERGERFLAG, DYNFLAG, etc.) are SET by this routine
C     
C     3. Output format optimized for P-Pdot, P-B, P-E plots:
C        - Column 4: PERIOD
C        - Column 5: PDOT
C        - Column 6: BMAG
C        - Column 16: E_TOTAL
C        - Column 18: SCENARIO_ID
C     
C     4. Call from mdot.F:
C        IF (KSTAR(I) .EQ. 13) THEN
C           IF (XMNS0(I) .EQ. 0.0D0) THEN
C              XMNS0(I) = BODY(I)
C              AGE0(I) = TIME
C           END IF
C           CALL calc_and_save_pulsar_params(I)
C        END IF
C
C     5. File initialization in file_init.F:
C        IF (KZ(XX) .GT. 0) THEN  ! Replace XX with appropriate index
C           OPEN(UNIT=55, FILE='pulsar.55', STATUS='UNKNOWN')
C           CLOSE(55)
C        END IF
C
C***********************************************************************
