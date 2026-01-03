# Integration Guide - Pulsar Subroutine into NBODY6++GPU

Step-by-step guide for integrating the pulsar evolution subroutine into NBODY6++GPU.

**Author:** Maria Rah  
**Version:** 4.0 (CORRECTED)  
**Date:** January 2026

---

## Overview

This guide shows how to integrate `calc_and_save_pulsar_params_v4_CORRECTED.f` into NBODY6++GPU to enable pulsar evolution tracking during N-body simulations.

**Time required:** 30-60 minutes  
**Difficulty:** Intermediate  
**Prerequisites:** NBODY6++GPU installed and working

---

## Quick Integration Checklist

- [ ] Copy subroutine file to Ncode/
- [ ] Modify common6.h (add arrays)
- [ ] Modify input.F (initialize arrays)
- [ ] Modify mdot.F (call subroutine)
- [ ] Modify mydump.F (call subroutine)
- [ ] Modify file_init.F (add file unit)
- [ ] Recompile
- [ ] Test with simple case
- [ ] Verify output

---

## Step 1: Copy Subroutine File

```bash
# Navigate to NBODY6++GPU directory
cd /path/to/NBODY6++GPU/

# Copy subroutine to Ncode/ directory
cp /path/to/calc_and_save_pulsar_params_v4_CORRECTED.f Ncode/

# Verify
ls -lh Ncode/calc_and_save_pulsar_params_v4_CORRECTED.f
```

**Expected output:** File should be ~20 KB

---

## Step 2: Modify common6.h

**File:** `Ncode/common6.h`

**Add these arrays to COMMON block:**

```fortran
C     *** Pulsar evolution tracking (Maria Rah) ***
      REAL*8 BMAG(NMAX),PERIOD(NMAX),XMNS0(NMAX)
      REAL*8 ACCRETION_TIME(NMAX),EXCHANGE_TIME(NMAX)
      REAL*8 E_BIND0(NMAX)
      INTEGER KSTAR_COMP0(NMAX),DYNFLAG(NMAX),WIDE_BINARY_FLAG(NMAX)
      LOGICAL MSPHISTORY(NMAX)
      
      COMMON/PULSAR/ BMAG,PERIOD,XMNS0,ACCRETION_TIME,EXCHANGE_TIME,
     &               E_BIND0,KSTAR_COMP0,DYNFLAG,WIDE_BINARY_FLAG,
     &               MSPHISTORY
```

**Location:** Add after other COMMON blocks, before the END of file

**What this does:** Declares arrays to store pulsar properties for all stars

---

## Step 3: Modify input.F

**File:** `Ncode/input.F`

**Initialize pulsar arrays at startup:**

```fortran
C     *** Initialize pulsar arrays (Maria Rah) ***
      DO 1000 I = 1, NMAX
         BMAG(I) = 0.0D0
         PERIOD(I) = 0.0D0
         XMNS0(I) = 0.0D0
         ACCRETION_TIME(I) = 0.0D0
         EXCHANGE_TIME(I) = 0.0D0
         E_BIND0(I) = 0.0D0
         KSTAR_COMP0(I) = 0
         DYNFLAG(I) = 0
         WIDE_BINARY_FLAG(I) = 0
         MSPHISTORY(I) = .FALSE.
 1000 CONTINUE
```

**Location:** Add in the initialization section, after other array initializations

**What this does:** Sets all pulsar arrays to zero/false at simulation start

---

## Step 4: Modify mdot.F

**File:** `Ncode/mdot.F`

**Call subroutine when computing mass transfer:**

```fortran
C     *** Track pulsar evolution during mass transfer (Maria Rah) ***
      IF (KSTAR(I1) .EQ. 13 .OR. KSTAR(I2) .EQ. 13) THEN
         CALL calc_and_save_pulsar_params(TIME)
      END IF
```

**Location:** Add after mass transfer rate (DM/DT) calculation

**What this does:** Updates pulsar properties during active accretion

---

## Step 5: Modify mydump.F

**File:** `Ncode/mydump.F`

**Call subroutine during output dumps:**

```fortran
C     *** Save pulsar state at dump time (Maria Rah) ***
      CALL calc_and_save_pulsar_params(TIME)
```

**Location:** Add in the output section, after main dump write

**What this does:** Saves pulsar data at regular intervals

---

## Step 6: Modify file_init.F

**File:** `Ncode/file_init.F`

**Add file unit for pulsar output:**

```fortran
C     *** Pulsar output file unit (Maria Rah) ***
      INTEGER, PARAMETER :: UNIT_PULSAR = 55
```

**Location:** Add with other file unit declarations

**What this does:** Reserves unit 55 for pulsar.55_*.dat files

---

## Step 7: Recompile

```bash
# Clean previous build
make clean

# Rebuild with new subroutine
make

# Check for errors
echo $?  # Should be 0 if successful
```

**Common compilation errors:**

**Error 1:** `Undefined reference to BMAG`
- **Fix:** Check common6.h was modified correctly

**Error 2:** `Undefined reference to calc_and_save_pulsar_params`
- **Fix:** Verify subroutine file is in Ncode/

**Error 3:** `Syntax error in common6.h`
- **Fix:** Check COMMON block syntax (no typos)

---

## Step 8: Test Integration

### Simple Test Case

**Create test input:**

```bash
# File: test_pulsar.inp
1000              # N (small for testing)
3                 # KSTAR (some will become NS)
1.0               # RBAR
0.5               # ZMBAR
0.1               # BODY1
100.0             # BODYN
0.001             # Q
0.5               # VXROT
0.5               # VZROT
0.001             # RTIDE
1.0               # TIDAL(1)
0                 # TIDAL(2)
1                 # TIDAL(3)
0.02              # METALLICITY
10.0              # DTADJ
10.0              # DELTAT
1.0               # TCRIT
5.0               # ETAI/ETAR
1.0               # FS
0                 # KZ(1-50)
...
```

**Run test:**

```bash
./nbody6++.gpu < test_pulsar.inp > test_output.log &

# Monitor
tail -f test_output.log

# Wait for completion (should be fast for N=1000)
```

**Check for pulsar files:**

```bash
ls -lh pulsar.55_*.dat

# Should see files like:
# pulsar.55_00000000.dat
# pulsar.55_00001000.dat
# etc.
```

---

## Step 9: Verify Output

### Check File Format

```bash
# Look at first pulsar file
head -20 pulsar.55_00000000.dat
```

**Expected format:**
```
# Columns: TIME NAME KSTAR PERIOD PDOT BMAG X Y Z VX VY VZ FX FY FZ ...
  0.0000  1234  13  0.100  1.23e-15  1.00e+12  0.1  0.2  0.3 ...
```

### Verify Physics

**Load in Python:**

```python
import numpy as np

data = np.loadtxt('pulsar.55_00001000.dat')  # t=1 Myr
period = data[:, 3]
bmag = data[:, 5]

print(f"Periods: {period.min():.3f} - {period.max():.3f} s")
print(f"B-fields: {bmag.min():.2e} - {bmag.max():.2e} G")
```

**Sanity checks:**
- Periods: 0.001 - 10 s ✓
- B-fields: 10⁸ - 10¹³ G ✓
- No NaN or Inf values ✓
- Scenarios: 1-7 only ✓

---

## Step 10: Production Run

### Recommended Settings

**For globular cluster:**

```fortran
N = 100000           ! Realistic size
METALLICITY = 0.001  ! Low Z
BINARY_FRACTION = 0.3
TIDAL_FIELD = realistic_value
```

**Output frequency:**

In `mydump.F`, control how often to save:

```fortran
! Every 100 steps
IF (MOD(NSTEP, 100) .EQ. 0) THEN
   CALL calc_and_save_pulsar_params(TIME)
END IF
```

**Adjust based on:**
- Simulation length (longer → less frequent)
- Number of pulsars (more → less frequent)
- Available storage (limited → less frequent)

---

## Advanced Integration

### Custom Modifications

**1. Add new scenario:**

Edit subroutine, add after line 370:

```fortran
C     --- SCENARIO 8: Your custom scenario ---
IF (YOUR_CONDITION) THEN
   SCENARIO_ID = 8
END IF
```

**2. Change output format:**

Edit subroutine, modify WRITE statement (line ~500):

```fortran
WRITE(55,100) TIME, NAME, KSTAR, PERIOD, PDOT, BMAG, ...
100 FORMAT(F12.4, I8, I4, 17(1X,E12.5))  ! Add columns here
```

**3. Add new physics:**

In subroutine, add new Pdot component:

```fortran
! Your new torque
PDOT_YOUR_EFFECT = COEFFICIENT * YOUR_FORMULA

! Add to total
PDOT_TOTAL = PDOT_DIPOLE + PDOT_ACC + ... + PDOT_YOUR_EFFECT
```

### Performance Tuning

**For very large N (>1M):**

```fortran
! In mydump.F - only save NS
IF (KSTAR(I) .EQ. 13) THEN
   CALL calc_and_save_pulsar_params_single(I, TIME)
END IF
```

**Parallel output:**

Use MPI ranks to write separate files (advanced)

---

## Troubleshooting Integration

### Problem: Compilation fails

**Check:**
1. All files modified correctly?
2. Syntax errors in FORTRAN?
3. Subroutine file present?

**Debug:**
```bash
# Compile with verbose
make VERBOSE=1

# Check specific file
gfortran -c Ncode/calc_and_save_pulsar_params_v4_CORRECTED.f
```

### Problem: No output files

**Check:**
1. Are there NS in simulation? (KSTAR=13)
2. Is subroutine being called?
3. File permissions?

**Debug:**
```bash
# Add print statement in subroutine
PRINT *, 'Pulsar subroutine called at TIME=', TIME

# Check log
grep "Pulsar subroutine" output.log
```

### Problem: Segmentation fault

**Causes:**
- Array bounds exceeded
- Uninitialized arrays

**Fix:**
```bash
# Compile with bounds checking
make clean
make FFLAGS="-fbounds-check -g"

# Run with debugger
gdb ./nbody6++.gpu
```

### Problem: Wrong results

**Check:**
1. Using v4.0 CORRECTED version?
2. Coefficients correct?
3. Common block aligned?

**Verify coefficients in code:**
```fortran
XKACC = 2.0D-16    ! NOT 2.0D-5 (old version)
XKENV = 5.0D-29    ! NOT 5.0D-21
```

---

## Validation After Integration

### Run Test Scenarios

```bash
# Test all 7 scenarios
cd standalone_tests/fortran_codes/

for i in {1..7}; do
   ./test_scenario_${i}
done

# Compare with expected results
diff scenario1_output.dat ../colab_results/scenario1_isolated_output.dat
```

### Physics Checks

**1. B-field decay:**
```python
# At t=100 Myr, should have B/B0 ≈ 0.8187
tau = 500  # Myr
np.exp(-100/tau)  # = 0.8187
```

**2. Spin-down age:**
```python
# For MSPs, should be ~1 Gyr
tau_sd = P / (2 * abs(Pdot))
```

**3. Accretion spin-up:**
```python
# Scenario 3 should have Pdot < 0 (spin-up)
```

---

## Files Modified Summary

| File | Location | Purpose |
|------|----------|---------|
| common6.h | Ncode/ | Declare arrays |
| input.F | Ncode/ | Initialize arrays |
| mdot.F | Ncode/ | Call during accretion |
| mydump.F | Ncode/ | Call at dumps |
| file_init.F | Ncode/ | File unit |
| calc_and_save_pulsar_params_v4_CORRECTED.f | Ncode/ | Main subroutine |

---

## Next Steps

After successful integration:

1. ✅ Run production simulations
2. ✅ Analyze pulsar populations
3. ✅ Compare with observations
4. ✅ Publish results!

**See MANUAL.md for usage instructions**

**See PHYSICS_EXPLANATION.md for equation details**

---

## Support

**Integration problems?**
- Check this guide step-by-step
- Review standalone tests
- Contact: Maria Rah, Byurakan Astrophysical Observatory

---

**Integration complete!** ✅
