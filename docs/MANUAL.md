# User Manual - Pulsar Evolution Subroutine

Complete guide for using the pulsar evolution subroutine in NBODY6++GPU.

**Author:** Maria Rah  
**Version:** 4.0 (CORRECTED)  
**Date:** January 2026

---

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Configuration](#configuration)
4. [Running Simulations](#running-simulations)
5. [Output Files](#output-files)
6. [Interpreting Results](#interpreting-results)
7. [Troubleshooting](#troubleshooting)
8. [Examples](#examples)

---

## Introduction

### What is this subroutine?

This subroutine tracks pulsar evolution in NBODY6++GPU simulations by computing:
- Magnetic field decay: B(t) = B₀ exp(-t/τ)
- Spin period evolution from multiple torques
- Scenario identification (7 evolutionary pathways)
- Position, velocity, and energy tracking

### Who should use this?

Researchers simulating:
- Globular cluster dynamics
- Pulsar populations
- Millisecond pulsar formation
- Binary evolution with neutron stars

### What's new in v4.0?

✅ **Fixed critical bugs:**
- Removed PDOT_DECAY (was double counting B-field decay)
- Corrected 5 coefficients (errors ranged from 10⁸× to 10²⁰×!)

✅ **Improvements:**
- 7 scenario identification algorithm
- Interactive HTML dashboard
- Complete validation suite
- Comprehensive documentation

---

## Installation

### Prerequisites

**Required:**
- NBODY6++GPU (installed and working)
- gfortran compiler
- CUDA toolkit (for GPU acceleration)

**Optional:**
- Python 3.x (for analysis)
- numpy, matplotlib (for plotting)

### Quick Install

```bash
# 1. Navigate to NBODY6++GPU directory
cd /path/to/NBODY6++GPU/

# 2. Copy subroutine
cp /path/to/calc_and_save_pulsar_params_v4_CORRECTED.f Ncode/

# 3. Integrate (see INTEGRATION_GUIDE.md for details)

# 4. Recompile
make clean
make
```

### Verification

Test with provided scenarios:

```bash
# Run test simulation
cd standalone_tests/fortran_codes/
gfortran -O2 scenario1_young_pulsar.f90 -o test
./test

# Check output
ls -lh scenario1_isolated_output.dat
```

---

## Configuration

### Input File Setup

Add to your NBODY6++GPU input file (`.inp`):

```
# Enable pulsar tracking
KSTAR = 13    # Neutron star stellar type
```

### Example Input Files

**Without subroutine (baseline):**
```
# Paper5_RunA_WITHOUT_subroutine.inp
# Standard NBODY6++GPU run
```

**With subroutine:**
```
# Paper5_RunB_WITH_subroutine_CORRECTED.inp  
# Includes pulsar evolution tracking
```

See `../subroutine/` for complete examples.

---

## Running Simulations

### Basic Run

```bash
# Start simulation
./nbody6++.gpu < input.inp > output.log &

# Monitor progress
tail -f output.log
```

### Pulsar Output

The subroutine creates files:

```
pulsar.55_00000000.dat    # t = 0 Myr
pulsar.55_00001000.dat    # t = 1 Myr
pulsar.55_00010000.dat    # t = 10 Myr
...
```

**Naming:** `pulsar.55_TTTTTTTT.dat` where T = time in years (8 digits)

---

## Output Files

### Pulsar Data Format

Each `pulsar.55_*.dat` file contains:

```
# Columns (20 total):
1.  TIME      - Simulation time (N-body units)
2.  NAME      - Star ID
3.  KSTAR     - Stellar type (13 = NS)
4.  PERIOD    - Spin period (seconds)
5.  PDOT      - Period derivative (s/s)
6.  BMAG      - Magnetic field (Gauss)
7-9.  X,Y,Z   - Position (parsecs)
10-12. VX,VY,VZ - Velocity (km/s)
13-15. FX,FY,FZ - Force components
16-18. ENERGY terms
19. SCENARIO  - Evolutionary scenario (1-7)
20-21. LAT,LON - Galactic coordinates
```

### Scenario Codes

| Code | Scenario | Description |
|------|----------|-------------|
| 1 | Isolated Pulsar | Single NS, normal spin-down |
| 2 | Isolated MSP | Recycled, now single |
| 3 | Accreting MSP | Active accretion, spin-up |
| 4 | Dynamic MSP | Exchange/kicks in cluster |
| 5 | Wide Binary | Binary MSP, no accretion |
| 6 | NS-NS Merger | Double NS system |
| 7 | NS-BH Merger | NS-BH system |

---

## Interpreting Results

### Loading Data (Python)

```python
import numpy as np

# Load pulsar data
data = np.loadtxt('pulsar.55_00100000.dat')  # t=100 Myr

time = data[:, 0]
name = data[:, 1]
period = data[:, 3]     # seconds
pdot = data[:, 4]       # s/s
bmag = data[:, 5]       # Gauss
scenario = data[:, 18]

# Filter by scenario
young_pulsars = data[scenario == 1]
msps = data[scenario == 2]
```

### Key Diagnostics

**1. B-field decay check:**
```python
# Expected: B(t) = B0 * exp(-t/tau)
tau = 500e6  # 500 Myr in years
t = 100e6    # 100 Myr
B0 = 1e12

B_expected = B0 * np.exp(-t/tau)
print(f"Expected B/B0 = {B_expected/B0:.4f}")  # Should be 0.8187
```

**2. Spin-down age:**
```python
# Characteristic age
tau_sd = period / (2 * np.abs(pdot))
tau_sd_Gyr = tau_sd / (365.25 * 24 * 3600 * 1e9)
print(f"Spin-down age: {tau_sd_Gyr:.2f} Gyr")
```

**3. P-Pdot diagram:**
```python
import matplotlib.pyplot as plt

plt.scatter(period, np.abs(pdot), c=scenario, cmap='tab10')
plt.xscale('log')
plt.yscale('log')
plt.xlabel('Period (s)')
plt.ylabel('|Pdot| (s/s)')
plt.colorbar(label='Scenario')
plt.show()
```

---

## Troubleshooting

### Problem: No pulsar files created

**Causes:**
- No neutron stars in simulation (KSTAR ≠ 13)
- Subroutine not integrated correctly
- Output directory not writable

**Solution:**
```bash
# Check for NS
grep "KSTAR.*13" output.log

# Check integration
grep "calc_and_save_pulsar_params" Ncode/*.F

# Check permissions
ls -ld . 
```

### Problem: Unphysical results

**Symptoms:**
- Negative periods
- B-field increasing
- Extreme Pdot values

**Causes:**
- Using OLD subroutine (v3.0 with bugs)
- Wrong coefficients

**Solution:**
- Verify you have v4.0 CORRECTED version
- Check coefficients in code:
  ```fortran
  XKACC = 2.0D-16    ! NOT 2.0D-5
  XKENV = 5.0D-29    ! NOT 5.0D-21
  ```

### Problem: Compilation errors

**Error:** `Undefined reference to calc_and_save_pulsar_params`

**Solution:**
```bash
# Ensure subroutine is in Ncode/
ls Ncode/calc_and_save_pulsar_params_v4_CORRECTED.f

# Clean rebuild
make clean
make
```

### Problem: Scenario always = 1

**Cause:** Binary parameters not set correctly

**Solution:**
- Check that binaries exist in simulation
- Verify accretion rates (DM_DT) are computed
- Review scenario logic in subroutine

---

## Examples

### Example 1: Young Pulsar Population

**Setup:**
```
# input.inp
N = 100000           # 100k stars
KSTAR = 13          # Some NS
METALLICITY = 0.001 # Low Z
```

**Expected output:**
- Mostly Scenario 1 (isolated pulsars)
- B-fields: 10¹¹ - 10¹³ G
- Periods: 0.1 - 10 s
- Gradual spin-down over Gyr

### Example 2: MSP Formation

**Setup:**
```
# input.inp (dense core)
N = 500000
RBAR = 0.5 pc       # Dense
Binary fraction = 0.3
```

**Expected output:**
- Scenarios 2-5 (MSPs)
- B-fields: 10⁸ - 10⁹ G
- Periods: 1-10 ms
- Some accretion (Scenario 3)

### Example 3: Binary Evolution

**Setup:**
- Start with NS-MS binaries
- Allow accretion (RLOF)

**Track:**
- Transition: Scenario 1 → 3 (accretion starts)
- Period decrease (spin-up)
- Eventually: Scenario 2 or 5 (binary disrupted or wide)

---

## Advanced Usage

### Custom Scenarios

Modify scenario logic in subroutine (lines 200-370):

```fortran
! Add Scenario 8: Your custom condition
IF (YOUR_CONDITION) THEN
   SCENARIO_ID = 8
END IF
```

### Coefficient Tuning

**WARNING:** Only modify if you know what you're doing!

Current corrected values:
```fortran
XKFIELD = 3.2D19     ! Dipole
XKACC   = 2.0D-16    ! Accretion
XKENV   = 5.0D-29    ! Environment
XKDYN   = 1.0D-22    ! Dynamical
XKASYM  = 1.0D-27    ! Asymmetric
```

### Output Frequency

Control in `mydump.F`:
```fortran
! Write every N steps
IF (MOD(NSTEPI, 1000) .EQ. 0) THEN
   CALL calc_and_save_pulsar_params(...)
END IF
```

---

## Performance Tips

### Memory

- Each pulsar file ≈ 100 KB per 1000 pulsars
- For 10,000 pulsars over 10 Gyr (1000 snapshots): ≈ 1 GB

### Speed

- Subroutine adds < 1% overhead
- GPU acceleration not affected
- Bottleneck: File I/O (use SSD)

### Large Simulations

For N > 1M stars:
- Reduce output frequency
- Use binary format (modify subroutine)
- Post-process to select interesting objects

---

## See Also

- **INTEGRATION_GUIDE.md** - How to integrate into NBODY6++GPU
- **PHYSICS_EXPLANATION.md** - Physical equations
- **COEFFICIENT_CORRECTIONS.md** - Why corrections were needed
- **REFERENCES.md** - Citations

---

## Support

**Issues?**
1. Check this manual
2. Review `../standalone_tests/` examples
3. Verify using test scenarios
4. Contact: Maria Rah, Byurakan Astrophysical Observatory

---

**Happy simulating!** 🚀
