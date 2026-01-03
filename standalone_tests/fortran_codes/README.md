# Fortran Test Codes - All 7 Scenarios

**Author:** Maria Rah 
**Institution:** Byurakan Astrophysical Observatory  
**Date:** January 2026

---

## 📁 Package Contents

This package contains **7 standalone Fortran programs** to test pulsar evolution scenarios:

1. `scenario1_young_pulsar.f90` - Recently formed NS, strong B-field
2. `scenario2_isolated_msp.f90` - Recycled MSP, weak B-field
3. `scenario3_accreting_msp.f90` - Active accretion, spin-up
4. `scenario4_dynamic_msp.f90` - Dense environment, kicks
5. `scenario5_wide_binary.f90` - Binary MSP, no accretion
6. `scenario6_nsns_merger.f90` - NS-NS binary (partial test)
7. `scenario7_nsbh_merger.f90` - NS-BH binary (partial test)

---

## 🚀 How to Use in Google Colab

### Step 1: Upload Fortran File

Upload the `.f90` file to Colab

### Step 2: Install Compiler

```python
!apt-get install -y gfortran
```

### Step 3: Compile

```python
!gfortran -O2 -o test_scenario scenario1_young_pulsar.f90
```

(Replace `scenario1_young_pulsar.f90` with the appropriate file)

### Step 4: Run

```python
!./test_scenario
```

### Step 5: Analyze Results

```python
import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt('scenario1_output.dat')
time = data[:, 0]
B = data[:, 1]
P = data[:, 2]
Pdot = data[:, 3]

# Create your plots...
```

---

## 📊 Output Format

Each program creates a `.dat` file with columns:

```
TIME  B  P  Pdot  [additional components]  B/B0  P/P0
```

- **TIME:** Time in Myr
- **B:** Magnetic field in Gauss
- **P:** Period in seconds
- **Pdot:** Period derivative in s/s
- **B/B0:** Normalized B-field
- **P/P0:** Normalized period

---

## ⚙️ Compilation Options

**Basic:**
```bash
gfortran scenario1_young_pulsar.f90 -o test
```

**Optimized:**
```bash
gfortran -O2 -o test scenario1_young_pulsar.f90
```

**Debug:**
```bash
gfortran -g -Wall -fcheck=all scenario1_young_pulsar.f90 -o test
```

---

## 🔍 Scenarios Summary

### Scenario 1: Young Pulsar
- B₀ = 10¹² G
- P₀ = 100 ms
- Physics: Dipole spin-down only

### Scenario 2: Isolated MSP
- B₀ = 5×10⁸ G
- P₀ = 3 ms
- Physics: Dipole spin-down only

### Scenario 3: Accreting MSP
- B₀ = 3×10⁸ G
- P₀ = 2.5 ms
- Physics: Dipole + Accretion + Environment
- **Result:** Net spin-UP

### Scenario 4: Dynamic MSP
- B₀ = 3×10⁸ G
- P₀ = 4 ms
- Physics: Dipole + Environment + Dynamic + Asymmetric
- Includes: kicks, dense environment

### Scenario 5: Wide Binary MSP
- B₀ = 4×10⁸ G
- P₀ = 3.5 ms
- Physics: Dipole + Environment
- Binary with WD companion

### Scenario 6: NS-NS Merger
- B₀ = 5×10¹¹ G
- P₀ = 10 ms
- Physics: Dipole + Environment + Asymmetric
- **Note:** GW and Merge components disabled (coefficient errors)

### Scenario 7: NS-BH Merger
- B₀ = 5×10¹¹ G
- P₀ = 10 ms
- Physics: Dipole + Environment + Asymmetric
- **Note:** GW disabled, Merge N/A (BH has no surface)

---

## 📝 Notes

1. **Coefficients used are CORRECTED values** from Python testing
2. **Scenarios 6 & 7:** Partial tests only (GW physics needs verification)
3. **All programs:** Self-contained, no external dependencies
4. **Time step:** 0.1 Myr, Total time: 100 Myr

---

## 🐛 Issues Found

These tests identified coefficient errors in the original subroutine:

| Coefficient | Error Factor |
|-------------|--------------|
| XKDECAY | Remove (double counting) |
| XKACC | 10¹¹× too large |
| XKENV | 10⁸× too large |
| XKDYN | 10¹⁶× too large |
| XKASYM | 10²⁰× too large |
| XKGW | 10⁶⁴× too large |

---

**Ready for testing in Google Colab!** 🚀
