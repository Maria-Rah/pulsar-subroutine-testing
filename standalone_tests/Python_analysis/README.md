# Python Analysis - Pulsar Subroutine Testing

Standalone Python tests for validating pulsar evolution physics before Fortran implementation.

**Author:** Maria Rah  
**Institution:** Byurakan Astrophysical Observatory  
**Date:** January 2026

---

## Purpose

These Python scripts test the pulsar subroutine physics independently:
1. Verify mathematical formulations
2. Identify coefficient errors
3. Generate expected results for comparison
4. Create analysis figures

---

## Files (21 total)

### Python Test Scripts (7 files)

1. **scenario1_test.py** - Young Pulsar
   - Strong B-field (10¹² G)
   - Dipole spin-down only
   - B-field decay validation

2. **scenario2_test.py** - Isolated MSP
   - Weak B-field (5×10⁸ G)
   - Post-recycling evolution
   - Slow spin-down

3. **scenario3_test.py** - Accreting MSP
   - Active accretion
   - Net SPIN-UP effect
   - Multiple Pdot components

4. **scenario4_test.py** - Dynamic MSP
   - Dense environment
   - Dynamical kicks
   - Environmental effects

5. **scenario5_test.py** - Wide Binary MSP
   - Binary with WD companion
   - No accretion

6. **scenario6_test.py** - NS-NS Merger (partial)
   - GW disabled
   - Needs verification

7. **scenario7_test.py** - NS-BH Merger (partial)
   - Almost identical to Sc6

### Overview Analysis (1 file)

8. **overview_comparison.py** - All Scenarios
   - Comparison plots (B, P, Pdot, P-Pdot)
   - Summary statistics
   - Generates 4 comparison figures

### Results - Individual Scenarios (7 PNG files)

9. **scenario1_analysis.png** - Young Pulsar results
10. **scenario2_analysis.png** - Isolated MSP results
11. **scenario3_analysis.png** - Accreting MSP results
12. **scenario4_analysis.png** - Dynamic MSP results
13. **scenario5_analysis.png** - Wide Binary results
14. **scenario6_analysis.png** - NS-NS Merger results
15. **scenario7_analysis.png** - NS-BH Merger results

### Results - Overview Comparison (4 PNG files)

16. **comparison_bfield.png** - B-field evolution (all 7)
17. **comparison_period.png** - Period evolution (all 7)
18. **comparison_pdot.png** - Pdot evolution (all 7)
19. **comparison_ppdot.png** - P-Pdot diagram (all 7)

### Documentation (2 files)

20. **DASHBOARD_FINAL.html** - Interactive results dashboard
21. **README.md** - This file

---

## Usage

### View Interactive Dashboard

Open `DASHBOARD_FINAL.html` in web browser to see all results.

### Run Individual Test

```bash
python scenario1_test.py
```

**Output:**
- `scenario1_output.dat` - Evolution data
- `scenario1_analysis.png` - Analysis figure (already included)

### Run All Tests

```bash
python scenario1_test.py
python scenario2_test.py
python scenario3_test.py
python scenario4_test.py
python scenario5_test.py
python scenario6_test.py
python scenario7_test.py
```

### Create Overview Comparison

```bash
python overview_comparison.py
```

**Output:**
- `comparison_bfield.png` (already included)
- `comparison_period.png` (already included)
- `comparison_pdot.png` (already included)
- `comparison_ppdot.png` (already included)

---

## Requirements

```python
numpy
matplotlib
```

Install:
```bash
pip install numpy matplotlib
```

---

## Output Format

Each scenario generates `.dat` file with columns:

```
TIME(Myr)  B(G)  P(s)  Pdot(s/s)  [scenario-specific]  B/B0  P/P0
```

---

## Corrected Coefficients

These tests use **corrected** physics coefficients:

| Coefficient | Value | Purpose |
|-------------|-------|---------|
| XKFIELD | 3.2×10¹⁹ | Dipole radiation |
| XKTAU | 1.5773×10¹⁶ s | B-field decay (500 Myr) |
| XKACC | 2.0×10⁻¹⁶ | Accretion spin-up |
| XKENV | 5.0×10⁻²⁹ | Environmental |
| XKDYN | 1.0×10⁻²² | Dynamical |
| XKASYM | 1.0×10⁻²⁷ | Asymmetric kicks |

**Note:** XKGW and XKMERGE disabled (need verification)

---

## Key Results

### Coefficient Corrections Found:

- **XKDECAY:** Removed (double counting)
- **XKACC:** 10¹¹× too large
- **XKENV:** 10⁸× too large
- **XKDYN:** 10¹⁶× too large
- **XKASYM:** 10²⁰× too large

### Scenario Highlights:

- **Sc1:** Young pulsar spins down from 100 ms → 2.3 s in 100 Myr
- **Sc2:** Isolated MSP very slow evolution (3 ms → 3.2 ms)
- **Sc3:** Net SPIN-UP due to accretion (ΔP < 0)
- **Sc4:** Environmental effects dominate for dynamic MSP
- **Sc5:** Wide binary similar to isolated MSP
- **Sc6 & Sc7:** Nearly identical (partial physics)

---

## Comparison with Fortran

After running Python tests:
1. Implement same physics in Fortran
2. Run Fortran standalone tests
3. Compare Python vs Fortran outputs
4. Validate coefficient corrections

See `../fortran_codes/` for Fortran implementations.

---

## References

### Python Implementation:
- Rah et al. (2024), Communications of BAO, Vol. 71, Is. 2, pp. 351
- Rah et al. (2025a), Communications of BAO, Vol. 72, Is. 1, pp. 55
- Rah et al. (2025b), Communications of BAO, Vol. 72, Is. 2, pp. 279

### Physics:
- Bhattacharya & van den Heuvel (1991) - B-field decay
- Alpar et al. (1982) - Accretion spin-up
- Ostriker et al. (1970) - Environmental effects

---

## Notes

1. **Python first, Fortran second:** Validate physics in Python before Fortran
2. **Standalone tests:** No NBODY6++GPU dependencies
3. **Coefficient corrections:** Critical for accurate results
4. **Scenarios 6 & 7:** Partial implementation (GW physics needs work)

---

## Contact

Maria Rah  
Byurakan Astrophysical Observatory  
Armenia

For questions about Python implementation or physics validation.

---

**Python tests validated the subroutine physics! ✓**
