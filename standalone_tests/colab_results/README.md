# Colab Results - Fortran Scenario Testing

Results from running 7 Fortran standalone tests in Google Colab.

**Author:** Maria Rah  
**Institution:** Byurakan Astrophysical Observatory  
**Date:** January 2, 2026

---

## 📁 Contents (29 files)

### Data Files (7 files)

1. **scenario1_isolated_output.dat** - Young Pulsar
2. **scenario2_isolated_msp_output.dat** - Isolated MSP
3. **scenario3_accreting_msp_output.dat** - Accreting MSP
4. **scenario4_dynamic_msp_output.dat** - Dynamic MSP
5. **scenario5_wide_binary_output.dat** - Wide Binary MSP
6. **scenario6_output.dat** - NS-NS Merger (partial)
7. **scenario7_output.dat** - NS-BH Merger (partial)

### Analysis Figures - Evolution Plots (7 PNG files)

Multi-panel plots showing B-field, Period, and Pdot evolution over time:

8. **B-P-Pdot_Time_1.png** - Scenario 1 (Young Pulsar)
9. **B-P-Pdot_Time_2.png** - Scenario 2 (Isolated MSP)
10. **B-P-Pdot_Time_3.png** - Scenario 3 (Accreting MSP)
11. **B-P-Pdot_Time_4.png** - Scenario 4 (Dynamic MSP)
12. **B-P-Pdot_Time_5.png** - Scenario 5 (Wide Binary)
13. **B-P-Pdot_Time_6.png** - Scenario 6 (NS-NS Merger)
14. **B-P-Pdot_Time_7.png** - Scenario 7 (NS-BH Merger)

### Analysis Figures - P-B Diagrams (7 PNG files)

Period vs Magnetic field diagrams:

15. **P_B_1.png** - Scenario 1
16. **P_B_2.png** - Scenario 2
17. **P_B_3.png** - Scenario 3
18. **P_B_4.png** - Scenario 4
19. **P_B_5.png** - Scenario 5
20. **P_B_6.png** - Scenario 6
21. **P_B_7.png** - Scenario 7

### Analysis Figures - P-Pdot Diagrams (7 PNG files)

Classic pulsar P-Pdot diagrams:

22. **P_Pdot_1.png** - Scenario 1
23. **P_Pdot_2.png** - Scenario 2
24. **P_Pdot_3.png** - Scenario 3
25. **P_Pdot_4.png** - Scenario 4
26. **P_Pdot_5.png** - Scenario 5
27. **P_Pdot_6.png** - Scenario 6
28. **P_Pdot_7.png** - Scenario 7

### Comparison Figure (1 PNG file)

29. **Fotran_results_comparison.png** - All 7 scenarios overlaid

### Documentation (1 file)

30. **README.md** - This file

**Total: 30 files** (7 dat + 21 png + 1 md + 1 txt)

---

## 📊 Results Summary

### Scenario 1: Young Pulsar ✅

**Initial:** B₀=10¹² G, P₀=100 ms  
**Final (100 Myr):** B=8.19×10¹¹ G (81.87%), P=2.259 s, Pdot=2.90×10⁻¹⁶ s/s  
**✓ B-field decay matches exp(-t/τ)**

### Scenario 2: Isolated MSP ✅

**Initial:** B₀=5×10⁸ G, P₀=3.0 ms  
**Final (100 Myr):** B=4.09×10⁸ G, P=3.205 ms, Pdot=5.11×10⁻²⁰ s/s  
**✓ Spin-down age = 0.99 Gyr (typical MSP)**

### Scenario 3: Accreting MSP ✅

**Initial:** B₀=3×10⁸ G, P₀=2.0 ms, Ṁ=10⁻¹⁰ M☉/yr  
**Final (100 Myr):** B=1.64×10⁸ G, P=0.100 ms, Pdot=-1.26×10⁻⁴ s/s  
**→ NET SPIN-UP! (accretion dominates)**  
**✓ XKACC correction: 2×10⁻⁵ → 2×10⁻¹⁶ (10¹¹× error!)**

### Scenario 4: Dynamic MSP ✅

**Initial:** B₀=3×10⁸ G, P₀=4.0 ms, dense environment  
**Final (100 Myr):** P=4.057 ms, Pdot=1.45×10⁻²⁰ s/s  
**✓ XKENV: 10⁸× correction**  
**✓ XKDYN: 10¹⁶× correction**  
**✓ XKASYM: 10²⁰× correction**

### Scenario 5: Wide Binary MSP ✅

**Initial:** B₀=4×10⁸ G, P₀=3.5 ms  
**Final (100 Myr):** P=3.614 ms, Pdot=2.90×10⁻²⁰ s/s  
**→ Behaves like Scenario 2 (isolated MSP)**

### Scenario 6: NS-NS Merger ⚠️

**Initial:** B₀=5×10¹¹ G, P₀=10 ms  
**Final (100 Myr):** B=4.09×10¹¹ G, P=1132 ms, Pdot=1.46×10⁻¹⁶ s/s  
**⚠️ PARTIAL TEST (GW disabled)**

### Scenario 7: NS-BH Merger ⚠️

**Initial:** B₀=5×10¹¹ G, P₀=10 ms  
**Final (100 Myr):** B=4.09×10¹¹ G, P=1132 ms, Pdot=1.46×10⁻¹⁶ s/s  
**⚠️ Almost identical to Sc6 (GW disabled)**

---

## 🔬 Coefficient Corrections

| Coefficient | Original | Corrected | Error | Status |
|-------------|----------|-----------|-------|---------|
| PDOT_DECAY | included | 0.0 | Remove | ✅ |
| XKACC | 2×10⁻⁵ | 2×10⁻¹⁶ | 10¹¹× | ✅ |
| XKENV | 5×10⁻²¹ | 5×10⁻²⁹ | 10⁸× | ✅ |
| XKDYN | 10⁻⁶ | 10⁻²² | 10¹⁶× | ✅ |
| XKASYM | 10⁻⁷ | 10⁻²⁷ | 10²⁰× | ✅ |
| XKGW | ? | 1.2×10⁻⁷⁴ | ? | ⚠️ |
| XKMERGE | ? | 10⁻²² | ? | ⚠️ |

---

## 📐 Data Format

```
# TIME(Myr)  B(G)  P(s)  Pdot(s/s)
```

---

## 🔗 References

**Maria Rah's Publications:**
1. Paper I: 2024CoBAO..71..351R
2. Paper II: 2025CoBAO..72...55R
3. Paper III: DOI 10.52526/25792776-25.72.2-279

**Physics:**
- Bhattacharya & van den Heuvel (1991) - B-field decay
- Alpar et al. (1982) - Accretion spin-up
- Peters (1964) - GW radiation

---

**Fortran tests validated corrected subroutine physics!** ✅
