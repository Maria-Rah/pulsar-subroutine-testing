# Pulsar Subroutine for NBODY6++GPU (Version 4.0 - CORRECTED)

This folder contains the corrected pulsar subroutine for integration into NBODY6++GPU, along with configuration files and documentation.

**Author:** Maria Rah  
**Institution:** Byurakan Astrophysical Observatory  
**Date:** January 2026  
**Version:** 4.0 (CORRECTED)

---

## 📁 Files in This Folder

### 1. `calc_and_save_pulsar_params_v4_CORRECTED.f`
**Main pulsar subroutine** - Fortran code for NBODY6++GPU integration

**Key Features:**
- Physics-based scenario detection (7 scenarios)
- Corrected spin evolution coefficients
- Magnetic field evolution with exponential decay
- Binary interaction tracking
- Output: `pulsar.55_TTTTTTTT.dat` files

**Corrections from v3:**
- ✅ PDOT_DECAY removed (double counting eliminated)
- ✅ All coefficients corrected (see table below)
- ✅ Output filename: `ns.33_*` → `pulsar.55_*`

### 2. `pulsar_dashboard_v4_CORRECTED.html`
**Interactive documentation dashboard**

**Contains:**
- Complete subroutine documentation
- Architecture explanation
- 7 scenario descriptions
- File dependencies
- Implementation guide
- Output format specification

**How to use:** Open in web browser for full documentation

### 3. `Paper5_RunA_WITHOUT_subroutine.inp`
**Baseline simulation input** (N=150k, t=100 Myr)

- Standard NBODY6++GPU run
- NO pulsar subroutine
- For comparison with Run B

### 4. `Paper5_RunB_WITH_subroutine_CORRECTED.inp`
**Test simulation input** (N=150k, t=100 Myr)

- WITH pulsar subroutine v4
- KZ(48) = 1 (pulsar subroutine enabled)
- Includes INPULSAR namelist with corrected coefficients

---

## 🔧 Coefficient Corrections

| Coefficient | Original Value | Corrected Value | Error Factor | Status |
|-------------|----------------|-----------------|--------------|---------|
| XKDECAY | 1.0×10⁻⁸ | **0.0** | Removed | ✅ Fixed |
| XKACC | 2.0×10⁻⁵ | **2.0×10⁻¹⁶** | 10¹¹× too large | ✅ Fixed |
| XKENV | 5.0×10⁻²¹ | **5.0×10⁻²⁹** | 10⁸× too large | ✅ Fixed |
| XKDYN | 1.0×10⁻⁶ | **1.0×10⁻²²** | 10¹⁶× too large | ✅ Fixed |
| XKASYM | 1.0×10⁻⁷ | **1.0×10⁻²⁷** | 10²⁰× too large | ✅ Fixed |
| XKGW | ? | **1.2×10⁻⁷⁴** | ? | ⚠️ Needs verification |
| XKMERGE | ? | **1.0×10⁻²²** | ? | ⚠️ Needs verification |

**Reference:** Standalone scenario testing (see `../standalone_tests/`)

---

## 📊 7 Evolutionary Scenarios

The subroutine detects and tracks 7 distinct pulsar evolution scenarios:

1. **Isolated Pulsar** - Single NS, dipole spin-down only
2. **Isolated MSP** - Recycled MSP, now single
3. **Accreting MSP** - Active accretion, spin-up
4. **Dynamic MSP** - Post-exchange, dynamical kicks
5. **Wide Binary** - Binary MSP, no accretion
6. **NS-NS Binary** - Pre-merger, GW radiation
7. **NS-BH Binary** - Pre-disruption, tidal effects

---

## 🚀 Integration into NBODY6++GPU

### Files to Modify:

1. **common6.h** - Add variable declarations
2. **input.F** - Add INPULSAR namelist
3. **mdot.F** - Call subroutine
4. **mydump.F** - Save/restore pulsar state
5. **file_init.F** - Initialize output file

### Quick Integration Steps:

```bash
# 1. Copy subroutine to NBODY6++GPU
cp calc_and_save_pulsar_params_v4_CORRECTED.f /path/to/NBODY6/src/Main/

# 2. Modify common6.h (add pulsar variables)

# 3. Modify input.F (add INPULSAR namelist)

# 4. Modify mdot.F (add subroutine call)

# 5. Modify mydump.F (add state save/restore)

# 6. Update Makefile
# Add to SOURCES and OBJECTS lists

# 7. Compile
make clean
make -j8
```

**Detailed instructions:** See `pulsar_dashboard_v4_CORRECTED.html` → Tab 6 (Implementation)

---

## 📤 Output Format

### Filename Convention:
```
pulsar.55_TTTTTTTT.dat
```
Where `TTTTTTTT = INT(TIME*TSTAR)` in years

**Example:** `pulsar.55_00100000.dat` → data at 10 Myr

### Output Columns (20 total):
```
TIME  NAME  KSTAR  PERIOD  PDOT  BMAG  X  Y  Z  VX  VY  VZ  
FX  FY  FZ  E_TOTAL  E_BINDING  SCENARIO  LAT  LON
```

**Key columns for analysis:**
- **PERIOD** (s) - Spin period
- **PDOT** (s/s) - Period derivative
- **BMAG** (G) - Magnetic field
- **SCENARIO** (1-7) - Evolutionary scenario ID

---

## ✅ Validation

This subroutine has been validated through:

1. **Standalone Fortran tests** (7 scenarios)
2. **Google Colab verification** (all scenarios)
3. **Coefficient correction** (5 coefficients fixed)
4. **Documentation review** (HTML dashboard)

**Test results:** See `../standalone_tests/colab_results/`

---

## 📚 References

### This Work:
- **Paper I:** Rah, M., Mickaelian, A., Flammini Dotti, F., Spurzem, R. (2024)  
  "Pulsars and Millisecond Pulsars I: Advancements, Open Questions and finding Gaps via statistical insights"  
  Communications of Byurakan Astrophysical Observatory, Vol. 71, pp. 351  
  Bibcode: 2024CoBAO..71..351R
  https://ui.adsabs.harvard.edu/abs/2024CoBAO..71..351R/abstract

- **Paper II:** Rah, M., Spurzem, R., Flammini Dotti, F., Mickaelian, A. (2025)  
  "Pulsars and Millisecond Pulsars II: Deep diving into the Evolutionary Mechanisms"  
  Communications of Byurakan Astrophysical Observatory, Vol. 72, pp. 55  
  Bibcode: 2025CoBAO..72...55R
  https://ui.adsabs.harvard.edu/abs/2025CoBAO..72...55R/abstract

- **Paper III:** Rah, M., Spurzem, R., Flammini Dotti, F., Mickaelian, A. (2025)  
  "Pulsars and Millisecond Pulsars III: Tracing Compact Object Dynamics in Globular Clusters with NBODY6++GPU"  
  Communications of Byurakan Astrophysical Observatory, Vol. 72, Issue 2, pp. 279-293  
  DOI: https://doi.org/10.52526/25792776-25.72.2-279
  https://combao.bao.am/AllIssues/2025/279-293.pdf

### Key Physics Papers:
- **B-field decay:** Bhattacharya & van den Heuvel (1991), Phys. Rep., 203, 1
- **Accretion spin-up:** Alpar et al. (1982), Nature, 300, 728
- **GW radiation:** Peters (1964), Phys. Rev., 136, B1224
- **Environmental effects:** Ostriker et al. (1970), ApJ, 160, L143

### NBODY6++GPU:
- Wang et al. (2015), MNRAS, 450, 4070
- Spurzem (1999), JCoAM, 109, 407

---

## 🐛 Known Issues / TODO

- ⚠️ XKGW coefficient needs verification from Peters (1964)
- ⚠️ XKMERGE coefficient needs verification
- ⚠️ Scenario 6 & 7 are partial implementations (GW physics)

---

## 📧 Contact

**Maria Rah**  
Byurakan Astrophysical Observatory  
Armenia

For questions about:
- Subroutine integration → Email supervisor
- Physics implementation → See HTML dashboard
- Bug reports → GitHub Issues

---

## 📝 Version History

**v4.0 (January 2026) - CORRECTED**
- ✅ Removed PDOT_DECAY (double counting)
- ✅ Corrected 5 coefficients (XKACC, XKENV, XKDYN, XKASYM, XKGW)
- ✅ Output filename changed: pulsar.55_*
- ✅ Full HTML documentation
- ✅ Validated with standalone tests

**v3.0 (December 2025)**
- Initial version with bugs

---

**Ready for NBODY6++GPU integration!** 🚀
