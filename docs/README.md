# Documentation - Pulsar Subroutine for NBODY6++GPU

Comprehensive documentation for the pulsar evolution subroutine.

**Author:** Maria Rah  
**Institution:** Byurakan Astrophysical Observatory  
**Last Updated:** January 2026

---

## 📚 Contents

### Documentation Files

1. **MANUAL.md** - Complete user manual
   - Installation guide
   - Usage instructions
   - Examples
   - Troubleshooting

2. **INTEGRATION_GUIDE.md** - NBODY6++GPU integration
   - Step-by-step integration
   - File modifications
   - Compilation instructions
   - Testing procedures

3. **PHYSICS_EXPLANATION.md** - Physics & equations
   - Magnetic dipole radiation
   - B-field decay
   - Accretion spin-up
   - Environmental torques
   - Gravitational wave radiation
   - All physical formulations

4. **COEFFICIENT_CORRECTIONS.md** - Corrections documentation
   - Original vs corrected values
   - Error analysis
   - Validation results
   - Impact on simulations

5. **REFERENCES.md** - Complete bibliography
   - Maria Rah's publications
   - Key physics papers
   - NBODY6++GPU references
   - Observational data sources

### Publications

Located in `publications/` folder:

- **Paper_I_2024_CoBAO_71_351.pdf**  
  "Pulsars and Millisecond Pulsars I: Advancements, Open Questions and finding Gaps via statistical insights"

- **Paper_II_2025_CoBAO_72_55.pdf**  
  "Pulsars and Millisecond Pulsars II: Deep diving into the Evolutionary Mechanisms"

- **Paper_III_2025_CoBAO_72_279.pdf**  
  "Pulsars and Millisecond Pulsars III: Tracing Compact Object Dynamics in Globular Clusters with NBODY6++GPU"

---

## 🚀 Quick Start

**New users:** Start with **MANUAL.md**

**Integration:** See **INTEGRATION_GUIDE.md**

**Physics details:** Read **PHYSICS_EXPLANATION.md**

**Corrections info:** Check **COEFFICIENT_CORRECTIONS.md**

---

## 📖 Documentation Guide

### For Users

If you want to **use** the subroutine:
1. Read MANUAL.md
2. Follow INTEGRATION_GUIDE.md
3. Run tests from `../standalone_tests/`

### For Developers

If you want to **modify** the subroutine:
1. Read PHYSICS_EXPLANATION.md
2. Study COEFFICIENT_CORRECTIONS.md
3. Review code in `../subroutine/`

### For Researchers

If you want to **cite** or **understand** the work:
1. Read publications in `publications/`
2. Check REFERENCES.md
3. Review PHYSICS_EXPLANATION.md

---

## 📊 Version History

**v4.0 (January 2026) - CORRECTED:**
- Removed PDOT_DECAY (double counting)
- Corrected 5 coefficients (10⁸× to 10²⁰× errors!)
- Full validation with 7 scenarios
- Complete documentation
- HTML interactive dashboard

**v3.0 (December 2025):**
- Initial version (had bugs)

---

## 🔗 Related Folders

- `../subroutine/` - Subroutine code and input files
- `../standalone_tests/` - Validation tests
  - `python_analysis/` - Python scenario tests
  - `fortran_codes/` - Fortran scenario tests
  - `colab_results/` - Google Colab results

---

## 📧 Contact

**Maria Rah**  
Byurakan Astrophysical Observatory  
Armenia

For questions about:
- **Physics:** See PHYSICS_EXPLANATION.md
- **Integration:** See INTEGRATION_GUIDE.md
- **Bugs:** Check MANUAL.md troubleshooting
- **Publications:** See `publications/` folder

---

## 📝 Citation

If you use this subroutine, please cite:

```
Rah et al. (2025), "Pulsars and Millisecond Pulsars III: Tracing Compact 
Object Dynamics in Globular Clusters with NBODY6++GPU", Communications of 
Byurakan Astrophysical Observatory, Vol. 72, Issue 2, pp. 279-293
DOI: 10.52526/25792776-25.72.2-279
```

---

**Complete documentation for pulsar evolution in NBODY6++GPU!** ✅
