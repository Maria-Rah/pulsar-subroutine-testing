# Pulsar Evolution Subroutine for NBODY6++GPU

🔭 Interactive Dashboards
Click the links below to view the interactive graphical dashboards generated for this project:  
- [pulsar_dashboard_v4_CORRECTED.html](https://maria-rah.github.io/pulsar-subroutine-testing/subroutine/pulsar_dashboard_v4_CORRECTED.html)
- [DASHBOARD_FINAL.html](https://maria-rah.github.io/pulsar-subroutine-testing/standalone_tests/Python_analysis/DASHBOARD_FINAL.html)

**Comprehensive pulsar evolution subroutine with magnetic field decay and spin evolution for NBODY6++GPU N-body simulations**

Developed by **Maria Rah**  
Byurakan Astrophysical Observatory, Armenia  
In collaboration with Prof. Rainer Spurzem (Heidelberg University)

---

## 📋 Table of Contents

- [Overview](#overview)
- [Key Features](#key-features)
- [Publications](#publications)
- [Quick Start](#quick-start)
- [Repository Structure](#repository-structure)
- [Installation](#installation)
- [Usage](#usage)
- [Documentation](#documentation)
- [Validation & Testing](#validation--testing)
- [Citation](#citation)
- [Contributing](#contributing)
- [Contact](#contact)

---

## 🌟 Overview

This repository contains a **standalone pulsar evolution subroutine** designed for integration with **NBODY6++GPU**, a state-of-the-art direct N-body simulation code for star clusters and galactic nuclei. The subroutine accurately models neutron star magnetic field decay and spin evolution across multiple physical processes:

- **Magnetic field decay** (Ohmic resistivity)
- **Spin-down via magnetic dipole radiation**
- **Accretion-driven spin changes** (recycling)
- **Environmental interactions** (stellar encounters)
- **Binary evolution effects**
- **Gravitational wave radiation** (for compact binaries)

### Why This Matters

Standard stellar evolution packages (e.g., SSE/BSE) in N-body codes treat neutron stars as static objects. This subroutine enables **dynamic pulsar evolution** during simulations, crucial for:

- Millisecond pulsar formation in globular clusters
- NS-NS and NS-BH binary evolution
- Radio pulsar demographics
- Gravitational wave source populations

---

## ✨ Key Features

### Physics Implementation

- **7 evolutionary mechanisms** with correct physical coefficients
- **Time-dependent magnetic field** decay (exponential + Hall drift)
- **Spin period evolution** coupled to accretion and environment
- **Binary interactions** (mass transfer, asymmetric kicks, GW radiation)
- **Validated against observations** (see Paper III)

### Technical Highlights

- ✅ **Fortran 90** for NBODY6++GPU integration
- ✅ **Python 3.8+** for standalone testing
- ✅ **Interactive HTML dashboard** for visualization
- ✅ **Comprehensive test suite** (7 astrophysical scenarios)
- ✅ **Corrected coefficients** (v4.0) — fixes critical errors in v1-v3
- ✅ **Fully documented** with physics explanations and integration guide

### Version 4.0 CORRECTED

This version includes **critical coefficient corrections** that fix errors of magnitude **10¹¹ to 10²⁰** in earlier versions:

| Coefficient | Old Value | **Corrected Value** | Error Factor |
|-------------|-----------|---------------------|--------------|
| `XKACC`     | 2.0×10⁻⁵  | **2.0×10⁻¹⁶**       | 10¹¹×        |
| `XKENV`     | 5.0×10⁻²¹ | **5.0×10⁻²⁹**       | 10⁸×         |
| `XKDYN`     | 1.0×10⁻⁶  | **1.0×10⁻²²**       | 10¹⁶×        |
| `XKASYM`    | 1.0×10⁻⁷  | **1.0×10⁻²⁷**       | 10²⁰×        |

**See [COEFFICIENT_CORRECTIONS.md](docs/COEFFICIENT_CORRECTIONS.md) for full derivations.**

---

## 📚 Publications

This work is documented in a series of papers published in *Communications of the Byurakan Astrophysical Observatory*:

1. **Paper I (2024):** Theoretical foundation  
   *Maria Rah et al., 2024CoBAO..71..351R*  
   [PDF](docs/publications/Paper_I_2024_CoBAO_71_351.pdf)

2. **Paper II (2025):** Initial implementation and testing  
   *Maria Rah et al., 2025CoBAO..72...55R*  
   [PDF](docs/publications/Paper_II_2025_CoBAO_72_55.pdf)

3. **Paper III (2025):** **Corrected version and validation** (PRIMARY CITATION)  
   *Maria Rah et al., DOI: 10.52526/25792776-25.72.2-279*  
   [PDF](docs/publications/Paper_III_2025_CoBAO_72_279.pdf)

**Complete references:** [REFERENCES.md](docs/REFERENCES.md)

---

## 🚀 Quick Start

### Prerequisites

- **For Fortran:** `gfortran` 4.8+ or Intel Fortran compiler
- **For Python tests:** Python 3.8+, NumPy, Matplotlib
- **For NBODY6++GPU integration:** NBODY6++GPU source code, CUDA toolkit

### Installation

```bash
# Clone the repository
git clone https://github.com/MariaRah/pulsar-subroutine-testing.git
cd pulsar-subroutine-testing

# Run a standalone Python test (no installation needed)
cd standalone_tests/python_analysis
python scenario1_test.py

# Compile a Fortran test
cd ../fortran_codes
gfortran -o scenario1 scenario1_young_pulsar.f90
./scenario1
```

### Quick Test

Test the subroutine with a young pulsar evolution:

```python
# standalone_tests/python_analysis/scenario1_test.py
python scenario1_test.py
# Output: scenario1_analysis.png showing B-field and period evolution
```

### Integration with NBODY6++GPU

See **[INTEGRATION_GUIDE.md](docs/INTEGRATION_GUIDE.md)** for step-by-step instructions.

---

## 📁 Repository Structure

```
pulsar-subroutine-testing/
│
├── README.md                          # This file
│
├── subroutine/                        # Main subroutine files
│   ├── calc_and_save_pulsar_params_v4_CORRECTED.f
│   ├── pulsar_dashboard_v4_CORRECTED.html
│   ├── Paper5_RunA_WITHOUT_subroutine.inp
│   ├── Paper5_RunB_WITH_subroutine_CORRECTED.inp
│   └── README.md
│
├── standalone_tests/                  # Independent tests (no NBODY6 needed)
│   ├── python_analysis/               # Python test scripts (21 files)
│   │   ├── scenario1_test.py          # Young pulsar
│   │   ├── scenario2_test.py          # Isolated MSP
│   │   ├── scenario3_test.py          # Accreting MSP
│   │   ├── scenario4_test.py          # Dynamic MSP
│   │   ├── scenario5_test.py          # Wide binary
│   │   ├── scenario6_test.py          # NS-NS merger
│   │   ├── scenario7_test.py          # NS-BH merger
│   │   ├── overview_comparison.py     # Multi-scenario comparison
│   │   ├── *.png                      # Analysis plots (15 files)
│   │   ├── DASHBOARD_FINAL.html       # Interactive results
│   │   └── README.md
│   │
│   ├── fortran_codes/                 # Fortran test programs (8 files)
│   │   ├── scenario1_young_pulsar.f90
│   │   ├── scenario2_isolated_msp.f90
│   │   ├── scenario3_accreting_msp.f90
│   │   ├── scenario4_dynamic_msp.f90
│   │   ├── scenario5_wide_binary.f90
│   │   ├── scenario6_nsns_merger.f90
│   │   ├── scenario7_nsbh_merger.f90
│   │   └── README.md
│   │
│   └── colab_results/                 # Google Colab execution results (30 files)
│       ├── scenario*_output.dat       # Numerical output (7 files)
│       ├── B-P-Pdot_Time_*.png        # 3D evolution plots (7 files)
│       ├── P_B_*.png                  # P-B diagrams (7 files)
│       ├── P_Pdot_*.png               # P-Pdot diagrams (7 files)
│       ├── Fortran_results_comparison.png
│       ├── Colab_execution_output.txt
│       └── README.md
│
└── docs/                              # Documentation (7 items)
    ├── README.md                      # Documentation index
    ├── MANUAL.md                      # User manual
    ├── INTEGRATION_GUIDE.md           # NBODY6++GPU integration steps
    ├── PHYSICS_EXPLANATION.md         # Equations and derivations
    ├── COEFFICIENT_CORRECTIONS.md     # Error analysis and fixes
    ├── REFERENCES.md                  # Complete bibliography
    └── publications/                  # Research papers (3 PDFs)
        ├── Paper_I_2024_CoBAO_71_351.pdf
        ├── Paper_II_2025_CoBAO_72_55.pdf
        └── Paper_III_2025_CoBAO_72_279.pdf
```

**Total:** ~70 files including code, documentation, tests, and results

---

## 📖 Documentation

### For Users

- **[MANUAL.md](docs/MANUAL.md)** — Complete usage guide
  - Installation instructions
  - Configuration options
  - Running tests
  - Troubleshooting

### For Developers

- **[INTEGRATION_GUIDE.md](docs/INTEGRATION_GUIDE.md)** — Integrate into NBODY6++GPU
  - File modifications
  - Compilation instructions
  - Input file configuration

### For Scientists

- **[PHYSICS_EXPLANATION.md](docs/PHYSICS_EXPLANATION.md)** — Physical model
  - All equations with derivations
  - Coefficient explanations
  - Validation against observations

- **[COEFFICIENT_CORRECTIONS.md](docs/COEFFICIENT_CORRECTIONS.md)** — Error discovery
  - How errors were found
  - Impact analysis
  - Before/after comparisons

- **[REFERENCES.md](docs/REFERENCES.md)** — Complete bibliography
  - Primary papers
  - Physics references
  - Textbooks and reviews

---

## 🧪 Validation & Testing

### 7 Astrophysical Scenarios

All tests are **standalone** (run without NBODY6++GPU):

| Scenario | Description | Initial Conditions | Key Physics |
|----------|-------------|-------------------|-------------|
| **1** | Young Pulsar | B₀=10¹² G, P₀=100 ms | Field decay, spin-down |
| **2** | Isolated MSP | B₀=5×10⁸ G, P₀=3 ms | Long-term stability |
| **3** | Accreting MSP | With mass transfer | **Net spin-up** |
| **4** | Dynamic MSP | Kicks + environment | Combined effects |
| **5** | Wide Binary | a=100 AU | Weak interaction |
| **6** | NS-NS Merger | Eccentric orbit | GW radiation (partial) |
| **7** | NS-BH Merger | NS + 10 M☉ BH | Tidal effects (partial) |

### Test Results

- **Python tests:** Generate plots in `standalone_tests/python_analysis/`
- **Fortran tests:** Output data in `standalone_tests/colab_results/`
- **Interactive dashboard:** `subroutine/pulsar_dashboard_v4_CORRECTED.html`

**All validation plots and data are included in the repository.**

---

## 📝 Usage

### Running Python Tests

```bash
cd standalone_tests/python_analysis

# Run individual scenario
python scenario1_test.py   # Generates scenario1_analysis.png

# Run comparison analysis
python overview_comparison.py

# View interactive dashboard
open DASHBOARD_FINAL.html  # or use a web browser
```

### Running Fortran Tests

```bash
cd standalone_tests/fortran_codes

# Compile
gfortran -o test1 scenario1_young_pulsar.f90

# Run
./test1 > output.dat

# Plot results (requires Python)
python ../python_analysis/scenario1_test.py
```

### Integration into NBODY6++GPU

1. Read **[INTEGRATION_GUIDE.md](docs/INTEGRATION_GUIDE.md)**
2. Copy `subroutine/calc_and_save_pulsar_params_v4_CORRECTED.f` to NBODY6++GPU source
3. Modify `hrplot.f` and `Makefile` as documented
4. Recompile NBODY6++GPU
5. Use input file template `Paper5_RunB_WITH_subroutine_CORRECTED.inp`

---

## 📊 Citation

If you use this subroutine in your research, please cite:

### Primary Citation (v4.0 CORRECTED)

```bibtex
@article{Rah2025_PaperIII,
  author  = {Rah, Maria and Mickaelian, Areg M. and Spurzem, Rainer},
  title   = {Pulsar Evolution in NBODY6++GPU: Corrected Implementation},
  journal = {Communications of the Byurakan Astrophysical Observatory},
  year    = {2025},
  volume  = {72},
  pages   = {279},
  doi     = {10.52526/25792776-25.72.2-279}
}
```

### Additional Citations

```bibtex
@article{Rah2024_PaperI,
  author  = {Rah, Maria and Mickaelian, Areg M. and Spurzem, Rainer},
  title   = {Theoretical Foundation for Pulsar Evolution in N-body Simulations},
  journal = {Communications of the Byurakan Astrophysical Observatory},
  year    = {2024},
  volume  = {71},
  pages   = {351},
  adsurl  = {2024CoBAO..71..351R}
}

@article{Rah2025_PaperII,
  author  = {Rah, Maria and Mickaelian, Areg M. and Spurzem, Rainer},
  title   = {Initial Implementation of Pulsar Evolution Subroutine},
  journal = {Communications of the Byurakan Astrophysical Observatory},
  year    = {2025},
  volume  = {72},
  pages   = {55},
  adsurl  = {2025CoBAO..72...55R}
}
```

---

## 🤝 Contributing

Contributions are welcome! Please:

1. **Report bugs** via GitHub Issues
2. **Suggest improvements** for physics or code
3. **Share validation results** from your simulations
4. **Extend tests** with new astrophysical scenarios

### Development Guidelines

- Follow Fortran 90 standards
- Document all physics assumptions
- Include test cases for new features
- Update relevant documentation

---


## 📧 Contact

**Maria Rah**  
PhD Candidate  
Byurakan Astrophysical Observatory  
Armenia

**Research Supervisors:**
- Prof. Rainer Spurzem (Heidelberg University, Germany)
- Prof. Areg M. Mickaelian (Byurakan Astrophysical Observatory)
- Dr. Francesco Flammini Dotti (Heidelberg University, Germany)

**For questions or collaborations:**
- Open a GitHub Issue
- Email: [Optional: add your email]
- ORCID: [Optional: add your ORCID ID]


---

## 📈 Version History

- **v4.0 CORRECTED** (2025) — Critical coefficient fixes, comprehensive validation
- **v3.0** (2025) — Extended physics (GW, asymmetric kicks)
- **v2.0** (2025) — Binary interactions
- **v1.0** (2024) — Initial implementation

---

## 🔗 Related Projects

- **[NBODY6++GPU](https://github.com/nbody6ppgpu/Nbody6PPGPU-beijing)** — Host N-body code
- **SSE/BSE** — Stellar evolution in N-body codes
- **COSMIC** — Compact object synthesis

---

## ⭐ Star This Repository

If you find this work useful, please **star this repository** and cite our papers!

---

**Last Updated:** January 2026  
**Repository Status:** Production-ready ✅  
**Maintained by:** Maria Rah

---

