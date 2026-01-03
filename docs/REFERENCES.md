# References - Pulsar Evolution Subroutine

Complete bibliography for the pulsar evolution subroutine.

**Author:** Maria Rah  
**Date:** January 2026

---

## Maria Rah's Publications

### Paper I (2024)

**Full Citation:**
```
Rah, M., et al. (2024)
"Pulsars and Millisecond Pulsars I: Advancements, Open Questions 
and finding Gaps via statistical insights"
Communications of Byurakan Astrophysical Observatory
Volume 71, pp. 351
Bibcode: 2024CoBAO..71..351R
```

**Content:**
- Statistical analysis of pulsar populations
- Identification of gaps in current understanding
- Observational constraints on pulsar evolution
- Motivation for improved evolutionary models

**Key Results:**
- MSP formation efficiency in globular clusters
- B-field distribution bimodality
- Spin period-field correlations
- Open questions requiring N-body simulations

---

### Paper II (2025)

**Full Citation:**
```
Rah, M., et al. (2025)
"Pulsars and Millisecond Pulsars II: Deep diving into the 
Evolutionary Mechanisms"
Communications of Byurakan Astrophysical Observatory
Volume 72, pp. 55
Bibcode: 2025CoBAO..72...55R
```

**Content:**
- Detailed physics of pulsar evolution
- Accretion-induced spin-up mechanisms
- Magnetic field decay timescales
- Environmental effects in clusters

**Key Results:**
- Recycling timescales ~100 Myr
- Critical accretion rate for spin-up
- Environmental torques in dense cores
- Binary evolution pathways to MSPs

---

### Paper III (2025) ⭐ PRIMARY REFERENCE

**Full Citation:**
```
Rah, M., et al. (2025)
"Pulsars and Millisecond Pulsars III: Tracing Compact Object 
Dynamics in Globular Clusters with NBODY6++GPU"
Communications of Byurakan Astrophysical Observatory
Volume 72, Issue 2, pp. 279-293
DOI: 10.52526/25792776-25.72.2-279
```

**Content:**
- **This subroutine implementation**
- Integration into NBODY6++GPU
- Validation with 7 test scenarios
- Coefficient corrections documentation
- Application to globular cluster simulations

**Key Results:**
- Corrected coefficient values
- 7 evolutionary scenarios identified
- Validation of physics implementation
- First self-consistent pulsar evolution in N-body

**⭐ CITE THIS PAPER when using the subroutine! ⭐**

---

## Key Physics Papers

### Magnetic Field Decay

**Bhattacharya & van den Heuvel (1991)**
```
Bhattacharya, D., & van den Heuvel, E. P. J. (1991)
"Formation and evolution of binary and millisecond radio pulsars"
Physics Reports, 203, 1-124
DOI: 10.1016/0370-1573(91)90064-S
```

**Why important:**
- Standard reference for B-field decay
- Decay timescale τ ~ 500 Myr
- Exponential decay model
- Observational support

**Key equation:**
```
B(t) = B₀ exp(-t/τ)
```

---

### Magnetic Dipole Radiation

**Ostriker & Gunn (1969)**
```
Ostriker, J. P., & Gunn, J. E. (1969)
"On the Nature of Pulsars. I. Theory"
The Astrophysical Journal, 157, 1395
DOI: 10.1086/150160
```

**Why important:**
- First rigorous treatment of pulsar spin-down
- Magnetic dipole radiation formula
- Braking index predictions

**Key equation:**
```
Ṗ = (32π⁴R⁶B²sin²α) / (3c³IP)
```

---

### Accretion-Induced Spin-Up

**Alpar et al. (1982)**
```
Alpar, M. A., Cheng, A. F., Ruderman, M. A., & Shaham, J. (1982)
"A new class of radio pulsars"
Nature, 300, 728-730
DOI: 10.1038/300728a0
```

**Why important:**
- Proposed recycling scenario for MSPs
- Accretion spins up old pulsars
- Explains MSP formation in binaries

**Key concept:**
- Old NS + accretion → recycled MSP
- Explains P ~ 1-10 ms, B ~ 10⁸ G

**Radhakrishnan & Srinivasan (1982)**
```
Radhakrishnan, V., & Srinivasan, G. (1982)
"On the origin of the recently discovered ultra-rapid pulsar"
Current Science, 51, 1096-1099
```

**Alternative recycling scenario**

---

### Environmental Torques

**Ostriker et al. (1970)**
```
Ostriker, J. P., Rees, M. J., & Silk, J. (1970)
"Some Observable Consequences of Accretion by Defunct Pulsars"
Astrophysical Letters, 6, 179
```

**Why important:**
- Environmental drag on pulsars
- ISM/cluster medium interactions
- Secondary effect on spin evolution

---

### Gravitational Wave Radiation

**Peters (1964)**
```
Peters, P. C. (1964)
"Gravitational Radiation and the Motion of Two Point Masses"
Physical Review, 136, 1224-1232
DOI: 10.1103/PhysRev.136.B1224
```

**Why important:**
- GW energy loss in binaries
- Orbital decay rates
- Essential for NS-NS/NS-BH binaries

**Key equation:**
```
dE/dt = -(32/5) × (G⁴/c⁵) × (m₁m₂(m₁+m₂)/a⁵)
```

**⚠️ Note:** XKGW coefficient needs verification from this paper

---

## N-body Simulation

### NBODY6++GPU

**Wang et al. (2015)**
```
Wang, L., Spurzem, R., Aarseth, S., Nitadori, K., Berczik, P., 
Kouwenhoven, M. B. N., & Naab, T. (2015)
"NBODY6++GPU: Ready for the gravitational million-body problem"
Monthly Notices of the Royal Astronomical Society, 450, 4070-4080
DOI: 10.1093/mnras/stv817
```

**Why important:**
- GPU-accelerated N-body code
- Handles 10⁶ particles
- Stellar evolution included
- Binary evolution

**Spurzem (1999)**
```
Spurzem, R. (1999)
"Direct N-body Simulations"
Journal of Computational and Applied Mathematics, 109, 407-432
DOI: 10.1016/S0377-0427(99)00166-1
```

**Review of direct N-body methods**

---

## Observational References

### Pulsar Catalogs

**Manchester et al. (2005) - ATNF Catalog**
```
Manchester, R. N., Hobbs, G. B., Teoh, A., & Hobbs, M. (2005)
"The Australia Telescope National Facility Pulsar Catalogue"
The Astronomical Journal, 129, 1993-2006
DOI: 10.1086/428488
```

**Online:** http://www.atnf.csiro.au/people/pulsar/psrcat/

**Contains:**
- ~3000 known pulsars
- Periods, Pdot, B-fields
- Binary parameters
- Timing solutions

---

### MSP Reviews

**Lorimer (2008)**
```
Lorimer, D. R. (2008)
"Binary and Millisecond Pulsars"
Living Reviews in Relativity, 11, 8
DOI: 10.12942/lrr-2008-8
```

**Comprehensive review:**
- MSP formation
- Binary evolution
- Observational properties
- Population synthesis

**Tauris & van den Heuvel (2006)**
```
Tauris, T. M., & van den Heuvel, E. P. J. (2006)
"Formation of Millisecond Pulsars"
In: Compact Stellar X-ray Sources, 
Cambridge Astrophysics Series 39, pp. 623-665
DOI: 10.1017/CBO9780511536281.017
```

**Detailed evolutionary tracks**

---

### Globular Cluster Pulsars

**Freire et al. (2017)**
```
Freire, P. C. C., et al. (2017)
"Long-term observations of the pulsars in 47 Tucanae - II. 
Proper motions, accelerations and jerks"
Monthly Notices of the Royal Astronomical Society, 471, 857-876
DOI: 10.1093/mnras/stx1533
```

**Why important:**
- MSPs in dense cluster (47 Tuc)
- Proper motions and accelerations
- Cluster potential measurements
- Validation data for simulations

**Ransom et al. (2005)**
```
Ransom, S. M., et al. (2005)
"Twenty-One Millisecond Pulsars in Terzan 5 Using the 
Green Bank Telescope"
Science, 307, 892-896
DOI: 10.1126/science.1108484
```

**Record MSP count in single cluster**

---

## Population Synthesis

**Portegies Zwart & Verbunt (1996)**
```
Portegies Zwart, S. F., & Verbunt, F. (1996)
"Population synthesis of high-mass binaries"
Astronomy & Astrophysics, 309, 179-196
```

**Binary evolution Monte Carlo**

**Pfahl et al. (2003)**
```
Pfahl, E., Rappaport, S., & Podsiadlowski, P. (2003)
"The Galactic Population of Low- and Intermediate-Mass X-ray Binaries"
The Astrophysical Journal, 597, 1036-1048
DOI: 10.1086/378632
```

**LMXB formation channels**

---

## Stellar Evolution

**Hurley et al. (2000)**
```
Hurley, J. R., Pols, O. R., & Tout, C. A. (2000)
"Comprehensive analytic formulae for stellar evolution 
as a function of mass and metallicity"
Monthly Notices of the Royal Astronomical Society, 315, 543-569
DOI: 10.1046/j.1365-8711.2000.03426.x
```

**Used in NBODY6++GPU**

**Hurley et al. (2002)**
```
Hurley, J. R., Tout, C. A., & Pols, O. R. (2002)
"Evolution of binary stars and the effect of tides on 
binary populations"
Monthly Notices of the Royal Astronomical Society, 329, 897-928
DOI: 10.1046/j.1365-8711.2002.05038.x
```

**Binary stellar evolution**

---

## Technical References

### Fortran Programming

**Metcalf et al. (2011)**
```
Metcalf, M., Reid, J., & Cohen, M. (2011)
"Modern Fortran Explained"
Oxford University Press
ISBN: 978-0-19-960142-4
```

**Chapman (2018)**
```
Chapman, S. J. (2018)
"Fortran for Scientists and Engineers (4th ed.)"
McGraw-Hill Education
ISBN: 978-0-07-339233-7
```

---

### CUDA/GPU Programming

**Sanders & Kandrot (2010)**
```
Sanders, J., & Kandrot, E. (2010)
"CUDA by Example: An Introduction to General-Purpose GPU Programming"
Addison-Wesley Professional
ISBN: 978-0-13-138768-3
```

**Nitadori & Aarseth (2012)**
```
Nitadori, K., & Aarseth, S. J. (2012)
"Accelerating NBODY6 with graphics processing units"
Monthly Notices of the Royal Astronomical Society, 424, 545-552
DOI: 10.1111/j.1365-2966.2012.21227.x
```

**GPU acceleration in NBODY6**

---

## Textbooks & Reviews

### Pulsar Astronomy

**Lorimer & Kramer (2004)**
```
Lorimer, D. R., & Kramer, M. (2004)
"Handbook of Pulsar Astronomy"
Cambridge Observing Handbooks for Research Astronomers
Cambridge University Press
ISBN: 978-0-521-82823-9
```

**Comprehensive pulsar reference**

**Lyne & Graham-Smith (2012)**
```
Lyne, A., & Graham-Smith, F. (2012)
"Pulsar Astronomy (4th ed.)"
Cambridge Astrophysics Series 48
Cambridge University Press
ISBN: 978-0-521-83954-9
```

**Classic textbook**

---

### Stellar Dynamics

**Binney & Tremaine (2008)**
```
Binney, J., & Tremaine, S. (2008)
"Galactic Dynamics (2nd ed.)"
Princeton University Press
ISBN: 978-0-691-13026-2
```

**Standard reference**

**Heggie & Hut (2003)**
```
Heggie, D., & Hut, P. (2003)
"The Gravitational Million-Body Problem"
Cambridge University Press
ISBN: 978-0-521-77486-4
```

**N-body methods**

---

## Online Resources

### Databases

**ATNF Pulsar Catalogue:**
http://www.atnf.csiro.au/people/pulsar/psrcat/

**NASA ADS (Astrophysics Data System):**
https://ui.adsabs.harvard.edu/

**arXiv.org (Preprints):**
https://arxiv.org/archive/astro-ph

---

### Code Repositories

**NBODY6++GPU:**
https://github.com/nbody6ppgpu/Nbody6PPGPU-beijing

**This subroutine:**
https://github.com/[your-repo]/pulsar-subroutine-testing

---

## Citation Guidelines

### How to Cite This Work

**For the subroutine itself:**
```
Rah, M., et al. (2025), "Pulsars and Millisecond Pulsars III: 
Tracing Compact Object Dynamics in Globular Clusters with NBODY6++GPU",
Communications of Byurakan Astrophysical Observatory, 72(2), 279-293
DOI: 10.52526/25792776-25.72.2-279
```

**For NBODY6++GPU:**
```
Wang, L., et al. (2015), "NBODY6++GPU: Ready for the gravitational 
million-body problem", MNRAS, 450, 4070-4080
```

**For physics formulations:**
```
Bhattacharya & van den Heuvel (1991) - B-field decay
Alpar et al. (1982) - MSP recycling
Ostriker & Gunn (1969) - Dipole radiation
[others as appropriate]
```

---

## Acknowledgments

This work builds upon decades of theoretical and observational pulsar research. We acknowledge the foundational contributions of:

- **Jocelyn Bell Burnell & Antony Hewish** - Pulsar discovery (1967)
- **Thomas Gold** - Rotating neutron star model (1968)
- **Joseph Taylor & Russell Hulse** - Binary pulsar PSR B1913+16 (1974)
- **All pulsar observers** - Building the empirical database
- **NBODY6++GPU team** - Simulation framework

---

## Version History

**v4.0 (January 2026):**
- Added Maria Rah et al. (2025) Paper III
- Updated coefficient references
- Added validation citations

**v3.0 (December 2025):**
- Initial reference list

---

**Complete bibliography for scientific rigor!** ✅
