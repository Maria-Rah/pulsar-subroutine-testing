"""
Scenario 4: Dynamic MSP - Standalone Test
=========================================

Tests pulsar subroutine physics for dynamically formed MSP:
- Weak magnetic field (B₀ = 3×10⁸ G)
- Fast rotation (P₀ = 4 ms)
- Dense environment + dynamical effects

Author: Maria Rah
Date: January 2026
"""

import numpy as np

PI = np.pi
CLIGHT = 2.99792458e10
YI = 1.0e6
XK = 1.0e45

XKFIELD = 3.2e19
XKTAU = 1.5773e16
XKENV = 5.0e-29
XKDYN = 1.0e-22      # CORRECTED
XKASYM = 1.0e-27     # CORRECTED

def evolve_dynamic_msp(P0=0.004, B0=3e8, rho=1e4, vrel=1e6, vkick=250e5, t_max=100.0, dt=0.1):
    """Evolve dynamic MSP"""
    
    dt_sec = dt * 1e6 * 365.25 * 24 * 3600
    t_max_sec = t_max * 1e6 * 365.25 * 24 * 3600
    
    n_steps = int(t_max_sec / dt_sec) + 1
    time = np.zeros(n_steps)
    B = np.zeros(n_steps)
    P = np.zeros(n_steps)
    Pdot = np.zeros(n_steps)
    
    B[0] = B0
    P[0] = P0
    
    for i in range(1, n_steps):
        t = i * dt_sec
        time[i] = t / (1e6 * 365.25 * 24 * 3600)
        
        B[i] = B0 * np.exp(-t / XKTAU)
        
        Pdot_dipole = XKFIELD * (B[i]**2) * (YI**6) * (np.sin(PI/2)**2) / (XK * CLIGHT**3 * P[i-1])
        Pdot_env = XKENV * rho * vrel * (P[i-1]**2)
        Pdot_dyn = XKDYN * rho * (vkick**2) * P[i-1]
        Pdot_asym = XKASYM * (vkick**4) * P[i-1]
        
        Pdot[i] = Pdot_dipole + Pdot_env + Pdot_dyn + Pdot_asym
        P[i] = P[i-1] + Pdot[i] * dt_sec
    
    return {'time': time, 'B': B, 'P': P, 'Pdot': Pdot, 'B_norm': B/B0, 'P_norm': P/P0}

if __name__ == "__main__":
    print("Scenario 4: Dynamic MSP")
    data = evolve_dynamic_msp()
    output = np.column_stack([data['time'], data['B'], data['P'], data['Pdot'],
                               data['B_norm'], data['P_norm']])
    np.savetxt('scenario4_output.dat', output, fmt='%.6e')
    print("✓ Data saved: scenario4_output.dat")
