"""
Scenario 2: Isolated MSP - Standalone Test
==========================================

Tests pulsar subroutine physics for recycled millisecond pulsar:
- Weak magnetic field (B₀ = 5×10⁸ G)
- Fast rotation (P₀ = 3 ms)
- Post-accretion, now isolated

Author: Maria Rah
Institution: Byurakan Astrophysical Observatory
Date: January 2026
"""

import numpy as np
import matplotlib.pyplot as plt

# Physical constants
PI = np.pi
CLIGHT = 2.99792458e10
YI = 1.0e6
XK = 1.0e45

# Corrected coefficients
XKFIELD = 3.2e19
XKTAU = 1.5773e16

def evolve_isolated_msp(P0=0.003, B0=5e8, t_max=100.0, dt=0.1):
    """Evolve isolated MSP"""
    
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
        
        alpha = PI / 2.0
        Pdot[i] = XKFIELD * (B[i]**2) * (YI**6) * (np.sin(alpha)**2) / (XK * CLIGHT**3 * P[i-1])
        
        P[i] = P[i-1] + Pdot[i] * dt_sec
    
    return {
        'time': time, 'B': B, 'P': P, 'Pdot': Pdot,
        'B_norm': B / B0, 'P_norm': P / P0
    }

if __name__ == "__main__":
    print("Scenario 2: Isolated MSP")
    data = evolve_isolated_msp()
    
    output = np.column_stack([data['time'], data['B'], data['P'], data['Pdot'],
                               data['B_norm'], data['P_norm']])
    np.savetxt('scenario2_output.dat', output, fmt='%.6e')
    print("✓ Data saved: scenario2_output.dat")
