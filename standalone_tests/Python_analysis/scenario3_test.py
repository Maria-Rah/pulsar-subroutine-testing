"""
Scenario 3: Accreting MSP - Standalone Test
===========================================

Tests pulsar subroutine physics for accreting MSP:
- Weak magnetic field (B₀ = 3×10⁸ G)
- Fast rotation (P₀ = 2.5 ms)
- Active accretion causing spin-UP

Author: Maria Rah
Date: January 2026
"""

import numpy as np
import matplotlib.pyplot as plt

PI = np.pi
CLIGHT = 2.99792458e10
YI = 1.0e6
XK = 1.0e45

XKFIELD = 3.2e19
XKTAU = 1.5773e16
XKACC = 2.0e-16      # CORRECTED
XKENV = 5.0e-29      # CORRECTED

def evolve_accreting_msp(P0=0.0025, B0=3e8, Mdot=1e-10, rho=1e4, vrel=1e6, t_max=100.0, dt=0.1):
    """Evolve accreting MSP with spin-up"""
    
    dt_sec = dt * 1e6 * 365.25 * 24 * 3600
    t_max_sec = t_max * 1e6 * 365.25 * 24 * 3600
    
    n_steps = int(t_max_sec / dt_sec) + 1
    time = np.zeros(n_steps)
    B = np.zeros(n_steps)
    P = np.zeros(n_steps)
    Pdot = np.zeros(n_steps)
    Pdot_dipole = np.zeros(n_steps)
    Pdot_acc = np.zeros(n_steps)
    Pdot_env = np.zeros(n_steps)
    
    B[0] = B0
    P[0] = P0
    
    for i in range(1, n_steps):
        t = i * dt_sec
        time[i] = t / (1e6 * 365.25 * 24 * 3600)
        
        B[i] = B0 * np.exp(-t / XKTAU)
        
        alpha = PI / 2.0
        Pdot_dipole[i] = XKFIELD * (B[i]**2) * (YI**6) * (np.sin(alpha)**2) / (XK * CLIGHT**3 * P[i-1])
        
        Pdot_acc[i] = -XKACC * Mdot * P[i-1]  # Spin-UP (negative)
        Pdot_env[i] = XKENV * rho * vrel * (P[i-1]**2)
        
        Pdot[i] = Pdot_dipole[i] + Pdot_acc[i] + Pdot_env[i]
        
        P[i] = P[i-1] + Pdot[i] * dt_sec
    
    return {
        'time': time, 'B': B, 'P': P, 'Pdot': Pdot,
        'Pdot_dipole': Pdot_dipole, 'Pdot_acc': Pdot_acc, 'Pdot_env': Pdot_env,
        'B_norm': B / B0, 'P_norm': P / P0
    }

if __name__ == "__main__":
    print("Scenario 3: Accreting MSP")
    data = evolve_accreting_msp()
    
    output = np.column_stack([data['time'], data['B'], data['P'], data['Pdot'],
                               data['Pdot_dipole'], data['Pdot_acc'], data['Pdot_env'],
                               data['B_norm'], data['P_norm']])
    np.savetxt('scenario3_output.dat', output, fmt='%.6e')
    print("✓ Data saved: scenario3_output.dat")
    print(f"✓ Net SPIN-UP: ΔP = {(data['P'][-1]-data['P'][0])*1000:.2f} ms (negative = spin-up)")
