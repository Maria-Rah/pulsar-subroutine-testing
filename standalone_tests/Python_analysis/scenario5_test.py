"""
Scenario 5: Wide Binary MSP - Standalone Test
=============================================

Tests pulsar subroutine physics for wide binary MSP:
- Weak magnetic field (B₀ = 4×10⁸ G)
- Fast rotation (P₀ = 3.5 ms)
- Binary with WD companion, no accretion

Author: Maria Rah
Institution: Byurakan Astrophysical Observatory
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

def evolve_wide_binary(P0=0.0035, B0=4e8, rho=1e3, vrel=1e5, t_max=100.0, dt=0.1):
    """Evolve wide binary MSP"""
    
    dt_sec = dt * 1e6 * 365.25 * 24 * 3600
    t_max_sec = t_max * 1e6 * 365.25 * 24 * 3600
    n_steps = int(t_max_sec / dt_sec) + 1
    
    time = np.zeros(n_steps)
    B = np.zeros(n_steps)
    P = np.zeros(n_steps)
    Pdot = np.zeros(n_steps)
    Pdot_dipole = np.zeros(n_steps)
    Pdot_env = np.zeros(n_steps)
    
    B[0] = B0
    P[0] = P0
    
    for i in range(1, n_steps):
        t = i * dt_sec
        time[i] = t / (1e6 * 365.25 * 24 * 3600)
        
        # B-field decay
        B[i] = B0 * np.exp(-t / XKTAU)
        
        # Dipole spin-down
        alpha = PI / 2.0
        Pdot_dipole[i] = XKFIELD * (B[i]**2) * (YI**6) * (np.sin(alpha)**2) / (XK * CLIGHT**3 * P[i-1])
        
        # Environmental
        Pdot_env[i] = XKENV * rho * vrel * (P[i-1]**2)
        
        # Total
        Pdot[i] = Pdot_dipole[i] + Pdot_env[i]
        P[i] = P[i-1] + Pdot[i] * dt_sec
    
    return {
        'time': time, 'B': B, 'P': P, 'Pdot': Pdot,
        'Pdot_dipole': Pdot_dipole, 'Pdot_env': Pdot_env,
        'B_norm': B / B0, 'P_norm': P / P0
    }

if __name__ == "__main__":
    print("="*60)
    print("Scenario 5: Wide Binary MSP")
    print("="*60)
    
    data = evolve_wide_binary()
    
    print(f"\nInitial: B={data['B'][0]:.2e} G, P={data['P'][0]*1000:.2f} ms")
    print(f"Final:   B={data['B'][-1]:.2e} G, P={data['P'][-1]*1000:.2f} ms")
    print(f"ΔP = {(data['P'][-1]-data['P'][0])*1000:.2f} ms")
    
    output = np.column_stack([
        data['time'], data['B'], data['P'], data['Pdot'],
        data['Pdot_dipole'], data['Pdot_env'],
        data['B_norm'], data['P_norm']
    ])
    np.savetxt('scenario5_output.dat', output, fmt='%.6e')
    print("\n✓ Data saved: scenario5_output.dat")
    print("="*60)
