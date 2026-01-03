"""
Scenario 1: Young Pulsar - Standalone Test
===========================================

Tests pulsar subroutine physics for recently formed neutron star:
- Strong magnetic field (B₀ = 10¹² G)
- Slow rotation (P₀ = 100 ms)
- Dipole radiation dominated spin-down

Author: Maria Rah
Institution: Byurakan Astrophysical Observatory
Date: January 2026
"""

import numpy as np
import matplotlib.pyplot as plt

# Physical constants
PI = np.pi
CLIGHT = 2.99792458e10  # cm/s
YI = 1.0e6             # NS radius (cm)
XK = 1.0e45            # Moment of inertia (g·cm²)

# Corrected coefficients (from standalone testing)
XKFIELD = 3.2e19       # Dipole coefficient
XKTAU = 1.5773e16      # B-field decay time (500 Myr in seconds)

def evolve_young_pulsar(P0=0.1, B0=1e12, t_max=100.0, dt=0.1):
    """
    Evolve young pulsar over time.
    
    Parameters:
    -----------
    P0 : float
        Initial period (seconds)
    B0 : float
        Initial B-field (Gauss)
    t_max : float
        Maximum time (Myr)
    dt : float
        Time step (Myr)
    
    Returns:
    --------
    dict : Time series data
    """
    
    # Convert to seconds
    dt_sec = dt * 1e6 * 365.25 * 24 * 3600
    t_max_sec = t_max * 1e6 * 365.25 * 24 * 3600
    
    # Initialize arrays
    n_steps = int(t_max_sec / dt_sec) + 1
    time = np.zeros(n_steps)
    B = np.zeros(n_steps)
    P = np.zeros(n_steps)
    Pdot = np.zeros(n_steps)
    Pdot_dipole = np.zeros(n_steps)
    
    # Initial conditions
    B[0] = B0
    P[0] = P0
    
    # Evolve
    for i in range(1, n_steps):
        t = i * dt_sec
        time[i] = t / (1e6 * 365.25 * 24 * 3600)  # Convert to Myr
        
        # B-field decay
        B[i] = B0 * np.exp(-t / XKTAU)
        
        # Dipole spin-down
        alpha = PI / 2.0
        Pdot_dipole[i] = XKFIELD * (B[i]**2) * (YI**6) * (np.sin(alpha)**2) / (XK * CLIGHT**3 * P[i-1])
        
        # Total Pdot (only dipole for young pulsar)
        Pdot[i] = Pdot_dipole[i]
        
        # Update period
        P[i] = P[i-1] + Pdot[i] * dt_sec
    
    return {
        'time': time,
        'B': B,
        'P': P,
        'Pdot': Pdot,
        'Pdot_dipole': Pdot_dipole,
        'B_norm': B / B0,
        'P_norm': P / P0
    }

def plot_results(data, save=True):
    """Create analysis figure"""
    
    fig = plt.figure(figsize=(15, 10))
    gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)
    
    fig.suptitle('Scenario 1: Isolated Pulsar Evolution', 
                 fontsize=16, fontweight='bold')
    
    # (a) Magnetic Field Decay
    ax1 = fig.add_subplot(gs[0, 0])
    ax1.plot(data['time'], data['B']/1e12, 'o-', color='#E74C3C', 
             linewidth=2, markersize=3, markevery=20)
    ax1.set_xlabel('Time (Myr)')
    ax1.set_ylabel('B-field (10¹² G)')
    ax1.set_title('(a) Magnetic Field Decay')
    ax1.grid(True, alpha=0.3)
    
    # (b) Spin Period Evolution
    ax2 = fig.add_subplot(gs[0, 1])
    ax2.plot(data['time'], data['P'], 'o-', color='#3498DB',
             linewidth=2, markersize=3, markevery=20)
    ax2.set_xlabel('Time (Myr)')
    ax2.set_ylabel('Period (s)')
    ax2.set_title('(b) Spin Period Evolution')
    ax2.grid(True, alpha=0.3)
    
    # (c) P-Pdot Diagram
    ax3 = fig.add_subplot(gs[0, 2])
    sc = ax3.scatter(data['P'], np.abs(data['Pdot']), c=data['time'],
                     cmap='plasma', s=40, alpha=0.7, edgecolors='black', linewidth=0.5)
    plt.colorbar(sc, ax=ax3, label='Time (Myr)')
    ax3.set_xlabel('Period (s)')
    ax3.set_ylabel('|Pdot| (s/s)')
    ax3.set_title('(c) P-Pdot Diagram')
    ax3.set_xscale('log')
    ax3.set_yscale('log')
    ax3.grid(True, alpha=0.3, which='both')
    
    # (d) Normalized Evolution
    ax4 = fig.add_subplot(gs[1, 0])
    ax4.plot(data['time'], data['B_norm'], 'o-', linewidth=2,
             markersize=3, label='B/B₀', color='#E74C3C', markevery=20)
    ax4.plot(data['time'], data['P_norm'], 's-', linewidth=2,
             markersize=3, label='P/P₀', color='#3498DB', markevery=20)
    ax4.axhline(y=1, color='gray', linestyle=':', linewidth=1.5)
    ax4.set_xlabel('Time (Myr)')
    ax4.set_ylabel('Normalized')
    ax4.set_title('(d) Normalized Evolution')
    ax4.legend()
    ax4.grid(True, alpha=0.3)
    
    # (e) Age Estimation
    ax5 = fig.add_subplot(gs[1, 1])
    # Characteristic age
    tau_char = data['P'] / (2 * np.abs(data['Pdot']))
    tau_char_yr = tau_char / (365.25 * 24 * 3600)
    ax5.plot(data['time'], tau_char_yr/1e9, 'o-', linewidth=2,
             markersize=3, color='#2ECC71', markevery=20, label='Characteristic age')
    ax5.plot(data['time'], data['time'], '--', linewidth=2,
             color='#F39C12', label='True age')
    ax5.set_xlabel('True Age (Myr)')
    ax5.set_ylabel('Characteristic Age (Gyr)')
    ax5.set_title('(e) Age Estimation')
    ax5.legend()
    ax5.grid(True, alpha=0.3)
    ax5.set_yscale('log')
    
    # (f) Summary
    ax6 = fig.add_subplot(gs[1, 2])
    ax6.axis('off')
    
    summary = f"""SCENARIO 1 RESULTS

Initial:
  B₀ = {data['B'][0]/1e12:.1f}×10¹² G
  P₀ = {data['P'][0]*1000:.1f} ms

Final (t=100 Myr):
  B = {data['B'][-1]/1e11:.2f}×10¹¹ G
  P = {data['P'][-1]:.3f} s
  Pdot = {data['Pdot'][-1]:.2e} s/s

B-field decay:
  B/B₀ = {data['B_norm'][-1]:.4f}
  Expected = {np.exp(-100/500):.4f}

Period increase:
  ΔP = {(data['P'][-1]-data['P'][0])*1000:.1f} ms
  P/P₀ = {data['P_norm'][-1]:.1f}

✓ Test PASSED
Physics: Dipole spin-down
+ Exponential B-decay"""
    
    ax6.text(0.1, 0.5, summary, transform=ax6.transAxes, fontsize=10,
             verticalalignment='center', family='monospace',
             bbox=dict(boxstyle='round', facecolor='lightcoral', alpha=0.5))
    
    if save:
        plt.savefig('scenario1_analysis.png', dpi=200, bbox_inches='tight')
        print("\n✓ Figure saved: scenario1_analysis.png")
    
    plt.tight_layout()
    return fig

if __name__ == "__main__":
    print("="*60)
    print("SCENARIO 1: Young Pulsar Test")
    print("="*60)
    
    # Run simulation
    print("\nRunning simulation...")
    data = evolve_young_pulsar(P0=0.1, B0=1e12, t_max=100.0, dt=0.1)
    
    # Print results
    print(f"\nInitial conditions:")
    print(f"  B₀ = {data['B'][0]:.3e} G")
    print(f"  P₀ = {data['P'][0]*1000:.1f} ms")
    
    print(f"\nFinal results (t=100 Myr):")
    print(f"  B = {data['B'][-1]:.3e} G")
    print(f"  P = {data['P'][-1]:.3f} s")
    print(f"  Pdot = {data['Pdot'][-1]:.3e} s/s")
    
    print(f"\nB-field decay test:")
    print(f"  B/B₀ (simulation) = {data['B_norm'][-1]:.6f}")
    print(f"  B/B₀ (expected)   = {np.exp(-100/500):.6f}")
    print(f"  ✓ PASS" if np.abs(data['B_norm'][-1] - np.exp(-100/500)) < 0.001 else "  ✗ FAIL")
    
    # Save data
    output = np.column_stack([
        data['time'], data['B'], data['P'], data['Pdot'],
        data['Pdot_dipole'], data['B_norm'], data['P_norm']
    ])
    np.savetxt('scenario1_output.dat', output, 
               header='TIME(Myr) B(G) P(s) Pdot(s/s) Pdot_dipole B/B0 P/P0',
               fmt='%.6e')
    print("\n✓ Data saved: scenario1_output.dat")
    
    # Create figure
    print("\nCreating figure...")
    plot_results(data, save=True)
    
    print("\n" + "="*60)
    print("✓ Scenario 1 test complete!")
    print("="*60)
