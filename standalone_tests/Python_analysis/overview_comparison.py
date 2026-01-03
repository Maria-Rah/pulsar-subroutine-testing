"""
Overview Comparison: All 7 Scenarios
====================================

Creates comparison plots for all scenarios:
- B-field evolution
- Period evolution  
- Pdot evolution
- P-Pdot diagram

Author: Maria Rah
Institution: Byurakan Astrophysical Observatory
Date: January 2026
"""

import numpy as np
import matplotlib.pyplot as plt

def load_all_scenarios():
    """Load output data from all 7 scenarios"""
    scenarios = {}
    
    for i in range(1, 8):
        try:
            data = np.loadtxt(f'scenario{i}_output.dat')
            scenarios[i] = {
                'time': data[:, 0],
                'B': data[:, 1],
                'P': data[:, 2],
                'Pdot': data[:, 3]
            }
        except:
            print(f"⚠️  Warning: scenario{i}_output.dat not found")
    
    return scenarios

def plot_comparison(scenarios, save=True):
    """Create 4-panel comparison figure"""
    
    fig, axes = plt.subplots(2, 2, figsize=(16, 12))
    fig.suptitle('All Scenarios: Evolution Comparison', fontsize=18, fontweight='bold')
    
    colors = ['#E74C3C', '#3498DB', '#2ECC71', '#F39C12', '#9B59B6', '#E67E22', '#1ABC9C']
    labels = ['Sc1: Young Pulsar', 'Sc2: Isolated MSP', 'Sc3: Accreting MSP',
              'Sc4: Dynamic MSP', 'Sc5: Wide Binary MSP', 'Sc6: NS-NS Merger', 'Sc7: NS-BH Merger']
    
    # (a) B-field Evolution
    ax = axes[0, 0]
    for i in range(1, 8):
        if i in scenarios:
            data = scenarios[i]
            ax.plot(data['time'], data['B']/1e11, 'o-', 
                   color=colors[i-1], label=labels[i-1],
                   linewidth=2, markersize=3, markevery=20)
    ax.set_xlabel('Time (Myr)', fontsize=12)
    ax.set_ylabel('B-field (10¹¹ G)', fontsize=12)
    ax.set_title('(a) B-field Evolution: All Scenarios', fontsize=14, fontweight='bold')
    ax.legend(fontsize=10)
    ax.grid(True, alpha=0.3)
    
    # Add annotations
    ax.annotate('Sc6 & Sc7\noverlap\n(identical physics)', 
                xy=(60, 4.5), fontsize=10,
                bbox=dict(boxstyle='round', facecolor='yellow', alpha=0.7))
    ax.annotate('Sc2, Sc3, Sc4, Sc5\noverlap\n(all MSPs, similar B-decay)',
                xy=(40, 0.5), fontsize=10,
                bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.7))
    
    # (b) Period Evolution
    ax = axes[0, 1]
    for i in range(1, 8):
        if i in scenarios:
            data = scenarios[i]
            ax.plot(data['time'], data['P'], 'o-',
                   color=colors[i-1], label=labels[i-1],
                   linewidth=2, markersize=3, markevery=20)
    ax.set_xlabel('Time (Myr)', fontsize=12)
    ax.set_ylabel('Period (s)', fontsize=12)
    ax.set_title('(b) Period Evolution: All Scenarios', fontsize=14, fontweight='bold')
    ax.legend(fontsize=10)
    ax.grid(True, alpha=0.3)
    
    # Add annotations
    ax.annotate('Sc6 & Sc7\noverlap', 
                xy=(30, 1.1), fontsize=10,
                bbox=dict(boxstyle='round', facecolor='yellow', alpha=0.7))
    ax.annotate('Sc2 & Sc5\nnearly overlap\n(isolated vs wide binary)',
                xy=(60, 0.15), fontsize=10,
                bbox=dict(boxstyle='round', facecolor='lightgreen', alpha=0.7))
    
    # (c) Pdot Evolution
    ax = axes[1, 0]
    for i in range(1, 8):
        if i in scenarios:
            data = scenarios[i]
            ax.plot(data['time'], np.abs(data['Pdot']), 'o-',
                   color=colors[i-1], label=labels[i-1],
                   linewidth=2, markersize=3, markevery=20)
    ax.set_xlabel('Time (Myr)', fontsize=12)
    ax.set_ylabel('|Pdot| (s/s)', fontsize=12)
    ax.set_title('(c) Pdot Evolution: All Scenarios', fontsize=14, fontweight='bold')
    ax.set_yscale('log')
    ax.legend(fontsize=10)
    ax.grid(True, alpha=0.3, which='both')
    
    # Add annotations
    ax.annotate('Sc6 & Sc7\noverlap', 
                xy=(70, 2e-15), fontsize=10,
                bbox=dict(boxstyle='round', facecolor='yellow', alpha=0.7))
    ax.annotate('Sc2, Sc4, Sc5\nclose together\n(MSPs)',
                xy=(40, 5e-20), fontsize=10,
                bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.7))
    
    # (d) P-Pdot Diagram
    ax = axes[1, 1]
    for i in range(1, 8):
        if i in scenarios:
            data = scenarios[i]
            ax.scatter(data['P'], np.abs(data['Pdot']),
                      c=colors[i-1], label=labels[i-1],
                      s=50, alpha=0.7, edgecolors='black', linewidth=0.8)
    ax.set_xlabel('Period (s)', fontsize=12)
    ax.set_ylabel('|Pdot| (s/s)', fontsize=12)
    ax.set_title('(d) All Scenarios: P-Pdot Diagram', fontsize=14, fontweight='bold')
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.legend(fontsize=10)
    ax.grid(True, alpha=0.3, which='both')
    
    plt.tight_layout()
    
    if save:
        plt.savefig('comparison_bfield.png', dpi=200, bbox_inches='tight')
        plt.savefig('comparison_period.png', dpi=200, bbox_inches='tight')
        plt.savefig('comparison_pdot.png', dpi=200, bbox_inches='tight')
        plt.savefig('comparison_ppdot.png', dpi=200, bbox_inches='tight')
        print("\n✓ Comparison figures saved!")
    
    return fig

if __name__ == "__main__":
    print("="*60)
    print("OVERVIEW COMPARISON: All 7 Scenarios")
    print("="*60)
    
    # Load all data
    print("\nLoading scenario data...")
    scenarios = load_all_scenarios()
    print(f"✓ Loaded {len(scenarios)} scenarios")
    
    # Create comparison plots
    print("\nCreating comparison plots...")
    plot_comparison(scenarios, save=True)
    
    # Summary statistics
    print("\n" + "="*60)
    print("SUMMARY STATISTICS")
    print("="*60)
    
    for i in range(1, 8):
        if i in scenarios:
            data = scenarios[i]
            print(f"\nScenario {i}:")
            print(f"  Initial: B={data['B'][0]:.2e} G, P={data['P'][0]*1000:.2f} ms")
            print(f"  Final:   B={data['B'][-1]:.2e} G, P={data['P'][-1]*1000:.2f} ms")
            print(f"  ΔP = {(data['P'][-1]-data['P'][0])*1000:.2f} ms")
    
    print("\n" + "="*60)
    print("✓ Overview comparison complete!")
    print("="*60)
