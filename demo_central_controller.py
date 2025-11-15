"""
🧠 CENTRAL CONTROLLER - Complete System Demo
Demonstrates the full AI Fitness Coach with all brains connected
"""

import sys
import os
from pathlib import Path

# Set UTF-8 encoding for Windows
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

# Add paths
sys.path.append(str(Path(__file__).parent))

from central_controller import CentralController

def main():
    print("\n" + "=" * 80)
    print("CENTRAL CONTROLLER - AI FITNESS COACH")
    print("=" * 80)
    print("\nThe Brainstem that coordinates all AI units:\n")
    print("  NLP Brain        -> Understands emotion & intent")
    print("  Logic Brain      -> Checks safety rules (Prolog)")
    print("  ML Brain         -> Predicts optimal workout")
    print("  Personality Brain -> Generates Coach Atlas response")
    print("\n" + "=" * 80)
    
    # Simple demo without full initialization
    print("\n Creating Central Controller...")
    controller = CentralController()
    
    print("\n" + "=" * 80)
    print("DEMO SCENARIOS")
    print("=" * 80)
    
    scenarios = [
        "I want to train chest today!",
        "I'm exhausted but still want to workout",
        "My shoulder is hurting badly"
    ]
    
    for i, msg in enumerate(scenarios, 1):
        print(f"\n--- Scenario {i} ---")
        print(f"User: {msg}")
        
        decision = controller.process(msg)
        
        print(f"\nResult:")
        print(f"  Workout: {decision.workout_recommendation}")
        print(f"  Safety: {decision.safety_status}")
        print(f"  Response: {decision.final_response[:100]}...")
        print(f"  Time: {decision.total_execution_time_ms:.1f}ms")
    
    # Show statistics
    print("\n" + "=" * 80)
    print("STATISTICS")
    print("=" * 80)
    stats = controller.get_stats()
    print(f"  Total Decisions: {stats['total_decisions']}")
    print(f"  Average Time: {stats['average_response_time_ms']:.1f}ms")
    print(f"  Safety Interventions: {stats['safety_interventions']}")
    
    print("\n" + "=" * 80)
    print("SUCCESS! Central Controller is operational.")
    print("=" * 80 + "\n")

if __name__ == "__main__":
    main()
