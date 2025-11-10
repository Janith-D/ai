#!/usr/bin/env python3
"""
pyswip_demo.py - Quick Python ↔ Prolog integration example using PySwip

This demo shows how to:
1. Load the fitness KB from Python
2. Query exercises by muscle group
3. Find substitutions for exercises
4. Check contraindications
5. Get exercise details

Prerequisites:
  pip install pyswip

Note: PySwip requires SWI-Prolog to be installed and on PATH.
"""

from pyswip import Prolog


def demo_basic_queries():
    """Demonstrate basic queries to the fitness KB."""
    print("=" * 60)
    print("PySwip Integration Demo - Fitness KB Queries")
    print("=" * 60)
    
    # Initialize Prolog engine
    prolog = Prolog()
    
    # Load the knowledge base
    print("\n1. Loading knowledge base...")
    prolog.consult("kb_generated.pl")
    print("   ✓ KB loaded successfully")
    
    # Query 1: Count total exercises
    print("\n2. Counting exercises...")
    count = 0
    for _ in prolog.query("entity(ID, exercise, _)"):
        count += 1
    print(f"   Found {count} exercises in KB")
    
    # Query 2: Find exercises that use quadriceps
    print("\n3. Finding exercises that use quadriceps...")
    quad_exercises = []
    for result in prolog.query("fact(ID, uses_muscle, m_quadriceps), entity(ID, exercise, Name)"):
        quad_exercises.append((result['ID'], result['Name']))
    
    print(f"   Found {len(quad_exercises)} exercises using quadriceps")
    for ex_id, ex_name in quad_exercises[:5]:  # Show first 5
        print(f"     - {ex_id}: {ex_name}")
    if len(quad_exercises) > 5:
        print(f"     ... and {len(quad_exercises) - 5} more")
    
    # Query 3: Find substitutions for a specific exercise
    print("\n4. Finding substitutions for exercise e_ex_001...")
    substitutions = []
    for result in prolog.query("fact(SubID, substitution_for, e_ex_001), entity(SubID, exercise, Name)"):
        substitutions.append((result['SubID'], result['Name']))
    
    if substitutions:
        print(f"   Found {len(substitutions)} substitution(s):")
        for sub_id, sub_name in substitutions:
            print(f"     - {sub_id}: {sub_name}")
    else:
        print("   No substitutions found")
    
    # Query 4: Check contraindications
    print("\n5. Finding exercises contraindicated for knee injuries...")
    contraindicated = []
    for result in prolog.query("fact(ID, contraindicated_for, cond_knee), entity(ID, exercise, Name)"):
        contraindicated.append((result['ID'], result['Name']))
    
    print(f"   Found {len(contraindicated)} exercises contraindicated for knee injuries")
    for ex_id, ex_name in contraindicated[:5]:  # Show first 5
        print(f"     - {ex_id}: {ex_name}")
    if len(contraindicated) > 5:
        print(f"     ... and {len(contraindicated) - 5} more")
    
    # Query 5: Get exercise attributes
    print("\n6. Getting attributes for exercise e_ex_001...")
    for result in prolog.query("entity(e_ex_001, exercise, Name)"):
        print(f"   Name: {result['Name']}")
    
    # Get difficulty
    for result in prolog.query("attr(e_ex_001, difficulty, Diff)"):
        print(f"   Difficulty: {result['Diff']}")
    
    # Get muscles used
    muscles = []
    for result in prolog.query("fact(e_ex_001, uses_muscle, MuscleID), entity(MuscleID, muscle, MuscleName)"):
        muscles.append(result['MuscleName'])
    if muscles:
        print(f"   Muscles: {', '.join(muscles)}")
    
    # Get equipment
    equipment = []
    for result in prolog.query("fact(e_ex_001, requires_equipment, EqID), entity(EqID, equipment, EqName)"):
        equipment.append(result['EqName'])
    if equipment:
        print(f"   Equipment: {', '.join(equipment)}")
    
    print("\n" + "=" * 60)
    print("Demo completed successfully!")
    print("=" * 60)


def demo_advanced_query():
    """Demonstrate a more complex query combining multiple conditions."""
    print("\n\n" + "=" * 60)
    print("Advanced Query Example")
    print("=" * 60)
    
    prolog = Prolog()
    prolog.consult("kb_generated.pl")
    
    # Find beginner exercises that use quadriceps and require no equipment
    print("\nFinding beginner exercises using quadriceps with no equipment...")
    
    query = """
        fact(ID, uses_muscle, m_quadriceps),
        attr(ID, difficulty, beginner),
        fact(ID, requires_equipment, eq_none),
        entity(ID, exercise, Name)
    """
    
    results = list(prolog.query(query))
    
    if results:
        print(f"Found {len(results)} exercise(s):")
        for result in results:
            print(f"  - {result['ID']}: {result['Name']}")
    else:
        print("No exercises found matching criteria")


def demo_python_function():
    """Show how to wrap Prolog queries in Python functions."""
    print("\n\n" + "=" * 60)
    print("Python Function Wrapper Example")
    print("=" * 60)
    
    prolog = Prolog()
    prolog.consult("kb_generated.pl")
    
    def get_exercises_by_muscle(muscle_id: str) -> list:
        """Get all exercises that use a specific muscle."""
        exercises = []
        query = f"fact(ID, uses_muscle, {muscle_id}), entity(ID, exercise, Name)"
        for result in prolog.query(query):
            exercises.append({
                'id': result['ID'],
                'name': result['Name']
            })
        return exercises
    
    def is_contraindicated(exercise_id: str, condition_id: str) -> bool:
        """Check if an exercise is contraindicated for a condition."""
        query = f"fact({exercise_id}, contraindicated_for, {condition_id})"
        return len(list(prolog.query(query))) > 0
    
    # Test the functions
    print("\nTesting get_exercises_by_muscle('m_chest')...")
    chest_exercises = get_exercises_by_muscle('m_chest')
    print(f"Found {len(chest_exercises)} chest exercises")
    for ex in chest_exercises[:3]:
        print(f"  - {ex['id']}: {ex['name']}")
    
    print("\nTesting is_contraindicated('e_ex_001', 'cond_knee')...")
    contraindicated = is_contraindicated('e_ex_001', 'cond_knee')
    print(f"  Result: {'Yes' if contraindicated else 'No'}")


if __name__ == '__main__':
    try:
        demo_basic_queries()
        demo_advanced_query()
        demo_python_function()
        
        print("\n✅ All demos completed successfully!")
        print("\nNext steps:")
        print("  - Wrap queries in a Python API class")
        print("  - Add caching for frequently used queries")
        print("  - Create a recommendation engine using these queries")
        print("  - Build a FastAPI endpoint that exposes this functionality")
        
    except Exception as e:
        print(f"\n❌ Error: {e}")
        print("\nTroubleshooting:")
        print("  1. Ensure SWI-Prolog is installed and on PATH")
        print("  2. Install PySwip: pip install pyswip")
        print("  3. Make sure kb_generated.pl is in the current directory")
        raise
