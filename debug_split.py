#!/usr/bin/env python3
"""Debug the optimal_split pattern test"""

from pyswip import Prolog

# Initialize Prolog
prolog = Prolog()
prolog.consult("workout_rules.pl")
prolog.consult("tests/test_facts.pl")

print("=" * 60)
print("DEBUGGING: Optimal Split Pattern Test")
print("=" * 60)

# The schedule from test_facts.pl
schedule_str = """[
    day(1, e_chest_press),
    day(2, e_squats),
    day(3, rest),
    day(4, e_lat_pulldown),
    day(5, e_deadlift),
    day(6, rest),
    day(7, rest)
]"""

print(f"\nSchedule: {schedule_str}")

# Test 1: Get the schedule
print("\n1. Testing: test_schedule(optimal_split, Schedule)")
results = list(prolog.query("test_schedule(optimal_split, Schedule)"))
if results:
    print(f"   ✓ Found schedule: {results[0]['Schedule']}")
else:
    print("   ✗ Failed to find schedule")

# Test 2: Check what pattern is detected
print("\n2. Testing: respects_split_pattern(Schedule, Result)")
query = "test_schedule(optimal_split, Schedule), respects_split_pattern(Schedule, Result)"
results = list(prolog.query(query))
print(f"   Results: {results}")
if results:
    for r in results:
        print(f"   Pattern detected: {r.get('Result', 'NONE')}")
else:
    print("   ✗ No patterns found")

# Test 3: Check each exercise's type
print("\n3. Checking exercise types:")
exercises = ['e_chest_press', 'e_squats', 'e_lat_pulldown', 'e_deadlift']
for ex in exercises:
    upper = list(prolog.query(f"is_upper_body({ex})"))
    lower = list(prolog.query(f"is_lower_body({ex})"))
    if upper:
        print(f"   {ex}: UPPER BODY")
    elif lower:
        print(f"   {ex}: LOWER BODY")
    else:
        print(f"   {ex}: UNKNOWN")

# Test 4: Check each pair
print("\n4. Checking exercise pairs in schedule:")
pairs = [
    ('e_chest_press', 'e_squats'),      # Day 1 -> Day 2
    ('e_squats', 'e_lat_pulldown'),     # Day 2 -> Day 4 (skipping rest)
    ('e_lat_pulldown', 'e_deadlift')    # Day 4 -> Day 5
]
for ex1, ex2 in pairs:
    upper1 = list(prolog.query(f"is_upper_body({ex1})"))
    lower1 = list(prolog.query(f"is_lower_body({ex1})"))
    upper2 = list(prolog.query(f"is_upper_body({ex2})"))
    lower2 = list(prolog.query(f"is_lower_body({ex2})"))
    
    type1 = "UPPER" if upper1 else "LOWER" if lower1 else "?"
    type2 = "UPPER" if upper2 else "LOWER" if lower2 else "?"
    alternates = (upper1 and lower2) or (lower1 and upper2)
    
    print(f"   {ex1} ({type1}) -> {ex2} ({type2}): {'✓ ALTERNATES' if alternates else '✗ SAME GROUP'}")

# Test 5: The full failing query
print("\n5. Testing the FULL query from test_rules_python.py:")
query = "test_schedule(optimal_split, Schedule), respects_split_pattern(Schedule, Result), Result = balanced"
results = list(prolog.query(query))
print(f"   Query: {query}")
if results:
    print(f"   ✓ PASS - Results: {results}")
else:
    print("   ✗ FAIL - No results (pattern is not 'balanced')")

print("\n" + "=" * 60)
