"""Debug script to investigate injury detection issue"""
from pyswip import Prolog

print("=" * 70)
print("DEBUGGING INJURY DETECTION ISSUE")
print("=" * 70)

prolog = Prolog()
prolog.consult("workout_rules.pl")
prolog.consult("tests/test_facts.pl")

print("\n[STEP 1] Testing: safe_for_user(e_squats, u_knee_patient)")
result = list(prolog.query("safe_for_user(e_squats, u_knee_patient)"))
print(f"Result: {'SAFE' if len(result) > 0 else 'BLOCKED'}")
print(f"Expected: BLOCKED")
print(f"Query returned {len(result)} solutions: {result}")

print("\n[STEP 2] Checking muscles used by squats")
muscles = list(prolog.query("fact(e_squats, uses_muscle, M)"))
print(f"Muscles used by squats: {[m['M'] for m in muscles]}")

print("\n[STEP 3] Checking user injuries")
injuries = list(prolog.query("user_profile(u_knee_patient, injury(C))"))
print(f"User injuries: {[i['C'] for i in injuries]}")

if injuries:
    condition = injuries[0]['C']
    print(f"\n[STEP 4] Checking injury-muscle mapping for {condition}")
    mappings = list(prolog.query(f"injury_affects_muscle({condition}, M)"))
    print(f"Muscles affected by {condition}: {[m['M'] for m in mappings]}")
    
    print("\n[STEP 5] Checking for overlap")
    used_muscles = [m['M'] for m in muscles]
    affected_muscles = [m['M'] for m in mappings]
    
    print(f"Used by workout: {used_muscles}")
    print(f"Affected by injury: {affected_muscles}")
    
    overlap = set(used_muscles) & set(affected_muscles)
    print(f"Overlap: {overlap}")
    
    if overlap:
        print("✓ There IS overlap → should be BLOCKED")
    else:
        print("✗ No overlap found → that's why it's passing as SAFE")

print("\n[STEP 6] Checking if user has ANY injuries")
has_injury = list(prolog.query("user_profile(u_knee_patient, injury(_))"))
print(f"User has injuries: {len(has_injury) > 0}")

print("\n[STEP 7] Testing the second clause (no injuries)")
print("The second clause of safe_for_user succeeds if user has NO injuries:")
print("safe_for_user(_, UserID) :- \\+ user_profile(UserID, injury(_)).")
no_injury_check = list(prolog.query(r"\+ user_profile(u_knee_patient, injury(_))"))
print(f"Second clause result: {len(no_injury_check) > 0}")
print(f"This should be FALSE since user HAS injuries")

print("\n" + "=" * 70)
print("DIAGNOSIS:")
print("=" * 70)
if len(result) > 0 and len(has_injury) > 0:
    print("❌ ISSUE FOUND: User HAS injuries but safe_for_user still succeeds!")
    print("   This means BOTH clauses might be succeeding incorrectly.")
else:
    print("✓ Logic appears correct")
