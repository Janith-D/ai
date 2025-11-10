"""Trace which clause of safe_for_user is being used"""
from pyswip import Prolog

prolog = Prolog()

# Reload with trace
prolog.consult("workout_rules.pl")
prolog.consult("tests/test_facts.pl")

print("Testing individual conditions:\n")

print("1. Does user have injuries?")
has_injuries = list(prolog.query("user_profile(u_knee_patient, injury(_))"))
print(f"   Result: {'YES' if has_injuries else 'NO'}")

print("\n2. First clause check (no injuries):")
first_clause = list(prolog.query(r"\+ user_profile(u_knee_patient, injury(_))"))
print(f"   Result: {'SUCCEEDS' if first_clause else 'FAILS'}")
print(f"   (Should FAIL because user HAS injuries)")

print("\n3. Second clause check (muscle overlap):")
overlap_check = list(prolog.query("""
    findall(Muscle, fact(e_squats, uses_muscle, Muscle), UsedMuscles),
    findall(InjuryMuscle, (
        user_profile(u_knee_patient, injury(Condition)),
        injury_affects_muscle(Condition, InjuryMuscle)
    ), InjuredMuscles),
    member(M, UsedMuscles),
    member(M, InjuredMuscles)
"""))
print(f"   Overlap found: {'YES' if overlap_check else 'NO'}")
print(f"   Muscles: {overlap_check[0] if overlap_check else 'None'}")

print("\n4. Negation of overlap:")
no_overlap = list(prolog.query(r"""
    findall(Muscle, fact(e_squats, uses_muscle, Muscle), UsedMuscles),
    findall(InjuryMuscle, (
        user_profile(u_knee_patient, injury(Condition)),
        injury_affects_muscle(Condition, InjuryMuscle)
    ), InjuredMuscles),
    \+ (member(M, UsedMuscles), member(M, InjuredMuscles))
"""))
print(f"   No overlap: {'YES' if no_overlap else 'NO'}")
print(f"   (Should be NO because there IS overlap)")

print("\n5. Full safe_for_user query:")
safe = list(prolog.query("safe_for_user(e_squats, u_knee_patient)"))
print(f"   Result: {'SAFE' if safe else 'BLOCKED'}")
print(f"   Solutions: {len(safe)}")

print("\n6. Let's check if UsedMuscles is being populated:")
used_muscles_check = list(prolog.query("findall(Muscle, fact(e_squats, uses_muscle, Muscle), UsedMuscles), UsedMuscles \\= []"))
print(f"   UsedMuscles not empty: {'YES' if used_muscles_check else 'NO'}")
if used_muscles_check:
    print(f"   Muscles: {used_muscles_check[0]}")
