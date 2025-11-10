"""Check why findall is not finding muscles"""
from pyswip import Prolog

prolog = Prolog()
prolog.consult("workout_rules.pl")
prolog.consult("tests/test_facts.pl")

print("Testing findall for e_squats muscles:")
result = list(prolog.query("findall(Muscle, fact(e_squats, uses_muscle, Muscle), UsedMuscles)"))
print(f"Result: {result}")

print("\nDirect query:")
result2 = list(prolog.query("fact(e_squats, uses_muscle, M)"))
print(f"Direct: {result2}")

print("\nChecking the actual safe_for_user logic step by step:")
query = """
    findall(Muscle, fact(e_squats, uses_muscle, Muscle), UsedMuscles),
    findall(InjuryMuscle, (
        user_profile(u_knee_patient, injury(Condition)),
        injury_affects_muscle(Condition, InjuryMuscle)
    ), InjuredMuscles),
    member(M, UsedMuscles),
    member(M, InjuredMuscles)
"""
result3 = list(prolog.query(query))
print(f"Overlap found: {result3}")
print(f"Should block if overlap exists: {len(result3) > 0}")
