"""Test the helper predicate directly"""
from pyswip import Prolog

prolog = Prolog()
prolog.consult("workout_rules.pl")
prolog.consult("tests/test_facts.pl")

print("Testing has_muscle_overlap helper:")
result = list(prolog.query("has_muscle_overlap([m_quadriceps], [m_quadriceps, m_hamstrings, m_calves])"))
print(f"Overlap found: {'YES' if result else 'NO'}")

print("\nTesting negation of has_muscle_overlap:")
result2 = list(prolog.query(r"\+ has_muscle_overlap([m_quadriceps], [m_quadriceps, m_hamstrings, m_calves])"))
print(f"No overlap: {'YES' if result2 else 'NO'}")
print("(Should be NO because there IS overlap)")

print("\nTesting the full second clause manually:")
query = """
    user_profile(u_knee_patient, injury(_)),
    findall(Muscle, fact(e_squats, uses_muscle, Muscle), UsedMuscles),
    findall(InjuryMuscle, (
        user_profile(u_knee_patient, injury(Condition)),
        injury_affects_muscle(Condition, InjuryMuscle)
    ), InjuredMuscles),
    \+ has_muscle_overlap(UsedMuscles, InjuredMuscles)
"""
result3 = list(prolog.query(query))
print(f"Second clause succeeds: {'YES' if result3 else 'NO'}")
print(f"(Should be NO - workout should be BLOCKED)")

if result3:
    print(f"Data: {result3[0]}")
