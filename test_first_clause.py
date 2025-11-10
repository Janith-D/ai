"""Test first clause directly"""
from pyswip import Prolog

prolog = Prolog()
prolog.consult("workout_rules.pl")
prolog.consult("tests/test_facts.pl")

print("Testing first clause of safe_for_user:")
print("This clause should only succeed if user has NO injuries\n")

print("Check 1: User has injuries?")
has_injury = list(prolog.query("user_profile(u_knee_patient, injury(_))"))
print(f"  Result: {'YES' if has_injury else 'NO'}")

print("\nCheck 2: First clause condition (negation):")
first_clause_cond = list(prolog.query(r"\+ user_profile(u_knee_patient, injury(_))"))
print(f"  \\+ user_profile(u_knee_patient, injury(_)) = {len(first_clause_cond) > 0}")
print(f"  (Should be FALSE/empty)")

print("\nCheck 3: Try to call safe_for_user with explicit first clause:")
# This should fail
try:
    result = list(prolog.query("user_profile(u_knee_patient, injury(_)), !, fail"))
    print(f"  Explicit block: {result}")
except:
    print("  Error in query")

print("\nCheck 4: Directly test safe_for_user:")
result = list(prolog.query("safe_for_user(e_squats, u_knee_patient)"))
print(f"  Result: {'SAFE' if result else 'BLOCKED'}")
print(f"  Solutions: {len(result)}")

print("\nCheck 5: Test with a healthy user (should pass):")
result_healthy = list(prolog.query("safe_for_user(e_squats, u_healthy_morning)"))
print(f"  Result: {'SAFE' if result_healthy else 'BLOCKED'}")
print(f"  (Should be SAFE)")

print("\nCheck 6: Test chest with knee injury (should pass):")
result_chest = list(prolog.query("safe_for_user(e_chest_press, u_knee_patient)"))
print(f"  Result: {'SAFE' if result_chest else 'BLOCKED'}")
print(f"  (Should be SAFE - no overlap)")
