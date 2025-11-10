"""
test_rules_python.py

Python-based edge case testing for workout rules using PySwip.
Tests the same scenarios as test_facts.pl but through the Python interface.
"""

from pyswip import Prolog
import sys

class WorkoutRulesTester:
    def __init__(self):
        self.prolog = Prolog()
        # Load workout rules and test data
        self.prolog.consult("workout_rules.pl")
        self.prolog.consult("tests/test_facts.pl")
        self.tests_passed = 0
        self.tests_failed = 0
        
    def test_case(self, name, query, expected_result):
        """Run a single test case"""
        print(f"\n[TEST] {name}")
        print(f"  Query: {query}")
        print(f"  Expected: {expected_result}")
        
        try:
            results = list(self.prolog.query(query))
            
            if expected_result == "fail":
                if len(results) == 0:
                    print(f"  [PASS] Query failed as expected")
                    self.tests_passed += 1
                else:
                    print(f"  [FAIL] Query succeeded but should have failed")
                    print(f"     Results: {results}")
                    self.tests_failed += 1
            elif expected_result == "success":
                if len(results) > 0:
                    print(f"  [PASS] Query succeeded as expected")
                    print(f"     Results: {results}")
                    self.tests_passed += 1
                else:
                    print(f"  [FAIL] Query failed but should have succeeded")
                    self.tests_failed += 1
            else:
                # Check specific result value
                if len(results) > 0:
                    actual = results[0]
                    if str(actual) == str(expected_result):
                        print(f"  [PASS] Got expected result")
                        print(f"     Result: {actual}")
                        self.tests_passed += 1
                    else:
                        print(f"  [FAIL] Wrong result")
                        print(f"     Expected: {expected_result}")
                        print(f"     Got: {actual}")
                        self.tests_failed += 1
                else:
                    print(f"  [FAIL] No results returned")
                    self.tests_failed += 1
                    
        except Exception as e:
            print(f"  [ERROR] {str(e)}")
            self.tests_failed += 1
    
    def run_all_tests(self):
        """Run comprehensive edge case test suite"""
        print("=" * 70)
        print("WORKOUT RULES - PYTHON EDGE CASE TESTS")
        print("=" * 70)
        
        # Test 1: Recovery needs for poor sleep
        self.test_case(
            "Poor sleep needs 3+ recovery days",
            "needs_recovery(u_exhausted, Days), Days >= 3",
            "success"
        )
        
        # Test 2: Recovery needs for good sleep
        self.test_case(
            "Good sleep needs ≤2 recovery days",
            "needs_recovery(u_healthy_morning, Days), Days =< 2",
            "success"
        )
        
        # Test 3: Morning person + hard workout + morning time
        self.test_case(
            "Morning person can do hard workout in morning",
            "optimal_time_match(e_bench_press, u_healthy_morning, morning)",
            "success"
        )
        
        # Test 4: Morning person + hard workout + evening time
        self.test_case(
            "Morning person should NOT do hard workout in evening",
            "optimal_time_match(e_bench_press, u_healthy_morning, evening)",
            "fail"
        )
        
        # Test 5: Evening person + hard workout + evening time
        self.test_case(
            "Evening person can do hard workout in evening",
            "optimal_time_match(e_deadlift, u_evening_injured, evening)",
            "success"
        )
        
        # Test 6: Workout frequency - consecutive chest training
        self.test_case(
            "Third chest workout in week is too frequent",
            "test_schedule(consecutive_chest, Schedule), workout_frequency_ok(e_chest_press, Schedule, Result), Result = too_frequent",
            "success"
        )
        
        # Test 7: Healthy user with no injuries
        self.test_case(
            "Healthy user can do any workout",
            "safe_for_user(e_squats, u_healthy_morning)",
            "success"
        )
        
        # Test 8: Different workout types don't interfere
        self.test_case(
            "Upper body workout after lower body is OK",
            "safe_for_user(e_chest_press, u_healthy_morning)",
            "success"
        )
        
        # Test 9: Moderate sleep needs moderate recovery
        self.test_case(
            "Traveler with 6h sleep needs ~2 recovery days",
            "needs_recovery(u_traveler, Days), Days = 2",
            "success"
        )
        
        # Test 10: Easy workout for exhausted user
        self.test_case(
            "Easy workout acceptable for exhausted user",
            "optimal_time_match(e_plank, u_exhausted, afternoon)",
            "success"
        )
        
        # Test 11: Hard workout blocked for exhausted user
        self.test_case(
            "Hard workout NOT recommended for exhausted user",
            "optimal_time_match(e_deadlift, u_exhausted, morning)",
            "fail"
        )
        
        # Test 12: Check schedule respects split pattern
        self.test_case(
            "Optimal split schedule is balanced",
            "test_schedule(optimal_split, Schedule), respects_split_pattern(Schedule, Result), Result = balanced",
            "success"
        )
        
        # Test 13: Overuse detection
        self.test_case(
            "Overuse quad schedule has 4+ quad workouts",
            "test_schedule(overuse_quads, Schedule), findall(E, (member(day(_, E), Schedule), fact(E, uses_muscle, m_quadriceps)), Quads), length(Quads, Count), Count >= 4",
            "success"
        )
        
        # Test 14: Deload week - all easy workouts
        self.test_case(
            "Deload week contains only easy workouts",
            "test_schedule(deload_week, Schedule), member(day(_, Exercise), Schedule), attr(Exercise, difficulty, Diff), Diff = easy",
            "success"
        )
        
        # Test 15: Overtrainer needs maximum recovery
        self.test_case(
            "Overtrainer with poor sleep needs 3+ recovery days",
            "needs_recovery(u_overtrainer, Days), Days >= 3",
            "success"
        )
        
        # Print summary
        print("\n" + "=" * 70)
        print("TEST SUMMARY")
        print("=" * 70)
        total = self.tests_passed + self.tests_failed
        pass_rate = (self.tests_passed / total * 100) if total > 0 else 0
        print(f"Total Tests: {total}")
        print(f"Passed:      {self.tests_passed}")
        print(f"Failed:      {self.tests_failed}")
        print(f"Pass Rate:   {pass_rate:.1f}%")
        
        return self.tests_failed == 0

def main():
    tester = WorkoutRulesTester()
    success = tester.run_all_tests()
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
