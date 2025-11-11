"""
ML-Prolog Integration Bridge

This module provides the interface between the ML prediction engine
and the Prolog logic engine for workout validation and override.

Workflow:
1. ML predicts next workout
2. Prolog validates against safety/recovery/schedule rules
3. If validation fails, Prolog suggests safe alternative
"""

from pyswip import Prolog
from typing import Dict, List, Optional, Tuple
from datetime import datetime
import os


class MLPrologBridge:
    """
    Bridge between ML predictions and Prolog logical constraints.
    
    The ML model predicts the optimal workout based on patterns,
    while Prolog ensures the prediction respects safety rules,
    recovery needs, and training frequency constraints.
    """
    
    def __init__(self, rules_file: str = "workout_rules.pl", test_facts: str = "tests/test_facts.pl"):
        """
        Initialize the ML-Prolog bridge
        
        Args:
            rules_file: Path to main Prolog rules file
            test_facts: Path to test facts (for development)
        """
        self.prolog = Prolog()
        self.rules_loaded = False
        
        # Load Prolog rules
        if os.path.exists(rules_file):
            self.prolog.consult(rules_file)
            self.rules_loaded = True
            print(f"✅ Loaded Prolog rules: {rules_file}")
        else:
            print(f"⚠️  Warning: Rules file not found: {rules_file}")
        
        # Load test facts if available
        if os.path.exists(test_facts):
            self.prolog.consult(test_facts)
            print(f"✅ Loaded test facts: {test_facts}")
    
    def predict_with_validation(
        self, 
        ml_prediction: str,
        user_id: str,
        user_profile: Dict,
        workout_history: List[Dict] = None,
        date: str = None
    ) -> Dict:
        """
        Main workflow: ML predicts, Prolog validates
        
        Args:
            ml_prediction: ML model's predicted workout (e.g., "legs", "chest")
            user_id: User identifier
            user_profile: User data (injuries, fatigue, sleep, etc.)
            workout_history: Recent workout history
            date: Date for the workout (optional)
        
        Returns:
            {
                'final_workout': str,          # Final recommendation
                'ml_prediction': str,          # Original ML prediction
                'validation_status': str,      # 'approved' or 'override'
                'override_reason': str,        # Why ML was overridden (if any)
                'confidence': float,           # Confidence in recommendation
                'rules_triggered': List[str]   # Which Prolog rules fired
            }
        """
        if not self.rules_loaded:
            return {
                'final_workout': ml_prediction,
                'ml_prediction': ml_prediction,
                'validation_status': 'no_validation',
                'override_reason': 'Prolog rules not loaded',
                'confidence': 0.5,
                'rules_triggered': []
            }
        
        date = date or datetime.now().strftime('%Y-%m-%d')
        rules_triggered = []
        
        # STEP 1: Check if ML prediction is safe for user
        is_safe, safety_reason = self._check_safety(ml_prediction, user_id)
        
        if not is_safe:
            rules_triggered.append(f"safety_check: {safety_reason}")
            # ML prediction blocked - find alternative
            alternative = self._find_safe_alternative(user_id, ml_prediction, user_profile)
            return {
                'final_workout': alternative['workout'],
                'ml_prediction': ml_prediction,
                'validation_status': 'override',
                'override_reason': safety_reason,
                'confidence': 0.7,  # Prolog override has high confidence
                'rules_triggered': rules_triggered + alternative.get('rules', [])
            }
        
        # STEP 2: Check recovery needs
        recovery_needed, recovery_reason = self._check_recovery(user_id, user_profile)
        
        if recovery_needed and ml_prediction != 'rest':
            rules_triggered.append(f"recovery_check: {recovery_reason}")
            return {
                'final_workout': 'rest',
                'ml_prediction': ml_prediction,
                'validation_status': 'override',
                'override_reason': recovery_reason,
                'confidence': 0.9,  # Recovery rules have highest priority
                'rules_triggered': rules_triggered
            }
        
        # STEP 3: Check workout frequency (overtraining prevention)
        frequency_ok, frequency_reason = self._check_frequency(
            ml_prediction, user_id, workout_history
        )
        
        if not frequency_ok:
            rules_triggered.append(f"frequency_check: {frequency_reason}")
            alternative = self._find_safe_alternative(user_id, ml_prediction, user_profile)
            return {
                'final_workout': alternative['workout'],
                'ml_prediction': ml_prediction,
                'validation_status': 'override',
                'override_reason': frequency_reason,
                'confidence': 0.75,
                'rules_triggered': rules_triggered + alternative.get('rules', [])
            }
        
        # STEP 4: Check time-of-day match (if time info available)
        if 'preferred_time' in user_profile:
            time_ok, time_reason = self._check_time_match(
                ml_prediction, user_id, user_profile.get('preferred_time')
            )
            if not time_ok:
                rules_triggered.append(f"time_check: {time_reason}")
                # Time mismatch is a soft constraint - we note it but don't override
        
        # All checks passed - approve ML prediction
        return {
            'final_workout': ml_prediction,
            'ml_prediction': ml_prediction,
            'validation_status': 'approved',
            'override_reason': None,
            'confidence': 0.85,  # ML prediction validated by logic
            'rules_triggered': rules_triggered or ['all_checks_passed']
        }
    
    def _check_safety(self, workout: str, user_id: str) -> Tuple[bool, Optional[str]]:
        """
        Check if workout is safe for user (injury conflicts)
        
        Returns:
            (is_safe, reason)
        """
        # Map workout to exercise ID (assuming pattern from our test data)
        workout_map = {
            'chest': 'e_chest_press',
            'legs': 'e_squats',
            'back': 'e_deadlift',
            'shoulders': 'e_shoulder_press',
            'cardio': 'e_plank',
            'rest': 'rest'
        }
        
        exercise_id = workout_map.get(workout, workout)
        
        if workout == 'rest':
            return True, None
        
        # Query Prolog: safe_for_user(WorkoutID, UserID)
        query = f"safe_for_user({exercise_id}, {user_id})"
        
        try:
            results = list(self.prolog.query(query))
            if results:
                return True, None
            else:
                # Not safe - get reason from injury_affects_muscle
                injury_query = f"user_profile({user_id}, injury(Condition))"
                injury_results = list(self.prolog.query(injury_query))
                if injury_results:
                    condition = injury_results[0].get('Condition', 'unknown')
                    return False, f"Workout conflicts with {condition} injury"
                return False, "Workout not safe for user (injury conflict)"
        except Exception as e:
            print(f"⚠️  Prolog query error: {e}")
            return True, None  # Fail open - allow workout if query fails
    
    def _check_recovery(self, user_id: str, user_profile: Dict) -> Tuple[bool, Optional[str]]:
        """
        Check if user needs recovery day
        
        Returns:
            (needs_recovery, reason)
        """
        # Query Prolog: needs_recovery(UserID, Days)
        query = f"needs_recovery({user_id}, Days)"
        
        try:
            results = list(self.prolog.query(query))
            if results:
                days_needed = results[0].get('Days', 0)
                if days_needed >= 2:
                    fatigue = user_profile.get('fatigue_level', 'unknown')
                    sleep = user_profile.get('sleep_hours', 0)
                    return True, f"You need {days_needed} rest days (fatigue: {fatigue}, sleep: {sleep}h)"
            return False, None
        except Exception as e:
            print(f"⚠️  Prolog query error: {e}")
            return False, None
    
    def _check_frequency(
        self, 
        workout: str, 
        user_id: str, 
        workout_history: List[Dict]
    ) -> Tuple[bool, Optional[str]]:
        """
        Check if workout frequency is acceptable (not overtraining same muscle)
        
        Returns:
            (frequency_ok, reason)
        """
        if not workout_history or workout == 'rest':
            return True, None
        
        # Build schedule from history for Prolog
        # Note: This is simplified - in production, you'd create dynamic Prolog facts
        
        # For now, check simple rule: same muscle trained < 3 times in last 7 days
        recent_workouts = workout_history[-7:] if len(workout_history) >= 7 else workout_history
        same_muscle_count = sum(1 for w in recent_workouts if w.get('muscle_group') == workout)
        
        if same_muscle_count >= 3:
            return False, f"{workout.capitalize()} trained {same_muscle_count} times this week - overtraining risk"
        
        return True, None
    
    def _check_time_match(
        self, 
        workout: str, 
        user_id: str, 
        time_of_day: str
    ) -> Tuple[bool, Optional[str]]:
        """
        Check if workout matches user's optimal time of day
        
        Returns:
            (time_ok, reason)
        """
        if workout == 'rest':
            return True, None
        
        # Map workout to exercise
        workout_map = {
            'chest': 'e_chest_press',
            'legs': 'e_squats',
            'back': 'e_deadlift',
            'shoulders': 'e_shoulder_press',
        }
        
        exercise_id = workout_map.get(workout, workout)
        
        # Query Prolog: optimal_time_match(ExerciseID, UserID, TimeOfDay)
        query = f"optimal_time_match({exercise_id}, {user_id}, {time_of_day})"
        
        try:
            results = list(self.prolog.query(query))
            if results:
                return True, None
            else:
                return False, f"Workout better suited for different time of day"
        except Exception as e:
            return True, None  # Soft constraint - allow on error
    
    def _find_safe_alternative(
        self, 
        user_id: str, 
        blocked_workout: str,
        user_profile: Dict
    ) -> Dict:
        """
        Find a safe alternative workout when ML prediction is blocked
        
        Returns:
            {
                'workout': str,
                'reason': str,
                'rules': List[str]
            }
        """
        # Priority order for alternatives
        alternatives = ['rest', 'cardio', 'legs', 'chest', 'back', 'shoulders']
        
        # Remove blocked workout from alternatives
        alternatives = [w for w in alternatives if w != blocked_workout]
        
        # Try each alternative until we find a safe one
        for alt_workout in alternatives:
            is_safe, _ = self._check_safety(alt_workout, user_id)
            if is_safe:
                return {
                    'workout': alt_workout,
                    'reason': f'Safe alternative to {blocked_workout}',
                    'rules': ['alternative_finder']
                }
        
        # If nothing is safe, recommend rest
        return {
            'workout': 'rest',
            'reason': 'No safe workout alternatives available - recovery day',
            'rules': ['default_to_rest']
        }
    
    def explain_decision(self, decision: Dict) -> str:
        """
        Generate human-readable explanation of the decision
        
        Args:
            decision: Output from predict_with_validation()
        
        Returns:
            Formatted explanation string
        """
        lines = []
        lines.append("=" * 60)
        lines.append("WORKOUT DECISION EXPLANATION")
        lines.append("=" * 60)
        
        lines.append(f"\n🤖 ML Prediction: {decision['ml_prediction']}")
        lines.append(f"⚖️  Validation Status: {decision['validation_status'].upper()}")
        lines.append(f"✅ Final Recommendation: {decision['final_workout'].upper()}")
        lines.append(f"📊 Confidence: {decision['confidence']:.0%}")
        
        if decision['override_reason']:
            lines.append(f"\n⚠️  Override Reason:")
            lines.append(f"   {decision['override_reason']}")
        
        if decision['rules_triggered']:
            lines.append(f"\n🔍 Logic Rules Triggered:")
            for rule in decision['rules_triggered']:
                lines.append(f"   • {rule}")
        
        lines.append("\n" + "=" * 60)
        
        return "\n".join(lines)


def main():
    """Demo: Test ML-Prolog integration"""
    
    print("=" * 70)
    print("ML-PROLOG INTEGRATION BRIDGE - DEMO")
    print("=" * 70)
    
    # Initialize bridge
    bridge = MLPrologBridge()
    
    # Test scenarios
    test_cases = [
        {
            'name': 'Test 1: ML suggests legs, user has knee injury',
            'ml_prediction': 'legs',
            'user_id': 'u_knee_patient',
            'user_profile': {
                'fatigue_level': 'low',
                'sleep_hours': 8,
            },
            'workout_history': []
        },
        {
            'name': 'Test 2: ML suggests chest, user exhausted',
            'ml_prediction': 'chest',
            'user_id': 'u_exhausted',
            'user_profile': {
                'fatigue_level': 'high',
                'sleep_hours': 4,
            },
            'workout_history': []
        },
        {
            'name': 'Test 3: ML suggests legs, healthy user',
            'ml_prediction': 'legs',
            'user_id': 'u_healthy_morning',
            'user_profile': {
                'fatigue_level': 'low',
                'sleep_hours': 8,
            },
            'workout_history': []
        },
        {
            'name': 'Test 4: ML suggests chest, overtraining (3x this week)',
            'ml_prediction': 'chest',
            'user_id': 'u_healthy_morning',
            'user_profile': {
                'fatigue_level': 'medium',
                'sleep_hours': 7,
            },
            'workout_history': [
                {'muscle_group': 'chest'},
                {'muscle_group': 'legs'},
                {'muscle_group': 'chest'},
                {'muscle_group': 'rest'},
                {'muscle_group': 'chest'},
            ]
        },
    ]
    
    # Run test cases
    for i, test in enumerate(test_cases, 1):
        print(f"\n{'='*70}")
        print(f"{test['name']}")
        print('='*70)
        
        decision = bridge.predict_with_validation(
            ml_prediction=test['ml_prediction'],
            user_id=test['user_id'],
            user_profile=test['user_profile'],
            workout_history=test['workout_history']
        )
        
        print(bridge.explain_decision(decision))
        
        # Short summary
        if decision['validation_status'] == 'approved':
            print("✅ Result: ML prediction APPROVED")
        else:
            print(f"⚠️  Result: ML prediction OVERRIDDEN")
            print(f"   {test['ml_prediction']} → {decision['final_workout']}")
    
    print("\n" + "=" * 70)
    print("DEMO COMPLETE")
    print("=" * 70)


if __name__ == '__main__':
    main()
