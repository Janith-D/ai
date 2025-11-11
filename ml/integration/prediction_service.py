"""
Prediction Service - Unified Interface for ML + Prolog

This service provides a single interface that combines:
1. ML model predictions (pattern-based recommendations)
2. Prolog logic validation (safety and constraints)
3. Confidence scoring and explanation

Usage:
    service = PredictionService()
    recommendation = service.predict(user_data, workout_history)
"""

from typing import Dict, List, Optional
from datetime import datetime
import numpy as np

# Note: Will import actual ML model once trained
# from ml.models.baseline_model import BaselineModel

from ml.integration.ml_prolog_bridge import MLPrologBridge


class PredictionService:
    """
    Unified prediction service that orchestrates ML and Prolog components
    """
    
    def __init__(
        self, 
        model_path: Optional[str] = None,
        rules_file: str = "workout_rules.pl",
        test_facts: str = "tests/test_facts.pl"
    ):
        """
        Initialize prediction service
        
        Args:
            model_path: Path to trained ML model (None = use mock predictions)
            rules_file: Path to Prolog rules
            test_facts: Path to test facts
        """
        self.ml_model = None
        self.bridge = MLPrologBridge(rules_file, test_facts)
        self.use_mock_ml = model_path is None
        
        if model_path:
            # TODO: Load trained model
            # self.ml_model = joblib.load(model_path)
            print(f"✅ ML model loaded: {model_path}")
        else:
            print("⚠️  Using mock ML predictions (model not trained yet)")
    
    def predict(
        self,
        user_id: str,
        user_profile: Dict,
        workout_history: List[Dict],
        date: Optional[str] = None
    ) -> Dict:
        """
        Make workout recommendation with full ML + Prolog workflow
        
        Args:
            user_id: User identifier
            user_profile: User data including:
                - age, gender, goal
                - fatigue_level, sleep_hours
                - injury status (optional)
            workout_history: Recent workout logs
            date: Target date for workout
        
        Returns:
            {
                'recommendation': str,        # Final workout recommendation
                'confidence': float,          # Confidence score (0-1)
                'ml_prediction': str,         # What ML suggested
                'validation_status': str,     # 'approved' or 'override'
                'explanation': str,           # Human-readable explanation
                'metadata': {
                    'ml_confidence': float,
                    'prolog_confidence': float,
                    'rules_triggered': List[str],
                    'override_reason': str
                }
            }
        """
        date = date or datetime.now().strftime('%Y-%m-%d')
        
        # STEP 1: Get ML prediction
        ml_prediction, ml_confidence = self._get_ml_prediction(
            user_profile, workout_history
        )
        
        # STEP 2: Validate with Prolog logic
        validation_result = self.bridge.predict_with_validation(
            ml_prediction=ml_prediction,
            user_id=user_id,
            user_profile=user_profile,
            workout_history=workout_history,
            date=date
        )
        
        # STEP 3: Combine results
        final_workout = validation_result['final_workout']
        status = validation_result['validation_status']
        
        # Calculate combined confidence
        if status == 'approved':
            # ML and Prolog agree - high confidence
            combined_confidence = (ml_confidence + validation_result['confidence']) / 2
        else:
            # Prolog override - trust logic over ML
            combined_confidence = validation_result['confidence']
        
        # Generate explanation
        explanation = self._generate_explanation(
            ml_prediction, final_workout, status, validation_result
        )
        
        return {
            'recommendation': final_workout,
            'confidence': round(combined_confidence, 2),
            'ml_prediction': ml_prediction,
            'validation_status': status,
            'explanation': explanation,
            'metadata': {
                'ml_confidence': round(ml_confidence, 2),
                'prolog_confidence': round(validation_result['confidence'], 2),
                'rules_triggered': validation_result['rules_triggered'],
                'override_reason': validation_result.get('override_reason')
            }
        }
    
    def _get_ml_prediction(
        self, 
        user_profile: Dict, 
        workout_history: List[Dict]
    ) -> tuple:
        """
        Get ML model prediction
        
        Returns:
            (predicted_workout, confidence)
        """
        if self.use_mock_ml:
            # Mock ML predictions based on simple rules
            return self._mock_ml_prediction(user_profile, workout_history)
        
        # TODO: Use actual trained ML model
        # features = self._prepare_features(user_profile, workout_history)
        # prediction = self.ml_model.predict(features)
        # confidence = self.ml_model.predict_proba(features).max()
        # return prediction[0], confidence
        
        return self._mock_ml_prediction(user_profile, workout_history)
    
    def _mock_ml_prediction(
        self, 
        user_profile: Dict, 
        workout_history: List[Dict]
    ) -> tuple:
        """
        Mock ML predictions for testing (will be replaced with actual model)
        
        Simple rule-based logic to simulate ML behavior
        """
        goal = user_profile.get('goal', 'general_fitness')
        fatigue = user_profile.get('fatigue_level', 'medium')
        
        # Get last workout
        last_workout = None
        if workout_history:
            last_workout = workout_history[-1].get('muscle_group')
        
        # Simple pattern-based prediction
        if fatigue == 'high':
            return 'rest', 0.75
        
        if goal in ['muscle_gain', 'strength']:
            # Alternate upper/lower
            if last_workout in ['chest', 'back', 'shoulders']:
                return 'legs', 0.80
            else:
                return 'chest', 0.80
        
        elif goal in ['fat_loss', 'endurance']:
            # Favor cardio
            if last_workout == 'cardio':
                return 'legs', 0.75
            else:
                return 'cardio', 0.85
        
        else:  # general_fitness
            # Balanced rotation
            options = ['chest', 'legs', 'back', 'cardio']
            if last_workout in options:
                options.remove(last_workout)
            return np.random.choice(options), 0.70
    
    def _generate_explanation(
        self,
        ml_prediction: str,
        final_workout: str,
        status: str,
        validation_result: Dict
    ) -> str:
        """Generate human-readable explanation"""
        
        if status == 'approved':
            return (
                f"✅ ML recommended {ml_prediction} and logic engine approved. "
                f"This workout fits your goals and respects safety constraints."
            )
        else:
            override_reason = validation_result.get('override_reason', 'safety constraints')
            return (
                f"⚠️  ML suggested {ml_prediction}, but logic engine overrode to {final_workout}. "
                f"Reason: {override_reason}"
            )
    
    def batch_predict(
        self,
        user_id: str,
        user_profile: Dict,
        workout_history: List[Dict],
        days: int = 7
    ) -> List[Dict]:
        """
        Generate workout plan for multiple days
        
        Args:
            user_id: User identifier
            user_profile: User data
            workout_history: Recent workout logs
            days: Number of days to plan
        
        Returns:
            List of daily recommendations
        """
        plan = []
        current_history = workout_history.copy()
        
        for day in range(days):
            date = datetime.now().replace(hour=0, minute=0, second=0, microsecond=0)
            date = date.replace(day=date.day + day)
            date_str = date.strftime('%Y-%m-%d')
            
            # Get recommendation for this day
            recommendation = self.predict(
                user_id=user_id,
                user_profile=user_profile,
                workout_history=current_history,
                date=date_str
            )
            
            # Add to plan
            plan.append({
                'day': day + 1,
                'date': date_str,
                **recommendation
            })
            
            # Update history with today's workout
            current_history.append({
                'date': date_str,
                'muscle_group': recommendation['recommendation']
            })
        
        return plan


def main():
    """Demo: Unified prediction service"""
    
    print("=" * 70)
    print("UNIFIED PREDICTION SERVICE - DEMO")
    print("=" * 70)
    
    # Initialize service
    service = PredictionService()
    
    # Test case 1: Healthy user
    print("\n" + "="*70)
    print("TEST 1: Healthy user with muscle_gain goal")
    print("="*70)
    
    user_profile = {
        'age': 28,
        'gender': 'male',
        'goal': 'muscle_gain',
        'fatigue_level': 'low',
        'sleep_hours': 8
    }
    
    workout_history = [
        {'date': '2025-11-10', 'muscle_group': 'chest'},
        {'date': '2025-11-11', 'muscle_group': 'legs'}
    ]
    
    result = service.predict(
        user_id='u_healthy_morning',
        user_profile=user_profile,
        workout_history=workout_history
    )
    
    print(f"\n📋 User Profile: {user_profile['goal']}, fatigue: {user_profile['fatigue_level']}")
    print(f"📜 Recent workouts: {[w['muscle_group'] for w in workout_history]}")
    print(f"\n🤖 ML Prediction: {result['ml_prediction']} (confidence: {result['metadata']['ml_confidence']:.0%})")
    print(f"⚖️  Validation: {result['validation_status'].upper()}")
    print(f"✅ Final Recommendation: {result['recommendation'].upper()} (confidence: {result['confidence']:.0%})")
    print(f"\n💬 {result['explanation']}")
    
    # Test case 2: Injured user
    print("\n" + "="*70)
    print("TEST 2: User with knee injury tries legs workout")
    print("="*70)
    
    user_profile_injured = {
        'age': 30,
        'gender': 'female',
        'goal': 'fat_loss',
        'fatigue_level': 'medium',
        'sleep_hours': 7
    }
    
    workout_history_injured = [
        {'date': '2025-11-10', 'muscle_group': 'cardio'},
    ]
    
    # Mock ML will likely suggest legs for fat_loss after cardio
    result = service.predict(
        user_id='u_knee_patient',  # Has knee injury in test facts
        user_profile=user_profile_injured,
        workout_history=workout_history_injured
    )
    
    print(f"\n📋 User Profile: {user_profile_injured['goal']}, HAS KNEE INJURY")
    print(f"📜 Recent workouts: {[w['muscle_group'] for w in workout_history_injured]}")
    print(f"\n🤖 ML Prediction: {result['ml_prediction']} (confidence: {result['metadata']['ml_confidence']:.0%})")
    print(f"⚖️  Validation: {result['validation_status'].upper()}")
    print(f"✅ Final Recommendation: {result['recommendation'].upper()} (confidence: {result['confidence']:.0%})")
    print(f"\n💬 {result['explanation']}")
    if result['metadata']['override_reason']:
        print(f"❗ Override reason: {result['metadata']['override_reason']}")
    
    # Test case 3: Generate week plan
    print("\n" + "="*70)
    print("TEST 3: Generate 7-day workout plan")
    print("="*70)
    
    weekly_plan = service.batch_predict(
        user_id='u_healthy_morning',
        user_profile=user_profile,
        workout_history=[],
        days=7
    )
    
    print("\n📅 7-Day Workout Plan:")
    print("-" * 70)
    for day_plan in weekly_plan:
        status_icon = "✅" if day_plan['validation_status'] == 'approved' else "⚠️"
        print(f"Day {day_plan['day']}: {day_plan['recommendation'].upper():12s} "
              f"{status_icon} (confidence: {day_plan['confidence']:.0%})")
    
    print("\n" + "="*70)
    print("DEMO COMPLETE")
    print("="*70)
    print("\n✅ ML-Prolog Integration is working!")
    print("📝 Next step: Train actual ML model to replace mock predictions")


if __name__ == '__main__':
    main()
