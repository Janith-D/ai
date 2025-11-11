"""
NLP-ML Integration Module
Connects NLP insights with the ML prediction system
"""

import sys
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime, timedelta
import pandas as pd

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent))

from nlp.nlp_pipeline import NLPPipeline
from ml.models.advanced_xgboost_model import AdvancedXGBoostModel


class NLPMLIntegration:
    """
    Integrates NLP analysis with ML workout prediction
    
    Enriches ML predictions with:
    - User emotional state (motivation, fatigue)
    - User intent (wants to workout vs needs rest)
    - User preferences (target muscle groups, intensity)
    - Context-aware adjustments
    """
    
    def __init__(self, 
                 ml_model_path: str = 'ml/models/saved_models/advanced_xgb_latest',
                 load_emotion_model: bool = True):
        """
        Initialize NLP-ML integration
        
        Args:
            ml_model_path: Path to trained ML model
            load_emotion_model: Whether to load emotion detection model
        """
        print("🔗 Initializing NLP-ML Integration...")
        
        # Load NLP pipeline
        self.nlp = NLPPipeline(load_emotion_model=load_emotion_model)
        
        # Load ML model
        try:
            self.ml_model = AdvancedXGBoostModel.load(ml_model_path)
            print(f"✓ Loaded ML model from {ml_model_path}")
        except Exception as e:
            print(f"⚠ Warning: Could not load ML model: {e}")
            self.ml_model = None
        
        print("✓ NLP-ML Integration ready\n")
    
    def predict_with_message(self,
                            user_message: str,
                            user_data: Dict[str, any],
                            history_df: Optional[pd.DataFrame] = None) -> Dict[str, any]:
        """
        Predict workout using both ML and NLP analysis
        
        Args:
            user_message: User's message/request
            user_data: User profile data (goal, current stats)
            history_df: Optional workout history DataFrame
            
        Returns:
            Enhanced prediction with NLP insights:
            {
                'nlp_analysis': {...},
                'base_prediction': 'chest',
                'base_confidence': 0.85,
                'adjusted_prediction': 'rest',
                'adjusted_confidence': 0.90,
                'adjustment_reason': 'User is exhausted',
                'recommendation': 'Take a rest day...'
            }
        """
        # Analyze user message
        nlp_result = self.nlp.analyze(
            user_message,
            user_id=user_data.get('user_id', 'unknown'),
            include_prolog=True
        )
        
        # Get base ML prediction if model available
        base_prediction = None
        base_confidence = 0.0
        
        if self.ml_model and history_df is not None:
            # Prepare input for ML model
            ml_input = self._prepare_ml_input(user_data, history_df)
            base_prediction, base_confidence = self.ml_model.predict_workout(ml_input)
        
        # Apply NLP-based adjustments
        adjusted_prediction, adjusted_confidence, reason = self._apply_nlp_adjustments(
            base_prediction,
            base_confidence,
            nlp_result,
            user_data
        )
        
        # Generate recommendation
        recommendation = self._generate_recommendation(
            nlp_result,
            adjusted_prediction,
            adjusted_confidence
        )
        
        return {
            'nlp_analysis': nlp_result,
            'base_prediction': base_prediction,
            'base_confidence': base_confidence,
            'adjusted_prediction': adjusted_prediction,
            'adjusted_confidence': adjusted_confidence,
            'adjustment_reason': reason,
            'recommendation': recommendation,
            'timestamp': datetime.now().isoformat()
        }
    
    def _prepare_ml_input(self, 
                         user_data: Dict[str, any],
                         history_df: pd.DataFrame) -> pd.DataFrame:
        """
        Prepare input for ML model
        
        Args:
            user_data: User profile data
            history_df: Workout history
            
        Returns:
            DataFrame ready for ML model
        """
        # Get most recent workout data
        latest = history_df.iloc[-1] if len(history_df) > 0 else None
        
        # Build input dict
        input_dict = {
            'user_id': user_data.get('user_id', 0),
            'day': len(history_df) + 1,
            'goal': user_data.get('goal', 'general_fitness'),
            'intensity': latest['intensity'] if latest is not None else 5,
            'sleep_hours': user_data.get('sleep_hours', 7),
            'fatigue_level': user_data.get('fatigue_level', 3),
        }
        
        return pd.DataFrame([input_dict])
    
    def _apply_nlp_adjustments(self,
                              base_prediction: Optional[str],
                              base_confidence: float,
                              nlp_result: Dict[str, any],
                              user_data: Dict[str, any]) -> tuple:
        """
        Adjust ML prediction based on NLP insights
        
        Args:
            base_prediction: ML model's prediction
            base_confidence: ML model's confidence
            nlp_result: NLP analysis result
            user_data: User profile data
            
        Returns:
            (adjusted_prediction, adjusted_confidence, reason)
        """
        adjusted_prediction = base_prediction
        adjusted_confidence = base_confidence
        reason = "No adjustment needed"
        
        intent = nlp_result['intent']['intent']
        emotion = nlp_result.get('emotion', {})
        context = nlp_result.get('context', {})
        
        # Rule 1: User explicitly wants rest
        if intent == 'rest_request':
            adjusted_prediction = 'rest'
            adjusted_confidence = 0.95
            reason = "User explicitly requested rest"
            return adjusted_prediction, adjusted_confidence, reason
        
        # Rule 2: User is injured
        physical_states = context.get('physical_state', [])
        if 'injured' in physical_states:
            adjusted_prediction = 'rest'
            adjusted_confidence = 0.98
            reason = "User reported injury - safety first"
            return adjusted_prediction, adjusted_confidence, reason
        
        # Rule 3: Very low energy
        if emotion.get('energy_level') == 'low' and base_prediction != 'rest':
            # Check if user still wants to train
            if intent == 'plan_workout':
                # Lower intensity recommendation
                if base_prediction in ['chest', 'legs', 'back']:
                    # Suggest lighter alternative
                    adjusted_prediction = 'cardio'
                    adjusted_confidence = 0.75
                    reason = "Low energy - suggesting lighter cardio instead"
            else:
                adjusted_prediction = 'rest'
                adjusted_confidence = 0.85
                reason = "User has low energy and didn't explicitly want to train"
            return adjusted_prediction, adjusted_confidence, reason
        
        # Rule 4: User targets specific muscle group
        target_muscles = context.get('muscle_groups', [])
        if target_muscles and base_prediction not in target_muscles:
            # User has preference, adjust if reasonable
            if intent == 'plan_workout':
                adjusted_prediction = target_muscles[0]  # Take first mentioned
                adjusted_confidence = 0.80
                reason = f"User requested {target_muscles[0]} workout"
                return adjusted_prediction, adjusted_confidence, reason
        
        # Rule 5: Very sore
        if 'sore' in physical_states and 'tired' in physical_states:
            if base_prediction not in ['rest', 'cardio']:
                adjusted_prediction = 'cardio'
                adjusted_confidence = 0.70
                reason = "User very sore - suggesting active recovery"
                return adjusted_prediction, adjusted_confidence, reason
        
        # Rule 6: High motivation boost
        if emotion.get('fitness_emotion') == 'motivated' and emotion.get('confidence', 0) > 0.8:
            # Boost confidence slightly
            adjusted_confidence = min(base_confidence + 0.10, 1.0)
            reason = "User highly motivated - confidence boosted"
        
        return adjusted_prediction, adjusted_confidence, reason
    
    def _generate_recommendation(self,
                                nlp_result: Dict[str, any],
                                prediction: str,
                                confidence: float) -> str:
        """
        Generate human-friendly recommendation
        
        Args:
            nlp_result: NLP analysis
            prediction: Workout prediction
            confidence: Prediction confidence
            
        Returns:
            Recommendation string
        """
        emotion = nlp_result.get('emotion', {})
        intent = nlp_result['intent']['intent']
        context = nlp_result.get('context', {})
        
        # Build recommendation based on context
        recommendations = []
        
        # Greeting based on emotion
        if emotion.get('fitness_emotion') == 'motivated':
            recommendations.append("🔥 Love the energy!")
        elif emotion.get('fitness_emotion') == 'demotivated':
            recommendations.append("💪 I understand you're feeling low, let's start small.")
        elif emotion.get('fitness_emotion') == 'frustrated':
            recommendations.append("🤝 I hear you. Let's work through this together.")
        
        # Main recommendation
        if prediction == 'rest':
            recommendations.append(
                "I recommend taking a rest day. Recovery is crucial for progress! "
                "Focus on sleep, hydration, and light stretching."
            )
        else:
            intensity = context.get('intensity', 'moderate')
            recommendations.append(
                f"I recommend a {intensity} intensity {prediction} workout. "
                f"Based on your history and current state, this will help you progress toward your goals."
            )
        
        # Additional tips based on physical state
        physical_states = context.get('physical_state', [])
        if 'sore' in physical_states:
            recommendations.append("Since you're sore, make sure to warm up well and don't push too hard.")
        if 'tired' in physical_states:
            recommendations.append("You mentioned being tired - keep the session shorter if needed.")
        
        return " ".join(recommendations)
    
    def chat_based_prediction(self, 
                            user_message: str,
                            user_id: int = 0,
                            goal: str = 'general_fitness',
                            recent_workouts: Optional[List[str]] = None) -> Dict[str, any]:
        """
        Simplified prediction for demo/testing
        
        Args:
            user_message: User's message
            user_id: User ID
            goal: Fitness goal
            recent_workouts: List of recent workout types
            
        Returns:
            Prediction result
        """
        user_data = {
            'user_id': user_id,
            'goal': goal
        }
        
        # Create minimal history
        if recent_workouts:
            history_df = pd.DataFrame([
                {'workout_type': w, 'intensity': 5} 
                for w in recent_workouts
            ])
        else:
            history_df = pd.DataFrame()
        
        return self.predict_with_message(user_message, user_data, history_df)


# Test function
if __name__ == "__main__":
    print("=" * 80)
    print("🔗 NLP-ML INTEGRATION TEST")
    print("=" * 80)
    print()
    
    # Initialize integration (without loading full ML model for quick test)
    integration = NLPMLIntegration(load_emotion_model=True)
    
    # Test scenarios
    test_cases = [
        {
            'message': "I'm exhausted but I still want to train my legs",
            'goal': 'muscle_gain',
            'recent': ['chest', 'back', 'rest']
        },
        {
            'message': "Feeling super pumped! Let's do chest day!",
            'goal': 'muscle_gain',
            'recent': ['legs', 'rest', 'shoulders']
        },
        {
            'message': "My shoulder hurts, I think I need to rest",
            'goal': 'fat_loss',
            'recent': ['cardio', 'legs', 'arms']
        },
        {
            'message': "Too sore from leg day, but I still want to do something",
            'goal': 'endurance',
            'recent': ['legs', 'cardio', 'rest']
        },
    ]
    
    for i, case in enumerate(test_cases, 1):
        print(f"\n{'='*80}")
        print(f"Test {i}: \"{case['message']}\"")
        print(f"{'='*80}\n")
        
        result = integration.chat_based_prediction(
            case['message'],
            goal=case['goal'],
            recent_workouts=case['recent']
        )
        
        # Display results
        nlp = result['nlp_analysis']
        
        print("📊 NLP ANALYSIS:")
        if nlp.get('emotion'):
            print(f"  Emotion: {nlp['emotion']['fitness_emotion']} ({nlp['emotion']['confidence']:.0%})")
            print(f"  Energy: {nlp['emotion']['energy_level']}")
        print(f"  Intent: {nlp['intent']['intent']} ({nlp['intent']['confidence']:.0%})")
        if nlp['context']:
            print(f"  Context: {nlp['context']}")
        
        print(f"\n🎯 PREDICTION:")
        print(f"  Workout: {result['adjusted_prediction']}")
        print(f"  Confidence: {result['adjusted_confidence']:.0%}")
        print(f"  Reason: {result['adjustment_reason']}")
        
        print(f"\n💬 RECOMMENDATION:")
        print(f"  {result['recommendation']}")
    
    print("\n" + "=" * 80)
    print("✓ Integration Test Complete")
    print("=" * 80)
