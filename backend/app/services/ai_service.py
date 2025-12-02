"""
AI Coach Service - Interface to the AI brains
"""
import os
import sys
import json
from typing import Dict, Optional

# Add AI path
AI_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'ai'))
if AI_PATH not in sys.path:
    sys.path.insert(0, AI_PATH)

from central_controller import CentralController
from nlp.nlp_pipeline import NLPPipeline
from nlp.integration.nlp_ml_integration import NLPMLIntegration
from nlp.integration.nlp_prolog_integration import NLPPrologIntegration
from dialogue.coach_pipeline import CoachPipeline


class AICoachService:
    """Service to interact with AI Coach"""
    
    _instance = None
    _controller = None
    
    def __new__(cls):
        """Singleton pattern"""
        if cls._instance is None:
            cls._instance = super(AICoachService, cls).__new__(cls)
            cls._instance._initialized = False
        return cls._instance
    
    def __init__(self):
        """Initialize AI Coach"""
        if self._initialized:
            return
        
        try:
            print("🧠 Initializing AI Coach Service...")
            
            # Initialize all brains
            self.nlp = NLPPipeline()
            self.ml = NLPMLIntegration(self.nlp)
            self.logic = NLPPrologIntegration(self.nlp)
            self.personality = CoachPipeline()
            
            # Create central controller
            self._controller = CentralController(
                nlp_brain=self.nlp,
                logic_brain=self.logic,
                ml_brain=self.ml,
                personality_brain=self.personality
            )
            
            self._initialized = True
            print("✅ AI Coach Service ready!\n")
            
        except Exception as e:
            print(f"⚠️ AI Coach initialization error: {e}")
            print("⚠️ Running in fallback mode (no AI brains)")
            self._controller = CentralController()
            self._initialized = True
    
    def chat(self, message: str, user_data: Optional[Dict] = None) -> Dict:
        """
        Send message to AI Coach
        
        Args:
            message: User's message
            user_data: Optional user profile data
            
        Returns:
            Dictionary with response and metadata
        """
        try:
            # Process through central controller
            decision = self._controller.process(
                user_message=message,
                user_data=user_data or {},
                context={}
            )
            
            # Extract metadata
            nlp_data = decision.nlp_output.data if decision.nlp_output else {}
            ml_data = decision.ml_output.data if decision.ml_output else {}
            
            return {
                'response': decision.final_response,
                'workout_recommendation': decision.workout_recommendation,
                'safety_status': decision.safety_status,
                'confidence_score': decision.confidence_score,
                'emotion_detected': nlp_data.get('emotion'),
                'intent_detected': nlp_data.get('intent'),
                'energy_level': nlp_data.get('energy_level'),
                'brains_used': decision.decision_path,
                'processing_time_ms': decision.total_execution_time_ms,
                'metadata': {
                    'nlp_confidence': nlp_data.get('emotion_confidence'),
                    'ml_confidence': ml_data.get('confidence'),
                    'brains_active': decision.brains_used
                }
            }
            
        except Exception as e:
            print(f"❌ Chat error: {e}")
            return {
                'response': "I'm having trouble processing your request. Please try again.",
                'error': str(e),
                'workout_recommendation': 'REST',
                'safety_status': 'safe',
                'confidence_score': 0.0
            }
    
    def get_stats(self) -> Dict:
        """Get AI Coach statistics"""
        try:
            stats = self._controller.get_stats()
            return {
                'total_decisions': stats.get('total_decisions', 0),
                'nlp_calls': stats.get('nlp_calls', 0),
                'ml_calls': stats.get('ml_calls', 0),
                'logic_calls': stats.get('logic_calls', 0),
                'personality_calls': stats.get('personality_calls', 0),
                'safety_interventions': stats.get('safety_interventions', 0),
                'average_response_time_ms': stats.get('average_response_time_ms', 0)
            }
        except Exception as e:
            return {'error': str(e)}


# Global instance
ai_coach = AICoachService()
