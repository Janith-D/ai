"""
Coach Pipeline
Unified pipeline connecting all AI brains: NLP → ML → Prolog → Dialogue
"""

import sys
from pathlib import Path
from typing import Dict, Optional

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent))

from .personality import CoachPersonality
from .tone_adapter import ToneAdapter
from .response_templates import ResponseTemplates
from .dialogue_generator import DialogueGenerator, DialogueContext
from .memory_system import ConversationMemory


class CoachPipeline:
    """
    Unified AI Fitness Coach pipeline
    
    Architecture:
    User Message
         ↓
    NLP Analysis (emotion, intent, context)
         ↓
    ML Prediction (workout recommendation)
         ↓
    Prolog Validation (safety rules)
         ↓
    Dialogue Generation (Coach Atlas response)
         ↓
    Natural Response to User
    """
    
    def __init__(
        self,
        nlp_pipeline=None,
        ml_integration=None,
        prolog_integration=None,
        gemini_api_key: Optional[str] = None,
        enable_memory: bool = True
    ):
        """
        Initialize Coach Pipeline
        
        Args:
            nlp_pipeline: NLP pipeline instance
            ml_integration: NLP-ML integration instance
            prolog_integration: NLP-Prolog integration instance
            gemini_api_key: Gemini API key for dialogue generation
            enable_memory: Enable conversation memory
        """
        # Initialize dialogue components
        self.personality = CoachPersonality()
        self.tone_adapter = ToneAdapter()
        self.templates = ResponseTemplates()
        self.dialogue_generator = DialogueGenerator(
            api_key=gemini_api_key,
            personality=self.personality,
            tone_adapter=self.tone_adapter,
            templates=self.templates
        )
        
        # Initialize memory
        self.memory = ConversationMemory() if enable_memory else None
        
        # Store brain references
        self.nlp = nlp_pipeline
        self.ml_integration = ml_integration
        self.prolog_integration = prolog_integration
        
        # Stats
        self.stats = {
            'total_requests': 0,
            'nlp_analyses': 0,
            'ml_predictions': 0,
            'prolog_validations': 0,
            'responses_generated': 0,
            'safety_interventions': 0
        }
    
    def process_message(self, user_message: str, user_data: Optional[Dict] = None) -> Dict:
        """
        Process user message through full pipeline
        
        Args:
            user_message: User's input message
            user_data: Optional user data (age, weight, goals, etc.)
            
        Returns:
            Dictionary with response and metadata
        """
        self.stats['total_requests'] += 1
        
        result = {
            'user_message': user_message,
            'nlp_analysis': None,
            'ml_prediction': None,
            'prolog_reasoning': None,
            'response': None,
            'metadata': {},
            'errors': []
        }
        
        try:
            # Step 1: NLP Analysis
            nlp_analysis = self._run_nlp_analysis(user_message)
            result['nlp_analysis'] = nlp_analysis
            self.stats['nlp_analyses'] += 1
            
            # Step 2: ML Prediction (if NLP available)
            if self.ml_integration and nlp_analysis:
                ml_prediction = self._run_ml_prediction(nlp_analysis, user_data)
                result['ml_prediction'] = ml_prediction
                self.stats['ml_predictions'] += 1
            
            # Step 3: Prolog Validation (if available)
            if self.prolog_integration and nlp_analysis:
                prolog_reasoning = self._run_prolog_validation(nlp_analysis, result['ml_prediction'])
                result['prolog_reasoning'] = prolog_reasoning
                self.stats['prolog_validations'] += 1
                
                # Check for safety interventions
                if prolog_reasoning and prolog_reasoning.get('safety_intervention'):
                    self.stats['safety_interventions'] += 1
            
            # Step 4: Generate Response
            response = self._generate_response(result)
            result['response'] = response
            self.stats['responses_generated'] += 1
            
            # Step 5: Update Memory
            if self.memory and nlp_analysis:
                self._update_memory(
                    user_message=user_message,
                    bot_response=response,
                    nlp_analysis=nlp_analysis,
                    ml_prediction=result['ml_prediction']
                )
                result['metadata']['continuity_context'] = self.memory.get_continuity_context()
            
            # Add metadata
            result['metadata'].update({
                'pipeline_version': '1.0',
                'brains_used': self._get_active_brains(),
                'tone': nlp_analysis.get('emotion') if nlp_analysis else 'neutral',
                'safety_check': result['prolog_reasoning'] is not None
            })
            
        except Exception as e:
            result['errors'].append(f"Pipeline error: {str(e)}")
            result['response'] = self._generate_fallback_response(user_message)
        
        return result
    
    def _run_nlp_analysis(self, user_message: str) -> Optional[Dict]:
        """
        Run NLP analysis on user message
        
        Args:
            user_message: User's message
            
        Returns:
            NLP analysis results or None
        """
        if not self.nlp:
            return {
                'emotion': 'neutral',
                'intent': 'plan_workout',
                'extracted_context': {},
                'has_injury': False,
                'energy_level': None
            }
        
        try:
            # Run full NLP pipeline
            analysis = self.nlp.analyze(user_message)
            
            # Enhance with energy level estimation
            emotion = analysis.get('emotion', 'neutral')
            energy_map = {
                'tired': 20,
                'exhausted': 10,
                'demotivated': 40,
                'frustrated': 50,
                'anxious': 45,
                'motivated': 80,
                'excited': 90,
                'neutral': 60
            }
            analysis['energy_level'] = energy_map.get(emotion, 60)
            
            # Detect injury from context
            injury_keywords = ['pain', 'hurt', 'injured', 'sore', 'ache']
            has_injury = any(keyword in user_message.lower() for keyword in injury_keywords)
            analysis['has_injury'] = has_injury
            
            return analysis
            
        except Exception as e:
            print(f"⚠️ NLP analysis error: {e}")
            return None
    
    def _run_ml_prediction(
        self,
        nlp_analysis: Dict,
        user_data: Optional[Dict] = None
    ) -> Optional[Dict]:
        """
        Run ML prediction with NLP context
        
        Args:
            nlp_analysis: NLP analysis results
            user_data: User data
            
        Returns:
            ML prediction results or None
        """
        if not self.ml_integration:
            return None
        
        try:
            # Get recommendation from ML with NLP adjustments
            prediction = self.ml_integration.get_context_aware_recommendation(
                nlp_context=nlp_analysis,
                user_data=user_data or {}
            )
            
            return prediction
            
        except Exception as e:
            print(f"⚠️ ML prediction error: {e}")
            return None
    
    def _run_prolog_validation(
        self,
        nlp_analysis: Dict,
        ml_prediction: Optional[Dict] = None
    ) -> Optional[Dict]:
        """
        Run Prolog validation on recommendation
        
        Args:
            nlp_analysis: NLP analysis results
            ml_prediction: ML prediction results
            
        Returns:
            Prolog reasoning results or None
        """
        if not self.prolog_integration:
            return None
        
        try:
            # Generate Prolog facts from NLP
            facts = self.prolog_integration.generate_facts_from_nlp(nlp_analysis)
            
            # Check safety rules
            validation_result = {
                'facts_generated': len(facts),
                'validation': 'safe',
                'reasoning': None,
                'safety_intervention': False
            }
            
            # Simple safety checks (can be expanded with actual Prolog queries)
            if nlp_analysis.get('has_injury'):
                validation_result['validation'] = 'unsafe'
                validation_result['reasoning'] = 'Injury detected - rest required'
                validation_result['safety_intervention'] = True
            elif nlp_analysis.get('energy_level', 100) < 30:
                validation_result['validation'] = 'caution'
                validation_result['reasoning'] = 'Low energy - light activity recommended'
            
            return validation_result
            
        except Exception as e:
            print(f"⚠️ Prolog validation error: {e}")
            return None
    
    def _generate_response(self, pipeline_result: Dict) -> str:
        """
        Generate natural language response
        
        Args:
            pipeline_result: Results from pipeline processing
            
        Returns:
            Generated response string
        """
        nlp_analysis = pipeline_result.get('nlp_analysis')
        if not nlp_analysis:
            return self._generate_fallback_response(pipeline_result['user_message'])
        
        # Build dialogue context
        context = DialogueContext(
            user_message=pipeline_result['user_message'],
            nlp_analysis=nlp_analysis,
            ml_prediction=pipeline_result.get('ml_prediction'),
            prolog_reasoning=pipeline_result.get('prolog_reasoning'),
            memory=self.memory.get_user_summary() if self.memory else None
        )
        
        # Generate response
        return self.dialogue_generator.generate_response(context)
    
    def _generate_fallback_response(self, user_message: str) -> str:
        """
        Generate fallback response when pipeline fails
        
        Args:
            user_message: User's message
            
        Returns:
            Fallback response
        """
        return (
            "Hey! I'm Coach Atlas. I'm here to help with your fitness journey. "
            "Tell me about your workout goals, how you're feeling, or what you'd like to train today."
        )
    
    def _update_memory(
        self,
        user_message: str,
        bot_response: str,
        nlp_analysis: Dict,
        ml_prediction: Optional[Dict]
    ):
        """
        Update conversation memory
        
        Args:
            user_message: User's message
            bot_response: Bot's response
            nlp_analysis: NLP analysis
            ml_prediction: ML prediction
        """
        if not self.memory:
            return
        
        self.memory.add_turn(
            user_message=user_message,
            bot_response=bot_response,
            emotion=nlp_analysis.get('emotion', 'neutral'),
            intent=nlp_analysis.get('intent', 'unknown'),
            workout_recommended=ml_prediction.get('workout_type') if ml_prediction else None,
            energy_level=nlp_analysis.get('energy_level'),
            has_injury=nlp_analysis.get('has_injury', False)
        )
    
    def _get_active_brains(self) -> Dict[str, bool]:
        """
        Get status of active brains
        
        Returns:
            Dictionary with brain activation status
        """
        return {
            'nlp': self.nlp is not None,
            'ml': self.ml_integration is not None,
            'prolog': self.prolog_integration is not None,
            'dialogue': True,
            'memory': self.memory is not None
        }
    
    def get_stats(self) -> Dict:
        """
        Get pipeline statistics
        
        Returns:
            Dictionary with stats
        """
        return {
            **self.stats,
            'active_brains': self._get_active_brains(),
            'memory_enabled': self.memory is not None,
            'conversation_turns': len(self.memory.history) if self.memory else 0
        }
    
    def chat(self, message: str, user_data: Optional[Dict] = None) -> str:
        """
        Simple chat interface (returns only response text)
        
        Args:
            message: User message
            user_data: Optional user data
            
        Returns:
            Response string
        """
        result = self.process_message(message, user_data)
        return result['response']


# Test function
if __name__ == "__main__":
    print("=" * 80)
    print("🧠 COACH PIPELINE")
    print("=" * 80)
    print()
    
    # Initialize pipeline (without external integrations for testing)
    pipeline = CoachPipeline(enable_memory=True)
    
    print("✓ Pipeline initialized")
    print()
    
    # Test scenarios
    test_messages = [
        "I want to train chest today!",
        "I'm feeling really tired but want to workout",
        "My shoulder is hurting",
        "I don't feel motivated at all",
        "Let's do a killer leg workout!"
    ]
    
    print("=" * 80)
    print("💬 TEST CONVERSATIONS")
    print("=" * 80)
    print()
    
    for i, message in enumerate(test_messages, 1):
        print(f"{'='*80}")
        print(f"Conversation {i}")
        print(f"{'='*80}")
        print(f"👤 User: {message}")
        print()
        
        result = pipeline.process_message(message)
        
        print(f"🤖 Coach Atlas:")
        print(result['response'])
        print()
        
        if result['nlp_analysis']:
            print(f"📊 Analysis:")
            print(f"   Emotion: {result['nlp_analysis'].get('emotion')}")
            print(f"   Intent: {result['nlp_analysis'].get('intent')}")
            print(f"   Energy: {result['nlp_analysis'].get('energy_level')}%")
            print(f"   Has Injury: {result['nlp_analysis'].get('has_injury')}")
        print()
    
    # Show stats
    print("=" * 80)
    print("📊 PIPELINE STATISTICS")
    print("=" * 80)
    print()
    
    stats = pipeline.get_stats()
    for key, value in stats.items():
        if isinstance(value, dict):
            print(f"{key}:")
            for k, v in value.items():
                print(f"   {k}: {v}")
        else:
            print(f"{key}: {value}")
    
    # Show memory summary
    if pipeline.memory:
        print()
        print("=" * 80)
        print("🧠 MEMORY SUMMARY")
        print("=" * 80)
        print()
        
        user_summary = pipeline.memory.get_user_summary()
        for key, value in user_summary.items():
            print(f"{key}: {value}")
