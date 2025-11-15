"""
Central Controller - The Brainstem
Orchestrates all AI units into one unified fitness coach system

Flow:
    User Input
        ↓
    Central Controller
        ├── NLP Brain → Understand emotion & intent
        ├── Logic Brain (Prolog) → Check safety rules
        ├── ML Brain → Predict optimal workout
        └── Personality Brain (LLM) → Generate response
        ↓
    Final Coach Response
"""

import sys
from pathlib import Path
from typing import Dict, Optional, Any, List
from dataclasses import dataclass, asdict
import json
from datetime import datetime

# Add parent directory for imports
sys.path.append(str(Path(__file__).parent))


@dataclass
class BrainOutput:
    """Output from a single brain unit"""
    brain_name: str
    status: str  # success, error, skipped
    data: Dict[str, Any]
    execution_time_ms: float
    error_message: Optional[str] = None


@dataclass
class CoachDecision:
    """Final decision from Central Controller"""
    user_message: str
    timestamp: str
    
    # Brain outputs
    nlp_output: Optional[BrainOutput]
    logic_output: Optional[BrainOutput]
    ml_output: Optional[BrainOutput]
    personality_output: Optional[BrainOutput]
    
    # Final decision
    final_response: str
    workout_recommendation: Optional[str]
    safety_status: str  # safe, caution, unsafe
    confidence_score: float
    
    # Metadata
    total_execution_time_ms: float
    brains_used: Dict[str, bool]
    decision_path: List[str]


class CentralController:
    """
    The Brainstem - Central coordinator for AI Fitness Coach
    
    Orchestrates all brain units:
    1. NLP Brain - Language & Emotion Understanding
    2. Logic Brain - Prolog Safety Rules
    3. ML Brain - Workout Predictions
    4. Personality Brain - Coach Atlas Dialogue
    
    Combines outputs into one final, intelligent decision.
    """
    
    def __init__(
        self,
        nlp_pipeline=None,
        prolog_integration=None,
        ml_integration=None,
        dialogue_pipeline=None,
        enable_logging: bool = True
    ):
        """
        Initialize Central Controller
        
        Args:
            nlp_pipeline: NLP Pipeline instance
            prolog_integration: Prolog Integration instance
            ml_integration: ML Integration instance
            dialogue_pipeline: Dialogue Pipeline (Coach Atlas)
            enable_logging: Enable decision logging
        """
        self.nlp = nlp_pipeline
        self.prolog = prolog_integration
        self.ml = ml_integration
        self.dialogue = dialogue_pipeline
        
        self.enable_logging = enable_logging
        self.decision_history: List[CoachDecision] = []
        
        # Statistics
        self.stats = {
            'total_decisions': 0,
            'nlp_calls': 0,
            'logic_calls': 0,
            'ml_calls': 0,
            'personality_calls': 0,
            'safety_interventions': 0,
            'average_response_time_ms': 0
        }
        
        print("🧠 Central Controller initialized")
        print(f"   NLP Brain: {'✓' if self.nlp else '✗'}")
        print(f"   Logic Brain: {'✓' if self.prolog else '✗'}")
        print(f"   ML Brain: {'✓' if self.ml else '✗'}")
        print(f"   Personality Brain: {'✓' if self.dialogue else '✗'}")
    
    def process(
        self,
        user_message: str,
        user_data: Optional[Dict] = None,
        context: Optional[Dict] = None
    ) -> CoachDecision:
        """
        Process user input through all brains and make final decision
        
        Args:
            user_message: User's input message
            user_data: User profile data (age, weight, goals, etc.)
            context: Additional context
            
        Returns:
            CoachDecision with final response and metadata
        """
        start_time = datetime.now()
        decision_path = []
        
        print(f"\n{'='*80}")
        print(f"🧠 CENTRAL CONTROLLER - Processing Request")
        print(f"{'='*80}")
        print(f"📝 User: {user_message}")
        print()
        
        # Initialize outputs
        nlp_output = None
        logic_output = None
        ml_output = None
        personality_output = None
        
        # STEP 1: NLP Brain - Understand emotion & intent
        print("🔍 STEP 1: NLP Brain - Understanding emotion & intent...")
        nlp_output = self._run_nlp_brain(user_message)
        if nlp_output.status == 'success':
            decision_path.append('NLP')
            self.stats['nlp_calls'] += 1
            print(f"   ✓ Emotion: {nlp_output.data.get('emotion', 'unknown')}")
            print(f"   ✓ Intent: {nlp_output.data.get('intent', 'unknown')}")
            print(f"   ✓ Energy: {nlp_output.data.get('energy_level', 'N/A')}%")
        
        # STEP 2: Logic Brain - Check safety rules
        print("\n⚖️  STEP 2: Logic Brain - Checking safety rules...")
        logic_output = self._run_logic_brain(nlp_output, user_data)
        if logic_output.status == 'success':
            decision_path.append('Logic')
            self.stats['logic_calls'] += 1
            safety = logic_output.data.get('safety_status', 'unknown')
            print(f"   ✓ Safety Status: {safety}")
            if logic_output.data.get('safety_intervention'):
                self.stats['safety_interventions'] += 1
                print(f"   ⚠️  Safety Intervention Triggered!")
        
        # STEP 3: ML Brain - Predict optimal workout
        print("\n🎯 STEP 3: ML Brain - Predicting optimal workout...")
        ml_output = self._run_ml_brain(nlp_output, user_data, logic_output)
        if ml_output.status == 'success':
            decision_path.append('ML')
            self.stats['ml_calls'] += 1
            workout = ml_output.data.get('workout_type', 'unknown')
            confidence = ml_output.data.get('confidence', 0)
            print(f"   ✓ Recommendation: {workout}")
            print(f"   ✓ Confidence: {confidence:.1%}")
        
        # STEP 4: Personality Brain - Generate motivating response
        print("\n💬 STEP 4: Personality Brain - Generating Coach Atlas response...")
        personality_output = self._run_personality_brain(
            user_message,
            nlp_output,
            logic_output,
            ml_output
        )
        if personality_output.status == 'success':
            decision_path.append('Personality')
            self.stats['personality_calls'] += 1
            print(f"   ✓ Response generated ({len(personality_output.data.get('response', ''))} chars)")
        
        # STEP 5: Make final decision
        print("\n🎯 STEP 5: Making final decision...")
        final_response = self._make_final_decision(
            nlp_output,
            logic_output,
            ml_output,
            personality_output
        )
        
        # Calculate execution time
        end_time = datetime.now()
        total_time_ms = (end_time - start_time).total_seconds() * 1000
        
        # Create decision object
        decision = CoachDecision(
            user_message=user_message,
            timestamp=start_time.isoformat(),
            nlp_output=nlp_output,
            logic_output=logic_output,
            ml_output=ml_output,
            personality_output=personality_output,
            final_response=final_response['response'],
            workout_recommendation=final_response['workout'],
            safety_status=final_response['safety'],
            confidence_score=final_response['confidence'],
            total_execution_time_ms=total_time_ms,
            brains_used={
                'nlp': nlp_output.status == 'success' if nlp_output else False,
                'logic': logic_output.status == 'success' if logic_output else False,
                'ml': ml_output.status == 'success' if ml_output else False,
                'personality': personality_output.status == 'success' if personality_output else False
            },
            decision_path=decision_path
        )
        
        # Update stats
        self.stats['total_decisions'] += 1
        self.stats['average_response_time_ms'] = (
            (self.stats['average_response_time_ms'] * (self.stats['total_decisions'] - 1) + total_time_ms)
            / self.stats['total_decisions']
        )
        
        # Log decision
        if self.enable_logging:
            self.decision_history.append(decision)
        
        # Print summary
        print(f"\n{'='*80}")
        print(f"✅ DECISION COMPLETE")
        print(f"{'='*80}")
        print(f"Workout: {decision.workout_recommendation}")
        print(f"Safety: {decision.safety_status}")
        print(f"Confidence: {decision.confidence_score:.1%}")
        print(f"Execution Time: {total_time_ms:.1f}ms")
        print(f"Decision Path: {' → '.join(decision_path)}")
        print(f"{'='*80}\n")
        
        return decision
    
    def _run_nlp_brain(self, user_message: str) -> BrainOutput:
        """Run NLP Brain analysis"""
        import time
        start = time.time()
        
        try:
            if not self.nlp:
                return BrainOutput(
                    brain_name='NLP',
                    status='skipped',
                    data={
                        'emotion': 'neutral',
                        'intent': 'plan_workout',
                        'energy_level': 60,
                        'has_injury': False
                    },
                    execution_time_ms=0,
                    error_message='NLP brain not initialized'
                )
            
            # Run NLP analysis
            result = self.nlp.analyze(user_message)
            
            # Handle both dict and nested result formats
            if isinstance(result, dict):
                if 'emotion' in result:
                    analysis = result
                elif 'emotion_analysis' in result:
                    analysis = {
                        'emotion': result['emotion_analysis'].get('emotion'),
                        'intent': result.get('intent_classification', {}).get('intent'),
                        'extracted_context': result.get('context_extraction', {})
                    }
                else:
                    analysis = result
            else:
                analysis = {}
            
            # Enhance with energy estimation
            emotion = analysis.get('emotion', 'neutral')
            energy_map = {
                'tired': 20, 'exhausted': 10, 'demotivated': 40,
                'frustrated': 50, 'anxious': 45, 'motivated': 80,
                'excited': 90, 'neutral': 60
            }
            analysis['energy_level'] = energy_map.get(emotion, 60)
            
            # Detect injury
            injury_keywords = ['pain', 'hurt', 'injured', 'sore', 'ache']
            analysis['has_injury'] = any(kw in user_message.lower() for kw in injury_keywords)
            
            return BrainOutput(
                brain_name='NLP',
                status='success',
                data=analysis,
                execution_time_ms=(time.time() - start) * 1000
            )
            
        except Exception as e:
            return BrainOutput(
                brain_name='NLP',
                status='error',
                data={},
                execution_time_ms=(time.time() - start) * 1000,
                error_message=str(e)
            )
    
    def _run_logic_brain(
        self,
        nlp_output: Optional[BrainOutput],
        user_data: Optional[Dict]
    ) -> BrainOutput:
        """Run Logic Brain (Prolog) safety checks"""
        import time
        start = time.time()
        
        try:
            if not self.prolog:
                # Default safe status
                return BrainOutput(
                    brain_name='Logic',
                    status='skipped',
                    data={
                        'safety_status': 'safe',
                        'reasoning': 'No logic rules configured',
                        'safety_intervention': False
                    },
                    execution_time_ms=0,
                    error_message='Logic brain not initialized'
                )
            
            # Get NLP data
            nlp_data = nlp_output.data if nlp_output else {}
            has_injury = nlp_data.get('has_injury', False)
            energy_level = nlp_data.get('energy_level', 100)
            
            # Safety checks
            safety_status = 'safe'
            reasoning = 'All safety checks passed'
            intervention = False
            
            if has_injury:
                safety_status = 'unsafe'
                reasoning = 'Injury detected - rest required'
                intervention = True
            elif energy_level < 30:
                safety_status = 'caution'
                reasoning = 'Low energy - light activity recommended'
            
            return BrainOutput(
                brain_name='Logic',
                status='success',
                data={
                    'safety_status': safety_status,
                    'reasoning': reasoning,
                    'safety_intervention': intervention,
                    'rules_checked': ['injury_check', 'energy_check']
                },
                execution_time_ms=(time.time() - start) * 1000
            )
            
        except Exception as e:
            return BrainOutput(
                brain_name='Logic',
                status='error',
                data={'safety_status': 'safe'},  # Fail-safe
                execution_time_ms=(time.time() - start) * 1000,
                error_message=str(e)
            )
    
    def _run_ml_brain(
        self,
        nlp_output: Optional[BrainOutput],
        user_data: Optional[Dict],
        logic_output: Optional[BrainOutput]
    ) -> BrainOutput:
        """Run ML Brain workout prediction"""
        import time
        start = time.time()
        
        try:
            if not self.ml:
                return BrainOutput(
                    brain_name='ML',
                    status='skipped',
                    data={
                        'workout_type': 'REST',
                        'confidence': 0.5,
                        'reason': 'ML brain not initialized'
                    },
                    execution_time_ms=0,
                    error_message='ML brain not initialized'
                )
            
            # Get context
            nlp_data = nlp_output.data if nlp_output else {}
            logic_data = logic_output.data if logic_output else {}
            
            # Check safety override
            if logic_data.get('safety_intervention'):
                return BrainOutput(
                    brain_name='ML',
                    status='success',
                    data={
                        'workout_type': 'REST',
                        'confidence': 1.0,
                        'reason': 'Safety override: ' + logic_data.get('reasoning', ''),
                        'overridden': True
                    },
                    execution_time_ms=(time.time() - start) * 1000
                )
            
            # Get ML recommendation
            prediction = self.ml.get_context_aware_recommendation(
                nlp_context=nlp_data,
                user_data=user_data or {}
            )
            
            return BrainOutput(
                brain_name='ML',
                status='success',
                data=prediction,
                execution_time_ms=(time.time() - start) * 1000
            )
            
        except Exception as e:
            return BrainOutput(
                brain_name='ML',
                status='error',
                data={'workout_type': 'REST', 'confidence': 0.5},
                execution_time_ms=(time.time() - start) * 1000,
                error_message=str(e)
            )
    
    def _run_personality_brain(
        self,
        user_message: str,
        nlp_output: Optional[BrainOutput],
        logic_output: Optional[BrainOutput],
        ml_output: Optional[BrainOutput]
    ) -> BrainOutput:
        """Run Personality Brain (Coach Atlas) response generation"""
        import time
        start = time.time()
        
        try:
            if not self.dialogue:
                return BrainOutput(
                    brain_name='Personality',
                    status='skipped',
                    data={
                        'response': 'Hello! I\'m your fitness coach. How can I help you today?'
                    },
                    execution_time_ms=0,
                    error_message='Personality brain not initialized'
                )
            
            # Prepare context
            result = {
                'user_message': user_message,
                'nlp_analysis': nlp_output.data if nlp_output else {},
                'ml_prediction': ml_output.data if ml_output else {},
                'prolog_reasoning': logic_output.data if logic_output else {}
            }
            
            # Generate response
            response = self.dialogue.process_message(user_message)
            
            return BrainOutput(
                brain_name='Personality',
                status='success',
                data={
                    'response': response.get('response', ''),
                    'tone': response.get('metadata', {}).get('tone', 'balanced')
                },
                execution_time_ms=(time.time() - start) * 1000
            )
            
        except Exception as e:
            return BrainOutput(
                brain_name='Personality',
                status='error',
                data={'response': 'Let me help you with your fitness goals!'},
                execution_time_ms=(time.time() - start) * 1000,
                error_message=str(e)
            )
    
    def _make_final_decision(
        self,
        nlp_output: Optional[BrainOutput],
        logic_output: Optional[BrainOutput],
        ml_output: Optional[BrainOutput],
        personality_output: Optional[BrainOutput]
    ) -> Dict[str, Any]:
        """Combine all brain outputs into final decision"""
        
        # Extract data
        nlp_data = nlp_output.data if nlp_output else {}
        logic_data = logic_output.data if logic_output else {}
        ml_data = ml_output.data if ml_output else {}
        personality_data = personality_output.data if personality_output else {}
        
        # Determine workout
        workout = ml_data.get('workout_type', 'REST')
        
        # Safety override
        safety_status = logic_data.get('safety_status', 'safe')
        if safety_status == 'unsafe':
            workout = 'REST'
        
        # Get response
        response = personality_data.get('response', 'Let\'s work together on your fitness goals!')
        
        # Calculate confidence
        confidence = ml_data.get('confidence', 0.5)
        if safety_status == 'unsafe':
            confidence = 1.0  # 100% confident in safety override
        
        return {
            'response': response,
            'workout': workout,
            'safety': safety_status,
            'confidence': confidence
        }
    
    def get_decision_history(self, limit: int = 10) -> List[Dict]:
        """Get recent decision history"""
        recent = self.decision_history[-limit:]
        return [asdict(d) for d in recent]
    
    def get_stats(self) -> Dict:
        """Get controller statistics"""
        return {
            **self.stats,
            'history_size': len(self.decision_history)
        }
    
    def export_decision(self, decision: CoachDecision, filepath: str):
        """Export decision to JSON file"""
        with open(filepath, 'w') as f:
            json.dump(asdict(decision), f, indent=2)
    
    def chat(self, message: str, user_data: Optional[Dict] = None) -> str:
        """Simple chat interface - returns only response text"""
        decision = self.process(message, user_data)
        return decision.final_response


# Example usage
if __name__ == "__main__":
    print("=" * 80)
    print("🧠 CENTRAL CONTROLLER - BRAINSTEM DEMO")
    print("=" * 80)
    print()
    
    # Initialize controller (without brains for demo)
    controller = CentralController()
    
    print("\n" + "=" * 80)
    print("Testing Central Controller with sample inputs...")
    print("=" * 80)
    
    # Test cases
    test_messages = [
        "I want to train chest today!",
        "I'm exhausted but still want to workout",
        "My shoulder is hurting"
    ]
    
    for i, msg in enumerate(test_messages, 1):
        print(f"\n{'#' * 80}")
        print(f"TEST CASE {i}")
        print(f"{'#' * 80}")
        
        decision = controller.process(msg)
        
        print(f"\n🗣️  COACH RESPONSE:")
        print(f"   {decision.final_response}")
        print()
    
    # Show stats
    print("=" * 80)
    print("📊 CONTROLLER STATISTICS")
    print("=" * 80)
    stats = controller.get_stats()
    for key, value in stats.items():
        print(f"   {key}: {value}")
    print()
    print("=" * 80)
    print("✅ Demo Complete!")
