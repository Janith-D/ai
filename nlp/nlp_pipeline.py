"""
Unified NLP Pipeline
Combines emotion detection, intent classification, and context extraction
"""

from typing import Dict, List, Optional
from datetime import datetime
import json

from .emotion_detector import EmotionDetector
from .intent_classifier import IntentClassifier
from .context_extractor import ContextExtractor
from .preprocessor import TextPreprocessor


class NLPPipeline:
    """
    Complete NLP pipeline for AI Fitness Coach
    
    Processes user messages through:
    1. Text preprocessing
    2. Emotion detection
    3. Intent classification
    4. Context extraction
    
    Outputs structured JSON for ML and Prolog integration
    """
    
    def __init__(self, 
                 load_emotion_model: bool = True,
                 confidence_threshold: float = 0.3):
        """
        Initialize the NLP pipeline
        
        Args:
            load_emotion_model: Whether to load the emotion detection model
            confidence_threshold: Minimum confidence for intent classification
        """
        print("🧠 Initializing NLP Pipeline...")
        
        # Initialize components
        self.preprocessor = TextPreprocessor(remove_stopwords=False)
        self.emotion_detector = EmotionDetector() if load_emotion_model else None
        self.intent_classifier = IntentClassifier(confidence_threshold=confidence_threshold)
        self.context_extractor = ContextExtractor()
        
        print("✓ NLP Pipeline ready\n")
    
    def analyze(self, 
                text: str, 
                user_id: Optional[str] = None,
                include_prolog: bool = False,
                timestamp: Optional[datetime] = None) -> Dict[str, any]:
        """
        Analyze user message through complete pipeline
        
        Args:
            text: User message text
            user_id: Optional user identifier
            include_prolog: Whether to generate Prolog facts
            timestamp: Optional timestamp (defaults to now)
            
        Returns:
            Complete analysis dictionary:
            {
                'text': 'original text',
                'preprocessed': 'cleaned text',
                'emotion': {...},
                'intent': {...},
                'context': {...},
                'prolog_facts': [...],  # if include_prolog=True
                'timestamp': '2025-11-12T10:30:00'
            }
        """
        if timestamp is None:
            timestamp = datetime.now()
        
        # Preprocess text
        preprocessed = self.preprocessor.preprocess(text, lowercase=True)
        
        # Detect emotion
        emotion_result = None
        if self.emotion_detector:
            emotion_result = self.emotion_detector.detect(text)
        
        # Classify intent
        intent_result = self.intent_classifier.classify(text)
        
        # Extract context
        context_result = self.context_extractor.extract_all(text)
        
        # Build result
        result = {
            'text': text,
            'preprocessed': preprocessed,
            'emotion': emotion_result,
            'intent': intent_result,
            'context': context_result,
            'timestamp': timestamp.isoformat()
        }
        
        # Add Prolog facts if requested
        if include_prolog and user_id:
            result['prolog_facts'] = self._generate_prolog_facts(
                user_id, emotion_result, intent_result, context_result, timestamp
            )
        
        return result
    
    def _generate_prolog_facts(self,
                               user_id: str,
                               emotion: Optional[Dict],
                               intent: Dict,
                               context: Dict,
                               timestamp: datetime) -> List[str]:
        """
        Generate Prolog facts from analysis
        
        Args:
            user_id: User identifier
            emotion: Emotion detection result
            intent: Intent classification result
            context: Context extraction result
            timestamp: Timestamp
            
        Returns:
            List of Prolog fact strings
        """
        facts = []
        date_str = timestamp.strftime('%Y-%m-%d')
        time_str = timestamp.strftime('%H:%M:%S')
        
        # Emotion facts
        if emotion:
            facts.append(
                f"emotion('{user_id}', {emotion['fitness_emotion']}, '{date_str}', {emotion['confidence']})."
            )
            facts.append(
                f"energy_level('{user_id}', {emotion['energy_level']}, '{date_str}')."
            )
        
        # Intent facts
        facts.append(
            f"intent('{user_id}', {intent['intent']}, '{date_str}', '{time_str}')."
        )
        
        # Context facts (from context extractor)
        context_facts = self.context_extractor.to_prolog_facts(user_id, '', timestamp)
        facts.extend(context_facts)
        
        return facts
    
    def analyze_batch(self, 
                     texts: List[str],
                     user_id: Optional[str] = None,
                     include_prolog: bool = False) -> List[Dict[str, any]]:
        """
        Analyze multiple messages
        
        Args:
            texts: List of user messages
            user_id: Optional user identifier
            include_prolog: Whether to generate Prolog facts
            
        Returns:
            List of analysis results
        """
        return [
            self.analyze(text, user_id=user_id, include_prolog=include_prolog)
            for text in texts
        ]
    
    def get_summary(self, text: str) -> str:
        """
        Get human-readable summary of analysis
        
        Args:
            text: User message
            
        Returns:
            Summary string
        """
        result = self.analyze(text)
        
        parts = []
        
        # Emotion
        if result['emotion']:
            emotion = result['emotion']
            parts.append(f"Emotion: {emotion['fitness_emotion']} ({emotion['confidence']:.0%})")
        
        # Intent
        intent = result['intent']
        parts.append(f"Intent: {intent['intent']} ({intent['confidence']:.0%})")
        
        # Context
        if result['context']:
            context_parts = []
            ctx = result['context']
            
            if 'muscle_groups' in ctx:
                context_parts.append(f"Muscles: {', '.join(ctx['muscle_groups'])}")
            if 'goal' in ctx:
                context_parts.append(f"Goal: {ctx['goal']}")
            if 'intensity' in ctx:
                context_parts.append(f"Intensity: {ctx['intensity']}")
            
            if context_parts:
                parts.append("Context: " + ", ".join(context_parts))
        
        return " | ".join(parts)
    
    def to_ml_features(self, text: str) -> Dict[str, any]:
        """
        Convert NLP analysis to features for ML model
        
        Args:
            text: User message
            
        Returns:
            Dictionary of features for ML
        """
        result = self.analyze(text)
        
        features = {}
        
        # Emotion features
        if result['emotion']:
            emotion = result['emotion']
            features['emotion_motivated'] = 1 if emotion['fitness_emotion'] == 'motivated' else 0
            features['emotion_tired'] = 1 if emotion['fitness_emotion'] == 'demotivated' else 0
            features['energy_level'] = {
                'low': 0,
                'medium': 1,
                'high': 2
            }.get(emotion['energy_level'], 1)
        
        # Intent features
        intent = result['intent']
        features['intent_workout'] = 1 if intent['intent'] == 'plan_workout' else 0
        features['intent_rest'] = 1 if intent['intent'] == 'rest_request' else 0
        
        # Context features
        context = result['context']
        if 'muscle_groups' in context:
            for muscle in ['chest', 'legs', 'back', 'arms', 'shoulders', 'abs', 'cardio']:
                features[f'target_{muscle}'] = 1 if muscle in context['muscle_groups'] else 0
        
        if 'intensity' in context:
            features['preferred_intensity'] = {
                'low': 0,
                'medium': 1,
                'high': 2
            }.get(context['intensity'], 1)
        
        return features
    
    def export_json(self, text: str, filepath: str):
        """
        Export analysis to JSON file
        
        Args:
            text: User message
            filepath: Output file path
        """
        result = self.analyze(text, include_prolog=True, user_id='demo_user')
        
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(result, f, indent=2, ensure_ascii=False)
        
        print(f"✓ Analysis exported to {filepath}")


# Test function
if __name__ == "__main__":
    # Initialize pipeline
    pipeline = NLPPipeline()
    
    # Test messages
    test_messages = [
        "I'm exhausted but I still want to train my legs",
        "Feeling super motivated today! Let's crush chest day!",
        "I'm too sore from yesterday, need a light recovery workout",
        "How many calories should I eat to lose weight?",
        "I don't feel like working out today, too lazy",
    ]
    
    print("=" * 80)
    print("🧠 NLP PIPELINE TEST")
    print("=" * 80)
    print()
    
    for i, msg in enumerate(test_messages, 1):
        print(f"\n{'='*80}")
        print(f"Test {i}: \"{msg}\"")
        print(f"{'='*80}\n")
        
        # Full analysis
        result = pipeline.analyze(msg, user_id='test_user', include_prolog=True)
        
        # Display results
        print("📝 PREPROCESSED:")
        print(f"  {result['preprocessed']}\n")
        
        if result['emotion']:
            print("😊 EMOTION:")
            emotion = result['emotion']
            print(f"  Base: {emotion['emotion']} ({emotion['confidence']:.1%})")
            print(f"  Fitness: {emotion['fitness_emotion']}")
            print(f"  Energy: {emotion['energy_level']}\n")
        
        print("🎯 INTENT:")
        intent = result['intent']
        print(f"  {intent['intent']} ({intent['confidence']:.1%})")
        if intent['all_intents']:
            print(f"  Alternatives: {intent['all_intents']}\n")
        
        if result['context']:
            print("🔍 CONTEXT:")
            for key, value in result['context'].items():
                print(f"  {key}: {value}")
            print()
        
        if result.get('prolog_facts'):
            print("🔧 PROLOG FACTS:")
            for fact in result['prolog_facts']:
                print(f"  {fact}")
            print()
        
        # ML Features
        ml_features = pipeline.to_ml_features(msg)
        print("🤖 ML FEATURES:")
        for key, value in ml_features.items():
            print(f"  {key}: {value}")
        
        print()
    
    print("\n" + "=" * 80)
    print("✓ NLP Pipeline Test Complete")
    print("=" * 80)
