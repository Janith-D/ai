"""
Emotion Detection Module
Detects emotional state from user messages using pretrained transformers
"""

from transformers import pipeline
from typing import Dict, List, Tuple
import warnings


class EmotionDetector:
    """
    Detects emotions in text using pretrained DistilRoBERTa model
    
    Emotions detected:
    - anger, disgust, fear, joy, neutral, sadness, surprise
    
    Fitness-specific interpretations:
    - joy → motivated, energized
    - sadness → demotivated, low_energy
    - anger → frustrated
    - fear → anxious, uncertain
    - neutral → calm, balanced
    """
    
    # Emotion mappings for fitness context
    EMOTION_FITNESS_MAP = {
        'joy': 'motivated',
        'sadness': 'demotivated',
        'anger': 'frustrated',
        'fear': 'anxious',
        'surprise': 'surprised',
        'disgust': 'unmotivated',
        'neutral': 'neutral'
    }
    
    # Energy level mapping
    EMOTION_ENERGY_MAP = {
        'motivated': 'high',
        'demotivated': 'low',
        'frustrated': 'medium',
        'anxious': 'low',
        'surprised': 'medium',
        'unmotivated': 'low',
        'neutral': 'medium'
    }
    
    def __init__(self, model_name: str = "j-hartmann/emotion-english-distilroberta-base"):
        """
        Initialize emotion detector with pretrained model
        
        Args:
            model_name: Hugging Face model name for emotion classification
        """
        print(f"Loading emotion detection model: {model_name}...")
        
        # Suppress warnings about pipeline
        warnings.filterwarnings('ignore', category=FutureWarning)
        
        try:
            self.model = pipeline(
                "text-classification",
                model=model_name,
                top_k=None  # Get all emotion scores
            )
            print("✓ Emotion detection model loaded successfully")
        except Exception as e:
            print(f"⚠ Warning: Could not load emotion model: {e}")
            print("Using rule-based fallback emotion detection")
            self.model = None
    
    def _detect_emotion_fallback(self, text: str) -> Dict[str, any]:
        """
        Rule-based emotion detection fallback
        
        Args:
            text: Input text
            
        Returns:
            Emotion analysis dict
        """
        text_lower = text.lower()
        
        # Simple keyword-based detection
        if any(word in text_lower for word in ['tired', 'exhausted', 'sore', 'fatigue', 'worn']):
            emotion = 'sadness'
            confidence = 0.75
        elif any(word in text_lower for word in ['motivated', 'excited', 'pumped', 'ready', 'energized', 'strong']):
            emotion = 'joy'
            confidence = 0.80
        elif any(word in text_lower for word in ['frustrated', 'angry', 'annoyed', 'stuck']):
            emotion = 'anger'
            confidence = 0.75
        elif any(word in text_lower for word in ['worried', 'nervous', 'scared', 'anxious', 'uncertain']):
            emotion = 'fear'
            confidence = 0.70
        elif any(word in text_lower for word in ['lazy', 'unmotivated', 'bored', "don't want"]):
            emotion = 'disgust'
            confidence = 0.70
        else:
            emotion = 'neutral'
            confidence = 0.60
        
        fitness_emotion = self.EMOTION_FITNESS_MAP.get(emotion, 'neutral')
        
        return {
            'emotion': emotion,
            'fitness_emotion': fitness_emotion,
            'confidence': confidence,
            'energy_level': self.EMOTION_ENERGY_MAP.get(fitness_emotion, 'medium')
        }
    
    def detect(self, text: str) -> Dict[str, any]:
        """
        Detect emotion in text
        
        Args:
            text: Input text from user
            
        Returns:
            Dictionary with emotion analysis:
            {
                'emotion': 'joy',
                'fitness_emotion': 'motivated',
                'confidence': 0.95,
                'energy_level': 'high',
                'all_scores': [...]
            }
        """
        if not text or not text.strip():
            return {
                'emotion': 'neutral',
                'fitness_emotion': 'neutral',
                'confidence': 1.0,
                'energy_level': 'medium',
                'all_scores': []
            }
        
        # Use fallback if model not loaded
        if self.model is None:
            return self._detect_emotion_fallback(text)
        
        try:
            # Get emotion predictions
            results = self.model(text)[0]
            
            # Sort by score
            results_sorted = sorted(results, key=lambda x: x['score'], reverse=True)
            
            # Get top emotion
            top_emotion = results_sorted[0]['label']
            confidence = results_sorted[0]['score']
            
            # Map to fitness context
            fitness_emotion = self.EMOTION_FITNESS_MAP.get(top_emotion, 'neutral')
            energy_level = self.EMOTION_ENERGY_MAP.get(fitness_emotion, 'medium')
            
            return {
                'emotion': top_emotion,
                'fitness_emotion': fitness_emotion,
                'confidence': round(confidence, 3),
                'energy_level': energy_level,
                'all_scores': [
                    {'emotion': r['label'], 'score': round(r['score'], 3)}
                    for r in results_sorted[:3]  # Top 3 emotions
                ]
            }
            
        except Exception as e:
            print(f"Error in emotion detection: {e}")
            return self._detect_emotion_fallback(text)
    
    def detect_batch(self, texts: List[str]) -> List[Dict[str, any]]:
        """
        Detect emotions in multiple texts
        
        Args:
            texts: List of input texts
            
        Returns:
            List of emotion analysis dicts
        """
        return [self.detect(text) for text in texts]
    
    def is_positive_emotion(self, text: str) -> bool:
        """
        Check if text expresses positive emotion
        
        Args:
            text: Input text
            
        Returns:
            True if positive emotion detected
        """
        result = self.detect(text)
        positive_emotions = ['joy', 'motivated', 'surprise']
        return result['emotion'] in positive_emotions or result['fitness_emotion'] in positive_emotions
    
    def get_energy_level(self, text: str) -> str:
        """
        Get energy level from text
        
        Args:
            text: Input text
            
        Returns:
            Energy level: 'high', 'medium', or 'low'
        """
        result = self.detect(text)
        return result['energy_level']


# Test function
if __name__ == "__main__":
    detector = EmotionDetector()
    
    test_messages = [
        "I'm exhausted but I still want to train my legs",
        "Feeling super motivated today! Let's crush chest day!",
        "I'm so sore from yesterday's workout, need a break",
        "I don't feel like working out today, too lazy",
        "Worried I'm not making progress fast enough"
    ]
    
    print("\n🧠 Emotion Detection Test:\n")
    for msg in test_messages:
        result = detector.detect(msg)
        print(f"Message: \"{msg}\"")
        print(f"  → Emotion: {result['emotion']} (confidence: {result['confidence']})")
        print(f"  → Fitness Context: {result['fitness_emotion']} | Energy: {result['energy_level']}")
        print()
