"""
Language & Emotion Understanding Unit
The emotional brain of the AI Fitness Coach

This module provides:
- Emotion detection (tired, motivated, frustrated, happy, etc.)
- Intent recognition (workout, rest, diet questions, motivation)
- Context extraction (muscle groups, goals, timing)
- Integration with ML and Prolog reasoning systems
"""

from .emotion_detector import EmotionDetector
from .intent_classifier import IntentClassifier
from .context_extractor import ContextExtractor
from .nlp_pipeline import NLPPipeline

__all__ = [
    'EmotionDetector',
    'IntentClassifier', 
    'ContextExtractor',
    'NLPPipeline'
]

__version__ = '1.0.0'
