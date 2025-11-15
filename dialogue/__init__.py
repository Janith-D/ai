"""
Personality & Dialogue Module
The expressive brain of the AI Fitness Coach
"""

from .personality import CoachPersonality
from .tone_adapter import ToneAdapter
from .response_templates import ResponseTemplates
from .dialogue_generator import DialogueGenerator
from .memory_system import ConversationMemory
from .coach_pipeline import CoachPipeline

__all__ = [
    'CoachPersonality',
    'ToneAdapter',
    'ResponseTemplates',
    'DialogueGenerator',
    'ConversationMemory',
    'CoachPipeline'
]

__version__ = '1.0.0'
