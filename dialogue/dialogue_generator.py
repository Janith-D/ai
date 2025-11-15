"""
Dialogue Generator
Integrates with Gemini API to generate Coach Atlas responses
"""

import os
import json
from typing import Dict, Optional, List
from dataclasses import dataclass

try:
    import google.generativeai as genai
    GEMINI_AVAILABLE = True
except ImportError:
    GEMINI_AVAILABLE = False

from .personality import CoachPersonality
from .tone_adapter import ToneAdapter
from .response_templates import ResponseTemplates


@dataclass
class DialogueContext:
    """Context for dialogue generation"""
    user_message: str
    nlp_analysis: Dict
    ml_prediction: Optional[Dict] = None
    prolog_reasoning: Optional[Dict] = None
    memory: Optional[Dict] = None
    tone_guidance: Optional[Dict] = None


class DialogueGenerator:
    """
    Generates Coach Atlas responses using Gemini API
    
    Combines:
    - Personality system (Coach Atlas character)
    - Tone adaptation (emotion-aware)
    - NLP analysis (emotion, intent, context)
    - ML predictions (workout recommendations)
    - Prolog reasoning (safety rules)
    - Conversation memory (continuity)
    """
    
    def __init__(
        self,
        api_key: Optional[str] = None,
        personality: Optional[CoachPersonality] = None,
        tone_adapter: Optional[ToneAdapter] = None,
        templates: Optional[ResponseTemplates] = None,
        use_fallback: bool = True
    ):
        """
        Initialize dialogue generator
        
        Args:
            api_key: Gemini API key (or set GEMINI_API_KEY env var)
            personality: Coach personality (defaults to Coach Atlas)
            tone_adapter: Tone adapter (defaults to new instance)
            templates: Response templates (defaults to new instance)
            use_fallback: Use template fallback if API fails
        """
        self.personality = personality or CoachPersonality()
        self.tone_adapter = tone_adapter or ToneAdapter()
        self.templates = templates or ResponseTemplates()
        self.use_fallback = use_fallback
        
        # Initialize Gemini
        self.model = None
        if GEMINI_AVAILABLE:
            api_key = api_key or os.getenv('GEMINI_API_KEY')
            if api_key:
                try:
                    genai.configure(api_key=api_key)
                    self.model = genai.GenerativeModel('gemini-pro')
                    print("✓ Gemini API initialized successfully")
                except Exception as e:
                    print(f"⚠️ Gemini API initialization failed: {e}")
                    if not use_fallback:
                        raise
            else:
                print("⚠️ No Gemini API key found (set GEMINI_API_KEY env var)")
        else:
            print("⚠️ google-generativeai not installed (pip install google-generativeai)")
    
    def generate_response(
        self,
        context: DialogueContext,
        max_length: int = 500
    ) -> str:
        """
        Generate Coach Atlas response
        
        Args:
            context: Dialogue context with all inputs
            max_length: Maximum response length in tokens
            
        Returns:
            Generated response
        """
        # Get tone guidance
        emotion = context.nlp_analysis.get('emotion', 'neutral')
        energy_level = context.nlp_analysis.get('energy_level')
        injury_present = context.nlp_analysis.get('has_injury', False)
        
        tone_guidance = self.tone_adapter.get_tone_guidance(
            emotion=emotion,
            energy_level=energy_level,
            injury_present=injury_present
        )
        
        # Try Gemini API first
        if self.model:
            try:
                response = self._generate_with_gemini(context, tone_guidance, max_length)
                if response:
                    return response
            except Exception as e:
                print(f"⚠️ Gemini API error: {e}")
                if not self.use_fallback:
                    raise
        
        # Fallback to template-based generation
        return self._generate_with_template(context, tone_guidance)
    
    def _generate_with_gemini(
        self,
        context: DialogueContext,
        tone_guidance: Dict,
        max_length: int
    ) -> Optional[str]:
        """
        Generate response using Gemini API
        
        Args:
            context: Dialogue context
            tone_guidance: Tone guidance from adapter
            max_length: Maximum response length
            
        Returns:
            Generated response or None
        """
        # Build prompt
        system_prompt = self.personality.get_system_prompt()
        user_prompt = self._build_prompt(context, tone_guidance)
        
        full_prompt = f"{system_prompt}\n\n{user_prompt}"
        
        # Generate
        response = self.model.generate_content(
            full_prompt,
            generation_config=genai.GenerationConfig(
                max_output_tokens=max_length,
                temperature=0.8,
                top_p=0.9,
                top_k=40
            )
        )
        
        if response and response.text:
            return response.text.strip()
        
        return None
    
    def _build_prompt(
        self,
        context: DialogueContext,
        tone_guidance: Dict
    ) -> str:
        """
        Build prompt for Gemini API
        
        Args:
            context: Dialogue context
            tone_guidance: Tone guidance from adapter
            
        Returns:
            Formatted prompt
        """
        prompt_parts = [
            "=== USER MESSAGE ===",
            context.user_message,
            "",
            "=== ANALYSIS ===",
            self._format_analysis(context),
            "",
            "=== TONE GUIDANCE ===",
            f"Tone: {tone_guidance['tone']} ({tone_guidance['description']})",
            f"Emphasis: {tone_guidance['emphasis_level']}",
            f"Key phrases: {', '.join(tone_guidance['key_phrases'])}",
            f"Emoji intensity: {tone_guidance['emoji_intensity']}",
            "",
            "=== YOUR RESPONSE ===",
            "Respond as Coach Atlas following the personality guidelines above."
        ]
        
        return "\n".join(prompt_parts)
    
    def _format_analysis(self, context: DialogueContext) -> str:
        """
        Format analysis section for prompt
        
        Args:
            context: Dialogue context
            
        Returns:
            Formatted analysis string
        """
        parts = []
        
        # NLP analysis
        nlp = context.nlp_analysis
        parts.append(f"Emotion: {nlp.get('emotion', 'unknown')}")
        parts.append(f"Intent: {nlp.get('intent', 'unknown')}")
        
        if nlp.get('energy_level') is not None:
            parts.append(f"Energy: {nlp['energy_level']}%")
        
        if nlp.get('has_injury'):
            parts.append(f"⚠️ Injury detected: {nlp.get('injury_type', 'unspecified')}")
        
        extracted = nlp.get('extracted_context', {})
        if extracted.get('muscle_groups'):
            parts.append(f"Target muscles: {', '.join(extracted['muscle_groups'])}")
        if extracted.get('goals'):
            parts.append(f"Goals: {', '.join(extracted['goals'])}")
        
        # ML prediction
        if context.ml_prediction:
            ml = context.ml_prediction
            parts.append(f"\nML Recommendation: {ml.get('workout_type', 'N/A')} (confidence: {ml.get('confidence', 0):.0%})")
            if ml.get('adjusted_by_nlp'):
                parts.append(f"Adjusted: {ml.get('adjustment_reason', 'NLP context')}")
        
        # Prolog reasoning
        if context.prolog_reasoning:
            prolog = context.prolog_reasoning
            if prolog.get('validation'):
                parts.append(f"\nSafety Check: {prolog['validation']}")
            if prolog.get('reasoning'):
                parts.append(f"Logic: {prolog['reasoning']}")
        
        # Memory
        if context.memory:
            mem = context.memory
            if mem.get('recent_patterns'):
                parts.append(f"\nRecent patterns: {mem['recent_patterns']}")
            if mem.get('notable_context'):
                parts.append(f"Context: {mem['notable_context']}")
        
        return "\n".join(parts)
    
    def _generate_with_template(
        self,
        context: DialogueContext,
        tone_guidance: Dict
    ) -> str:
        """
        Generate response using templates (fallback)
        
        Args:
            context: Dialogue context
            tone_guidance: Tone guidance from adapter
            
        Returns:
            Generated response
        """
        # Select template
        intent = context.nlp_analysis.get('intent', 'plan_workout')
        has_injury = context.nlp_analysis.get('has_injury', False)
        needs_motivation = tone_guidance['tone'] in ['encouraging', 'empathetic']
        
        template_name = self.templates.select_template(
            intent=intent,
            has_injury=has_injury,
            needs_motivation=needs_motivation
        )
        
        # Build fields
        fields = self._build_template_fields(context, tone_guidance, template_name)
        
        # Fill template
        try:
            return self.templates.fill_template(template_name, fields)
        except ValueError:
            # Ultimate fallback
            return self._generate_basic_response(context, tone_guidance)
    
    def _build_template_fields(
        self,
        context: DialogueContext,
        tone_guidance: Dict,
        template_name: str
    ) -> Dict[str, str]:
        """
        Build template fields from context
        
        Args:
            context: Dialogue context
            tone_guidance: Tone guidance
            template_name: Name of template
            
        Returns:
            Dictionary of template fields
        """
        nlp = context.nlp_analysis
        ml = context.ml_prediction or {}
        emotion = nlp.get('emotion', 'neutral')
        
        # Get supportive phrase for tone
        supportive_phrase = tone_guidance['key_phrases'][0]
        
        # Base fields
        fields = {
            'emotion_acknowledgment': self._get_emotion_acknowledgment(emotion, supportive_phrase),
            'recommendation': ml.get('workout_type', 'rest day'),
            'reasoning': ml.get('adjustment_reason', 'Based on your current state'),
            'motivation': self._get_motivation(emotion, tone_guidance),
            'next_action': self._get_next_action(ml.get('workout_type', 'rest'))
        }
        
        # Template-specific fields
        if template_name == 'injury_safety':
            fields.update({
                'urgency_acknowledgment': "Hold up — this needs attention.",
                'safety_priority': "Your safety is my top priority.",
                'explanation': "We can't risk making this worse.",
                'next_steps': "Rest, monitor the pain, and see a professional if it persists.",
                'reassurance': "We'll get through this and come back stronger."
            })
        elif template_name == 'rest_day':
            fields.update({
                'validation': supportive_phrase,
                'education': "Rest is when your body actually builds strength.",
                'reframe': "This isn't stopping — it's strategic recovery."
            })
        elif template_name == 'motivation_boost':
            fields.update({
                'validation': supportive_phrase,
                'perspective_shift': "You don't need motivation — you just need to start.",
                'win_reminder': "You've done this before, and you can do it again.",
                'small_step': "Just commit to 10 minutes. Start small."
            })
        
        return fields
    
    def _get_emotion_acknowledgment(self, emotion: str, supportive_phrase: str) -> str:
        """Get emotion acknowledgment"""
        ack_map = {
            'tired': "I hear you — you're feeling tired.",
            'demotivated': "I get it — motivation isn't always there.",
            'excited': "I love that energy!",
            'frustrated': "I understand the frustration.",
            'anxious': "I see you're feeling anxious about this."
        }
        return ack_map.get(emotion, supportive_phrase)
    
    def _get_motivation(self, emotion: str, tone_guidance: Dict) -> str:
        """Get motivation statement"""
        if tone_guidance['tone'] == 'gentle':
            return "You're doing great just by showing up."
        elif tone_guidance['tone'] == 'encouraging':
            return "You've got this — one step at a time."
        elif tone_guidance['tone'] == 'energetic':
            return "Let's make today count!"
        else:
            return "Keep moving forward."
    
    def _get_next_action(self, workout_type: str) -> str:
        """Get next action"""
        return f"Let's start with {workout_type.lower()}. You ready?"
    
    def _generate_basic_response(
        self,
        context: DialogueContext,
        tone_guidance: Dict
    ) -> str:
        """
        Generate basic response (ultimate fallback)
        
        Args:
            context: Dialogue context
            tone_guidance: Tone guidance
            
        Returns:
            Basic response
        """
        nlp = context.nlp_analysis
        ml = context.ml_prediction or {}
        
        emotion = nlp.get('emotion', 'neutral')
        workout = ml.get('workout_type', 'rest day')
        supportive_phrase = tone_guidance['key_phrases'][0]
        
        return f"{supportive_phrase}\n\nBased on your current state, I recommend: {workout}.\n\nLet's take this one step at a time. You've got this!"


# Test function
if __name__ == "__main__":
    print("=" * 80)
    print("🗣️ DIALOGUE GENERATOR")
    print("=" * 80)
    print()
    
    generator = DialogueGenerator(use_fallback=True)
    
    # Test context
    test_context = DialogueContext(
        user_message="I'm exhausted but I still want to train my legs",
        nlp_analysis={
            'emotion': 'tired',
            'intent': 'plan_workout',
            'energy_level': 20,
            'has_injury': False,
            'extracted_context': {
                'muscle_groups': ['legs'],
                'goals': [],
                'intensity': None
            }
        },
        ml_prediction={
            'workout_type': 'CARDIO',
            'confidence': 0.65,
            'adjusted_by_nlp': True,
            'adjustment_reason': 'Low energy detected - switched from LEGS to light CARDIO'
        },
        prolog_reasoning={
            'validation': 'safe',
            'reasoning': 'Low energy requires adjustment'
        }
    )
    
    print("📋 Test Context:")
    print(f"   User: {test_context.user_message}")
    print(f"   Emotion: {test_context.nlp_analysis['emotion']}")
    print(f"   Energy: {test_context.nlp_analysis['energy_level']}%")
    print(f"   ML Recommendation: {test_context.ml_prediction['workout_type']}")
    print()
    
    print("=" * 80)
    print("💬 GENERATED RESPONSE:")
    print("=" * 80)
    response = generator.generate_response(test_context)
    print(response)
    print()
    
    print("=" * 80)
    print(f"✓ Using {'Gemini API' if generator.model else 'Template Fallback'}")
