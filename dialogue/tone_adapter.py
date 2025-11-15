"""
Tone Adapter
Adjusts communication tone based on user's emotional state
"""

from typing import Dict, Optional
from dataclasses import dataclass


@dataclass
class ToneProfile:
    """Configuration for a specific communication tone"""
    name: str
    description: str
    word_choices: list
    sentence_style: str
    emphasis_level: str
    pacing: str
    supportive_phrases: list


class ToneAdapter:
    """
    Adapts Coach Atlas tone based on user emotion and context
    
    Maps emotions to appropriate tones:
    - Tired/low energy → gentle, supportive
    - Unmotivated → encouraging, gentle push
    - Excited → energetic, enthusiastic
    - Frustrated → calm, grounding
    - Anxious → reassuring, steady
    """
    
    def __init__(self):
        """Initialize tone profiles"""
        self.tones = self._initialize_tones()
        
    def _initialize_tones(self) -> Dict[str, ToneProfile]:
        """
        Create tone profiles for different situations
        
        Returns:
            Dictionary of tone profiles
        """
        return {
            'gentle': ToneProfile(
                name='gentle',
                description='Soft, understanding, non-demanding',
                word_choices=['maybe', 'could', 'might', 'gently', 'ease into'],
                sentence_style='short, simple, reassuring',
                emphasis_level='low',
                pacing='slow',
                supportive_phrases=[
                    "I hear you",
                    "That's totally okay",
                    "No pressure",
                    "Listen to your body",
                    "It's okay to take it easy",
                    "You're doing great just by showing up",
                    "Rest is progress too"
                ]
            ),
            
            'encouraging': ToneProfile(
                name='encouraging',
                description='Positive reinforcement, gentle motivation',
                word_choices=['you can', 'let\'s try', 'you\'ve got this', 'believe', 'capable'],
                sentence_style='positive, forward-looking',
                emphasis_level='medium',
                pacing='moderate',
                supportive_phrases=[
                    "You've got this",
                    "I believe in you",
                    "Let's do this together",
                    "You're stronger than you think",
                    "One step at a time",
                    "Small progress is still progress",
                    "You've done hard things before"
                ]
            ),
            
            'energetic': ToneProfile(
                name='energetic',
                description='High energy, enthusiastic, action-oriented',
                word_choices=['let\'s go', 'crush', 'power', 'strong', 'amazing', 'fire'],
                sentence_style='exclamatory, dynamic, confident',
                emphasis_level='high',
                pacing='fast',
                supportive_phrases=[
                    "LET'S DO THIS",
                    "You're on fire today",
                    "This is your moment",
                    "Channel that energy",
                    "You're unstoppable",
                    "Make today count",
                    "Time to level up"
                ]
            ),
            
            'calm': ToneProfile(
                name='calm',
                description='Steady, grounding, rational',
                word_choices=['breathe', 'steady', 'step back', 'perspective', 'balance'],
                sentence_style='measured, clear, logical',
                emphasis_level='low-medium',
                pacing='slow-moderate',
                supportive_phrases=[
                    "Let's take a breath",
                    "Step back for a moment",
                    "It's okay to pause",
                    "Frustration is normal",
                    "We'll figure this out",
                    "One thing at a time",
                    "Progress isn't always linear"
                ]
            ),
            
            'empathetic': ToneProfile(
                name='empathetic',
                description='Deep understanding, validating, supportive',
                word_choices=['understand', 'feel', 'valid', 'real', 'with you'],
                sentence_style='validating, warm, present',
                emphasis_level='medium',
                pacing='moderate',
                supportive_phrases=[
                    "I understand how you feel",
                    "That sounds really hard",
                    "Your feelings are valid",
                    "You're not alone in this",
                    "It's okay to feel this way",
                    "I'm here with you",
                    "This is tough, and you're handling it"
                ]
            ),
            
            'balanced': ToneProfile(
                name='balanced',
                description='Neutral, professional, clear',
                word_choices=['recommend', 'suggest', 'consider', 'based on', 'effective'],
                sentence_style='clear, informative, professional',
                emphasis_level='medium',
                pacing='moderate',
                supportive_phrases=[
                    "Here's what makes sense",
                    "Based on your situation",
                    "Let me explain",
                    "Here's the plan",
                    "This is what I recommend",
                    "Let's work through this",
                    "Here's what we know"
                ]
            ),
            
            'serious': ToneProfile(
                name='serious',
                description='Safety-focused, firm, protective',
                word_choices=['must', 'important', 'serious', 'priority', 'safety'],
                sentence_style='direct, clear, urgent when needed',
                emphasis_level='high',
                pacing='moderate-fast',
                supportive_phrases=[
                    "This is important",
                    "Safety comes first",
                    "We need to address this",
                    "Let's not take chances",
                    "Your health is priority",
                    "I'm concerned about",
                    "We need to be careful here"
                ]
            )
        }
    
    def select_tone(
        self,
        emotion: str,
        energy_level: Optional[float] = None,
        context: Optional[str] = None,
        injury_present: bool = False
    ) -> str:
        """
        Select appropriate tone based on user state
        
        Args:
            emotion: User's emotional state
            energy_level: User's energy (0-100)
            context: Situation context
            injury_present: Whether user has injury
            
        Returns:
            Selected tone name
        """
        # Injury always triggers serious tone
        if injury_present:
            return 'serious'
        
        # Map emotions to tones
        emotion_tone_map = {
            'motivated': 'encouraging',
            'demotivated': 'encouraging',
            'tired': 'gentle',
            'exhausted': 'gentle',
            'frustrated': 'calm',
            'angry': 'calm',
            'anxious': 'empathetic',
            'stressed': 'empathetic',
            'sad': 'empathetic',
            'depressed': 'empathetic',
            'excited': 'energetic',
            'confident': 'energetic',
            'neutral': 'balanced',
            'calm': 'balanced'
        }
        
        # Base tone from emotion
        base_tone = emotion_tone_map.get(emotion, 'balanced')
        
        # Adjust based on energy level
        if energy_level is not None:
            if energy_level < 30 and base_tone not in ['gentle', 'empathetic']:
                base_tone = 'gentle'
            elif energy_level > 80 and base_tone == 'balanced':
                base_tone = 'encouraging'
        
        # Context overrides
        if context:
            if 'injury' in context.lower() or 'pain' in context.lower():
                return 'serious'
            elif 'rest' in context.lower() and base_tone not in ['gentle', 'empathetic']:
                base_tone = 'calm'
        
        return base_tone
    
    def get_tone_profile(self, tone_name: str) -> ToneProfile:
        """
        Get tone profile by name
        
        Args:
            tone_name: Name of tone
            
        Returns:
            ToneProfile object
        """
        return self.tones.get(tone_name, self.tones['balanced'])
    
    def get_supportive_phrase(self, tone_name: str) -> str:
        """
        Get a random supportive phrase for tone
        
        Args:
            tone_name: Name of tone
            
        Returns:
            Supportive phrase
        """
        import random
        tone = self.get_tone_profile(tone_name)
        return random.choice(tone.supportive_phrases)
    
    def blend_tones(self, primary: str, secondary: str, weight: float = 0.7) -> Dict:
        """
        Blend two tones (for complex emotional states)
        
        Args:
            primary: Primary tone name
            secondary: Secondary tone name
            weight: Weight of primary (0-1)
            
        Returns:
            Dictionary with blended tone guidance
        """
        primary_tone = self.get_tone_profile(primary)
        secondary_tone = self.get_tone_profile(secondary)
        
        return {
            'primary_tone': primary,
            'secondary_tone': secondary,
            'weight': weight,
            'guidance': f"Lead with {primary_tone.description}, but incorporate {secondary_tone.description}",
            'key_phrases': primary_tone.supportive_phrases[:2] + secondary_tone.supportive_phrases[:1],
            'emphasis': primary_tone.emphasis_level if weight > 0.6 else 'medium',
            'pacing': primary_tone.pacing
        }
    
    def get_tone_guidance(
        self,
        emotion: str,
        energy_level: Optional[float] = None,
        context: Optional[str] = None,
        injury_present: bool = False
    ) -> Dict:
        """
        Get complete tone guidance for response generation
        
        Args:
            emotion: User's emotional state
            energy_level: User's energy (0-100)
            context: Situation context
            injury_present: Whether user has injury
            
        Returns:
            Dictionary with tone guidance
        """
        tone_name = self.select_tone(emotion, energy_level, context, injury_present)
        tone_profile = self.get_tone_profile(tone_name)
        
        return {
            'tone': tone_name,
            'description': tone_profile.description,
            'emphasis_level': tone_profile.emphasis_level,
            'pacing': tone_profile.pacing,
            'key_phrases': tone_profile.supportive_phrases[:3],
            'word_choices': tone_profile.word_choices[:5],
            'sentence_style': tone_profile.sentence_style,
            'emoji_intensity': self._get_emoji_intensity(tone_name)
        }
    
    def _get_emoji_intensity(self, tone_name: str) -> str:
        """
        Get emoji usage intensity for tone
        
        Args:
            tone_name: Name of tone
            
        Returns:
            Emoji intensity level
        """
        intensity_map = {
            'gentle': 'minimal',
            'encouraging': 'moderate',
            'energetic': 'high',
            'calm': 'minimal',
            'empathetic': 'minimal',
            'balanced': 'moderate',
            'serious': 'minimal'
        }
        return intensity_map.get(tone_name, 'moderate')


# Test function
if __name__ == "__main__":
    adapter = ToneAdapter()
    
    print("=" * 80)
    print("🎵 TONE ADAPTER")
    print("=" * 80)
    print()
    
    # Test scenarios
    scenarios = [
        {
            'name': 'Tired User',
            'emotion': 'tired',
            'energy_level': 20,
            'context': 'wants to workout',
            'injury_present': False
        },
        {
            'name': 'Excited User',
            'emotion': 'excited',
            'energy_level': 90,
            'context': 'ready to train',
            'injury_present': False
        },
        {
            'name': 'Injured User',
            'emotion': 'frustrated',
            'energy_level': 60,
            'context': 'shoulder pain',
            'injury_present': True
        },
        {
            'name': 'Unmotivated User',
            'emotion': 'demotivated',
            'energy_level': 50,
            'context': 'skipped workouts',
            'injury_present': False
        },
        {
            'name': 'Anxious User',
            'emotion': 'anxious',
            'energy_level': 40,
            'context': 'worried about progress',
            'injury_present': False
        }
    ]
    
    for scenario in scenarios:
        print(f"📋 {scenario['name']}")
        print(f"   Emotion: {scenario['emotion']}, Energy: {scenario['energy_level']}%")
        
        guidance = adapter.get_tone_guidance(
            emotion=scenario['emotion'],
            energy_level=scenario['energy_level'],
            context=scenario['context'],
            injury_present=scenario['injury_present']
        )
        
        print(f"   Selected Tone: {guidance['tone']}")
        print(f"   Description: {guidance['description']}")
        print(f"   Key Phrases: {', '.join(guidance['key_phrases'])}")
        print(f"   Emoji Intensity: {guidance['emoji_intensity']}")
        print()
    
    # Test tone blending
    print("🎨 Tone Blending Example:")
    blended = adapter.blend_tones('encouraging', 'calm', 0.6)
    print(f"   Primary: {blended['primary_tone']} (60%)")
    print(f"   Secondary: {blended['secondary_tone']} (40%)")
    print(f"   Guidance: {blended['guidance']}")
    print(f"   Key Phrases: {', '.join(blended['key_phrases'])}")
