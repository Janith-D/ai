"""
Context Extraction Module
Extracts fitness-specific context from user messages
"""

from typing import Dict, List, Optional, Set
import re
from datetime import datetime, timedelta


class ContextExtractor:
    """
    Extracts fitness context from text
    
    Extracts:
    - Muscle groups (chest, legs, back, arms, shoulders, abs)
    - Fitness goals (muscle_gain, fat_loss, endurance, strength, general_fitness)
    - Time references (today, tomorrow, morning, evening)
    - Intensity indicators (hard, easy, light, heavy)
    - Duration/frequency (daily, 3 times a week, etc.)
    """
    
    # Muscle groups
    MUSCLE_GROUPS = {
        'chest': ['chest', 'pecs', 'pectorals', 'bench'],
        'legs': ['legs', 'quads', 'hamstrings', 'calves', 'thighs', 'leg day'],
        'back': ['back', 'lats', 'traps', 'deadlift', 'rows'],
        'shoulders': ['shoulders', 'delts', 'deltoids', 'shoulder'],
        'arms': ['arms', 'biceps', 'triceps', 'forearms'],
        'abs': ['abs', 'core', 'abdominals', 'stomach'],
        'cardio': ['cardio', 'running', 'cycling', 'treadmill', 'hiit', 'aerobic'],
    }
    
    # Fitness goals
    GOAL_PATTERNS = {
        'muscle_gain': ['muscle gain', 'bulk', 'bulking', 'gain muscle', 'build muscle', 'hypertrophy'],
        'fat_loss': ['fat loss', 'cut', 'cutting', 'lose weight', 'weight loss', 'lean', 'shred'],
        'endurance': ['endurance', 'stamina', 'cardio', 'marathon', 'distance'],
        'strength': ['strength', 'strong', 'powerlifting', 'heavy lifting', 'max'],
        'general_fitness': ['fitness', 'healthy', 'stay fit', 'maintain', 'tone'],
    }
    
    # Time references
    TIME_PATTERNS = {
        'today': ['today', 'now', 'right now', 'this moment'],
        'tomorrow': ['tomorrow', 'next day'],
        'morning': ['morning', 'am', 'early'],
        'afternoon': ['afternoon', 'midday', 'noon'],
        'evening': ['evening', 'night', 'pm', 'late'],
        'this_week': ['this week', 'week'],
        'next_week': ['next week'],
    }
    
    # Intensity indicators
    INTENSITY_PATTERNS = {
        'high': ['hard', 'intense', 'heavy', 'max', 'push', 'crush', 'beast mode'],
        'medium': ['moderate', 'normal', 'regular', 'standard'],
        'low': ['light', 'easy', 'gentle', 'recovery', 'warm up'],
    }
    
    # Physical state indicators
    PHYSICAL_STATE = {
        'sore': ['sore', 'aching', 'stiff', 'tight'],
        'tired': ['tired', 'exhausted', 'fatigued', 'worn out', 'drained'],
        'energized': ['energized', 'pumped', 'ready', 'strong', 'fresh'],
        'injured': ['pain', 'hurt', 'injury', 'injured', 'strain'],
    }
    
    def __init__(self):
        """Initialize context extractor"""
        pass
    
    def extract_muscle_groups(self, text: str) -> List[str]:
        """
        Extract mentioned muscle groups
        
        Args:
            text: Input text
            
        Returns:
            List of muscle groups found
        """
        text_lower = text.lower()
        found_muscles = []
        
        for muscle, keywords in self.MUSCLE_GROUPS.items():
            for keyword in keywords:
                if keyword in text_lower:
                    if muscle not in found_muscles:
                        found_muscles.append(muscle)
                    break
        
        return found_muscles
    
    def extract_goal(self, text: str) -> Optional[str]:
        """
        Extract fitness goal
        
        Args:
            text: Input text
            
        Returns:
            Goal name or None
        """
        text_lower = text.lower()
        
        for goal, patterns in self.GOAL_PATTERNS.items():
            for pattern in patterns:
                if pattern in text_lower:
                    return goal
        
        return None
    
    def extract_time(self, text: str) -> Optional[str]:
        """
        Extract time reference
        
        Args:
            text: Input text
            
        Returns:
            Time reference or None
        """
        text_lower = text.lower()
        
        for time_ref, patterns in self.TIME_PATTERNS.items():
            for pattern in patterns:
                if pattern in text_lower:
                    return time_ref
        
        return None
    
    def extract_intensity(self, text: str) -> Optional[str]:
        """
        Extract intensity level
        
        Args:
            text: Input text
            
        Returns:
            Intensity level or None
        """
        text_lower = text.lower()
        
        for intensity, patterns in self.INTENSITY_PATTERNS.items():
            for pattern in patterns:
                if pattern in text_lower:
                    return intensity
        
        return None
    
    def extract_physical_state(self, text: str) -> List[str]:
        """
        Extract physical state indicators
        
        Args:
            text: Input text
            
        Returns:
            List of physical states
        """
        text_lower = text.lower()
        found_states = []
        
        for state, keywords in self.PHYSICAL_STATE.items():
            for keyword in keywords:
                if keyword in text_lower:
                    if state not in found_states:
                        found_states.append(state)
                    break
        
        return found_states
    
    def extract_frequency(self, text: str) -> Optional[Dict[str, any]]:
        """
        Extract workout frequency
        
        Args:
            text: Input text
            
        Returns:
            Frequency dict or None
        """
        text_lower = text.lower()
        
        # Pattern: X times per week/day
        times_pattern = r'(\d+)\s*(?:times?|x)\s*(?:per|a|every)?\s*(week|day|month)'
        match = re.search(times_pattern, text_lower)
        
        if match:
            count = int(match.group(1))
            period = match.group(2)
            return {
                'count': count,
                'period': period,
                'description': f"{count} times per {period}"
            }
        
        # Pattern: daily, weekly
        if 'daily' in text_lower or 'every day' in text_lower:
            return {'count': 7, 'period': 'week', 'description': 'daily'}
        
        if 'weekly' in text_lower or 'every week' in text_lower:
            return {'count': 1, 'period': 'week', 'description': 'weekly'}
        
        return None
    
    def extract_duration(self, text: str) -> Optional[int]:
        """
        Extract duration in minutes
        
        Args:
            text: Input text
            
        Returns:
            Duration in minutes or None
        """
        text_lower = text.lower()
        
        # Pattern: X minutes/hours
        duration_pattern = r'(\d+)\s*(minute|min|hour|hr)s?'
        match = re.search(duration_pattern, text_lower)
        
        if match:
            value = int(match.group(1))
            unit = match.group(2)
            
            if unit in ['hour', 'hr']:
                return value * 60
            else:
                return value
        
        return None
    
    def extract_all(self, text: str) -> Dict[str, any]:
        """
        Extract all context information
        
        Args:
            text: Input text
            
        Returns:
            Dictionary with all extracted context
        """
        context = {
            'muscle_groups': self.extract_muscle_groups(text),
            'goal': self.extract_goal(text),
            'time': self.extract_time(text),
            'intensity': self.extract_intensity(text),
            'physical_state': self.extract_physical_state(text),
            'frequency': self.extract_frequency(text),
            'duration': self.extract_duration(text),
        }
        
        # Remove None values
        context = {k: v for k, v in context.items() if v is not None and v != []}
        
        return context
    
    def to_prolog_facts(self, user_id: str, text: str, timestamp: Optional[datetime] = None) -> List[str]:
        """
        Convert extracted context to Prolog facts
        
        Args:
            user_id: User identifier
            text: Input text
            timestamp: Optional timestamp (defaults to now)
            
        Returns:
            List of Prolog fact strings
        """
        if timestamp is None:
            timestamp = datetime.now()
        
        context = self.extract_all(text)
        facts = []
        
        date_str = timestamp.strftime('%Y-%m-%d')
        
        # Muscle groups
        for muscle in context.get('muscle_groups', []):
            facts.append(f"target_muscle('{user_id}', '{muscle}', '{date_str}').")
        
        # Goal
        if 'goal' in context:
            facts.append(f"user_goal('{user_id}', {context['goal']}).")
        
        # Intensity
        if 'intensity' in context:
            facts.append(f"preferred_intensity('{user_id}', {context['intensity']}).")
        
        # Physical state
        for state in context.get('physical_state', []):
            facts.append(f"physical_state('{user_id}', {state}, '{date_str}').")
        
        # Time preference
        if 'time' in context:
            facts.append(f"time_preference('{user_id}', {context['time']}).")
        
        return facts


# Test function
if __name__ == "__main__":
    extractor = ContextExtractor()
    
    test_messages = [
        "I want to build muscle in my chest, training 4 times a week",
        "I'm too sore from leg day yesterday, need light cardio",
        "Let's do intense shoulders tomorrow morning",
        "I want to lose weight, can you help me with a plan?",
        "My back hurts, need to take it easy",
        "Ready for a hard 60 minute workout today!"
    ]
    
    print("\n🔍 Context Extraction Test:\n")
    for msg in test_messages:
        context = extractor.extract_all(msg)
        print(f"Message: \"{msg}\"")
        print(f"  → Context: {context}")
        
        # Show Prolog facts
        facts = extractor.to_prolog_facts('user123', msg)
        if facts:
            print(f"  → Prolog Facts:")
            for fact in facts:
                print(f"      {fact}")
        print()
