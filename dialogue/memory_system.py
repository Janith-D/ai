"""
Conversation Memory System
Tracks conversation history and user patterns for continuity
"""

import json
from typing import Dict, List, Optional
from datetime import datetime, timedelta
from dataclasses import dataclass, asdict
from collections import defaultdict


@dataclass
class ConversationTurn:
    """Single conversation turn"""
    timestamp: str
    user_message: str
    bot_response: str
    emotion: str
    intent: str
    workout_recommended: Optional[str] = None
    energy_level: Optional[float] = None
    has_injury: bool = False


@dataclass
class UserPattern:
    """Detected user pattern"""
    pattern_type: str
    description: str
    frequency: int
    first_seen: str
    last_seen: str
    severity: str  # low, medium, high


class ConversationMemory:
    """
    Manages conversation history and user patterns
    
    Tracks:
    - Recent conversation turns
    - Recurring patterns (injuries, motivation dips, goal changes)
    - User preferences and tendencies
    - Long-term progress markers
    """
    
    def __init__(self, max_history: int = 50):
        """
        Initialize conversation memory
        
        Args:
            max_history: Maximum number of turns to keep
        """
        self.max_history = max_history
        self.history: List[ConversationTurn] = []
        self.patterns: List[UserPattern] = []
        self.user_profile = {
            'first_interaction': None,
            'total_interactions': 0,
            'workout_types_completed': defaultdict(int),
            'common_emotions': defaultdict(int),
            'common_intents': defaultdict(int),
            'average_energy': None,
            'injury_count': 0,
            'goals': []
        }
    
    def add_turn(
        self,
        user_message: str,
        bot_response: str,
        emotion: str,
        intent: str,
        workout_recommended: Optional[str] = None,
        energy_level: Optional[float] = None,
        has_injury: bool = False
    ):
        """
        Add conversation turn to memory
        
        Args:
            user_message: User's message
            bot_response: Bot's response
            emotion: Detected emotion
            intent: Detected intent
            workout_recommended: Recommended workout type
            energy_level: User's energy level (0-100)
            has_injury: Whether user has injury
        """
        turn = ConversationTurn(
            timestamp=datetime.now().isoformat(),
            user_message=user_message,
            bot_response=bot_response,
            emotion=emotion,
            intent=intent,
            workout_recommended=workout_recommended,
            energy_level=energy_level,
            has_injury=has_injury
        )
        
        self.history.append(turn)
        
        # Trim history if needed
        if len(self.history) > self.max_history:
            self.history = self.history[-self.max_history:]
        
        # Update user profile
        self._update_profile(turn)
        
        # Detect patterns
        self._detect_patterns()
    
    def _update_profile(self, turn: ConversationTurn):
        """
        Update user profile with new turn
        
        Args:
            turn: Conversation turn
        """
        if self.user_profile['first_interaction'] is None:
            self.user_profile['first_interaction'] = turn.timestamp
        
        self.user_profile['total_interactions'] += 1
        
        if turn.workout_recommended:
            self.user_profile['workout_types_completed'][turn.workout_recommended] += 1
        
        self.user_profile['common_emotions'][turn.emotion] += 1
        self.user_profile['common_intents'][turn.intent] += 1
        
        if turn.energy_level is not None:
            # Update running average
            current_avg = self.user_profile['average_energy'] or 0
            total = self.user_profile['total_interactions']
            self.user_profile['average_energy'] = (
                (current_avg * (total - 1) + turn.energy_level) / total
            )
        
        if turn.has_injury:
            self.user_profile['injury_count'] += 1
    
    def _detect_patterns(self):
        """Detect recurring patterns in conversation history"""
        if len(self.history) < 3:
            return  # Need at least 3 turns to detect patterns
        
        # Get recent turns (last 10)
        recent = self.history[-10:]
        
        # Pattern: Recurring injuries
        injury_turns = [t for t in recent if t.has_injury]
        if len(injury_turns) >= 2:
            self._add_pattern(
                pattern_type='recurring_injury',
                description='Multiple injury reports detected',
                frequency=len(injury_turns),
                severity='high' if len(injury_turns) >= 3 else 'medium'
            )
        
        # Pattern: Low energy trend
        low_energy_turns = [t for t in recent if t.energy_level and t.energy_level < 40]
        if len(low_energy_turns) >= 3:
            self._add_pattern(
                pattern_type='low_energy_trend',
                description='Consistent low energy levels',
                frequency=len(low_energy_turns),
                severity='medium'
            )
        
        # Pattern: Motivation struggles
        demotivated_turns = [t for t in recent if t.emotion in ['demotivated', 'tired', 'frustrated']]
        if len(demotivated_turns) >= 4:
            self._add_pattern(
                pattern_type='motivation_struggle',
                description='Multiple signs of low motivation',
                frequency=len(demotivated_turns),
                severity='medium'
            )
        
        # Pattern: Consistent engagement
        if len(recent) >= 7 and all(
            (datetime.fromisoformat(recent[i].timestamp) - 
             datetime.fromisoformat(recent[i-1].timestamp)) < timedelta(days=3)
            for i in range(1, len(recent))
        ):
            self._add_pattern(
                pattern_type='consistent_engagement',
                description='Regular, frequent interactions',
                frequency=len(recent),
                severity='low'  # Positive pattern
            )
    
    def _add_pattern(
        self,
        pattern_type: str,
        description: str,
        frequency: int,
        severity: str
    ):
        """
        Add or update pattern
        
        Args:
            pattern_type: Type of pattern
            description: Pattern description
            frequency: Frequency count
            severity: Severity level
        """
        # Check if pattern already exists
        existing = next((p for p in self.patterns if p.pattern_type == pattern_type), None)
        
        if existing:
            existing.frequency = frequency
            existing.last_seen = datetime.now().isoformat()
            existing.severity = severity
        else:
            pattern = UserPattern(
                pattern_type=pattern_type,
                description=description,
                frequency=frequency,
                first_seen=datetime.now().isoformat(),
                last_seen=datetime.now().isoformat(),
                severity=severity
            )
            self.patterns.append(pattern)
    
    def get_recent_context(self, num_turns: int = 3) -> List[Dict]:
        """
        Get recent conversation context
        
        Args:
            num_turns: Number of recent turns to retrieve
            
        Returns:
            List of recent turns as dictionaries
        """
        recent = self.history[-num_turns:]
        return [asdict(turn) for turn in recent]
    
    def get_patterns_summary(self) -> Dict:
        """
        Get summary of detected patterns
        
        Returns:
            Dictionary with pattern summary
        """
        if not self.patterns:
            return {'has_patterns': False, 'patterns': []}
        
        return {
            'has_patterns': True,
            'patterns': [asdict(p) for p in self.patterns],
            'high_severity_count': sum(1 for p in self.patterns if p.severity == 'high'),
            'medium_severity_count': sum(1 for p in self.patterns if p.severity == 'medium')
        }
    
    def get_continuity_context(self) -> Optional[str]:
        """
        Get context string for response continuity
        
        Returns:
            Contextual string or None
        """
        if len(self.history) < 2:
            return None
        
        contexts = []
        
        # Check last workout
        last_workout = next(
            (t for t in reversed(self.history) if t.workout_recommended),
            None
        )
        if last_workout:
            days_ago = (datetime.now() - datetime.fromisoformat(last_workout.timestamp)).days
            if days_ago <= 7:
                contexts.append(f"Last workout was {last_workout.workout_recommended} {days_ago} day(s) ago")
        
        # Check patterns
        high_priority_patterns = [p for p in self.patterns if p.severity in ['high', 'medium']]
        if high_priority_patterns:
            pattern = high_priority_patterns[0]
            contexts.append(f"Pattern detected: {pattern.description}")
        
        # Check emotion trend
        recent_emotions = [t.emotion for t in self.history[-5:]]
        if recent_emotions.count('demotivated') >= 3:
            contexts.append("You've been struggling with motivation lately")
        elif recent_emotions.count('excited') >= 3:
            contexts.append("You've been really motivated lately")
        
        return " | ".join(contexts) if contexts else None
    
    def get_user_summary(self) -> Dict:
        """
        Get comprehensive user summary
        
        Returns:
            Dictionary with user profile summary
        """
        return {
            'total_interactions': self.user_profile['total_interactions'],
            'first_interaction': self.user_profile['first_interaction'],
            'most_common_emotion': max(
                self.user_profile['common_emotions'],
                key=self.user_profile['common_emotions'].get,
                default='neutral'
            ),
            'most_common_intent': max(
                self.user_profile['common_intents'],
                key=self.user_profile['common_intents'].get,
                default='unknown'
            ),
            'average_energy': round(self.user_profile['average_energy'], 1) if self.user_profile['average_energy'] else None,
            'injury_count': self.user_profile['injury_count'],
            'workout_distribution': dict(self.user_profile['workout_types_completed']),
            'detected_patterns': len(self.patterns)
        }
    
    def save_to_file(self, filepath: str):
        """
        Save memory to JSON file
        
        Args:
            filepath: Path to save file
        """
        data = {
            'history': [asdict(turn) for turn in self.history],
            'patterns': [asdict(pattern) for pattern in self.patterns],
            'user_profile': {
                **self.user_profile,
                'workout_types_completed': dict(self.user_profile['workout_types_completed']),
                'common_emotions': dict(self.user_profile['common_emotions']),
                'common_intents': dict(self.user_profile['common_intents'])
            }
        }
        
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
    
    def load_from_file(self, filepath: str):
        """
        Load memory from JSON file
        
        Args:
            filepath: Path to load file
        """
        with open(filepath, 'r') as f:
            data = json.load(f)
        
        self.history = [ConversationTurn(**turn) for turn in data['history']]
        self.patterns = [UserPattern(**pattern) for pattern in data['patterns']]
        
        profile = data['user_profile']
        self.user_profile = {
            **profile,
            'workout_types_completed': defaultdict(int, profile['workout_types_completed']),
            'common_emotions': defaultdict(int, profile['common_emotions']),
            'common_intents': defaultdict(int, profile['common_intents'])
        }


# Test function
if __name__ == "__main__":
    print("=" * 80)
    print("🧠 CONVERSATION MEMORY")
    print("=" * 80)
    print()
    
    memory = ConversationMemory()
    
    # Simulate conversation history
    test_turns = [
        ("I want to train chest", "Let's do a chest workout!", "motivated", "plan_workout", "CHEST", 80, False),
        ("I'm feeling tired today", "Let's take it easy", "tired", "plan_workout", "CARDIO", 30, False),
        ("My shoulder hurts", "Rest day for safety", "frustrated", "report_injury", "REST", 50, True),
        ("I don't feel like training", "Let's start small", "demotivated", "ask_motivation", None, 40, False),
        ("Still feeling low energy", "Light activity today", "tired", "plan_workout", "CARDIO", 35, False),
        ("Let's do legs!", "Great energy today!", "excited", "plan_workout", "LEGS", 85, False),
        ("Shoulder still hurts", "Continued rest needed", "frustrated", "report_injury", "REST", 50, True),
    ]
    
    for user_msg, bot_msg, emotion, intent, workout, energy, injury in test_turns:
        memory.add_turn(user_msg, bot_msg, emotion, intent, workout, energy, injury)
    
    print("📊 User Summary:")
    summary = memory.get_user_summary()
    for key, value in summary.items():
        print(f"   {key}: {value}")
    print()
    
    print("🔍 Detected Patterns:")
    patterns_summary = memory.get_patterns_summary()
    if patterns_summary['has_patterns']:
        for pattern in patterns_summary['patterns']:
            print(f"   • {pattern['pattern_type']}: {pattern['description']}")
            print(f"     Frequency: {pattern['frequency']}, Severity: {pattern['severity']}")
    else:
        print("   No patterns detected")
    print()
    
    print("💬 Continuity Context:")
    context = memory.get_continuity_context()
    print(f"   {context if context else 'No context available'}")
    print()
    
    print("📝 Recent Conversation (last 3 turns):")
    recent = memory.get_recent_context(3)
    for i, turn in enumerate(recent, 1):
        print(f"   {i}. User: {turn['user_message']}")
        print(f"      Bot: {turn['bot_response']}")
        print(f"      Emotion: {turn['emotion']}, Energy: {turn['energy_level']}%")
