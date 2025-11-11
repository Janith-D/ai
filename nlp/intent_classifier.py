"""
Intent Classification Module
Recognizes user intent from messages
"""

from typing import Dict, List, Tuple
import re


class IntentClassifier:
    """
    Classifies user intent for fitness coaching
    
    Intents:
    - plan_workout: User wants to work out
    - rest_request: User needs rest/recovery
    - diet_question: Questions about nutrition/diet
    - progress_check: Check progress/results
    - motivation_request: Needs motivation/encouragement
    - update_goal: Change fitness goals
    - injury_report: Report pain/injury
    - schedule_query: Questions about schedule/timing
    - general_chat: General conversation
    """
    
    # Intent patterns (keyword-based rules)
    INTENT_PATTERNS = {
        'plan_workout': [
            r'\b(train|workout|exercise|lift|gym|work out)\b(?!.*\?)',  # Exclude questions
            r'\b(chest|legs|back|arms|shoulders|abs)\s+(day|workout)\b',
            r'\b(let\'?s\s+go|ready\s+to|want\s+to\s+train)\b',
            r'\b(i\s+want\s+to\s+(train|workout|exercise))\b',
        ],
        'rest_request': [
            r'\b(rest|recovery|break|off\s+day|skip)\b',
            r'\b(tired|exhausted|sore|fatigue|worn\s+out)\b',
            r'\b(need\s+a\s+break|too\s+tired|can\'?t\s+train)\b',
            r'\b(need\s+(a\s+)?rest|take\s+(a\s+)?rest)\b',
        ],
        'diet_question': [
            r'\b(diet|nutrition|food|meal|eat|calorie|protein|carb)\b',
            r'\b(what\s+should\s+i\s+eat|meal\s+plan|food\s+plan)\b',
            r'\b(how\s+much.*eat|how\s+many.*calorie)\b',
        ],
        'progress_check': [
            r'\b(progress|result|achievement|gain|loss|improve)\b',
            r'\b(how\s+am\s+i\s+doing|check\s+my|see\s+my)\b',
            r'\b(stats|track|measure|weight|muscle)\b',
            r'\b(how\'?s\s+my\s+progress|am\s+i\s+doing)\b',
        ],
        'motivation_request': [
            r'\b(motivat|encourage|inspire|push|boost)\s*(me)?\b',
            r'\b(can\s+you\s+(motivat|encourage|inspire|push|help)\s+me)\b',
            r'\b(need\s+(some\s+)?(motivat|encourage|inspire))\b',
            r'\b(lazy|unmotivated|don\'?t\s+feel\s+like|don\'?t\s+want)\b',
            r'\b(give\s+up|quit|difficult|hard|struggle)\b',
        ],
        'update_goal': [
            r'\b(change\s+(my\s+)?goal|switch\s+(my\s+)?goal)\b',
            r'\b(switch\s+to|change\s+to)\b',
            r'\b(want\s+to\s+switch|going\s+to\s+switch)\b',
            r'\b(goal|target|aim|objective)\b',
            r'\b(bulk|bulking|cut|cutting|lose\s+weight|gain\s+muscle|get\s+lean)\b',
        ],
        'injury_report': [
            r'\b(pain|hurt|hurts|injury|injure|injured|strain|sprain)\b',
            r'\b(my\s+(shoulder|knee|back|elbow|wrist|ankle)\s+(hurt|pain))\b',
            r'\b(sore.*bad|really\s+sore|sharp\s+pain)\b',
            r'\b(can\'?t\s+move|uncomfortable|ache|aching)\b',
        ],
        'schedule_query': [
            r'\b(when\s+should|what\s+time\s+should)\b',
            r'\b(when\s+(do|should)\s+i\s+(train|workout))\b',
            r'\b(how\s+often|how\s+many.*week|frequency)\b',
            r'\b(schedule|timing)\b',
        ],
    }
    
    # Confidence boosters for strong indicators
    STRONG_INDICATORS = {
        'plan_workout': ['want to workout', 'want to train', 'ready to', 'gym', "let's go"],
        'rest_request': ['rest day', 'need rest', 'need a rest', 'too tired', 'skip'],
        'diet_question': ['calorie', 'protein', 'meal plan', 'what should i eat'],
        'progress_check': ['my progress', 'how am i doing', 'check my stats', "how's my"],
        'motivation_request': ['motivate me', 'can you motivate', 'need motivation', "don't feel like", 'encourage me'],
        'update_goal': ['change goal', 'change my goal', 'switch goal', 'switch my goal', 'switch to', 'want to switch', 'bulk up', 'lose weight', 'bulking', 'cutting'],
        'injury_report': ['my shoulder hurts', 'my knee hurts', 'injury', 'sharp pain', 'hurt', 'injured'],
        'schedule_query': ['when should', 'what time', 'how often', 'when do i'],
    }
    
    # Negative indicators (if present, reduce score for certain intents)
    NEGATIVE_INDICATORS = {
        'plan_workout': [
            r'\b(when\s+should|what\s+time|how\s+often)\b',  # These are schedule queries
            r'\b(hurt|pain|injury)\b',  # These are injury reports
        ],
    }
    
    def __init__(self, confidence_threshold: float = 0.3):
        """
        Initialize intent classifier
        
        Args:
            confidence_threshold: Minimum confidence for intent classification
        """
        self.confidence_threshold = confidence_threshold
        
        # Compile regex patterns for efficiency
        self.compiled_patterns = {
            intent: [re.compile(pattern, re.IGNORECASE) for pattern in patterns]
            for intent, patterns in self.INTENT_PATTERNS.items()
        }
        
        # Compile negative indicator patterns
        self.compiled_negative_patterns = {
            intent: [re.compile(pattern, re.IGNORECASE) for pattern in patterns]
            for intent, patterns in self.NEGATIVE_INDICATORS.items()
        }
    
    def classify(self, text: str) -> Dict[str, any]:
        """
        Classify user intent from text
        
        Args:
            text: Input text from user
            
        Returns:
            Dictionary with intent classification:
            {
                'intent': 'plan_workout',
                'confidence': 0.85,
                'all_intents': [...]
            }
        """
        if not text or not text.strip():
            return {
                'intent': 'general_chat',
                'confidence': 1.0,
                'all_intents': []
            }
        
        text_lower = text.lower()
        
        # Calculate scores for each intent
        intent_scores = {}
        
        for intent, patterns in self.compiled_patterns.items():
            score = 0.0
            matches = 0
            
            # Check pattern matches
            for pattern in patterns:
                if pattern.search(text):
                    matches += 1
                    score += 0.35  # Base score for pattern match (increased from 0.3)
            
            # Check strong indicators
            if intent in self.STRONG_INDICATORS:
                for indicator in self.STRONG_INDICATORS[intent]:
                    if indicator in text_lower:
                        score += 0.6  # Boost for strong indicator (increased from 0.5)
            
            # Apply negative indicators (reduce score if present)
            if intent in self.compiled_negative_patterns:
                for neg_pattern in self.compiled_negative_patterns[intent]:
                    if neg_pattern.search(text):
                        score *= 0.3  # Reduce score by 70%
            
            # Normalize score
            if matches > 0 or score > 0:
                intent_scores[intent] = min(score, 1.0)
        
        # Sort by score
        sorted_intents = sorted(
            intent_scores.items(),
            key=lambda x: x[1],
            reverse=True
        )
        
        # Get top intent
        if sorted_intents and sorted_intents[0][1] >= self.confidence_threshold:
            top_intent = sorted_intents[0][0]
            confidence = sorted_intents[0][1]
        else:
            top_intent = 'general_chat'
            confidence = 0.5
        
        return {
            'intent': top_intent,
            'confidence': round(confidence, 3),
            'all_intents': [
                {'intent': intent, 'score': round(score, 3)}
                for intent, score in sorted_intents[:3]
            ] if sorted_intents else []
        }
    
    def classify_batch(self, texts: List[str]) -> List[Dict[str, any]]:
        """
        Classify multiple texts
        
        Args:
            texts: List of input texts
            
        Returns:
            List of intent classifications
        """
        return [self.classify(text) for text in texts]
    
    def is_action_intent(self, text: str) -> bool:
        """
        Check if intent requires action (not just chat)
        
        Args:
            text: Input text
            
        Returns:
            True if action-requiring intent
        """
        result = self.classify(text)
        action_intents = [
            'plan_workout', 'rest_request', 'diet_question',
            'progress_check', 'update_goal', 'injury_report'
        ]
        return result['intent'] in action_intents
    
    def get_intent_category(self, text: str) -> str:
        """
        Get broad category of intent
        
        Args:
            text: Input text
            
        Returns:
            Category: 'training', 'nutrition', 'support', 'information'
        """
        result = self.classify(text)
        intent = result['intent']
        
        if intent in ['plan_workout', 'rest_request', 'injury_report']:
            return 'training'
        elif intent in ['diet_question']:
            return 'nutrition'
        elif intent in ['motivation_request', 'general_chat']:
            return 'support'
        else:  # progress_check, update_goal, schedule_query
            return 'information'


# Test function
if __name__ == "__main__":
    classifier = IntentClassifier()
    
    test_messages = [
        "I want to train my chest today",
        "I'm too tired, need a rest day",
        "How many calories should I eat?",
        "Can you motivate me? I don't feel like working out",
        "I want to switch my goal to fat loss",
        "My shoulder hurts when I lift",
        "When should I train tomorrow?",
        "How's my progress looking?",
        "Hey, how are you?"
    ]
    
    print("\n🎯 Intent Classification Test:\n")
    for msg in test_messages:
        result = classifier.classify(msg)
        print(f"Message: \"{msg}\"")
        print(f"  → Intent: {result['intent']} (confidence: {result['confidence']})")
        if result['all_intents']:
            print(f"  → Top alternatives: {result['all_intents'][:2]}")
        print()
