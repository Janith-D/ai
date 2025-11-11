"""
NLP-Prolog Integration Module
Generates Prolog facts from NLP analysis for logical reasoning
"""

import sys
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime
import subprocess

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent))

from nlp.nlp_pipeline import NLPPipeline


class NLPPrologIntegration:
    """
    Integrates NLP analysis with Prolog reasoning system
    
    Converts NLP insights into Prolog facts:
    - emotion/4: emotion(UserID, Emotion, Date, Confidence)
    - intent/4: intent(UserID, Intent, Date, Time)
    - energy_level/3: energy_level(UserID, Level, Date)
    - physical_state/3: physical_state(UserID, State, Date)
    - target_muscle/3: target_muscle(UserID, Muscle, Date)
    - user_goal/2: user_goal(UserID, Goal)
    """
    
    def __init__(self, 
                 prolog_facts_dir: str = 'prolog/facts/nlp',
                 load_emotion_model: bool = True):
        """
        Initialize NLP-Prolog integration
        
        Args:
            prolog_facts_dir: Directory to save Prolog fact files
            load_emotion_model: Whether to load emotion detection model
        """
        print("🔗 Initializing NLP-Prolog Integration...")
        
        # Load NLP pipeline
        self.nlp = NLPPipeline(load_emotion_model=load_emotion_model)
        
        # Create facts directory
        self.facts_dir = Path(prolog_facts_dir)
        self.facts_dir.mkdir(parents=True, exist_ok=True)
        
        print(f"✓ Facts directory: {self.facts_dir}")
        print("✓ NLP-Prolog Integration ready\n")
    
    def message_to_prolog(self,
                         user_message: str,
                         user_id: str,
                         timestamp: Optional[datetime] = None) -> List[str]:
        """
        Convert user message to Prolog facts
        
        Args:
            user_message: User's message
            user_id: User identifier
            timestamp: Optional timestamp (defaults to now)
            
        Returns:
            List of Prolog fact strings
        """
        if timestamp is None:
            timestamp = datetime.now()
        
        # Analyze message
        result = self.nlp.analyze(user_message, user_id=user_id, timestamp=timestamp)
        
        facts = []
        date_str = timestamp.strftime('%Y-%m-%d')
        time_str = timestamp.strftime('%H:%M:%S')
        
        # Emotion facts
        if result.get('emotion'):
            emotion = result['emotion']
            facts.append(
                f"emotion('{user_id}', {emotion['fitness_emotion']}, '{date_str}', {emotion['confidence']})."
            )
            facts.append(
                f"energy_level('{user_id}', {emotion['energy_level']}, '{date_str}')."
            )
            
            # Additional emotion details
            facts.append(
                f"base_emotion('{user_id}', {emotion['emotion']}, '{date_str}')."
            )
        
        # Intent facts
        intent = result['intent']
        facts.append(
            f"intent('{user_id}', {intent['intent']}, '{date_str}', '{time_str}')."
        )
        facts.append(
            f"intent_confidence('{user_id}', {intent['intent']}, {intent['confidence']})."
        )
        
        # Context facts
        context = result.get('context', {})
        
        # Muscle groups
        for muscle in context.get('muscle_groups', []):
            facts.append(
                f"target_muscle('{user_id}', {muscle}, '{date_str}')."
            )
        
        # Goal
        if 'goal' in context:
            facts.append(
                f"user_goal('{user_id}', {context['goal']})."
            )
        
        # Intensity preference
        if 'intensity' in context:
            facts.append(
                f"preferred_intensity('{user_id}', {context['intensity']}, '{date_str}')."
            )
        
        # Physical state
        for state in context.get('physical_state', []):
            facts.append(
                f"physical_state('{user_id}', {state}, '{date_str}')."
            )
        
        # Time preference
        if 'time' in context:
            facts.append(
                f"time_preference('{user_id}', {context['time']})."
            )
        
        # Frequency
        if 'frequency' in context:
            freq = context['frequency']
            facts.append(
                f"workout_frequency('{user_id}', {freq['count']}, {freq['period']})."
            )
        
        # Duration
        if 'duration' in context:
            facts.append(
                f"preferred_duration('{user_id}', {context['duration']})."
            )
        
        return facts
    
    def save_facts(self,
                  user_message: str,
                  user_id: str,
                  filename: Optional[str] = None,
                  append: bool = False) -> str:
        """
        Save Prolog facts to file
        
        Args:
            user_message: User's message
            user_id: User identifier
            filename: Optional filename (defaults to user_id.pl)
            append: Whether to append to existing file
            
        Returns:
            Path to saved file
        """
        if filename is None:
            filename = f"{user_id}_nlp.pl"
        
        filepath = self.facts_dir / filename
        
        # Generate facts
        facts = self.message_to_prolog(user_message, user_id)
        
        # Add header comment
        header = f"% NLP Facts for user {user_id}\n"
        header += f"% Generated: {datetime.now().isoformat()}\n"
        header += f"% Message: \"{user_message}\"\n\n"
        
        # Write to file
        mode = 'a' if append else 'w'
        with open(filepath, mode, encoding='utf-8') as f:
            if not append:
                f.write(header)
            f.write('\n'.join(facts))
            f.write('\n\n')
        
        print(f"✓ Saved {len(facts)} facts to {filepath}")
        return str(filepath)
    
    def batch_process(self,
                     messages: List[tuple],
                     output_file: str = 'batch_nlp_facts.pl') -> str:
        """
        Process multiple messages and save to single file
        
        Args:
            messages: List of (user_id, message, timestamp) tuples
            output_file: Output filename
            
        Returns:
            Path to saved file
        """
        filepath = self.facts_dir / output_file
        
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("% Batch NLP Facts\n")
            f.write(f"% Generated: {datetime.now().isoformat()}\n")
            f.write(f"% Total messages: {len(messages)}\n\n")
            
            for user_id, message, timestamp in messages:
                f.write(f"% User: {user_id}\n")
                f.write(f"% Message: \"{message}\"\n")
                
                facts = self.message_to_prolog(message, user_id, timestamp)
                f.write('\n'.join(facts))
                f.write('\n\n')
        
        print(f"✓ Processed {len(messages)} messages to {filepath}")
        return str(filepath)
    
    def create_reasoning_rules(self, output_file: str = 'nlp_reasoning_rules.pl') -> str:
        """
        Create Prolog rules for reasoning with NLP facts
        
        Args:
            output_file: Output filename
            
        Returns:
            Path to saved file
        """
        filepath = self.facts_dir / output_file
        
        rules = """
% NLP-based Reasoning Rules
% Use these rules to make inferences from NLP facts

% Check if user needs rest based on emotion and physical state
needs_rest(UserID, Date) :-
    (emotion(UserID, demotivated, Date, _) ; 
     emotion(UserID, frustrated, Date, _)),
    physical_state(UserID, tired, Date).

needs_rest(UserID, Date) :-
    physical_state(UserID, injured, Date).

needs_rest(UserID, Date) :-
    energy_level(UserID, low, Date),
    \\+ intent(UserID, plan_workout, Date, _).

% Check if user is highly motivated
highly_motivated(UserID, Date) :-
    emotion(UserID, motivated, Date, Confidence),
    Confidence > 0.8,
    energy_level(UserID, high, Date).

% Check if user wants specific muscle workout
wants_muscle_workout(UserID, Muscle, Date) :-
    target_muscle(UserID, Muscle, Date),
    intent(UserID, plan_workout, Date, _).

% Check if user prefers high intensity
prefers_high_intensity(UserID, Date) :-
    preferred_intensity(UserID, high, Date),
    energy_level(UserID, high, Date).

% Safe to train (no injuries, reasonable energy)
safe_to_train(UserID, Date) :-
    \\+ physical_state(UserID, injured, Date),
    energy_level(UserID, Level, Date),
    Level \\= low.

% Recommend rest
recommend_rest(UserID, Date, Reason) :-
    needs_rest(UserID, Date),
    Reason = 'User needs rest based on emotion and physical state'.

recommend_rest(UserID, Date, Reason) :-
    intent(UserID, rest_request, Date, _),
    Reason = 'User explicitly requested rest'.

% Recommend workout with specific muscle
recommend_workout(UserID, Muscle, Date, Reason) :-
    wants_muscle_workout(UserID, Muscle, Date),
    safe_to_train(UserID, Date),
    format(atom(Reason), 'User wants ~w workout and is safe to train', [Muscle]).

% Recommend light workout for recovery
recommend_light_workout(UserID, Date, Reason) :-
    physical_state(UserID, sore, Date),
    intent(UserID, plan_workout, Date, _),
    \\+ physical_state(UserID, injured, Date),
    Reason = 'User is sore but wants to train - suggest light workout'.

% Get user motivation level
motivation_level(UserID, Date, high) :-
    highly_motivated(UserID, Date).

motivation_level(UserID, Date, low) :-
    (emotion(UserID, demotivated, Date, _) ;
     emotion(UserID, frustrated, Date, _)).

motivation_level(UserID, Date, medium) :-
    \\+ motivation_level(UserID, Date, high),
    \\+ motivation_level(UserID, Date, low).

% Emotional support needed
needs_emotional_support(UserID, Date) :-
    (emotion(UserID, demotivated, Date, _) ;
     emotion(UserID, frustrated, Date, _) ;
     emotion(UserID, anxious, Date, _)).

needs_emotional_support(UserID, Date) :-
    intent(UserID, motivation_request, Date, _).
"""
        
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(rules)
        
        print(f"✓ Created reasoning rules in {filepath}")
        return str(filepath)


# Test function
if __name__ == "__main__":
    print("=" * 80)
    print("🔗 NLP-PROLOG INTEGRATION TEST")
    print("=" * 80)
    print()
    
    # Initialize integration
    integration = NLPPrologIntegration(
        prolog_facts_dir='../../prolog/facts/nlp',
        load_emotion_model=True
    )
    
    # Test messages
    test_messages = [
        ("user123", "I'm exhausted but I still want to train my legs", datetime.now()),
        ("user123", "Feeling super pumped! Let's do chest!", datetime.now()),
        ("user456", "My shoulder hurts, I need to rest", datetime.now()),
        ("user456", "How many calories should I eat to lose weight?", datetime.now()),
    ]
    
    print("\n📝 CONVERTING MESSAGES TO PROLOG FACTS:\n")
    
    # Process individual messages
    for user_id, message, timestamp in test_messages:
        print(f"User: {user_id}")
        print(f"Message: \"{message}\"\n")
        
        facts = integration.message_to_prolog(message, user_id, timestamp)
        
        print("Generated Facts:")
        for fact in facts:
            print(f"  {fact}")
        print()
        
        # Save to individual file
        integration.save_facts(message, user_id, append=True)
    
    # Batch process all messages
    print("\n📦 BATCH PROCESSING:")
    batch_file = integration.batch_process(test_messages)
    print(f"✓ Batch file created: {batch_file}\n")
    
    # Create reasoning rules
    print("🧠 CREATING REASONING RULES:")
    rules_file = integration.create_reasoning_rules()
    print(f"✓ Rules file created: {rules_file}\n")
    
    print("=" * 80)
    print("✓ Integration Test Complete")
    print("=" * 80)
    print("\n📌 To use in Prolog, load the facts and rules:")
    print("   ?- consult('prolog/facts/nlp/batch_nlp_facts.pl').")
    print("   ?- consult('prolog/facts/nlp/nlp_reasoning_rules.pl').")
    print("   ?- needs_rest('user123', '2025-11-12').")
