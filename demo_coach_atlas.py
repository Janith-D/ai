"""
🎭 Coach Atlas Demo
Interactive demonstration of the complete Personality & Dialogue Unit
"""

import sys
from pathlib import Path

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from dialogue import CoachPipeline


def print_header(title: str):
    """Print formatted header"""
    print("\n" + "=" * 80)
    print(f"  {title}")
    print("=" * 80 + "\n")


def print_conversation(user_msg: str, coach_response: str, metadata: dict = None):
    """Print formatted conversation"""
    print(f"👤 User: {user_msg}")
    print()
    print(f"🤖 Coach Atlas:")
    print(f"   {coach_response}")
    
    if metadata:
        print()
        print(f"   📊 Analysis:")
        if 'emotion' in metadata:
            print(f"      Emotion: {metadata['emotion']}")
        if 'intent' in metadata:
            print(f"      Intent: {metadata['intent']}")
        if 'tone' in metadata:
            print(f"      Tone: {metadata['tone']}")
        if 'energy' in metadata:
            print(f"      Energy: {metadata['energy']}%")
    print()


def demo_personality():
    """Demo 1: Coach Atlas Personality"""
    print_header("🎭 DEMO 1: Coach Atlas Personality")
    
    from dialogue import CoachPersonality
    
    personality = CoachPersonality()
    
    print("Coach Identity:")
    print(f"  Name: {personality.traits.name}")
    print(f"  Archetype: {personality.traits.archetype}")
    print(f"  Description: {personality.traits.description}")
    print()
    
    print("Core Values:")
    for value in personality.traits.values:
        print(f"  • {value.capitalize()}")
    print()
    
    print("Sample Greetings:")
    times = ['morning', 'afternoon', 'evening']
    for time in times:
        greeting = personality.get_greeting("Alex", time)
        print(f"  {time.capitalize()}: {greeting}")
    print()


def demo_tone_adaptation():
    """Demo 2: Tone Adaptation"""
    print_header("🎵 DEMO 2: Emotion-Aware Tone Adaptation")
    
    from dialogue import ToneAdapter
    
    adapter = ToneAdapter()
    
    scenarios = [
        ('tired', 20, 'Feeling exhausted after work'),
        ('excited', 90, 'Ready for an intense workout'),
        ('frustrated', 50, 'Dealing with shoulder injury'),
        ('anxious', 40, 'Worried about progress'),
        ('motivated', 75, 'Determined to reach goals')
    ]
    
    for emotion, energy, context in scenarios:
        guidance = adapter.get_tone_guidance(emotion, energy_level=energy)
        print(f"📋 {context}")
        print(f"   Emotion: {emotion} | Energy: {energy}%")
        print(f"   → Selected Tone: {guidance['tone']}")
        print(f"   → Description: {guidance['description']}")
        print(f"   → Key Phrases: {', '.join(guidance['key_phrases'][:2])}")
        print()


def demo_response_templates():
    """Demo 3: Response Templates"""
    print_header("📝 DEMO 3: Response Templates")
    
    from dialogue import ResponseTemplates
    
    templates = ResponseTemplates()
    
    scenarios = [
        ('workout_plan', 'Planning a workout'),
        ('injury_safety', 'Handling an injury'),
        ('motivation_boost', 'Needing motivation'),
        ('progress_check', 'Checking progress')
    ]
    
    for template_name, description in scenarios:
        template = templates.get_template(template_name)
        print(f"📄 {description}")
        print(f"   Template: {template.name}")
        print(f"   Category: {template.category}")
        print(f"   Structure: {' → '.join(template.structure[:3])}...")
        print()


def demo_conversation_memory():
    """Demo 4: Conversation Memory"""
    print_header("🧠 DEMO 4: Conversation Memory & Pattern Detection")
    
    from dialogue import ConversationMemory
    
    memory = ConversationMemory()
    
    # Simulate conversation history
    conversations = [
        ("I want to train chest", "Let's do chest!", "motivated", "plan_workout", "CHEST", 80, False),
        ("Feeling tired today", "Let's take it easy", "tired", "plan_workout", "CARDIO", 30, False),
        ("My shoulder hurts", "Rest day for safety", "frustrated", "report_injury", "REST", 50, True),
        ("Still low on energy", "Light activity", "tired", "plan_workout", "CARDIO", 35, False),
        ("Shoulder still bothering me", "Continued rest", "frustrated", "report_injury", "REST", 50, True),
    ]
    
    for user_msg, bot_msg, emotion, intent, workout, energy, injury in conversations:
        memory.add_turn(user_msg, bot_msg, emotion, intent, workout, energy, injury)
    
    print("📊 User Summary:")
    summary = memory.get_user_summary()
    print(f"   Total Interactions: {summary['total_interactions']}")
    print(f"   Average Energy: {summary['average_energy']}%")
    print(f"   Most Common Emotion: {summary['most_common_emotion']}")
    print(f"   Injury Count: {summary['injury_count']}")
    print()
    
    print("🔍 Detected Patterns:")
    patterns = memory.get_patterns_summary()
    if patterns['has_patterns']:
        for pattern in patterns['patterns']:
            severity_emoji = {'low': '✅', 'medium': '⚠️', 'high': '🚨'}[pattern['severity']]
            print(f"   {severity_emoji} {pattern['pattern_type']}: {pattern['description']}")
    print()
    
    print("💬 Continuity Context:")
    context = memory.get_continuity_context()
    print(f"   {context if context else 'No context'}")
    print()


def demo_complete_pipeline():
    """Demo 5: Complete Pipeline"""
    print_header("🚀 DEMO 5: Complete Coach Atlas Pipeline")
    
    coach = CoachPipeline(enable_memory=True)
    
    test_messages = [
        "I want to train chest today! Feeling strong!",
        "I'm exhausted but I still want to workout",
        "My shoulder is really hurting",
    ]
    
    for i, message in enumerate(test_messages, 1):
        result = coach.process_message(message)
        
        nlp = result.get('nlp_analysis', {})
        metadata = {
            'emotion': nlp.get('emotion'),
            'intent': nlp.get('intent'),
            'energy': nlp.get('energy_level')
        }
        
        print_conversation(message, result['response'], metadata)
        print("-" * 80)
    
    # Show stats
    print()
    print("📊 Coach Pipeline Statistics:")
    stats = coach.get_stats()
    print(f"   Total Requests: {stats['total_requests']}")
    print(f"   Responses Generated: {stats['responses_generated']}")
    print(f"   Conversation Turns: {stats['conversation_turns']}")
    print()


def interactive_chat():
    """Demo 6: Interactive Chat"""
    print_header("💬 DEMO 6: Interactive Chat with Coach Atlas")
    
    print("Chat with Coach Atlas! Type 'quit' to exit.\n")
    
    coach = CoachPipeline(enable_memory=True)
    
    sample_messages = [
        "I want to train chest today",
        "I'm feeling really tired",
        "My shoulder hurts",
        "I need some motivation",
        "Let's do a killer leg workout"
    ]
    
    print("💡 Try these sample messages:")
    for i, msg in enumerate(sample_messages, 1):
        print(f"   {i}. {msg}")
    print()
    
    while True:
        try:
            user_input = input("👤 You: ").strip()
            
            if not user_input:
                continue
            
            if user_input.lower() in ['quit', 'exit', 'q']:
                print("\n🤖 Coach Atlas: Great session! Keep up the amazing work! 💪")
                break
            
            # Check if it's a sample number
            if user_input.isdigit():
                idx = int(user_input) - 1
                if 0 <= idx < len(sample_messages):
                    user_input = sample_messages[idx]
                    print(f"👤 You: {user_input}")
            
            result = coach.process_message(user_input)
            print(f"\n🤖 Coach Atlas:")
            print(f"   {result['response']}\n")
            
        except KeyboardInterrupt:
            print("\n\n🤖 Coach Atlas: See you next time! 💪")
            break
        except Exception as e:
            print(f"\n⚠️ Error: {e}\n")


def run_all_demos():
    """Run all demos"""
    print("\n" + "=" * 80)
    print("  🎭 COACH ATLAS - PERSONALITY & DIALOGUE UNIT DEMO")
    print("=" * 80)
    print()
    print("  Demonstrating the complete 'expressive brain' of the AI Fitness Coach")
    print()
    input("  Press Enter to start the demos...")
    
    demos = [
        demo_personality,
        demo_tone_adaptation,
        demo_response_templates,
        demo_conversation_memory,
        demo_complete_pipeline
    ]
    
    for demo in demos:
        try:
            demo()
            input("  Press Enter to continue...")
        except KeyboardInterrupt:
            print("\n\nDemo interrupted. Exiting...")
            return
    
    # Ask if user wants interactive chat
    print()
    choice = input("Would you like to try interactive chat with Coach Atlas? (y/n): ").lower()
    if choice in ['y', 'yes']:
        interactive_chat()
    
    # Final summary
    print_header("🎉 Demo Complete!")
    print("All demos completed successfully!")
    print()
    print("✨ Coach Atlas Features Demonstrated:")
    print("   ✅ Consistent personality and core values")
    print("   ✅ Emotion-aware tone adaptation (7 tones)")
    print("   ✅ Structured response templates (8 types)")
    print("   ✅ Conversation memory and pattern detection")
    print("   ✅ Complete unified pipeline (NLP → ML → Prolog → Dialogue)")
    print()
    print("📚 For more information, see:")
    print("   • dialogue/README.md - Complete documentation")
    print("   • docs/DIALOGUE_IMPLEMENTATION_REPORT.md - Implementation report")
    print("   • tests/test_dialogue.py - Test suite (100% passing)")
    print()
    print("💪 Coach Atlas is ready to help users become their strongest selves!")
    print()


if __name__ == "__main__":
    try:
        run_all_demos()
    except KeyboardInterrupt:
        print("\n\nDemo interrupted. Goodbye! 👋")
