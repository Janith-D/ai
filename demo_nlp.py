"""
Interactive NLP Demo
Experience the Language & Emotion Understanding Unit in action!
"""

import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent))

from nlp.nlp_pipeline import NLPPipeline
from nlp.integration import NLPMLIntegration


def print_header(title):
    """Print formatted header"""
    print("\n" + "="*80)
    print(f"  {title}")
    print("="*80 + "\n")


def print_analysis(result):
    """Print formatted analysis"""
    # Emotion
    if result.get('emotion'):
        emotion = result['emotion']
        print(f"😊 EMOTION:")
        print(f"   Base: {emotion['emotion']} ({emotion['confidence']:.0%})")
        print(f"   Fitness: {emotion['fitness_emotion']}")
        print(f"   Energy: {emotion['energy_level']}\n")
    
    # Intent
    intent = result['intent']
    print(f"🎯 INTENT:")
    print(f"   {intent['intent']} ({intent['confidence']:.0%})\n")
    
    # Context
    if result['context']:
        print(f"🔍 CONTEXT:")
        for key, value in result['context'].items():
            print(f"   {key}: {value}")
        print()


def demo_basic_analysis():
    """Demo: Basic message analysis"""
    print_header("🧪 DEMO 1: Basic Message Analysis")
    
    pipeline = NLPPipeline()
    
    messages = [
        "I'm exhausted but I still want to train my legs",
        "Feeling super motivated today! Let's crush chest day!",
        "My shoulder hurts, I think I need to rest",
        "How many calories should I eat to lose weight?",
    ]
    
    for i, msg in enumerate(messages, 1):
        print(f"Message {i}: \"{msg}\"\n")
        result = pipeline.analyze(msg)
        print_analysis(result)
        print("-" * 80 + "\n")


def demo_ml_integration():
    """Demo: ML integration with NLP"""
    print_header("🔗 DEMO 2: NLP + ML Integration")
    
    integration = NLPMLIntegration(load_emotion_model=True)
    
    scenarios = [
        {
            'message': "I'm too tired for legs today",
            'goal': 'muscle_gain',
            'recent': ['chest', 'back', 'shoulders']
        },
        {
            'message': "Feeling great! Ready for an intense workout!",
            'goal': 'fat_loss',
            'recent': ['cardio', 'rest', 'legs']
        },
        {
            'message': "My knee hurts when I squat",
            'goal': 'strength',
            'recent': ['legs', 'back', 'chest']
        },
    ]
    
    for i, scenario in enumerate(scenarios, 1):
        print(f"Scenario {i}:")
        print(f"  Message: \"{scenario['message']}\"")
        print(f"  Goal: {scenario['goal']}")
        print(f"  Recent workouts: {scenario['recent']}\n")
        
        result = integration.chat_based_prediction(
            scenario['message'],
            goal=scenario['goal'],
            recent_workouts=scenario['recent']
        )
        
        # Display NLP analysis
        print("📊 NLP ANALYSIS:")
        nlp = result['nlp_analysis']
        if nlp.get('emotion'):
            print(f"  Emotion: {nlp['emotion']['fitness_emotion']} ({nlp['emotion']['confidence']:.0%})")
        print(f"  Intent: {nlp['intent']['intent']} ({nlp['intent']['confidence']:.0%})")
        if nlp['context']:
            print(f"  Context: {nlp['context']}\n")
        
        # Display prediction
        print("🎯 PREDICTION:")
        print(f"  Recommended: {result['adjusted_prediction'].upper()}")
        print(f"  Confidence: {result['adjusted_confidence']:.0%}")
        print(f"  Reason: {result['adjustment_reason']}\n")
        
        # Display recommendation
        print("💬 RECOMMENDATION:")
        print(f"  {result['recommendation']}\n")
        
        print("-" * 80 + "\n")


def demo_prolog_facts():
    """Demo: Prolog fact generation"""
    print_header("🔧 DEMO 3: Prolog Fact Generation")
    
    from nlp.integration import NLPPrologIntegration
    from datetime import datetime
    
    integration = NLPPrologIntegration(
        prolog_facts_dir='prolog/facts/nlp',
        load_emotion_model=True
    )
    
    messages = [
        ("user123", "I'm pumped for chest day!"),
        ("user123", "Too sore from yesterday, need rest"),
        ("user456", "I want to switch to fat loss"),
    ]
    
    for user_id, message in messages:
        print(f"User: {user_id}")
        print(f"Message: \"{message}\"\n")
        
        facts = integration.message_to_prolog(message, user_id, datetime.now())
        
        print("Generated Prolog Facts:")
        for fact in facts:
            print(f"  {fact}")
        print("\n" + "-" * 80 + "\n")


def demo_conversation_flow():
    """Demo: Multi-turn conversation"""
    print_header("💬 DEMO 4: Conversation Flow")
    
    pipeline = NLPPipeline()
    
    conversation = [
        ("User", "I want to start working out"),
        ("User", "My goal is to lose weight"),
        ("User", "But I'm feeling really tired today"),
        ("User", "Can you motivate me?"),
    ]
    
    print("Simulating a conversation with the AI Fitness Coach:\n")
    
    for i, (speaker, message) in enumerate(conversation, 1):
        print(f"Turn {i}:")
        print(f"  {speaker}: \"{message}\"\n")
        
        result = pipeline.analyze(message)
        
        # Show key insights
        emotion = result.get('emotion', {})
        intent = result['intent']
        
        print(f"  💡 AI Understanding:")
        if emotion:
            print(f"     • Emotion: {emotion['fitness_emotion']}")
        print(f"     • Intent: {intent['intent']}")
        if result['context']:
            print(f"     • Context: {result['context']}")
        
        # Generate appropriate response based on intent
        if intent['intent'] == 'plan_workout':
            response = "Great! Let's get you started with a fitness plan."
        elif intent['intent'] == 'update_goal':
            response = "Perfect! I'll adjust your program for fat loss."
        elif intent['intent'] == 'rest_request':
            response = "I understand. Recovery is important."
        elif intent['intent'] == 'motivation_request':
            if emotion.get('fitness_emotion') == 'demotivated':
                response = "💪 I hear you. Remember, even small progress is progress. Let's start with something light today!"
            else:
                response = "You've got this! Stay focused on your goals!"
        else:
            response = "I'm here to help you achieve your fitness goals!"
        
        print(f"\n  🤖 AI Coach: \"{response}\"\n")
        print("-" * 80 + "\n")


def interactive_mode():
    """Interactive mode: Chat with the NLP system"""
    print_header("🎮 INTERACTIVE MODE")
    
    print("Chat with the AI Fitness Coach NLP system!")
    print("Type 'quit' to exit.\n")
    
    pipeline = NLPPipeline()
    
    while True:
        try:
            user_input = input("You: ").strip()
            
            if user_input.lower() in ['quit', 'exit', 'q']:
                print("\n👋 Thanks for trying the NLP system!")
                break
            
            if not user_input:
                continue
            
            # Analyze
            result = pipeline.analyze(user_input)
            
            print()
            print_analysis(result)
            
            # Generate simple response
            intent = result['intent']['intent']
            emotion = result.get('emotion', {})
            
            if intent == 'plan_workout':
                context = result.get('context', {})
                muscles = context.get('muscle_groups', [])
                if muscles:
                    print(f"🤖 AI: Great! Let's plan a {muscles[0]} workout for you!\n")
                else:
                    print(f"🤖 AI: Perfect! Let's get you training!\n")
            elif intent == 'rest_request':
                print(f"🤖 AI: Rest is important. Take care of yourself!\n")
            elif intent == 'motivation_request':
                if emotion.get('fitness_emotion') == 'demotivated':
                    print(f"🤖 AI: 💪 You've got this! Small steps lead to big results!\n")
                else:
                    print(f"🤖 AI: Let's channel that energy into your training!\n")
            else:
                print(f"🤖 AI: I'm here to help with your fitness journey!\n")
            
        except KeyboardInterrupt:
            print("\n\n👋 Goodbye!")
            break


def main():
    """Main demo function"""
    print("""
    ╔═══════════════════════════════════════════════════════════════╗
    ║                                                               ║
    ║   🧠 LANGUAGE & EMOTION UNDERSTANDING UNIT                    ║
    ║      Interactive Demo                                         ║
    ║                                                               ║
    ╚═══════════════════════════════════════════════════════════════╝
    """)
    
    print("\nChoose a demo:\n")
    print("  1. Basic Message Analysis")
    print("  2. NLP + ML Integration")
    print("  3. Prolog Fact Generation")
    print("  4. Conversation Flow")
    print("  5. Interactive Mode (Chat)")
    print("  6. Run All Demos")
    print("  0. Exit\n")
    
    choice = input("Enter your choice (0-6): ").strip()
    
    if choice == '1':
        demo_basic_analysis()
    elif choice == '2':
        demo_ml_integration()
    elif choice == '3':
        demo_prolog_facts()
    elif choice == '4':
        demo_conversation_flow()
    elif choice == '5':
        interactive_mode()
    elif choice == '6':
        demo_basic_analysis()
        demo_ml_integration()
        demo_prolog_facts()
        demo_conversation_flow()
    elif choice == '0':
        print("\n👋 Goodbye!")
        return
    else:
        print("\n❌ Invalid choice. Please run the script again.")
        return
    
    print("\n" + "="*80)
    print("✓ Demo Complete!")
    print("="*80)
    print("\n📖 For more information, see: nlp/README.md")
    print("🧪 To run tests: python tests/test_nlp.py\n")


if __name__ == "__main__":
    main()
