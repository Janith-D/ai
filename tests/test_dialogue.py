"""
Comprehensive Integration Tests for Personality & Dialogue Unit
Tests Coach Atlas personality system with all components
"""

import sys
from pathlib import Path

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from dialogue.personality import CoachPersonality, PersonalityTraits
from dialogue.tone_adapter import ToneAdapter
from dialogue.response_templates import ResponseTemplates
from dialogue.memory_system import ConversationMemory
from dialogue.dialogue_generator import DialogueGenerator, DialogueContext
from dialogue.coach_pipeline import CoachPipeline


def test_personality_system():
    """Test personality traits and guidelines"""
    print("🧪 Testing Personality System...")
    
    personality = CoachPersonality()
    
    # Test basic attributes
    assert personality.traits.name == "Coach Atlas"
    assert personality.traits.archetype == "wise_mentor"
    assert len(personality.traits.values) == 5
    
    # Test system prompt
    prompt = personality.get_system_prompt()
    assert "Coach Atlas" in prompt
    assert "CORE VALUES" in prompt
    assert "TONE ADAPTATION" in prompt
    
    # Test tone recommendations
    tone = personality.get_tone_for_emotion('tired')
    assert tone == 'supportive'
    
    tone = personality.get_tone_for_emotion('excited')
    assert tone == 'enthusiastic'
    
    # Test greetings
    greeting = personality.get_greeting("Alex", "morning")
    assert "Good morning" in greeting
    assert "Alex" in greeting
    
    print("   ✓ Personality system working correctly")


def test_tone_adapter():
    """Test tone selection and adaptation"""
    print("🧪 Testing Tone Adapter...")
    
    adapter = ToneAdapter()
    
    # Test tone selection with different emotions
    tone = adapter.select_tone('tired', energy_level=20)
    assert tone == 'gentle'
    
    tone = adapter.select_tone('excited', energy_level=90)
    assert tone == 'energetic'  # 'excited' emotion maps to 'energetic' tone
    
    # Test injury override
    tone = adapter.select_tone('excited', energy_level=90, injury_present=True)
    assert tone == 'serious'
    
    # Test tone guidance
    guidance = adapter.get_tone_guidance('tired', energy_level=30)
    assert guidance['tone'] == 'gentle'
    assert 'key_phrases' in guidance
    assert len(guidance['key_phrases']) >= 3
    
    # Test tone blending
    blended = adapter.blend_tones('encouraging', 'calm', 0.7)
    assert blended['primary_tone'] == 'encouraging'
    assert blended['secondary_tone'] == 'calm'
    assert blended['weight'] == 0.7
    
    print("   ✓ Tone adapter working correctly")


def test_response_templates():
    """Test template system"""
    print("🧪 Testing Response Templates...")
    
    templates = ResponseTemplates()
    
    # Test template retrieval
    template = templates.get_template('workout_plan')
    assert template.name == 'workout_plan'
    assert template.category == 'planning'
    assert len(template.structure) > 0
    
    # Test template selection
    selected = templates.select_template('plan_workout', has_injury=False)
    assert selected == 'workout_plan'
    
    selected = templates.select_template('plan_workout', has_injury=True)
    assert selected == 'injury_safety'
    
    # Test template filling
    fields = templates.get_example_fields('workout_plan')
    filled = templates.fill_template('workout_plan', fields)
    assert len(filled) > 0  # Check that template was filled
    assert fields['recommendation'] in filled  # Check that field values are in output
    assert fields['reasoning'] in filled
    
    print("   ✓ Response templates working correctly")


def test_conversation_memory():
    """Test memory system"""
    print("🧪 Testing Conversation Memory...")
    
    memory = ConversationMemory()
    
    # Add conversation turns
    memory.add_turn(
        user_message="I want to train chest",
        bot_response="Let's do chest workout!",
        emotion="motivated",
        intent="plan_workout",
        workout_recommended="CHEST",
        energy_level=80,
        has_injury=False
    )
    
    memory.add_turn(
        user_message="I'm tired",
        bot_response="Let's rest",
        emotion="tired",
        intent="plan_workout",
        workout_recommended="REST",
        energy_level=20,
        has_injury=False
    )
    
    # Test history
    assert len(memory.history) == 2
    
    # Test user summary
    summary = memory.get_user_summary()
    assert summary['total_interactions'] == 2
    assert summary['most_common_emotion'] in ['motivated', 'tired']
    assert summary['average_energy'] == 50.0
    
    # Test recent context
    recent = memory.get_recent_context(2)
    assert len(recent) == 2
    
    print("   ✓ Conversation memory working correctly")


def test_dialogue_generator():
    """Test dialogue generation"""
    print("🧪 Testing Dialogue Generator...")
    
    generator = DialogueGenerator(use_fallback=True)
    
    # Test context
    context = DialogueContext(
        user_message="I'm tired but want to train",
        nlp_analysis={
            'emotion': 'tired',
            'intent': 'plan_workout',
            'energy_level': 30,
            'has_injury': False,
            'extracted_context': {}
        },
        ml_prediction={
            'workout_type': 'CARDIO',
            'confidence': 0.7,
            'adjusted_by_nlp': True
        }
    )
    
    # Generate response
    response = generator.generate_response(context)
    assert len(response) > 0
    assert isinstance(response, str)
    
    print("   ✓ Dialogue generator working correctly")


def test_coach_pipeline():
    """Test unified pipeline"""
    print("🧪 Testing Coach Pipeline...")
    
    pipeline = CoachPipeline(enable_memory=True)
    
    # Test message processing
    result = pipeline.process_message("I want to train chest today")
    
    assert 'user_message' in result
    assert 'nlp_analysis' in result
    assert 'response' in result
    assert len(result['response']) > 0
    
    # Test stats
    stats = pipeline.get_stats()
    assert stats['total_requests'] == 1
    assert stats['responses_generated'] == 1
    
    # Test chat interface
    response = pipeline.chat("I'm feeling tired")
    assert len(response) > 0
    
    print("   ✓ Coach pipeline working correctly")


def test_end_to_end_scenarios():
    """Test complete end-to-end scenarios"""
    print("🧪 Testing End-to-End Scenarios...")
    
    pipeline = CoachPipeline(enable_memory=True)
    
    scenarios = [
        {
            'message': "I want to train chest today!",
            'expected_emotion': 'motivated',
            'expected_tone': 'energetic'
        },
        {
            'message': "I'm exhausted and don't feel like working out",
            'expected_emotion': 'tired',
            'expected_tone': 'gentle'
        },
        {
            'message': "My shoulder is really hurting",
            'expected_emotion': 'frustrated',
            'expected_tone': 'serious'
        }
    ]
    
    for i, scenario in enumerate(scenarios, 1):
        result = pipeline.process_message(scenario['message'])
        
        # Verify response exists
        assert result['response'] is not None
        assert len(result['response']) > 0
        
        # Verify pipeline ran
        assert result['nlp_analysis'] is not None
        
        print(f"   ✓ Scenario {i}: {scenario['message'][:40]}... - Response generated")
    
    # Verify memory tracked all conversations
    if pipeline.memory:
        assert len(pipeline.memory.history) == len(scenarios)
        print(f"   ✓ Memory tracking: {len(scenarios)} turns recorded")
    
    print("   ✓ End-to-end scenarios working correctly")


def run_all_tests():
    """Run all integration tests"""
    print("=" * 80)
    print("🧪 PERSONALITY & DIALOGUE UNIT INTEGRATION TESTS")
    print("=" * 80)
    print()
    
    tests = [
        ("Personality System", test_personality_system),
        ("Tone Adapter", test_tone_adapter),
        ("Response Templates", test_response_templates),
        ("Conversation Memory", test_conversation_memory),
        ("Dialogue Generator", test_dialogue_generator),
        ("Coach Pipeline", test_coach_pipeline),
        ("End-to-End Scenarios", test_end_to_end_scenarios)
    ]
    
    passed = 0
    failed = 0
    
    for test_name, test_func in tests:
        try:
            test_func()
            passed += 1
        except Exception as e:
            print(f"   ✗ {test_name} FAILED: {e}")
            failed += 1
        print()
    
    print("=" * 80)
    print("📊 TEST RESULTS")
    print("=" * 80)
    print(f"Total Tests: {len(tests)}")
    print(f"Passed: {passed} ✓")
    print(f"Failed: {failed} ✗")
    print(f"Success Rate: {passed/len(tests)*100:.1f}%")
    print()
    
    if failed == 0:
        print("🎉 ALL TESTS PASSED!")
    else:
        print(f"⚠️ {failed} test(s) failed. Check output above.")
    
    return failed == 0


if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)
