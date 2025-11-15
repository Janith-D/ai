"""
Full Integration Test - All Brains Connected
Tests Central Controller with NLP, ML, Prolog, and Dialogue
"""

import sys
from pathlib import Path

# Add paths
sys.path.append(str(Path(__file__).parent))

from central_controller import CentralController
from nlp import NLPPipeline
from nlp.integration.nlp_ml_integration import NLPMLIntegration
from nlp.integration.nlp_prolog_integration import NLPPrologIntegration
from dialogue import CoachPipeline


def test_full_integration():
    """Test Central Controller with all brains connected"""
    
    print("=" * 80)
    print("🧠 FULL INTEGRATION TEST - ALL BRAINS CONNECTED")
    print("=" * 80)
    print()
    
    # Initialize all brains
    print("Initializing brains...")
    
    try:
        nlp = NLPPipeline()
        print("✓ NLP Brain initialized")
    except Exception as e:
        print(f"⚠️  NLP Brain: {e}")
        nlp = None
    
    try:
        ml_integration = NLPMLIntegration()
        print("✓ ML Brain initialized")
    except Exception as e:
        print(f"⚠️  ML Brain: {e}")
        ml_integration = None
    
    try:
        prolog_integration = NLPPrologIntegration()
        print("✓ Logic Brain (Prolog) initialized")
    except Exception as e:
        print(f"⚠️  Logic Brain: {e}")
        prolog_integration = None
    
    try:
        dialogue = CoachPipeline(
            nlp_pipeline=nlp,
            ml_integration=ml_integration,
            prolog_integration=prolog_integration,
            enable_memory=True
        )
        print("✓ Personality Brain (Coach Atlas) initialized")
    except Exception as e:
        print(f"⚠️  Personality Brain: {e}")
        dialogue = None
    
    print()
    
    # Create Central Controller
    controller = CentralController(
        nlp_pipeline=nlp,
        prolog_integration=prolog_integration,
        ml_integration=ml_integration,
        dialogue_pipeline=dialogue,
        enable_logging=True
    )
    
    print()
    print("=" * 80)
    print("🧪 RUNNING TEST SCENARIOS")
    print("=" * 80)
    
    # Test scenarios
    scenarios = [
        {
            'name': 'Motivated User - Chest Workout',
            'message': 'I want to train chest today! Feeling strong!',
            'user_data': {'age': 25, 'weight': 75, 'experience': 'intermediate'}
        },
        {
            'name': 'Tired User - Low Energy',
            'message': "I'm exhausted but I still want to workout",
            'user_data': {'age': 30, 'weight': 70, 'experience': 'beginner'}
        },
        {
            'name': 'Injured User - Safety Override',
            'message': 'My shoulder is really hurting',
            'user_data': {'age': 28, 'weight': 68, 'experience': 'intermediate'}
        },
        {
            'name': 'Unmotivated User - Needs Encouragement',
            'message': "I don't feel motivated at all",
            'user_data': {'age': 35, 'weight': 80, 'experience': 'beginner'}
        }
    ]
    
    results = []
    
    for i, scenario in enumerate(scenarios, 1):
        print(f"\n{'#' * 80}")
        print(f"SCENARIO {i}: {scenario['name']}")
        print(f"{'#' * 80}\n")
        
        try:
            decision = controller.process(
                user_message=scenario['message'],
                user_data=scenario['user_data']
            )
            
            print(f"\n📋 DECISION SUMMARY:")
            print(f"   Workout: {decision.workout_recommendation}")
            print(f"   Safety: {decision.safety_status}")
            print(f"   Confidence: {decision.confidence_score:.1%}")
            print(f"   Brains Used: {sum(decision.brains_used.values())}/4")
            print(f"   Path: {' → '.join(decision.decision_path)}")
            print()
            print(f"🗣️  COACH RESPONSE:")
            print(f"   {decision.final_response[:200]}...")
            print()
            
            results.append({
                'scenario': scenario['name'],
                'success': True,
                'workout': decision.workout_recommendation,
                'safety': decision.safety_status,
                'time_ms': decision.total_execution_time_ms
            })
            
        except Exception as e:
            print(f"❌ Error: {e}")
            results.append({
                'scenario': scenario['name'],
                'success': False,
                'error': str(e)
            })
    
    # Final summary
    print("=" * 80)
    print("📊 INTEGRATION TEST SUMMARY")
    print("=" * 80)
    print()
    
    successful = sum(1 for r in results if r.get('success'))
    total = len(results)
    
    print(f"Scenarios Tested: {total}")
    print(f"Successful: {successful}")
    print(f"Failed: {total - successful}")
    print(f"Success Rate: {successful/total*100:.1f}%")
    print()
    
    # Controller stats
    stats = controller.get_stats()
    print("Controller Statistics:")
    for key, value in stats.items():
        print(f"   {key}: {value}")
    print()
    
    # Brain usage
    print("Brain Activation:")
    if results:
        first_result = [r for r in results if r.get('success')]
        if first_result:
            print(f"   NLP Brain: {'✓' if nlp else '✗'}")
            print(f"   Logic Brain: {'✓' if prolog_integration else '✗'}")
            print(f"   ML Brain: {'✓' if ml_integration else '✗'}")
            print(f"   Personality Brain: {'✓' if dialogue else '✗'}")
    print()
    
    print("=" * 80)
    if successful == total:
        print("🎉 ALL TESTS PASSED!")
    else:
        print(f"⚠️  {total - successful} test(s) failed")
    print("=" * 80)
    print()
    
    return successful == total


if __name__ == "__main__":
    success = test_full_integration()
    sys.exit(0 if success else 1)
