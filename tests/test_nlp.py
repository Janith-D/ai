"""
Comprehensive Test Suite for NLP Module
Tests emotion detection, intent classification, context extraction, and integrations
"""

import sys
from pathlib import Path
from datetime import datetime
import json

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from nlp.emotion_detector import EmotionDetector
from nlp.intent_classifier import IntentClassifier
from nlp.context_extractor import ContextExtractor
from nlp.nlp_pipeline import NLPPipeline


def test_emotion_detector():
    """Test emotion detection module"""
    print("\n" + "="*80)
    print("🧪 TESTING EMOTION DETECTOR")
    print("="*80 + "\n")
    
    detector = EmotionDetector()
    
    test_cases = [
        ("I'm so tired and exhausted", "demotivated", "low"),
        ("Let's go! I'm pumped and ready!", "motivated", "high"),
        ("I'm frustrated with my progress", "frustrated", "medium"),
        ("Feeling great today", "motivated", "high"),
        ("My legs are so sore", "demotivated", "low"),
    ]
    
    passed = 0
    for text, expected_emotion, expected_energy in test_cases:
        result = detector.detect(text)
        status = "✓" if result['fitness_emotion'] == expected_emotion and result['energy_level'] == expected_energy else "✗"
        
        print(f"{status} \"{text}\"")
        print(f"   Expected: {expected_emotion} / {expected_energy}")
        print(f"   Got: {result['fitness_emotion']} / {result['energy_level']} (conf: {result['confidence']:.2f})")
        
        if status == "✓":
            passed += 1
        print()
    
    print(f"Results: {passed}/{len(test_cases)} tests passed\n")
    return passed == len(test_cases)


def test_intent_classifier():
    """Test intent classification module"""
    print("\n" + "="*80)
    print("🧪 TESTING INTENT CLASSIFIER")
    print("="*80 + "\n")
    
    classifier = IntentClassifier()
    
    test_cases = [
        ("I want to workout my chest", "plan_workout"),
        ("I need to rest today", "rest_request"),
        ("How many calories should I eat?", "diet_question"),
        ("Can you motivate me?", "motivation_request"),
        ("I want to switch to bulking", "update_goal"),
        ("My shoulder hurts", "injury_report"),
        ("When should I train tomorrow?", "schedule_query"),
        ("How am I doing?", "progress_check"),
    ]
    
    passed = 0
    for text, expected_intent in test_cases:
        result = classifier.classify(text)
        status = "✓" if result['intent'] == expected_intent else "✗"
        
        print(f"{status} \"{text}\"")
        print(f"   Expected: {expected_intent}")
        print(f"   Got: {result['intent']} (conf: {result['confidence']:.2f})")
        
        if status == "✓":
            passed += 1
        print()
    
    print(f"Results: {passed}/{len(test_cases)} tests passed\n")
    return passed >= len(test_cases) * 0.75  # 75% pass rate acceptable


def test_context_extractor():
    """Test context extraction module"""
    print("\n" + "="*80)
    print("🧪 TESTING CONTEXT EXTRACTOR")
    print("="*80 + "\n")
    
    extractor = ContextExtractor()
    
    test_cases = [
        {
            'text': "I want to train chest 4 times a week",
            'expected': {
                'has_muscle': ['chest'],
                'has_frequency': True
            }
        },
        {
            'text': "I'm too sore from leg day",
            'expected': {
                'has_muscle': ['legs'],
                'has_state': ['sore']
            }
        },
        {
            'text': "Let's do intense shoulders tomorrow morning",
            'expected': {
                'has_muscle': ['shoulders'],
                'has_intensity': 'high',
                'has_time': 'tomorrow'
            }
        },
        {
            'text': "I want to lose weight with cardio",
            'expected': {
                'has_goal': 'fat_loss',
                'has_muscle': ['cardio']
            }
        },
    ]
    
    passed = 0
    for case in test_cases:
        text = case['text']
        expected = case['expected']
        context = extractor.extract_all(text)
        
        checks = []
        
        # Check muscles
        if 'has_muscle' in expected:
            muscle_match = any(m in context.get('muscle_groups', []) for m in expected['has_muscle'])
            checks.append(muscle_match)
        
        # Check frequency
        if 'has_frequency' in expected:
            freq_match = 'frequency' in context
            checks.append(freq_match)
        
        # Check physical state
        if 'has_state' in expected:
            state_match = any(s in context.get('physical_state', []) for s in expected['has_state'])
            checks.append(state_match)
        
        # Check intensity
        if 'has_intensity' in expected:
            intensity_match = context.get('intensity') == expected['has_intensity']
            checks.append(intensity_match)
        
        # Check time
        if 'has_time' in expected:
            time_match = context.get('time') == expected['has_time']
            checks.append(time_match)
        
        # Check goal
        if 'has_goal' in expected:
            goal_match = context.get('goal') == expected['has_goal']
            checks.append(goal_match)
        
        all_passed = all(checks)
        status = "✓" if all_passed else "✗"
        
        print(f"{status} \"{text}\"")
        print(f"   Context: {context}")
        
        if all_passed:
            passed += 1
        print()
    
    print(f"Results: {passed}/{len(test_cases)} tests passed\n")
    return passed >= len(test_cases) * 0.75


def test_nlp_pipeline():
    """Test complete NLP pipeline"""
    print("\n" + "="*80)
    print("🧪 TESTING NLP PIPELINE")
    print("="*80 + "\n")
    
    pipeline = NLPPipeline()
    
    test_messages = [
        "I'm exhausted but I still want to train my legs",
        "Feeling great! Let's do chest day!",
        "Too sore from yesterday, need rest",
    ]
    
    passed = 0
    for msg in test_messages:
        try:
            result = pipeline.analyze(msg, user_id='test_user', include_prolog=True)
            
            # Check required fields
            required_fields = ['text', 'preprocessed', 'emotion', 'intent', 'context', 'timestamp']
            has_all = all(field in result for field in required_fields)
            
            status = "✓" if has_all else "✗"
            print(f"{status} \"{msg}\"")
            
            if has_all:
                print(f"   Emotion: {result['emotion']['fitness_emotion']}")
                print(f"   Intent: {result['intent']['intent']}")
                print(f"   Context: {list(result['context'].keys())}")
                passed += 1
            
            print()
        except Exception as e:
            print(f"✗ \"{msg}\"")
            print(f"   Error: {e}\n")
    
    print(f"Results: {passed}/{len(test_messages)} tests passed\n")
    return passed == len(test_messages)


def test_ml_features():
    """Test ML feature generation"""
    print("\n" + "="*80)
    print("🧪 TESTING ML FEATURE GENERATION")
    print("="*80 + "\n")
    
    pipeline = NLPPipeline()
    
    test_cases = [
        {
            'text': "I'm super motivated to train chest!",
            'expected_features': {
                'emotion_motivated': 1,
                'intent_workout': 1,
                'target_chest': 1,
            }
        },
        {
            'text': "I'm too tired, need rest",
            'expected_features': {
                'emotion_tired': 1,
                'intent_rest': 1,
                'energy_level': 0,  # low
            }
        },
    ]
    
    passed = 0
    for case in test_cases:
        text = case['text']
        expected = case['expected_features']
        
        features = pipeline.to_ml_features(text)
        
        # Check expected features
        matches = all(features.get(k) == v for k, v in expected.items())
        status = "✓" if matches else "✗"
        
        print(f"{status} \"{text}\"")
        print(f"   Expected features: {expected}")
        print(f"   Got features: {features}")
        
        if matches:
            passed += 1
        print()
    
    print(f"Results: {passed}/{len(test_cases)} tests passed\n")
    return passed >= len(test_cases) * 0.75


def run_all_tests():
    """Run all test suites"""
    print("\n" + "="*80)
    print("🧪 RUNNING COMPREHENSIVE NLP TEST SUITE")
    print("="*80)
    
    results = {
        'Emotion Detection': test_emotion_detector(),
        'Intent Classification': test_intent_classifier(),
        'Context Extraction': test_context_extractor(),
        'NLP Pipeline': test_nlp_pipeline(),
        'ML Features': test_ml_features(),
    }
    
    print("\n" + "="*80)
    print("📊 TEST RESULTS SUMMARY")
    print("="*80 + "\n")
    
    for test_name, passed in results.items():
        status = "✅ PASSED" if passed else "❌ FAILED"
        print(f"{status} - {test_name}")
    
    total_passed = sum(results.values())
    total_tests = len(results)
    
    print(f"\n{'='*80}")
    print(f"Overall: {total_passed}/{total_tests} test suites passed")
    print("="*80 + "\n")
    
    return all(results.values())


if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)
