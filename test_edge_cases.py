"""Quick test for intent classifier edge cases"""
from nlp.intent_classifier import IntentClassifier

classifier = IntentClassifier()

test_cases = [
    ("Can you motivate me?", "motivation_request"),
    ("My shoulder hurts", "injury_report"),
    ("When should I train tomorrow?", "schedule_query"),
    ("I need to rest today", "rest_request"),
    ("I want to switch to bulking", "update_goal"),
]

print("\n🧪 Intent Classifier Edge Case Tests:\n")
print("="*60)

passed = 0
for text, expected in test_cases:
    result = classifier.classify(text)
    status = "✓" if result['intent'] == expected else "✗"
    
    print(f"\n{status} \"{text}\"")
    print(f"   Expected: {expected}")
    print(f"   Got: {result['intent']} (confidence: {result['confidence']:.2f})")
    
    if status == "✓":
        passed += 1

print("\n" + "="*60)
print(f"\n✅ Result: {passed}/{len(test_cases)} tests passed\n")

if passed == len(test_cases):
    print("🎉 All edge cases fixed successfully!")
else:
    print("⚠️ Some edge cases still need work")
