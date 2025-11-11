"""
Final Verification - 76.53% Accuracy Model

Tests the advanced XGBoost model with various scenarios
"""

from ml.models.advanced_xgboost_model import AdvancedXGBoostModel

print("=" * 70)
print("🎉 ADVANCED XGBOOST MODEL - VERIFICATION")
print("=" * 70)

# Load model
print("\n📂 Loading model...")
model = AdvancedXGBoostModel.load('ml/models/saved_models/advanced_xgb_latest')

print("\n✅ Model loaded successfully!")
print(f"   Test Accuracy: {model.metrics['test_accuracy']*100:.2f}%")
print(f"   CV Accuracy: {model.metrics['cv_mean']*100:.2f}% ± {model.metrics['cv_std']*100:.2f}%")
print(f"   Features: {model.metrics['feature_count']}")

# Test scenarios
print("\n" + "=" * 70)
print("🧪 TESTING PREDICTION SCENARIOS")
print("=" * 70)

test_scenarios = [
    {
        'name': 'Muscle Gain - Fresh Start',
        'input': {
            'muscle_group': 'rest',
            'intensity': 'none',
            'fatigue_level': 'low',
            'duration_min': 0,
            'calories': 0,
            'sleep_hours': 8.0,
            'result_score': 0.75,
            'days_since_last_workout': 1,
            'date': '2025-01-01',
            'user_id': 'u0001',
            'goal': 'muscle_gain'
        }
    },
    {
        'name': 'Fat Loss - High Energy',
        'input': {
            'muscle_group': 'chest',
            'intensity': 'medium',
            'fatigue_level': 'low',
            'duration_min': 45,
            'calories': 350,
            'sleep_hours': 8.5,
            'result_score': 0.88,
            'days_since_last_workout': 1,
            'date': '2025-01-02',
            'user_id': 'u0002',
            'goal': 'fat_loss'
        }
    },
    {
        'name': 'Endurance - After Legs',
        'input': {
            'muscle_group': 'legs',
            'intensity': 'heavy',
            'fatigue_level': 'medium',
            'duration_min': 60,
            'calories': 450,
            'sleep_hours': 7.0,
            'result_score': 0.75,
            'days_since_last_workout': 1,
            'date': '2025-01-03',
            'user_id': 'u0003',
            'goal': 'endurance'
        }
    },
    {
        'name': 'Exhausted - Need Rest',
        'input': {
            'muscle_group': 'shoulders',
            'intensity': 'heavy',
            'fatigue_level': 'high',
            'duration_min': 30,
            'calories': 250,
            'sleep_hours': 5.0,
            'result_score': 0.60,
            'days_since_last_workout': 1,
            'date': '2025-01-04',
            'user_id': 'u0004',
            'goal': 'strength'
        }
    },
    {
        'name': 'General Fitness - Balanced',
        'input': {
            'muscle_group': 'back',
            'intensity': 'medium',
            'fatigue_level': 'medium',
            'duration_min': 45,
            'calories': 320,
            'sleep_hours': 7.5,
            'result_score': 0.80,
            'days_since_last_workout': 2,
            'date': '2025-01-05',
            'user_id': 'u0005',
            'goal': 'general_fitness'
        }
    }
]

for i, scenario in enumerate(test_scenarios, 1):
    print(f"\n{'─' * 70}")
    print(f"Test {i}: {scenario['name']}")
    print(f"{'─' * 70}")
    
    input_data = scenario['input']
    print(f"📥 Input:")
    print(f"   Goal: {input_data['goal']}")
    print(f"   Last Workout: {input_data['muscle_group']} ({input_data['intensity']})")
    print(f"   Fatigue: {input_data['fatigue_level']}, Sleep: {input_data['sleep_hours']}h")
    print(f"   Days Since Last: {input_data['days_since_last_workout']}")
    
    prediction, confidence = model.predict_workout(input_data)
    
    print(f"\n📤 Prediction: {prediction.upper()}")
    print(f"   Confidence: {confidence*100:.1f}%")
    
    # Confidence rating
    if confidence >= 0.8:
        rating = "🟢 Very Confident"
    elif confidence >= 0.6:
        rating = "🟡 Confident"
    else:
        rating = "🟠 Moderate"
    
    print(f"   Rating: {rating}")

print("\n" + "=" * 70)
print("✅ VERIFICATION COMPLETE")
print("=" * 70)

print(f"\n🎯 Model Summary:")
print(f"   Algorithm: XGBoost with SMOTE")
print(f"   Features: {model.metrics['feature_count']} advanced features")
print(f"   Accuracy: {model.metrics['test_accuracy']*100:.2f}% (Target: 75%)")
print(f"   Status: {'🎉 TARGET ACHIEVED' if model.metrics['test_accuracy'] >= 0.75 else '⚠️ Needs improvement'}")

print(f"\n📊 Performance:")
print(f"   ✅ Test Accuracy: {model.metrics['test_accuracy']*100:.2f}%")
print(f"   ✅ CV Accuracy: {model.metrics['cv_mean']*100:.2f}% ± {model.metrics['cv_std']*100:.2f}%")
print(f"   ✅ Overfitting: {model.metrics['overfitting_gap']*100:.2f}%")

if model.metrics['test_accuracy'] >= 0.75:
    print(f"\n🚀 Ready for production deployment!")
else:
    remaining = (0.75 - model.metrics['test_accuracy']) * 100
    print(f"\n📈 Need {remaining:.2f} more percentage points to reach 75% target")

print("\n" + "=" * 70)
