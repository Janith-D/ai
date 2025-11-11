"""Quick verification of improved model"""

from ml.models.improved_model import ImprovedModel

print("="*70)
print("IMPROVED MODEL - QUICK VERIFICATION")
print("="*70)

# Load model
model = ImprovedModel.load('ml/models/saved_models/improved_latest')

print("\n✅ Model loaded successfully!")
print(f"   Test Accuracy: {model.metrics.get('test_accuracy', 0)*100:.2f}%")
print(f"   Train Accuracy: {model.metrics.get('train_accuracy', 0)*100:.2f}%")
print(f"   Overfitting Gap: {model.metrics.get('overfitting_gap', 0)*100:.2f}%")
print(f"   CV Mean: {model.metrics.get('cv_mean', 0)*100:.2f}%")

# Test prediction
print("\n" + "="*70)
print("TEST PREDICTION")
print("="*70)

test_case = {
    'muscle_group': 'legs',
    'intensity': 'heavy',
    'fatigue_level': 'medium',
    'duration_min': 60,
    'calories': 450,
    'sleep_hours': 7.5,
    'result_score': 0.88,
    'days_since_last_workout': 2
}

print("\n📥 Input:")
for k, v in test_case.items():
    print(f"   {k}: {v}")

prediction, confidence = model.predict_workout(test_case)

print(f"\n📤 Prediction: {prediction.upper()}")
print(f"   Confidence: {confidence*100:.1f}%")

print("\n" + "="*70)
print("✅ VERIFICATION COMPLETE")
print("="*70)
