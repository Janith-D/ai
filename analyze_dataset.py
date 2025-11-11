"""
Analyze the synthetic dataset to assess if it's sufficient for ML training
"""

import pandas as pd
import numpy as np
from collections import Counter

# Load the dataset
df = pd.read_csv('ml/data/fitness_dataset.csv')

print("=" * 70)
print("DATASET SUFFICIENCY ANALYSIS")
print("=" * 70)

# 1. BASIC STATISTICS
print("\n📊 1. DATASET SIZE")
print(f"   Total samples: {len(df):,}")
print(f"   Features: {len(df.columns) - 1} (excluding target)")
print(f"   Target variable: 'next_workout'")

# Rule of thumb: 
# - Minimum: 100 samples per class
# - Good: 1000+ samples per class
# - Excellent: 5000+ samples per class

target_counts = df['next_workout'].value_counts()
print(f"\n   Target classes: {len(target_counts)}")
print(f"   Min samples per class: {target_counts.min():,}")
print(f"   Max samples per class: {target_counts.max():,}")
print(f"   Avg samples per class: {target_counts.mean():.0f}")

# 2. CLASS BALANCE
print("\n⚖️  2. CLASS BALANCE")
print("\n   Class distribution:")
for workout, count in target_counts.items():
    percentage = (count / len(df)) * 100
    bar = "█" * int(percentage / 2)
    status = "✓" if count >= 100 else "⚠️"
    print(f"   {status} {workout:12s}: {count:4,} ({percentage:5.1f}%) {bar}")

# Check imbalance ratio
max_class = target_counts.max()
min_class = target_counts.min()
imbalance_ratio = max_class / min_class
print(f"\n   Imbalance ratio: {imbalance_ratio:.1f}:1")
if imbalance_ratio < 3:
    print("   ✅ Well balanced (ratio < 3:1)")
elif imbalance_ratio < 10:
    print("   ⚠️  Moderate imbalance (ratio 3:1 - 10:1)")
else:
    print("   ❌ High imbalance (ratio > 10:1) - may need resampling")

# 3. FEATURE COVERAGE
print("\n🔍 3. FEATURE COVERAGE")
print("\n   Missing values:")
missing = df.isnull().sum()
if missing.sum() == 0:
    print("   ✅ No missing values!")
else:
    for col, count in missing[missing > 0].items():
        print(f"   ⚠️  {col}: {count} missing ({count/len(df)*100:.1f}%)")

print("\n   Feature types:")
for col in df.columns:
    if col == 'next_workout':
        continue
    dtype = df[col].dtype
    unique = df[col].nunique()
    print(f"   • {col:25s}: {str(dtype):10s} ({unique:4d} unique values)")

# 4. DATA QUALITY
print("\n🎯 4. DATA QUALITY CHECKS")

# Check for duplicate rows
duplicates = df.duplicated().sum()
print(f"   Duplicate rows: {duplicates} ({duplicates/len(df)*100:.1f}%)")
if duplicates == 0:
    print("   ✅ No duplicates")
else:
    print("   ⚠️  Consider removing duplicates")

# Check for constant features
print("\n   Feature variance:")
numeric_cols = df.select_dtypes(include=[np.number]).columns
for col in numeric_cols:
    if col == 'days_since_last_workout':
        continue
    variance = df[col].var()
    if variance == 0:
        print(f"   ❌ {col}: No variance (constant feature)")
    else:
        print(f"   ✅ {col}: Has variance")

# 5. SAMPLE SIZE ASSESSMENT
print("\n📏 5. SAMPLE SIZE ASSESSMENT")
print("\n   ML Model Requirements:")
print(f"   • Current dataset: {len(df):,} samples")
print(f"   • Train/test split (80/20): {int(len(df)*0.8):,} / {int(len(df)*0.2):,}")
print(f"   • Samples per class (train): ~{int(target_counts.mean()*0.8)}")

# Rule of thumb for classification:
# - Minimum viable: 10 * features * classes
# - Good: 50 * features * classes
# - Excellent: 100+ * features * classes

n_features = len(df.columns) - 1
n_classes = len(target_counts)
min_samples = 10 * n_features * n_classes
good_samples = 50 * n_features * n_classes
excellent_samples = 100 * n_features * n_classes

print(f"\n   Recommended sample sizes (for {n_features} features, {n_classes} classes):")
print(f"   • Minimum viable:  {min_samples:,} samples")
print(f"   • Good:            {good_samples:,} samples")
print(f"   • Excellent:       {excellent_samples:,} samples")
print(f"   • Your dataset:    {len(df):,} samples")

if len(df) >= excellent_samples:
    status = "✅ EXCELLENT"
elif len(df) >= good_samples:
    status = "✅ GOOD"
elif len(df) >= min_samples:
    status = "⚠️  MINIMUM VIABLE"
else:
    status = "❌ INSUFFICIENT"

print(f"\n   Status: {status}")

# 6. FINAL VERDICT
print("\n" + "=" * 70)
print("FINAL VERDICT")
print("=" * 70)

issues = []
recommendations = []

# Check critical issues
if len(df) < min_samples:
    issues.append("❌ Dataset too small")
    recommendations.append(f"Generate at least {min_samples:,} samples")
elif len(df) < good_samples:
    issues.append("⚠️  Dataset size is minimum viable but not ideal")
    recommendations.append(f"Consider increasing to {good_samples:,}+ samples for better accuracy")

if imbalance_ratio > 10:
    issues.append("❌ High class imbalance")
    recommendations.append("Apply SMOTE or class weighting to balance classes")
elif imbalance_ratio > 3:
    issues.append("⚠️  Moderate class imbalance")
    recommendations.append("Consider using class_weight='balanced' in RandomForest")

if min_class < 100:
    issues.append(f"⚠️  Some classes have < 100 samples")
    recommendations.append("Generate more samples for underrepresented classes")

# Print results
if not issues:
    print("\n✅ DATASET IS SUFFICIENT FOR TRAINING!")
    print("\n   Your dataset meets all requirements for training a RandomForest model.")
    print(f"   Expected baseline accuracy: 65-75%")
    print(f"   With tuning: 75-85%")
else:
    print("\n⚠️  DATASET HAS SOME ISSUES:")
    for issue in issues:
        print(f"   {issue}")
    
    print("\n💡 RECOMMENDATIONS:")
    for i, rec in enumerate(recommendations, 1):
        print(f"   {i}. {rec}")

# 7. ACTIONABLE SUGGESTIONS
print("\n" + "=" * 70)
print("ACTIONABLE NEXT STEPS")
print("=" * 70)

print("\n1️⃣  CURRENT STATE:")
if len(df) >= good_samples and imbalance_ratio < 3 and min_class >= 100:
    print("   ✅ You can proceed with model training!")
    print("   ✅ Dataset quality is good for baseline model")
elif len(df) >= min_samples:
    print("   ⚠️  Dataset is usable but has room for improvement")
    print("   ✅ You can start training and iterate")
else:
    print("   ❌ Dataset needs improvement before training")

print("\n2️⃣  QUICK IMPROVEMENTS (if needed):")
if len(df) < good_samples:
    print(f"   • Increase n_users to 150 or days_per_user to 120")
    print(f"     Target: {good_samples:,}+ samples")

if imbalance_ratio > 3:
    print("   • Use class_weight='balanced' in RandomForest")
    print("   • Or apply SMOTE oversampling to minority classes")

if min_class < 100:
    print("   • Generate more data for underrepresented classes")
    print(f"   • Classes with < 100 samples: {list(target_counts[target_counts < 100].index)}")

print("\n3️⃣  TRAINING STRATEGY:")
print("   ✅ Use train_test_split with stratify=y")
print("   ✅ Start with RandomForestClassifier (robust to small data)")
print("   ✅ Use cross-validation (5-fold) to validate performance")
print("   ✅ Monitor both training and test accuracy to detect overfitting")

print("\n" + "=" * 70)
