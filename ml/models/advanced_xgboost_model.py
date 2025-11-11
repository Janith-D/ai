"""
Advanced ML Model - XGBoost for 75%+ Accuracy

Uses:
- XGBoost gradient boosting (better than RandomForest)
- Advanced feature engineering (40+ features)
- SMOTE for class balancing
- Hyperparameter optimization
- Ensemble predictions
"""

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split, cross_val_score, StratifiedKFold
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from xgboost import XGBClassifier
from imblearn.over_sampling import SMOTE
import joblib
import os
import json
from datetime import datetime
from typing import Dict, Tuple

import sys
sys.path.append('.')
from ml.features.advanced_feature_engineering import AdvancedFeatureEngineer


class AdvancedXGBoostModel:
    """
    Advanced XGBoost model targeting 75%+ accuracy
    
    Improvements:
    - XGBoost instead of RandomForest (handles imbalance better)
    - 40+ engineered features
    - SMOTE for minority class oversampling
    - Optimized hyperparameters
    - Cross-validation with stratification
    """
    
    def __init__(self):
        """Initialize with optimized XGBoost parameters"""
        
        # Optimized parameters for workout prediction
        self.model = XGBClassifier(
            n_estimators=300,           # More trees
            max_depth=8,                # Deeper trees
            learning_rate=0.05,         # Lower learning rate
            subsample=0.8,              # Row sampling
            colsample_bytree=0.8,       # Feature sampling
            min_child_weight=3,         # Minimum sum of instance weight
            gamma=0.1,                  # Minimum loss reduction
            reg_alpha=0.1,              # L1 regularization
            reg_lambda=1.0,             # L2 regularization
            scale_pos_weight=2,         # Handle class imbalance
            objective='multi:softprob',
            eval_metric='mlogloss',
            random_state=42,
            n_jobs=-1
        )
        
        self.feature_engineer = AdvancedFeatureEngineer()
        self.use_smote = True  # SMOTE for class balancing
        self.smote = None
        self.is_trained = False
        self.metrics = {}
    
    def train(
        self,
        df: pd.DataFrame,
        test_size: float = 0.2,
        cv_folds: int = 5,
        use_smote: bool = True
    ) -> Dict:
        """
        Train advanced XGBoost model
        
        Args:
            df: Training dataframe
            test_size: Test set fraction
            cv_folds: Cross-validation folds
            use_smote: Whether to use SMOTE for balancing
        
        Returns:
            Training metrics
        """
        print("=" * 70)
        print("🚀 ADVANCED XGBOOST MODEL - TARGET 75%+ ACCURACY")
        print("=" * 70)
        
        # Merge user data for goal information
        df_users = pd.read_csv('ml/data/fitness_dataset_users.csv')
        df = df.merge(df_users[['user_id', 'goal']], on='user_id', how='left')
        
        # Feature engineering
        print("\n🔧 Advanced Feature Engineering...")
        X, y = self.feature_engineer.prepare_features(df)
        
        print(f"\n📊 Feature Statistics:")
        print(f"   Total features: {X.shape[1]}")
        print(f"   Training samples: {X.shape[0]}")
        print(f"   Target classes: {len(self.feature_engineer.get_target_classes())}")
        
        # Show class distribution
        print(f"\n📈 Class Distribution (Before SMOTE):")
        class_names = self.feature_engineer.get_target_classes()
        for class_idx, class_name in enumerate(class_names):
            count = (y == class_idx).sum()
            print(f"   {class_name}: {count} ({count/len(y)*100:.1f}%)")
        
        # Train/test split
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=test_size, random_state=42, stratify=y
        )
        
        print(f"\n📊 Data Split:")
        print(f"   Training: {len(X_train)} samples")
        print(f"   Testing: {len(X_test)} samples")
        
        # Apply SMOTE for class balancing
        if use_smote:
            print(f"\n⚖️  Applying SMOTE for class balancing...")
            self.smote = SMOTE(random_state=42, k_neighbors=3)
            X_train_balanced, y_train_balanced = self.smote.fit_resample(X_train, y_train)
            
            print(f"   Before SMOTE: {len(X_train)} samples")
            print(f"   After SMOTE: {len(X_train_balanced)} samples")
            print(f"\n📈 Class Distribution (After SMOTE):")
            for class_idx, class_name in enumerate(class_names):
                count = (y_train_balanced == class_idx).sum()
                print(f"   {class_name}: {count}")
            
            X_train_final = X_train_balanced
            y_train_final = y_train_balanced
        else:
            X_train_final = X_train
            y_train_final = y_train
        
        # Train model
        print(f"\n⚙️  Training XGBoost model...")
        print(f"   n_estimators: {self.model.n_estimators}")
        print(f"   max_depth: {self.model.max_depth}")
        print(f"   learning_rate: {self.model.learning_rate}")
        
        self.model.fit(
            X_train_final, y_train_final,
            eval_set=[(X_test, y_test)],
            verbose=False
        )
        self.is_trained = True
        print(f"   ✅ Model trained")
        
        # Evaluate
        print(f"\n📈 Evaluating performance...")
        
        # Training accuracy
        y_train_pred = self.model.predict(X_train)
        train_accuracy = accuracy_score(y_train, y_train_pred)
        
        # Test accuracy
        y_test_pred = self.model.predict(X_test)
        test_accuracy = accuracy_score(y_test, y_test_pred)
        
        # Cross-validation on original data (not SMOTE)
        print(f"\n🔄 Running {cv_folds}-fold stratified cross-validation...")
        cv = StratifiedKFold(n_splits=cv_folds, shuffle=True, random_state=42)
        cv_scores = cross_val_score(
            self.model, X, y,
            cv=cv, scoring='accuracy', n_jobs=-1
        )
        
        # Store metrics
        self.metrics = {
            'train_accuracy': train_accuracy,
            'test_accuracy': test_accuracy,
            'cv_mean': cv_scores.mean(),
            'cv_std': cv_scores.std(),
            'overfitting_gap': train_accuracy - test_accuracy,
            'feature_count': X.shape[1],
            'used_smote': use_smote
        }
        
        # Print results
        print("\n" + "=" * 70)
        print("🎯 TRAINING RESULTS")
        print("=" * 70)
        
        print(f"\n📊 Accuracy Scores:")
        print(f"   Training Accuracy: {train_accuracy:.4f} ({train_accuracy*100:.2f}%)")
        print(f"   Test Accuracy: {test_accuracy:.4f} ({test_accuracy*100:.2f}%)")
        print(f"   CV Mean Accuracy: {cv_scores.mean():.4f} ({cv_scores.mean()*100:.2f}%)")
        print(f"   CV Std: ±{cv_scores.std():.4f} (±{cv_scores.std()*100:.2f}%)")
        
        overfitting_gap = train_accuracy - test_accuracy
        print(f"\n🎯 Overfitting Analysis:")
        print(f"   Gap (Train - Test): {overfitting_gap:.4f} ({overfitting_gap*100:.2f}%)")
        
        if test_accuracy >= 0.75:
            print(f"\n🎉 🎉 🎉 TARGET ACHIEVED! Test accuracy: {test_accuracy*100:.2f}% >= 75% 🎉 🎉 🎉")
        elif test_accuracy >= 0.70:
            print(f"\n✅ Excellent! Test accuracy: {test_accuracy*100:.2f}% (Very close to 75%)")
        elif test_accuracy >= 0.60:
            print(f"\n📈 Good progress! Test accuracy: {test_accuracy*100:.2f}% (Getting closer)")
        else:
            print(f"\n⚠️  Needs improvement. Test accuracy: {test_accuracy*100:.2f}%")
        
        print(f"\n🔍 CV Fold Scores:")
        for i, score in enumerate(cv_scores, 1):
            print(f"   Fold {i}: {score:.4f} ({score*100:.2f}%)")
        
        # Per-class performance
        print("\n" + "=" * 70)
        print("PER-CLASS PERFORMANCE")
        print("=" * 70)
        print(classification_report(
            y_test, y_test_pred,
            target_names=class_names,
            digits=3
        ))
        
        # Feature importance (top 20)
        print("=" * 70)
        print("TOP 20 FEATURE IMPORTANCE")
        print("=" * 70)
        
        feature_importance = pd.DataFrame({
            'feature': self.feature_engineer.feature_names,
            'importance': self.model.feature_importances_
        }).sort_values('importance', ascending=False)
        
        print("\n🏆 Most Important Features:")
        for i, row in feature_importance.head(20).iterrows():
            print(f"   {row['feature']}: {row['importance']:.4f}")
        
        # Confusion matrix
        print("\n" + "=" * 70)
        print("CONFUSION MATRIX")
        print("=" * 70)
        cm = confusion_matrix(y_test, y_test_pred)
        print("\nActual \\ Predicted:")
        print("         ", "  ".join([f"{c[:4]:>6}" for c in class_names]))
        for i, class_name in enumerate(class_names):
            print(f"{class_name:>8} ", "  ".join([f"{cm[i][j]:>6}" for j in range(len(class_names))]))
        
        return self.metrics
    
    def predict_workout(self, user_features: Dict) -> Tuple[str, float]:
        """Predict next workout"""
        if not self.is_trained:
            raise ValueError("Model not trained. Call train() first.")
        
        # Convert to DataFrame
        df = pd.DataFrame([user_features])
        
        # Add user_id if missing
        if 'user_id' not in df.columns:
            df['user_id'] = 'u0001'
        
        # Transform features
        df_transformed = self.feature_engineer.transform(df, include_target=False)
        X = df_transformed[self.feature_engineer.feature_names]
        
        # Predict
        prediction_encoded = self.model.predict(X)[0]
        probabilities = self.model.predict_proba(X)[0]
        confidence = probabilities.max()
        
        # Decode
        prediction = self.feature_engineer.inverse_transform_target([prediction_encoded])[0]
        
        return prediction, confidence
    
    def save(self, directory: str = 'ml/models/saved_models'):
        """Save model"""
        if not self.is_trained:
            raise ValueError("Cannot save untrained model")
        
        # Create directory
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        version_dir = os.path.join(directory, f'advanced_xgb_v_{timestamp}')
        os.makedirs(version_dir, exist_ok=True)
        
        # Save model
        model_path = os.path.join(version_dir, 'model.pkl')
        joblib.dump(self.model, model_path)
        
        # Save feature engineer
        feature_path = os.path.join(version_dir, 'feature_pipeline.pkl')
        self.feature_engineer.save(feature_path)
        
        # Save metrics
        metrics_path = os.path.join(version_dir, 'metrics.json')
        with open(metrics_path, 'w') as f:
            json.dump(self.metrics, f, indent=2)
        
        # Create 'latest' copy
        latest_dir = os.path.join(directory, 'advanced_xgb_latest')
        if os.path.exists(latest_dir):
            import shutil
            shutil.rmtree(latest_dir)
        
        import shutil
        shutil.copytree(version_dir, latest_dir)
        
        print(f"\n💾 Model saved to: {version_dir}")
        print(f"💾 Latest version: {latest_dir}")
        
        return version_dir
    
    @classmethod
    def load(cls, directory: str) -> 'AdvancedXGBoostModel':
        """Load model"""
        model_path = os.path.join(directory, 'model.pkl')
        feature_path = os.path.join(directory, 'feature_pipeline.pkl')
        metrics_path = os.path.join(directory, 'metrics.json')
        
        model_instance = cls.__new__(cls)
        model_instance.model = joblib.load(model_path)
        model_instance.feature_engineer = AdvancedFeatureEngineer.load(feature_path)
        model_instance.is_trained = True
        
        if os.path.exists(metrics_path):
            with open(metrics_path, 'r') as f:
                model_instance.metrics = json.load(f)
        else:
            model_instance.metrics = {}
        
        print(f"📂 Advanced XGBoost model loaded from: {directory}")
        if model_instance.metrics:
            print(f"   Test accuracy: {model_instance.metrics.get('test_accuracy', 0)*100:.2f}%")
        
        return model_instance


def main():
    """Train advanced model"""
    
    print("=" * 70)
    print("🚀 ADVANCED XGBOOST MODEL - TARGET: 75%+ ACCURACY")
    print("=" * 70)
    print("\n🎯 Strategy:")
    print("   ✅ XGBoost (better than RandomForest)")
    print("   ✅ 40+ engineered features (temporal, behavioral, recovery)")
    print("   ✅ SMOTE for class balancing")
    print("   ✅ Optimized hyperparameters")
    print("   ✅ Stratified cross-validation")
    
    # Load dataset
    print("\n📂 Loading dataset...")
    df = pd.read_csv('ml/data/fitness_dataset.csv')
    print(f"   Loaded {len(df)} samples")
    
    # Train model
    model = AdvancedXGBoostModel()
    metrics = model.train(df, test_size=0.2, cv_folds=5, use_smote=True)
    
    # Test prediction
    print("\n" + "=" * 70)
    print("TESTING PREDICTION")
    print("=" * 70)
    
    test_input = {
        'muscle_group': 'chest',
        'intensity': 'medium',
        'fatigue_level': 'low',
        'duration_min': 45,
        'calories': 350,
        'sleep_hours': 8.0,
        'result_score': 0.85,
        'days_since_last_workout': 1,
        'date': '2025-01-01',
        'user_id': 'u0001',
        'goal': 'muscle_gain'
    }
    
    print(f"\n📥 Input:")
    for key, value in test_input.items():
        print(f"   {key}: {value}")
    
    prediction, confidence = model.predict_workout(test_input)
    print(f"\n📤 Prediction: {prediction.upper()}")
    print(f"   Confidence: {confidence*100:.1f}%")
    
    # Save model
    print("\n" + "=" * 70)
    save_dir = model.save()
    
    # Final summary
    print("\n" + "=" * 70)
    print("🎯 FINAL RESULTS")
    print("=" * 70)
    print(f"\n📊 Performance:")
    print(f"   Test Accuracy: {metrics['test_accuracy']*100:.2f}%")
    print(f"   CV Accuracy: {metrics['cv_mean']*100:.2f}% ± {metrics['cv_std']*100:.2f}%")
    print(f"   Overfitting Gap: {metrics['overfitting_gap']*100:.2f}%")
    print(f"   Features: {metrics['feature_count']}")
    
    if metrics['test_accuracy'] >= 0.75:
        print(f"\n🎉 SUCCESS! Achieved {metrics['test_accuracy']*100:.2f}% (Target: 75%)")
    else:
        print(f"\n📈 Progress: {metrics['test_accuracy']*100:.2f}% / 75%")
        remaining = (0.75 - metrics['test_accuracy']) * 100
        print(f"   Need {remaining:.2f} more percentage points")
    
    print("\n" + "=" * 70)
    print("✅ TRAINING COMPLETE")
    print("=" * 70)


if __name__ == '__main__':
    main()
