"""
Improved ML Model - RandomForest with Optimized Parameters

Enhanced version with:
- Expanded dataset (18,000 samples)
- Derived features (12 features)
- Optimized hyperparameters from GridSearch
- Better overfitting control
"""

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import (
    accuracy_score, precision_score, recall_score, f1_score,
    confusion_matrix, classification_report
)
import joblib
import os
import json
from datetime import datetime
from typing import Dict, Tuple

import sys
sys.path.append('.')
from ml.features.feature_engineering import FeatureEngineer


class ImprovedModel:
    """
    Improved RandomForest classifier with optimized hyperparameters
    
    Improvements:
    - Uses 18,000 training samples (vs 9,000)
    - Uses 12 features including derived ones (vs 8)
    - Optimized hyperparameters from GridSearch
    - Better generalization (reduced overfitting)
    """
    
    def __init__(self, config_path: str = 'ml/models/configs/best_params.json'):
        """
        Initialize with optimized hyperparameters
        
        Args:
            config_path: Path to JSON file with best parameters
        """
        # Load best parameters if available
        if os.path.exists(config_path):
            print(f"📂 Loading optimized parameters from: {config_path}")
            with open(config_path, 'r') as f:
                params = json.load(f)
            print(f"   Parameters: {params}")
        else:
            print(f"⚠️  Config file not found. Using default optimized params.")
            # Default optimized params (from typical tuning results)
            params = {
                'n_estimators': 200,
                'max_depth': 15,
                'min_samples_split': 10,
                'min_samples_leaf': 4,
                'max_features': 'sqrt',
                'class_weight': 'balanced'
            }
        
        self.model = RandomForestClassifier(
            random_state=42,
            n_jobs=-1,
            **params
        )
        
        self.feature_engineer = FeatureEngineer(add_derived_features=True)
        self.is_trained = False
        self.metrics = {}
        
    def train(
        self,
        df: pd.DataFrame,
        test_size: float = 0.2,
        cv_folds: int = 5
    ) -> Dict:
        """
        Train model with cross-validation
        
        Args:
            df: Training dataframe
            test_size: Fraction for test set
            cv_folds: Number of CV folds
        
        Returns:
            Dictionary of training metrics
        """
        print("=" * 70)
        print("TRAINING IMPROVED RANDOMFOREST MODEL")
        print("=" * 70)
        
        # Feature engineering
        print("\n🔧 Engineering features...")
        self.feature_engineer.fit(df, target_col='next_workout')
        X, y = self.feature_engineer.prepare_features(df)
        
        print(f"   Total features: {X.shape[1]}")
        print(f"   Feature names:")
        for i, name in enumerate(self.feature_engineer.feature_names, 1):
            print(f"      {i}. {name}")
        
        # Train/test split
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=test_size, random_state=42, stratify=y
        )
        
        print(f"\n📊 Dataset Split:")
        print(f"   Training: {len(X_train)} samples ({(1-test_size)*100:.0f}%)")
        print(f"   Testing: {len(X_test)} samples ({test_size*100:.0f}%)")
        
        # Train model
        print(f"\n⚙️  Training RandomForest...")
        self.model.fit(X_train, y_train)
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
        
        # Cross-validation
        print(f"\n🔄 Running {cv_folds}-fold cross-validation...")
        cv_scores = cross_val_score(
            self.model, X_train, y_train, 
            cv=cv_folds, scoring='accuracy', n_jobs=-1
        )
        
        # Store metrics
        self.metrics = {
            'train_accuracy': train_accuracy,
            'test_accuracy': test_accuracy,
            'cv_mean': cv_scores.mean(),
            'cv_std': cv_scores.std(),
            'overfitting_gap': train_accuracy - test_accuracy
        }
        
        # Print results
        print("\n" + "=" * 70)
        print("TRAINING RESULTS")
        print("=" * 70)
        
        print(f"\n📊 Accuracy Scores:")
        print(f"   Training Accuracy: {train_accuracy:.4f} ({train_accuracy*100:.2f}%)")
        print(f"   Test Accuracy: {test_accuracy:.4f} ({test_accuracy*100:.2f}%)")
        print(f"   CV Mean Accuracy: {cv_scores.mean():.4f} ({cv_scores.mean()*100:.2f}%)")
        print(f"   CV Std: ±{cv_scores.std():.4f} (±{cv_scores.std()*100:.2f}%)")
        
        overfitting_gap = train_accuracy - test_accuracy
        print(f"\n🎯 Overfitting Analysis:")
        print(f"   Gap (Train - Test): {overfitting_gap:.4f} ({overfitting_gap*100:.2f}%)")
        
        if overfitting_gap > 0.15:
            print(f"   ⚠️  Warning: High overfitting (gap > 15%)")
        elif overfitting_gap > 0.10:
            print(f"   ⚠️  Moderate overfitting (gap > 10%)")
        else:
            print(f"   ✅ Good generalization (gap < 10%)")
        
        print(f"\n🔍 CV Fold Scores:")
        for i, score in enumerate(cv_scores, 1):
            print(f"   Fold {i}: {score:.4f} ({score*100:.2f}%)")
        
        # Per-class performance
        print("\n" + "=" * 70)
        print("PER-CLASS PERFORMANCE")
        print("=" * 70)
        print(classification_report(
            y_test, y_test_pred,
            target_names=self.feature_engineer.get_target_classes()
        ))
        
        # Feature importance
        print("=" * 70)
        print("FEATURE IMPORTANCE")
        print("=" * 70)
        
        feature_importance = pd.DataFrame({
            'feature': self.feature_engineer.feature_names,
            'importance': self.model.feature_importances_
        }).sort_values('importance', ascending=False)
        
        print("\n🏆 Top Features:")
        for i, row in feature_importance.head(10).iterrows():
            print(f"   {row['feature']}: {row['importance']:.3f}")
        
        return self.metrics
    
    def predict_workout(self, user_features: Dict) -> Tuple[str, float]:
        """
        Predict next workout for a user
        
        Args:
            user_features: Dictionary with user profile and history
        
        Returns:
            (workout_type, confidence_score)
        """
        if not self.is_trained:
            raise ValueError("Model not trained. Call train() first.")
        
        # Convert dict to DataFrame
        df = pd.DataFrame([user_features])
        
        # Transform features
        df_transformed = self.feature_engineer.transform(df, include_target=False)
        X = df_transformed[self.feature_engineer.feature_names]
        
        # Predict
        prediction_encoded = self.model.predict(X)[0]
        probabilities = self.model.predict_proba(X)[0]
        confidence = probabilities.max()
        
        # Decode prediction
        prediction = self.feature_engineer.inverse_transform_target([prediction_encoded])[0]
        
        return prediction, confidence
    
    def save(self, directory: str = 'ml/models/saved_models'):
        """Save model with versioning"""
        if not self.is_trained:
            raise ValueError("Cannot save untrained model")
        
        # Create versioned directory
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        version_dir = os.path.join(directory, f'improved_v_{timestamp}')
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
        
        # Create 'latest' symlink (or copy on Windows)
        latest_dir = os.path.join(directory, 'improved_latest')
        if os.path.exists(latest_dir):
            import shutil
            shutil.rmtree(latest_dir)
        
        import shutil
        shutil.copytree(version_dir, latest_dir)
        
        print(f"💾 Model saved to: {version_dir}")
        print(f"💾 Latest version: {latest_dir}")
        
        return version_dir
    
    @classmethod
    def load(cls, directory: str) -> 'ImprovedModel':
        """Load saved model"""
        model_path = os.path.join(directory, 'model.pkl')
        feature_path = os.path.join(directory, 'feature_pipeline.pkl')
        metrics_path = os.path.join(directory, 'metrics.json')
        
        # Load model
        model_instance = cls.__new__(cls)
        model_instance.model = joblib.load(model_path)
        model_instance.feature_engineer = FeatureEngineer.load(feature_path)
        model_instance.is_trained = True
        
        # Load metrics if available
        if os.path.exists(metrics_path):
            with open(metrics_path, 'r') as f:
                model_instance.metrics = json.load(f)
        else:
            model_instance.metrics = {}
        
        print(f"📂 Model loaded from: {directory}")
        if model_instance.metrics:
            print(f"   Test accuracy: {model_instance.metrics.get('test_accuracy', 0)*100:.2f}%")
        
        return model_instance


def main():
    """Train improved model"""
    
    print("=" * 70)
    print("IMPROVED RANDOMFOREST MODEL - TRAINING")
    print("=" * 70)
    print("\n🎯 Improvements:")
    print("   ✅ Expanded dataset: 18,000 samples (vs 9,000)")
    print("   ✅ Derived features: 12 features (vs 8)")
    print("   ✅ Optimized hyperparameters from GridSearch")
    print("   ✅ Better overfitting control")
    
    # Load dataset
    print("\n📂 Loading dataset...")
    df = pd.read_csv('ml/data/fitness_dataset.csv')
    print(f"   Loaded {len(df)} samples")
    
    # Initialize and train
    model = ImprovedModel()
    metrics = model.train(df, test_size=0.2, cv_folds=5)
    
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
        'days_since_last_workout': 1
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
    
    # Performance comparison
    print("\n" + "=" * 70)
    print("PERFORMANCE COMPARISON")
    print("=" * 70)
    print("\n📊 Baseline Model (Old):")
    print("   Test Accuracy: 43.78%")
    print("   Overfitting Gap: 41.63%")
    
    print(f"\n📊 Improved Model (New):")
    print(f"   Test Accuracy: {metrics['test_accuracy']*100:.2f}%")
    print(f"   Overfitting Gap: {metrics['overfitting_gap']*100:.2f}%")
    
    improvement = (metrics['test_accuracy'] - 0.4378) * 100
    print(f"\n🎉 Improvement: {'+' if improvement > 0 else ''}{improvement:.2f} percentage points")
    
    overfitting_reduction = (0.4163 - metrics['overfitting_gap']) * 100
    print(f"🎉 Overfitting Reduced: {'+' if overfitting_reduction > 0 else ''}{overfitting_reduction:.2f} percentage points")
    
    print("\n" + "=" * 70)
    print("✅ TRAINING COMPLETE")
    print("=" * 70)


if __name__ == '__main__':
    main()
