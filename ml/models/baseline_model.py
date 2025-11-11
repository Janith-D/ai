"""
Baseline ML Model - RandomForest Classifier

Trains a RandomForest model to predict next workout based on:
- User profile (goal, fatigue, sleep)
- Workout history (last muscle group, intensity, recovery)
- Performance metrics (calories, result score)
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
from datetime import datetime
from typing import Dict, Tuple

import sys
sys.path.append('.')
from ml.features.feature_engineering import FeatureEngineer


class BaselineModel:
    """
    RandomForest classifier for workout prediction
    
    Features:
    - Handles class imbalance with class_weight='balanced'
    - Cross-validation for robust evaluation
    - Feature importance analysis
    - Model persistence with versioning
    """
    
    def __init__(
        self,
        n_estimators: int = 100,
        max_depth: int = 15,
        min_samples_split: int = 5,
        class_weight: str = 'balanced',
        random_state: int = 42
    ):
        """
        Initialize RandomForest model
        
        Args:
            n_estimators: Number of trees
            max_depth: Maximum tree depth
            min_samples_split: Minimum samples to split node
            class_weight: Handle imbalanced classes
            random_state: Seed for reproducibility
        """
        self.model = RandomForestClassifier(
            n_estimators=n_estimators,
            max_depth=max_depth,
            min_samples_split=min_samples_split,
            class_weight=class_weight,
            random_state=random_state,
            n_jobs=-1  # Use all CPU cores
        )
        
        self.feature_engineer = None
        self.trained = False
        self.training_metrics = {}
        self.feature_names = []
        
    def train(
        self,
        df: pd.DataFrame,
        test_size: float = 0.2,
        cv_folds: int = 5
    ) -> Dict:
        """
        Train model on dataset with train/test split and cross-validation
        
        Args:
            df: Training dataframe
            test_size: Fraction for test set
            cv_folds: Number of cross-validation folds
        
        Returns:
            Dictionary of training metrics
        """
        print("=" * 70)
        print("TRAINING RANDOMFOREST MODEL")
        print("=" * 70)
        
        # Initialize feature engineer
        print("\n📊 Step 1: Feature Engineering")
        self.feature_engineer = FeatureEngineer()
        df_transformed = self.feature_engineer.fit_transform(df)
        
        # Prepare features
        X, y = self.feature_engineer.prepare_features(df)
        self.feature_names = self.feature_engineer.feature_names
        
        print(f"\n   Dataset: {len(X)} samples, {X.shape[1]} features")
        print(f"   Target classes: {self.feature_engineer.get_target_classes()}")
        
        # Train/test split (stratified to maintain class balance)
        print(f"\n📊 Step 2: Train/Test Split ({int((1-test_size)*100)}/{int(test_size*100)})")
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=test_size, random_state=42, stratify=y
        )
        
        print(f"   Training set: {len(X_train)} samples")
        print(f"   Test set: {len(X_test)} samples")
        
        # Train model
        print(f"\n🤖 Step 3: Training RandomForest")
        print(f"   Parameters:")
        print(f"   • n_estimators: {self.model.n_estimators}")
        print(f"   • max_depth: {self.model.max_depth}")
        print(f"   • class_weight: {self.model.class_weight}")
        
        self.model.fit(X_train, y_train)
        self.trained = True
        print(f"   ✅ Model trained!")
        
        # Evaluate on training set
        print(f"\n📈 Step 4: Evaluation")
        y_train_pred = self.model.predict(X_train)
        y_test_pred = self.model.predict(X_test)
        
        train_accuracy = accuracy_score(y_train, y_train_pred)
        test_accuracy = accuracy_score(y_test, y_test_pred)
        
        print(f"\n   Training Accuracy: {train_accuracy:.2%}")
        print(f"   Test Accuracy:     {test_accuracy:.2%}")
        
        # Check for overfitting
        if train_accuracy - test_accuracy > 0.10:
            print(f"   ⚠️  Warning: Possible overfitting (gap: {(train_accuracy-test_accuracy):.2%})")
        else:
            print(f"   ✅ Good generalization (gap: {(train_accuracy-test_accuracy):.2%})")
        
        # Cross-validation
        print(f"\n🔄 Step 5: {cv_folds}-Fold Cross-Validation")
        cv_scores = cross_val_score(
            self.model, X_train, y_train, cv=cv_folds, scoring='accuracy'
        )
        
        print(f"   CV Scores: {[f'{s:.2%}' for s in cv_scores]}")
        print(f"   Mean CV Accuracy: {cv_scores.mean():.2%} (+/- {cv_scores.std()*2:.2%})")
        
        # Detailed metrics
        print(f"\n📊 Step 6: Detailed Test Set Metrics")
        precision = precision_score(y_test, y_test_pred, average='weighted', zero_division=0)
        recall = recall_score(y_test, y_test_pred, average='weighted')
        f1 = f1_score(y_test, y_test_pred, average='weighted')
        
        print(f"   Precision: {precision:.2%}")
        print(f"   Recall:    {recall:.2%}")
        print(f"   F1-Score:  {f1:.2%}")
        
        # Per-class performance
        print(f"\n📋 Per-Class Performance:")
        target_names = self.feature_engineer.get_target_classes()
        report = classification_report(
            y_test, y_test_pred,
            target_names=target_names,
            zero_division=0
        )
        print(report)
        
        # Feature importance
        print(f"\n🔍 Feature Importance:")
        importances = self.model.feature_importances_
        indices = np.argsort(importances)[::-1]
        
        for i in range(min(8, len(indices))):
            idx = indices[i]
            print(f"   {i+1}. {self.feature_names[idx]:30s} {importances[idx]:.3f}")
        
        # Store metrics
        self.training_metrics = {
            'train_accuracy': train_accuracy,
            'test_accuracy': test_accuracy,
            'cv_mean': cv_scores.mean(),
            'cv_std': cv_scores.std(),
            'precision': precision,
            'recall': recall,
            'f1': f1,
            'n_train': len(X_train),
            'n_test': len(X_test),
            'feature_importance': dict(zip(self.feature_names, importances))
        }
        
        print("\n" + "=" * 70)
        print("✅ TRAINING COMPLETE")
        print("=" * 70)
        
        return self.training_metrics
    
    def predict(self, X: pd.DataFrame) -> np.ndarray:
        """Make predictions (returns encoded labels)"""
        if not self.trained:
            raise ValueError("Model not trained. Call train() first.")
        
        return self.model.predict(X)
    
    def predict_proba(self, X: pd.DataFrame) -> np.ndarray:
        """Predict class probabilities"""
        if not self.trained:
            raise ValueError("Model not trained. Call train() first.")
        
        return self.model.predict_proba(X)
    
    def predict_workout(self, user_features: Dict) -> Tuple[str, float]:
        """
        Predict next workout from user features
        
        Args:
            user_features: Dictionary with keys matching feature columns
        
        Returns:
            (predicted_workout, confidence)
        """
        if not self.trained:
            raise ValueError("Model not trained. Call train() first.")
        
        # Create DataFrame with single row
        features_df = pd.DataFrame([user_features])
        
        # Transform features
        features_df_transformed = self.feature_engineer.transform(features_df, include_target=False)
        X = features_df_transformed[self.feature_names]
        
        # Predict
        prediction_encoded = self.model.predict(X)[0]
        probabilities = self.model.predict_proba(X)[0]
        confidence = probabilities.max()
        
        # Decode prediction
        prediction = self.feature_engineer.inverse_transform_target([prediction_encoded])[0]
        
        return prediction, confidence
    
    def save(self, directory: str = 'ml/models/saved_models'):
        """Save trained model with version timestamp"""
        if not self.trained:
            raise ValueError("Cannot save untrained model")
        
        os.makedirs(directory, exist_ok=True)
        
        # Create version timestamp
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        version_dir = os.path.join(directory, f'v_{timestamp}')
        os.makedirs(version_dir, exist_ok=True)
        
        # Save model
        model_path = os.path.join(version_dir, 'model.pkl')
        joblib.dump(self.model, model_path)
        
        # Save feature engineer
        pipeline_path = os.path.join(version_dir, 'feature_pipeline.pkl')
        self.feature_engineer.save(pipeline_path)
        
        # Save metadata
        metadata = {
            'timestamp': timestamp,
            'metrics': self.training_metrics,
            'feature_names': self.feature_names,
            'target_classes': self.feature_engineer.get_target_classes()
        }
        metadata_path = os.path.join(version_dir, 'metadata.pkl')
        joblib.dump(metadata, metadata_path)
        
        # Create "latest" symlink (just copy for Windows compatibility)
        latest_dir = os.path.join(directory, 'latest')
        if os.path.exists(latest_dir):
            import shutil
            shutil.rmtree(latest_dir)
        import shutil
        shutil.copytree(version_dir, latest_dir)
        
        print(f"\n💾 Model saved:")
        print(f"   Version: {version_dir}")
        print(f"   Latest: {latest_dir}")
        
        return version_dir
    
    @classmethod
    def load(cls, directory: str = 'ml/models/saved_models/latest') -> 'BaselineModel':
        """Load trained model from disk"""
        
        # Load model
        model_path = os.path.join(directory, 'model.pkl')
        sklearn_model = joblib.load(model_path)
        
        # Load feature engineer
        pipeline_path = os.path.join(directory, 'feature_pipeline.pkl')
        feature_engineer = FeatureEngineer.load(pipeline_path)
        
        # Load metadata
        metadata_path = os.path.join(directory, 'metadata.pkl')
        metadata = joblib.load(metadata_path)
        
        # Create instance
        instance = cls()
        instance.model = sklearn_model
        instance.feature_engineer = feature_engineer
        instance.trained = True
        instance.training_metrics = metadata['metrics']
        instance.feature_names = metadata['feature_names']
        
        print(f"📂 Model loaded from: {directory}")
        print(f"   Test accuracy: {metadata['metrics']['test_accuracy']:.2%}")
        
        return instance


def main():
    """Train and evaluate baseline model"""
    
    # Load dataset
    print("📂 Loading training data...")
    df = pd.read_csv('ml/data/fitness_dataset.csv')
    print(f"   Loaded {len(df)} samples\n")
    
    # Train model
    model = BaselineModel(
        n_estimators=100,
        max_depth=15,
        class_weight='balanced'
    )
    
    metrics = model.train(df, test_size=0.2, cv_folds=5)
    
    # Save model
    model.save()
    
    # Test loading
    print("\n" + "=" * 70)
    print("TESTING MODEL LOADING")
    print("=" * 70)
    
    loaded_model = BaselineModel.load()
    
    # Test prediction
    print("\n" + "=" * 70)
    print("TESTING SINGLE PREDICTION")
    print("=" * 70)
    
    test_user = {
        'muscle_group': 'chest',
        'intensity': 'medium',
        'fatigue_level': 'low',
        'duration_min': 50,
        'calories': 400,
        'sleep_hours': 8.0,
        'result_score': 0.85,
        'days_since_last_workout': 1
    }
    
    print(f"\n📋 Test User Profile:")
    for key, value in test_user.items():
        print(f"   {key}: {value}")
    
    prediction, confidence = loaded_model.predict_workout(test_user)
    
    print(f"\n🎯 Prediction: {prediction.upper()}")
    print(f"📊 Confidence: {confidence:.1%}")
    
    print("\n" + "=" * 70)
    print("✅ MODEL TRAINING AND TESTING COMPLETE")
    print("=" * 70)


if __name__ == '__main__':
    main()
