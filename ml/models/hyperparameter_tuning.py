"""
Hyperparameter Tuning for RandomForest Model

Uses GridSearchCV to find optimal parameters that reduce overfitting
and improve generalization performance.
"""

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV, cross_val_score, train_test_split
from sklearn.metrics import classification_report, accuracy_score
import time
from typing import Dict

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from ml.features.feature_engineering import FeatureEngineer


class HyperparameterTuner:
    """Optimize RandomForest hyperparameters to reduce overfitting"""
    
    def __init__(self, dataset_path: str = 'ml/data/fitness_dataset.csv'):
        self.dataset_path = dataset_path
        self.best_params = None
        self.best_score = None
        self.cv_results = None
        
    def load_and_prepare_data(self):
        """Load dataset and prepare features"""
        print("📂 Loading dataset...")
        df = pd.read_csv(self.dataset_path)
        print(f"   Loaded {len(df)} samples")
        
        # Use FeatureEngineer with derived features
        print("\n🔧 Engineering features...")
        engineer = FeatureEngineer(add_derived_features=True)
        
        # Fit and transform the data
        engineer.fit(df, target_col='next_workout')
        X, y = engineer.prepare_features(df)
        
        print(f"   Feature matrix: {X.shape}")
        print(f"   Target vector: {y.shape}")
        
        return X, y, engineer
    
    def define_param_grid(self) -> Dict:
        """
        Define hyperparameter search space
        
        Focus on parameters that control overfitting:
        - max_depth: Tree depth (lower = less overfitting)
        - min_samples_split: Min samples to split node
        - min_samples_leaf: Min samples at leaf node
        - n_estimators: Number of trees
        - max_features: Features per tree
        """
        param_grid = {
            'n_estimators': [100, 200, 300],
            'max_depth': [10, 15, 20, None],
            'min_samples_split': [5, 10, 20],
            'min_samples_leaf': [2, 4, 8],
            'max_features': ['sqrt', 'log2'],
            'class_weight': ['balanced']
        }
        
        print("\n🔍 Hyperparameter Search Space:")
        print(f"   n_estimators: {param_grid['n_estimators']}")
        print(f"   max_depth: {param_grid['max_depth']}")
        print(f"   min_samples_split: {param_grid['min_samples_split']}")
        print(f"   min_samples_leaf: {param_grid['min_samples_leaf']}")
        print(f"   max_features: {param_grid['max_features']}")
        
        total_combinations = (
            len(param_grid['n_estimators']) *
            len(param_grid['max_depth']) *
            len(param_grid['min_samples_split']) *
            len(param_grid['min_samples_leaf']) *
            len(param_grid['max_features'])
        )
        print(f"\n   Total combinations: {total_combinations}")
        
        return param_grid
    
    def run_grid_search(self, X, y, cv: int = 3):
        """
        Run GridSearchCV with cross-validation
        
        Args:
            X: Feature matrix
            y: Target vector
            cv: Number of cross-validation folds
        """
        print("\n" + "=" * 70)
        print("GRID SEARCH - HYPERPARAMETER TUNING")
        print("=" * 70)
        
        param_grid = self.define_param_grid()
        
        # Split data
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.2, random_state=42, stratify=y
        )
        
        print(f"\n📊 Data Split:")
        print(f"   Training: {len(X_train)} samples")
        print(f"   Testing: {len(X_test)} samples")
        
        # Initialize GridSearchCV
        rf = RandomForestClassifier(random_state=42, n_jobs=-1)
        
        print(f"\n⚙️  Starting Grid Search (CV={cv})...")
        print(f"   This may take 10-20 minutes...")
        
        start_time = time.time()
        
        grid_search = GridSearchCV(
            estimator=rf,
            param_grid=param_grid,
            cv=cv,
            scoring='accuracy',
            n_jobs=-1,
            verbose=2,
            return_train_score=True
        )
        
        grid_search.fit(X_train, y_train)
        
        elapsed = time.time() - start_time
        print(f"\n✅ Grid Search Complete!")
        print(f"   Time elapsed: {elapsed/60:.1f} minutes")
        
        # Store results
        self.best_params = grid_search.best_params_
        self.best_score = grid_search.best_score_
        self.cv_results = grid_search.cv_results_
        
        # Evaluate on test set
        y_pred = grid_search.best_estimator_.predict(X_test)
        test_accuracy = accuracy_score(y_test, y_pred)
        
        # Calculate train accuracy
        y_train_pred = grid_search.best_estimator_.predict(X_train)
        train_accuracy = accuracy_score(y_train, y_train_pred)
        
        # Show results
        print("\n" + "=" * 70)
        print("BEST HYPERPARAMETERS")
        print("=" * 70)
        for param, value in self.best_params.items():
            print(f"   {param}: {value}")
        
        print("\n" + "=" * 70)
        print("PERFORMANCE METRICS")
        print("=" * 70)
        print(f"   Best CV Accuracy: {self.best_score:.4f} ({self.best_score*100:.2f}%)")
        print(f"   Train Accuracy: {train_accuracy:.4f} ({train_accuracy*100:.2f}%)")
        print(f"   Test Accuracy: {test_accuracy:.4f} ({test_accuracy*100:.2f}%)")
        
        overfitting_gap = train_accuracy - test_accuracy
        print(f"\n   Overfitting Gap: {overfitting_gap:.4f} ({overfitting_gap*100:.2f}%)")
        
        if overfitting_gap > 0.10:
            print(f"   ⚠️  Warning: Still overfitting (gap > 10%)")
        else:
            print(f"   ✅ Overfitting reduced to acceptable level")
        
        # Detailed classification report
        print("\n" + "=" * 70)
        print("CLASSIFICATION REPORT (TEST SET)")
        print("=" * 70)
        print(classification_report(y_test, y_pred))
        
        return grid_search.best_estimator_, train_accuracy, test_accuracy
    
    def show_top_configs(self, top_n: int = 5):
        """Show top N performing configurations"""
        if self.cv_results is None:
            print("No results available. Run grid search first.")
            return
        
        results_df = pd.DataFrame(self.cv_results)
        results_df = results_df.sort_values('rank_test_score')
        
        print("\n" + "=" * 70)
        print(f"TOP {top_n} CONFIGURATIONS")
        print("=" * 70)
        
        for i in range(min(top_n, len(results_df))):
            row = results_df.iloc[i]
            print(f"\n🏆 Rank #{i+1}")
            print(f"   CV Score: {row['mean_test_score']:.4f}")
            print(f"   Train Score: {row['mean_train_score']:.4f}")
            print(f"   Parameters:")
            print(f"      n_estimators: {row['param_n_estimators']}")
            print(f"      max_depth: {row['param_max_depth']}")
            print(f"      min_samples_split: {row['param_min_samples_split']}")
            print(f"      min_samples_leaf: {row['param_min_samples_leaf']}")
            print(f"      max_features: {row['param_max_features']}")


def main():
    """Run hyperparameter tuning"""
    
    print("=" * 70)
    print("RANDOMFOREST HYPERPARAMETER TUNING")
    print("=" * 70)
    print("\nGoal: Reduce overfitting (85% train → 44% test) and improve accuracy")
    print("\nStrategy:")
    print("   1. Use expanded dataset (18,000 samples)")
    print("   2. Use derived features (12 total features)")
    print("   3. Optimize max_depth, min_samples_split, min_samples_leaf")
    print("   4. Test different n_estimators and max_features")
    
    tuner = HyperparameterTuner()
    
    # Load data
    X, y, engineer = tuner.load_and_prepare_data()
    
    # Run grid search
    best_model, train_acc, test_acc = tuner.run_grid_search(X, y, cv=3)
    
    # Show top configurations
    tuner.show_top_configs(top_n=3)
    
    # Save best parameters
    print("\n" + "=" * 70)
    print("💾 SAVING BEST CONFIGURATION")
    print("=" * 70)
    
    import json
    os.makedirs('ml/models/configs', exist_ok=True)
    config_path = 'ml/models/configs/best_params.json'
    
    with open(config_path, 'w') as f:
        json.dump(tuner.best_params, f, indent=2)
    
    print(f"   Saved to: {config_path}")
    
    print("\n" + "=" * 70)
    print("✅ HYPERPARAMETER TUNING COMPLETE")
    print("=" * 70)
    print(f"\n📈 Performance Improvement:")
    print(f"   Before: 43.78% test accuracy (41.63% overfitting)")
    print(f"   After: {test_acc*100:.2f}% test accuracy ({(train_acc-test_acc)*100:.2f}% overfitting)")
    
    improvement = (test_acc - 0.4378) * 100
    if improvement > 0:
        print(f"   🎉 Improvement: +{improvement:.2f} percentage points!")
    
    print(f"\n📝 Next Step: Use these parameters to retrain final model")


if __name__ == '__main__':
    main()
