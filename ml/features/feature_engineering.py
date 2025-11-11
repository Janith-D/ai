"""
Feature Engineering Pipeline

Transforms raw user data and workout history into ML-ready features.
Includes encoding, normalization, and derived feature creation.
"""

import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelEncoder, StandardScaler
from typing import Dict, List, Tuple
import joblib
import os


class FeatureEngineer:
    """
    Feature engineering for fitness workout predictions
    
    Transforms:
    - Categorical features → Label encoding
    - Numeric features → Standard scaling
    - Derived features → Workout frequency, rest deficit, etc.
    """
    
    def __init__(self, add_derived_features: bool = True):
        self.label_encoders = {}
        self.scaler = StandardScaler()
        self.feature_names = []
        self.fitted = False
        self.add_derived_features = add_derived_features
        
        # Define categorical columns to encode
        self.categorical_cols = [
            'muscle_group', 
            'intensity', 
            'fatigue_level'
        ]
        
        # Define numeric columns to scale (expanded with derived features)
        self.numeric_cols = [
            'duration_min',
            'calories',
            'sleep_hours',
            'result_score',
            'days_since_last_workout'
        ]
        
        # Derived features will be added if enabled
        if add_derived_features:
            self.numeric_cols.extend([
                'workout_frequency_7d',
                'rest_deficit',
                'recovery_score',
                'intensity_trend'
            ])
    
    def create_derived_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Create derived features that capture temporal patterns
        
        Features:
        - workout_frequency_7d: Number of workouts in last 7 days
        - rest_deficit: How much rest is needed (fatigue + sleep debt)
        - recovery_score: Overall recovery state (0-1)
        - intensity_trend: Average intensity of last 3 workouts
        
        Args:
            df: Dataframe with raw features
        
        Returns:
            Dataframe with added derived features
        """
        if not self.add_derived_features:
            return df
        
        df = df.copy()
        
        # 1. Workout frequency in last 7 days (simplified)
        # In real app, would calculate from date-based history
        # Here we estimate based on days_since_last_workout
        df['workout_frequency_7d'] = df['days_since_last_workout'].apply(
            lambda x: max(0, 7 - x) if x < 7 else 1
        )
        
        # 2. Rest deficit (higher = more rest needed)
        # Combines fatigue level and sleep debt
        fatigue_map = {'low': 0, 'medium': 1, 'high': 2}
        df['fatigue_numeric'] = df['fatigue_level'].map(fatigue_map)
        df['sleep_debt'] = (8.0 - df['sleep_hours']).clip(lower=0)
        df['rest_deficit'] = df['fatigue_numeric'] + df['sleep_debt']
        
        # 3. Recovery score (0-1, higher = better recovered)
        # Inverse of rest deficit, normalized
        df['recovery_score'] = 1.0 - (df['rest_deficit'] / df['rest_deficit'].max())
        df['recovery_score'] = df['recovery_score'].fillna(0.75)  # Default moderate
        
        # 4. Intensity trend (average of last 3 workouts)
        # Simplified: use current intensity as proxy
        intensity_map = {'none': 0, 'light': 1, 'medium': 2, 'heavy': 3}
        df['intensity_numeric'] = df['intensity'].map(intensity_map)
        df['intensity_trend'] = df['intensity_numeric'] * 0.7 + df['result_score'] * 0.3
        
        # Clean up temporary columns
        df = df.drop(['fatigue_numeric', 'sleep_debt', 'intensity_numeric'], axis=1)
        
        return df
    
    def fit(self, df: pd.DataFrame, target_col: str = 'next_workout') -> 'FeatureEngineer':
        """
        Fit encoders and scalers on training data
        
        Args:
            df: Training dataframe with all features
            target_col: Name of target column (will also be encoded)
        
        Returns:
            self (for method chaining)
        """
        print("🔧 Fitting feature engineering pipeline...")
        
        # Add derived features if enabled
        if self.add_derived_features:
            df = self.create_derived_features(df)
            print(f"   • Created 4 derived features")
        
        # Fit label encoders for categorical columns
        for col in self.categorical_cols:
            if col in df.columns:
                self.label_encoders[col] = LabelEncoder()
                self.label_encoders[col].fit(df[col])
                print(f"   • Encoded {col}: {len(self.label_encoders[col].classes_)} classes")
        
        # Fit label encoder for target
        if target_col in df.columns:
            self.label_encoders[target_col] = LabelEncoder()
            self.label_encoders[target_col].fit(df[target_col])
            print(f"   • Encoded {target_col}: {len(self.label_encoders[target_col].classes_)} classes")
        
        # Fit scaler on numeric columns
        numeric_data = df[self.numeric_cols]
        self.scaler.fit(numeric_data)
        print(f"   • Scaled {len(self.numeric_cols)} numeric features")
        
        self.fitted = True
        print("✅ Feature engineering pipeline fitted")
        
        return self
    
    def transform(self, df: pd.DataFrame, include_target: bool = True) -> pd.DataFrame:
        """
        Transform features using fitted encoders/scalers
        
        Args:
            df: Dataframe to transform
            include_target: Whether to encode target column
        
        Returns:
            Transformed dataframe with encoded/scaled features
        """
        if not self.fitted:
            raise ValueError("Pipeline not fitted. Call fit() first.")
        
        df_transformed = df.copy()
        
        # Add derived features if enabled
        if self.add_derived_features:
            df_transformed = self.create_derived_features(df_transformed)
        
        # Encode categorical columns
        for col in self.categorical_cols:
            if col in df_transformed.columns:
                df_transformed[f'{col}_encoded'] = self.label_encoders[col].transform(df_transformed[col])
        
        # Encode target if requested
        if include_target and 'next_workout' in df_transformed.columns:
            df_transformed['next_workout_encoded'] = self.label_encoders['next_workout'].transform(
                df_transformed['next_workout']
            )
        
        # Scale numeric columns
        df_transformed[self.numeric_cols] = self.scaler.transform(df_transformed[self.numeric_cols])
        
        return df_transformed
    
    def fit_transform(self, df: pd.DataFrame, target_col: str = 'next_workout') -> pd.DataFrame:
        """Fit and transform in one step"""
        self.fit(df, target_col)
        return self.transform(df)
    
    def prepare_features(self, df: pd.DataFrame) -> Tuple[pd.DataFrame, pd.Series]:
        """
        Prepare features and target for model training
        
        Args:
            df: Raw dataframe
        
        Returns:
            (X, y) where X is feature matrix and y is target vector
        """
        df_transformed = self.transform(df)
        
        # Select feature columns (encoded + scaled + derived)
        feature_cols = [
            'muscle_group_encoded',
            'intensity_encoded',
            'fatigue_level_encoded',
            'duration_min',
            'calories',
            'sleep_hours',
            'result_score',
            'days_since_last_workout'
        ]
        
        # Add derived features if enabled
        if self.add_derived_features:
            feature_cols.extend([
                'workout_frequency_7d',
                'rest_deficit',
                'recovery_score',
                'intensity_trend'
            ])
        
        X = df_transformed[feature_cols]
        y = df_transformed['next_workout_encoded']
        
        self.feature_names = feature_cols
        
        return X, y
    
    def inverse_transform_target(self, encoded_target: np.ndarray) -> np.ndarray:
        """Convert encoded target back to original labels"""
        return self.label_encoders['next_workout'].inverse_transform(encoded_target)
    
    def get_target_classes(self) -> List[str]:
        """Get list of target classes"""
        return list(self.label_encoders['next_workout'].classes_)
    
    def save(self, filepath: str):
        """Save fitted pipeline to disk"""
        if not self.fitted:
            raise ValueError("Cannot save unfitted pipeline")
        
        joblib.dump({
            'label_encoders': self.label_encoders,
            'scaler': self.scaler,
            'feature_names': self.feature_names,
            'categorical_cols': self.categorical_cols,
            'numeric_cols': self.numeric_cols,
            'add_derived_features': self.add_derived_features,
            'fitted': self.fitted
        }, filepath)
        
        print(f"💾 Saved feature pipeline to: {filepath}")
    
    @classmethod
    def load(cls, filepath: str) -> 'FeatureEngineer':
        """Load fitted pipeline from disk"""
        data = joblib.load(filepath)
        
        # Check if add_derived_features exists (for backward compatibility)
        add_derived = data.get('add_derived_features', False)
        engineer = cls(add_derived_features=add_derived)
        
        engineer.label_encoders = data['label_encoders']
        engineer.scaler = data['scaler']
        engineer.feature_names = data['feature_names']
        engineer.categorical_cols = data['categorical_cols']
        engineer.numeric_cols = data['numeric_cols']
        engineer.fitted = data['fitted']
        
        print(f"📂 Loaded feature pipeline from: {filepath}")
        return engineer


def main():
    """Test feature engineering pipeline with derived features"""
    
    print("=" * 70)
    print("FEATURE ENGINEERING PIPELINE - TEST (WITH DERIVED FEATURES)")
    print("=" * 70)
    
    # Load expanded dataset
    print("\n📂 Loading dataset...")
    df = pd.read_csv('ml/data/fitness_dataset.csv')
    print(f"   Loaded {len(df)} samples")
    
    # Show original data
    print("\n📊 Original Data (first 3 rows):")
    print(df[['muscle_group', 'intensity', 'fatigue_level', 'sleep_hours', 'next_workout']].head(3))
    
    # Initialize and fit pipeline WITH DERIVED FEATURES
    print("\n" + "=" * 70)
    print("TESTING WITH DERIVED FEATURES ENABLED")
    print("=" * 70)
    engineer = FeatureEngineer(add_derived_features=True)
    df_transformed = engineer.fit_transform(df)
    
    # Show transformed data
    print("\n📊 Transformed Data (first 3 rows):")
    feature_cols = [
        'muscle_group_encoded', 'intensity_encoded', 'fatigue_level_encoded',
        'sleep_hours', 'next_workout_encoded'
    ]
    print(df_transformed[feature_cols].head(3))
    
    # Prepare features for training
    print("\n" + "=" * 70)
    print("PREPARING FEATURES FOR MODEL TRAINING")
    print("=" * 70)
    
    X, y = engineer.prepare_features(df)
    
    print(f"\n✅ Feature matrix shape: {X.shape}")
    print(f"✅ Target vector shape: {y.shape}")
    print(f"\n📋 Feature columns:")
    for i, col in enumerate(engineer.feature_names, 1):
        print(f"   {i}. {col}")
    
    print(f"\n📋 Target classes: {engineer.get_target_classes()}")
    
    # Test inverse transform
    print("\n" + "=" * 70)
    print("TESTING INVERSE TRANSFORM")
    print("=" * 70)
    
    sample_encoded = y.head(5).values
    sample_decoded = engineer.inverse_transform_target(sample_encoded)
    
    print("\nEncoded → Decoded:")
    for enc, dec in zip(sample_encoded, sample_decoded):
        print(f"   {enc} → {dec}")
    
    # Save pipeline
    print("\n" + "=" * 70)
    os.makedirs('ml/features', exist_ok=True)
    engineer.save('ml/features/feature_pipeline.pkl')
    
    # Test loading
    engineer_loaded = FeatureEngineer.load('ml/features/feature_pipeline.pkl')
    print(f"✅ Pipeline loaded successfully")
    print(f"   Feature names match: {engineer_loaded.feature_names == engineer.feature_names}")
    
    print("\n" + "=" * 70)
    print("✅ FEATURE ENGINEERING TEST COMPLETE")
    print("=" * 70)


if __name__ == '__main__':
    main()
