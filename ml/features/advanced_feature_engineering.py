"""
Advanced Feature Engineering for 75%+ Accuracy

Implements sophisticated features including:
- Temporal patterns (workout sequences)
- User behavior profiles
- Recovery dynamics
- Training load management
- Muscle group rotation patterns
"""

import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelEncoder, StandardScaler
from typing import Dict, List, Tuple
import joblib


class AdvancedFeatureEngineer:
    """
    Advanced feature engineering with temporal and behavioral features
    Target: 75%+ accuracy
    """
    
    def __init__(self):
        self.label_encoders = {}
        self.scaler = StandardScaler()
        self.feature_names = []
        self.fitted = False
        
        self.categorical_cols = [
            'muscle_group',
            'intensity',
            'fatigue_level',
            'goal'  # Added user goal
        ]
        
        self.numeric_cols = []  # Will be populated dynamically
    
    def create_temporal_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Create time-series features capturing workout sequences
        
        Features:
        - last_3_muscle_groups: Pattern of recent workouts
        - consecutive_days: Training streak
        - rest_days_last_week: Recovery patterns
        - muscle_group_frequency: How often each muscle is trained
        - intensity_progression: Is intensity increasing/decreasing
        """
        df = df.copy()
        df = df.sort_values(['user_id', 'date']).reset_index(drop=True)
        
        # Initialize new features
        df['last_muscle_1'] = 'rest'
        df['last_muscle_2'] = 'rest'
        df['last_muscle_3'] = 'rest'
        df['consecutive_workout_days'] = 0
        df['rest_days_last_7'] = 0
        df['intensity_change'] = 0.0
        
        # Process by user
        for user_id in df['user_id'].unique():
            user_mask = df['user_id'] == user_id
            user_indices = df[user_mask].index
            
            for i, idx in enumerate(user_indices):
                if i > 0:
                    # Last muscle groups
                    df.at[idx, 'last_muscle_1'] = df.at[user_indices[i-1], 'muscle_group']
                    if i > 1:
                        df.at[idx, 'last_muscle_2'] = df.at[user_indices[i-2], 'muscle_group']
                    if i > 2:
                        df.at[idx, 'last_muscle_3'] = df.at[user_indices[i-3], 'muscle_group']
                    
                    # Consecutive workout days
                    if df.at[user_indices[i-1], 'muscle_group'] != 'rest':
                        df.at[idx, 'consecutive_workout_days'] = df.at[user_indices[i-1], 'consecutive_workout_days'] + 1
                    
                    # Rest days in last 7 workouts
                    if i >= 7:
                        last_7 = df.loc[user_indices[i-7:i], 'muscle_group']
                        df.at[idx, 'rest_days_last_7'] = (last_7 == 'rest').sum()
                    
                    # Intensity change
                    intensity_map = {'none': 0, 'light': 1, 'medium': 2, 'heavy': 3}
                    curr_int = intensity_map.get(df.at[idx, 'intensity'], 1)
                    prev_int = intensity_map.get(df.at[user_indices[i-1], 'intensity'], 1)
                    df.at[idx, 'intensity_change'] = curr_int - prev_int
        
        return df
    
    def create_muscle_rotation_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Create features about muscle group rotation patterns
        
        Features:
        - days_since_chest/back/legs/shoulders
        - muscle_group_balance: How balanced is the training
        - same_muscle_consecutive: Training same muscle back-to-back
        """
        df = df.copy()
        
        # Initialize
        muscle_groups = ['chest', 'back', 'legs', 'shoulders', 'cardio']
        for mg in muscle_groups:
            df[f'days_since_{mg}'] = 99  # Large default value
        
        df['same_muscle_consecutive'] = 0
        df['training_balance_score'] = 0.0
        
        # Process by user
        for user_id in df['user_id'].unique():
            user_mask = df['user_id'] == user_id
            user_indices = df[user_mask].index
            
            # Track last occurrence of each muscle group
            last_occurrence = {mg: -99 for mg in muscle_groups}
            
            for i, idx in enumerate(user_indices):
                curr_muscle = df.at[idx, 'muscle_group']
                
                # Update days since each muscle group
                for mg in muscle_groups:
                    if mg == curr_muscle:
                        last_occurrence[mg] = i
                        df.at[idx, f'days_since_{mg}'] = 0
                    else:
                        df.at[idx, f'days_since_{mg}'] = i - last_occurrence[mg]
                
                # Same muscle consecutive
                if i > 0:
                    prev_muscle = df.at[user_indices[i-1], 'muscle_group']
                    if curr_muscle == prev_muscle and curr_muscle != 'rest':
                        df.at[idx, 'same_muscle_consecutive'] = 1
                
                # Training balance (last 14 days)
                if i >= 14:
                    last_14 = df.loc[user_indices[i-14:i], 'muscle_group']
                    muscle_counts = last_14.value_counts()
                    # Balance = std deviation (lower = more balanced)
                    balance = 1.0 / (1.0 + muscle_counts.std())
                    df.at[idx, 'training_balance_score'] = balance
        
        return df
    
    def create_recovery_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Advanced recovery and fatigue features
        
        Features:
        - cumulative_fatigue: Accumulated fatigue over time
        - sleep_debt: Running sleep deficit
        - recovery_quality: Sleep × (1 - fatigue)
        - training_load: Intensity × duration weighted sum
        """
        df = df.copy()
        
        df['cumulative_fatigue'] = 0.0
        df['sleep_debt'] = 0.0
        df['recovery_quality'] = 0.0
        df['training_load_7d'] = 0.0
        
        fatigue_map = {'low': 0.3, 'medium': 0.6, 'high': 1.0}
        intensity_map = {'none': 0, 'light': 1, 'medium': 2, 'heavy': 3}
        
        for user_id in df['user_id'].unique():
            user_mask = df['user_id'] == user_id
            user_indices = df[user_mask].index
            
            cumulative_fatigue = 0.0
            cumulative_sleep_debt = 0.0
            
            for i, idx in enumerate(user_indices):
                # Cumulative fatigue (increases with workouts, decreases with rest)
                curr_fatigue = fatigue_map.get(df.at[idx, 'fatigue_level'], 0.6)
                if df.at[idx, 'muscle_group'] == 'rest':
                    cumulative_fatigue = max(0, cumulative_fatigue - 0.2)
                else:
                    cumulative_fatigue = min(1.0, cumulative_fatigue + curr_fatigue * 0.3)
                
                df.at[idx, 'cumulative_fatigue'] = cumulative_fatigue
                
                # Sleep debt
                sleep_deficit = max(0, 8.0 - df.at[idx, 'sleep_hours'])
                cumulative_sleep_debt = cumulative_sleep_debt * 0.8 + sleep_deficit
                df.at[idx, 'sleep_debt'] = cumulative_sleep_debt
                
                # Recovery quality
                recovery_quality = df.at[idx, 'sleep_hours'] * (1.0 - curr_fatigue)
                df.at[idx, 'recovery_quality'] = recovery_quality
                
                # Training load last 7 days
                if i >= 7:
                    last_7_intensity = [intensity_map.get(df.at[user_indices[j], 'intensity'], 0) 
                                       for j in range(i-7, i)]
                    last_7_duration = [df.at[user_indices[j], 'duration_min'] 
                                      for j in range(i-7, i)]
                    training_load = sum(int_val * dur for int_val, dur in zip(last_7_intensity, last_7_duration))
                    df.at[idx, 'training_load_7d'] = training_load
        
        return df
    
    def create_user_profile_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Features based on user characteristics and goals
        
        Features:
        - goal_alignment: Does workout match goal?
        - experience_factor: Multiplier based on experience
        - bmi: Body Mass Index
        - goal_specific patterns
        """
        df = df.copy()
        
        # Add user_id if not present (for testing)
        if 'user_id' not in df.columns:
            df['user_id'] = 'u0001'
        
        # Goal alignment (will be populated with user goal data)
        df['goal_muscle_gain'] = 0
        df['goal_fat_loss'] = 0
        df['goal_endurance'] = 0
        df['goal_strength'] = 0
        df['goal_general'] = 0
        
        # Set goal flags based on goal column
        if 'goal' in df.columns:
            df['goal_muscle_gain'] = (df['goal'] == 'muscle_gain').astype(int)
            df['goal_fat_loss'] = (df['goal'] == 'fat_loss').astype(int)
            df['goal_endurance'] = (df['goal'] == 'endurance').astype(int)
            df['goal_strength'] = (df['goal'] == 'strength').astype(int)
            df['goal_general'] = (df['goal'] == 'general_fitness').astype(int)
        
        return df
    
    def create_interaction_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Interaction features between different aspects
        
        Features:
        - fatigue_sleep_interaction
        - intensity_duration_interaction
        - recovery_training_load
        """
        df = df.copy()
        
        fatigue_map = {'low': 0, 'medium': 1, 'high': 2}
        intensity_map = {'none': 0, 'light': 1, 'medium': 2, 'heavy': 3}
        
        df['fatigue_numeric'] = df['fatigue_level'].map(fatigue_map).fillna(1)
        df['intensity_numeric'] = df['intensity'].map(intensity_map).fillna(1)
        
        # Interactions
        df['fatigue_sleep_interaction'] = df['fatigue_numeric'] * (8.0 - df['sleep_hours']).clip(lower=0)
        df['intensity_duration_interaction'] = df['intensity_numeric'] * df['duration_min'] / 100
        df['recovery_calories_interaction'] = df['sleep_hours'] * df['calories'] / 100
        
        return df
    
    def fit(self, df: pd.DataFrame, target_col: str = 'next_workout') -> 'AdvancedFeatureEngineer':
        """Fit the feature engineering pipeline"""
        print("🔧 Advanced Feature Engineering Pipeline")
        print("   Creating sophisticated features for 75%+ accuracy...")
        
        # Create all features
        df = self.create_temporal_features(df)
        print("   ✅ Temporal features (workout sequences)")
        
        df = self.create_muscle_rotation_features(df)
        print("   ✅ Muscle rotation patterns")
        
        df = self.create_recovery_features(df)
        print("   ✅ Advanced recovery dynamics")
        
        df = self.create_user_profile_features(df)
        print("   ✅ User profile features")
        
        df = self.create_interaction_features(df)
        print("   ✅ Interaction features")
        
        # Collect all numeric columns
        self.numeric_cols = [
            'duration_min', 'calories', 'sleep_hours', 'result_score',
            'days_since_last_workout', 'consecutive_workout_days',
            'rest_days_last_7', 'intensity_change',
            'days_since_chest', 'days_since_back', 'days_since_legs',
            'days_since_shoulders', 'days_since_cardio',
            'same_muscle_consecutive', 'training_balance_score',
            'cumulative_fatigue', 'sleep_debt', 'recovery_quality',
            'training_load_7d',
            'goal_muscle_gain', 'goal_fat_loss', 'goal_endurance',
            'goal_strength', 'goal_general',
            'fatigue_sleep_interaction', 'intensity_duration_interaction',
            'recovery_calories_interaction'
        ]
        
        # Fit encoders
        for col in self.categorical_cols:
            if col in df.columns:
                self.label_encoders[col] = LabelEncoder()
                self.label_encoders[col].fit(df[col])
        
        # Encode last_muscle features
        for i in range(1, 4):
            col = f'last_muscle_{i}'
            if col in df.columns:
                self.label_encoders[col] = LabelEncoder()
                self.label_encoders[col].fit(df[col])
                self.categorical_cols.append(col)
        
        # Fit target
        if target_col in df.columns:
            self.label_encoders[target_col] = LabelEncoder()
            self.label_encoders[target_col].fit(df[target_col])
        
        # Fit scaler
        numeric_data = df[self.numeric_cols]
        self.scaler.fit(numeric_data)
        
        self.fitted = True
        
        total_features = len(self.categorical_cols) + len(self.numeric_cols)
        print(f"\n   📊 Total features created: {total_features}")
        print(f"      Categorical: {len(self.categorical_cols)}")
        print(f"      Numeric: {len(self.numeric_cols)}")
        
        return self
    
    def transform(self, df: pd.DataFrame, include_target: bool = True) -> pd.DataFrame:
        """Transform data using fitted pipeline"""
        if not self.fitted:
            raise ValueError("Pipeline not fitted. Call fit() first.")
        
        # Create features
        df = self.create_temporal_features(df)
        df = self.create_muscle_rotation_features(df)
        df = self.create_recovery_features(df)
        df = self.create_user_profile_features(df)
        df = self.create_interaction_features(df)
        
        # Encode categoricals
        for col in self.categorical_cols:
            if col in df.columns:
                df[f'{col}_encoded'] = self.label_encoders[col].transform(df[col])
        
        # Encode target
        if include_target and 'next_workout' in df.columns:
            df['next_workout_encoded'] = self.label_encoders['next_workout'].transform(df['next_workout'])
        
        # Scale numerics
        df[self.numeric_cols] = self.scaler.transform(df[self.numeric_cols])
        
        return df
    
    def fit_transform(self, df: pd.DataFrame, target_col: str = 'next_workout') -> pd.DataFrame:
        """Fit and transform"""
        self.fit(df, target_col)
        return self.transform(df)
    
    def prepare_features(self, df: pd.DataFrame) -> Tuple[pd.DataFrame, pd.Series]:
        """Prepare X and y for training"""
        # Fit and transform if not fitted
        if not self.fitted:
            df_transformed = self.fit_transform(df)
        else:
            df_transformed = self.transform(df)
        
        # Feature columns
        feature_cols = [f'{col}_encoded' for col in self.categorical_cols] + self.numeric_cols
        
        X = df_transformed[feature_cols]
        y = df_transformed['next_workout_encoded']
        
        self.feature_names = feature_cols
        
        return X, y
    
    def get_target_classes(self) -> List[str]:
        """Get target classes"""
        return list(self.label_encoders['next_workout'].classes_)
    
    def inverse_transform_target(self, encoded_target: np.ndarray) -> np.ndarray:
        """Decode target"""
        return self.label_encoders['next_workout'].inverse_transform(encoded_target)
    
    def save(self, filepath: str):
        """Save pipeline"""
        if not self.fitted:
            raise ValueError("Cannot save unfitted pipeline")
        
        joblib.dump({
            'label_encoders': self.label_encoders,
            'scaler': self.scaler,
            'feature_names': self.feature_names,
            'categorical_cols': self.categorical_cols,
            'numeric_cols': self.numeric_cols,
            'fitted': self.fitted
        }, filepath)
        
        print(f"💾 Saved advanced feature pipeline to: {filepath}")
    
    @classmethod
    def load(cls, filepath: str) -> 'AdvancedFeatureEngineer':
        """Load pipeline"""
        data = joblib.load(filepath)
        
        engineer = cls()
        engineer.label_encoders = data['label_encoders']
        engineer.scaler = data['scaler']
        engineer.feature_names = data['feature_names']
        engineer.categorical_cols = data['categorical_cols']
        engineer.numeric_cols = data['numeric_cols']
        engineer.fitted = data['fitted']
        
        print(f"📂 Loaded advanced feature pipeline from: {filepath}")
        return engineer


if __name__ == '__main__':
    print("Testing Advanced Feature Engineering...")
    
    # Load dataset
    df = pd.read_csv('ml/data/fitness_dataset.csv')
    
    # Need to load user data for goals
    df_users = pd.read_csv('ml/data/fitness_dataset_users.csv')
    df = df.merge(df_users[['user_id', 'goal']], on='user_id', how='left')
    
    print(f"Loaded {len(df)} samples")
    
    # Create features
    engineer = AdvancedFeatureEngineer()
    X, y = engineer.prepare_features(df)
    
    print(f"\n✅ Feature matrix: {X.shape}")
    print(f"✅ Target vector: {y.shape}")
    print(f"\nFeature count: {len(engineer.feature_names)}")
