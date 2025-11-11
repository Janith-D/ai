"""
Synthetic Fitness Data Generator

Generates realistic training data for the ML model including:
- User profiles (age, gender, goals, experience)
- Workout logs (muscle groups, intensity, duration, fatigue)
- Target labels (next recommended workout)
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import List, Dict, Tuple
import random


class SyntheticDataGenerator:
    """Generate synthetic fitness training data"""
    
    # Constants
    GOALS = ['muscle_gain', 'fat_loss', 'endurance', 'strength', 'general_fitness']
    GENDERS = ['male', 'female']
    MUSCLE_GROUPS = ['chest', 'back', 'legs', 'shoulders', 'arms', 'cardio', 'rest']
    INTENSITIES = ['none', 'light', 'medium', 'heavy']
    FATIGUE_LEVELS = ['low', 'medium', 'high']
    EXPERIENCE_LEVELS = ['beginner', 'intermediate', 'advanced']
    
    # Workout patterns by goal (BALANCED to reduce cardio dominance)
    GOAL_PATTERNS = {
        'muscle_gain': {
            'split': ['chest', 'back', 'legs', 'shoulders', 'arms', 'chest', 'legs', 'rest'],
            'intensity_pref': ['medium', 'heavy'],
            'rest_frequency': 0.18
        },
        'fat_loss': {
            'split': ['cardio', 'legs', 'chest', 'cardio', 'back', 'shoulders', 'rest'],
            'intensity_pref': ['light', 'medium'],
            'rest_frequency': 0.15
        },
        'endurance': {
            'split': ['cardio', 'legs', 'chest', 'cardio', 'back', 'rest'],
            'intensity_pref': ['light', 'medium'],
            'rest_frequency': 0.20
        },
        'strength': {
            'split': ['legs', 'chest', 'back', 'legs', 'shoulders', 'arms', 'rest'],
            'intensity_pref': ['heavy'],
            'rest_frequency': 0.20
        },
        'general_fitness': {
            'split': ['chest', 'legs', 'cardio', 'back', 'shoulders', 'rest'],
            'intensity_pref': ['light', 'medium'],
            'rest_frequency': 0.18
        }
    }
    
    def __init__(self, seed: int = 42):
        """Initialize generator with random seed"""
        random.seed(seed)
        np.random.seed(seed)
        
    def generate_user_profile(self, user_id: str) -> Dict:
        """Generate a single user profile"""
        gender = random.choice(self.GENDERS)
        goal = random.choice(self.GOALS)
        experience = random.choice(self.EXPERIENCE_LEVELS)
        
        # Realistic age/height/weight distributions
        if gender == 'male':
            age = np.random.randint(18, 55)
            height_cm = np.random.randint(165, 195)
            weight_kg = np.random.randint(60, 110)
        else:
            age = np.random.randint(18, 55)
            height_cm = np.random.randint(155, 180)
            weight_kg = np.random.randint(45, 90)
            
        return {
            'user_id': user_id,
            'age': age,
            'gender': gender,
            'goal': goal,
            'height_cm': height_cm,
            'weight_kg': weight_kg,
            'experience_level': experience
        }
    
    def simulate_workout_log(
        self, 
        user_profile: Dict, 
        date: datetime,
        previous_workout: str = None,
        days_since_last: int = 1
    ) -> Dict:
        """Simulate a single workout day for a user"""
        
        goal = user_profile['goal']
        pattern = self.GOAL_PATTERNS[goal]
        
        # Decide if rest day based on fatigue/pattern
        rest_probability = pattern['rest_frequency']
        
        # Higher rest probability if:
        # - Just worked out (days_since_last == 1)
        # - Previous workout was heavy
        if days_since_last == 1:
            rest_probability += 0.15
        
        is_rest = random.random() < rest_probability
        
        if is_rest:
            muscle_group = 'rest'
            intensity = 'none'
            duration_min = 0
            calories = 0
            fatigue_level = random.choice(['low', 'medium'])  # Rest reduces fatigue
            sleep_hours = np.random.normal(7.5, 1.0)  # Better sleep on rest days
            result_score = np.random.uniform(0.7, 0.9)  # Good recovery
        else:
            # Choose muscle group from pattern (avoid same as previous)
            available_groups = [g for g in pattern['split'] if g != 'rest']
            if previous_workout and previous_workout in available_groups:
                available_groups = [g for g in available_groups if g != previous_workout]
            
            muscle_group = random.choice(available_groups) if available_groups else random.choice(self.MUSCLE_GROUPS[:-1])
            
            # Choose intensity based on goal
            intensity = random.choice(pattern['intensity_pref'])
            
            # Duration based on experience and intensity
            base_duration = {
                'beginner': (30, 45),
                'intermediate': (45, 60),
                'advanced': (60, 90)
            }[user_profile['experience_level']]
            
            duration_min = np.random.randint(*base_duration)
            
            # Calories based on intensity and duration
            calorie_multiplier = {
                'light': 5,
                'medium': 7,
                'heavy': 9
            }.get(intensity, 6)
            
            calories = int(duration_min * calorie_multiplier * np.random.uniform(0.9, 1.1))
            
            # Fatigue based on intensity
            fatigue_weights = {
                'light': [0.7, 0.25, 0.05],    # Mostly low fatigue
                'medium': [0.3, 0.5, 0.2],     # Mostly medium
                'heavy': [0.1, 0.4, 0.5]       # Mostly high fatigue
            }
            fatigue_level = random.choices(
                self.FATIGUE_LEVELS, 
                weights=fatigue_weights.get(intensity, [0.33, 0.33, 0.34])
            )[0]
            
            # Sleep (less sleep after hard workouts)
            sleep_hours = np.random.normal(7.0, 1.2)
            if fatigue_level == 'high':
                sleep_hours -= 0.5
            
            # Result score (performance quality)
            base_score = 0.75
            if fatigue_level == 'low':
                base_score += 0.1
            if sleep_hours >= 7:
                base_score += 0.05
            
            result_score = min(1.0, max(0.0, base_score + np.random.normal(0, 0.1)))
        
        # Clip values to reasonable ranges
        sleep_hours = max(4.0, min(10.0, sleep_hours))
        
        return {
            'user_id': user_profile['user_id'],
            'date': date.strftime('%Y-%m-%d'),
            'muscle_group': muscle_group,
            'intensity': intensity,
            'duration_min': duration_min,
            'calories': calories,
            'fatigue_level': fatigue_level,
            'sleep_hours': round(sleep_hours, 1),
            'result_score': round(result_score, 2),
            'days_since_last_workout': days_since_last
        }
    
    def predict_next_workout(
        self,
        user_profile: Dict,
        current_log: Dict,
        workout_history: List[Dict]
    ) -> str:
        """Rule-based logic to predict next workout (ground truth for training)"""
        
        goal = user_profile['goal']
        pattern = self.GOAL_PATTERNS[goal]
        current_muscle = current_log['muscle_group']
        fatigue = current_log['fatigue_level']
        sleep = current_log['sleep_hours']
        
        # Rule 1: High fatigue + poor sleep → rest
        if fatigue == 'high' and sleep < 6:
            return 'rest'
        
        # Rule 2: Just had rest → workout
        if current_muscle == 'rest':
            # Pick first non-rest from pattern
            available = [g for g in pattern['split'] if g != 'rest']
            return available[0] if available else 'cardio'
        
        # Rule 3: Check recent muscle groups (avoid same muscle 2 days in row)
        recent_muscles = [log['muscle_group'] for log in workout_history[-3:]]
        
        # Count how many times current muscle was trained recently
        recent_count = recent_muscles.count(current_muscle)
        if recent_count >= 2:
            # Force different muscle group
            available = [g for g in pattern['split'] if g != current_muscle and g != 'rest']
            if available:
                return random.choice(available)
        
        # Get next in rotation (balanced upper/lower split logic)
        if goal in ['muscle_gain', 'strength']:
            # Alternate upper/lower with specific muscle groups
            upper_groups = ['chest', 'back', 'shoulders', 'arms']
            lower_groups = ['legs']
            
            if current_muscle in upper_groups:
                # After upper body → legs or rest (70/30)
                return 'legs' if random.random() < 0.7 else 'rest'
            elif current_muscle in lower_groups:
                # After legs → specific upper body muscle (not random "upper")
                upper_muscles = ['chest', 'back', 'shoulders', 'arms']
                # Pick least recently used
                for muscle in upper_muscles:
                    if muscle not in recent_muscles[-2:]:
                        return muscle
                return random.choice(upper_muscles)
            else:
                # After cardio/rest → follow pattern
                available = [g for g in pattern['split'] if g != 'rest']
                return random.choice(available) if available else 'chest'
        
        elif goal in ['fat_loss', 'endurance']:
            # Balance cardio with specific muscle groups
            if current_muscle == 'cardio':
                # After cardio → specific muscle (not generic "upper")
                muscles = ['legs', 'chest', 'back', 'shoulders']
                # Pick one not used recently
                for muscle in muscles:
                    if muscle not in recent_muscles[-2:]:
                        return muscle
                return random.choice(muscles)
            else:
                # After strength → cardio or different muscle (60/40)
                if random.random() < 0.6:
                    return 'cardio'
                else:
                    available = [g for g in ['chest', 'back', 'legs', 'shoulders'] if g != current_muscle]
                    return random.choice(available) if available else 'cardio'
        
        else:  # general_fitness
            # Rotate through all muscle groups evenly
            available = [g for g in pattern['split'] if g != current_muscle and g != 'rest']
            if available:
                # Prefer muscles not used recently
                not_recent = [m for m in available if m not in recent_muscles[-2:]]
                return random.choice(not_recent) if not_recent else random.choice(available)
            return 'rest'
    
    def generate_dataset(
        self,
        n_users: int = 100,
        days_per_user: int = 90,
        output_file: str = None
    ) -> Tuple[pd.DataFrame, pd.DataFrame]:
        """
        Generate complete training dataset
        
        Args:
            n_users: Number of synthetic users
            days_per_user: Days of workout history per user
            output_file: Optional CSV file to save dataset
            
        Returns:
            (user_profiles_df, workout_logs_df)
        """
        print(f"🔄 Generating synthetic dataset...")
        print(f"   Users: {n_users}")
        print(f"   Days per user: {days_per_user}")
        
        user_profiles = []
        workout_logs = []
        
        for i in range(n_users):
            user_id = f"u{str(i+1).zfill(4)}"
            
            # Generate user profile
            profile = self.generate_user_profile(user_id)
            user_profiles.append(profile)
            
            # Generate workout logs
            start_date = datetime(2024, 1, 1)
            previous_workout = None
            history = []
            days_since_last = 1
            
            for day in range(days_per_user):
                current_date = start_date + timedelta(days=day)
                
                # Simulate workout log
                log = self.simulate_workout_log(
                    profile, 
                    current_date, 
                    previous_workout,
                    days_since_last
                )
                
                # Predict next workout (ground truth)
                next_workout = self.predict_next_workout(profile, log, history)
                log['next_workout'] = next_workout
                
                workout_logs.append(log)
                history.append(log)
                
                # Update tracking
                if log['muscle_group'] != 'rest':
                    previous_workout = log['muscle_group']
                    days_since_last = 1
                else:
                    days_since_last += 1
            
            if (i + 1) % 10 == 0:
                print(f"   ✓ Generated {i+1}/{n_users} users")
        
        # Convert to DataFrames
        users_df = pd.DataFrame(user_profiles)
        logs_df = pd.DataFrame(workout_logs)
        
        print(f"✅ Dataset generated!")
        print(f"   User profiles: {len(users_df)} rows")
        print(f"   Workout logs: {len(logs_df)} rows")
        
        # Save to CSV if requested
        if output_file:
            users_df.to_csv(output_file.replace('.csv', '_users.csv'), index=False)
            logs_df.to_csv(output_file, index=False)
            print(f"   💾 Saved to: {output_file}")
        
        return users_df, logs_df


def main():
    """Generate default dataset for training"""
    generator = SyntheticDataGenerator(seed=42)
    
    users_df, logs_df = generator.generate_dataset(
        n_users=100,
        days_per_user=90,
        output_file='ml/data/fitness_dataset.csv'
    )
    
    # Show sample
    print("\n📊 Sample Data:")
    print("\nUser Profiles:")
    print(users_df.head(3))
    print("\nWorkout Logs:")
    print(logs_df.head(10))
    
    # Statistics
    print("\n📈 Dataset Statistics:")
    print(f"\nGoal distribution:")
    print(users_df['goal'].value_counts())
    print(f"\nNext workout distribution:")
    print(logs_df['next_workout'].value_counts())
    print(f"\nFatigue level distribution:")
    print(logs_df['fatigue_level'].value_counts())


if __name__ == '__main__':
    main()
