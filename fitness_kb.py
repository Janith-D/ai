"""
fitness_kb.py - Python API wrapper for the Prolog fitness knowledge base

This module provides a clean Python interface to query the Prolog KB using PySwip.

Example usage:
    from fitness_kb import FitnessKB
    
    kb = FitnessKB("kb_generated.pl")
    
    # Get exercises by muscle
    exercises = kb.get_exercises_by_muscle("m_quadriceps")
    
    # Check contraindications
    safe = kb.is_safe_for_condition("e_ex_001", "cond_knee")
    
    # Find substitutions
    subs = kb.get_substitutions("e_ex_001")
"""

from typing import List, Dict, Optional
from pyswip import Prolog


class FitnessKB:
    """Python wrapper for the Prolog fitness knowledge base."""
    
    def __init__(self, kb_file: str = "kb_generated.pl"):
        """Initialize the knowledge base.
        
        Args:
            kb_file: Path to the Prolog KB file
        """
        self.prolog = Prolog()
        self.prolog.consult(kb_file)
    
    def get_all_exercises(self) -> List[Dict[str, str]]:
        """Get all exercises in the KB.
        
        Returns:
            List of exercise dictionaries with 'id' and 'name'
        """
        exercises = []
        for result in self.prolog.query("entity(ID, exercise, Name)"):
            exercises.append({
                'id': result['ID'],
                'name': result['Name']
            })
        return exercises
    
    def get_exercise_details(self, exercise_id: str) -> Optional[Dict]:
        """Get full details for an exercise.
        
        Args:
            exercise_id: Exercise ID (e.g., 'e_ex_001')
            
        Returns:
            Dictionary with exercise details or None if not found
        """
        # Get basic info
        query = f"entity({exercise_id}, exercise, Name)"
        results = list(self.prolog.query(query))
        if not results:
            return None
        
        details = {
            'id': exercise_id,
            'name': results[0]['Name'],
            'muscles': [],
            'equipment': [],
            'difficulty': None,
            'contraindications': [],
            'substitutions': []
        }
        
        # Get difficulty
        for result in self.prolog.query(f"attr({exercise_id}, difficulty, Diff)"):
            details['difficulty'] = result['Diff']
        
        # Get muscles
        for result in self.prolog.query(
            f"fact({exercise_id}, uses_muscle, MID), entity(MID, muscle, Name)"
        ):
            details['muscles'].append({
                'id': result['MID'],
                'name': result['Name']
            })
        
        # Get equipment
        for result in self.prolog.query(
            f"fact({exercise_id}, requires_equipment, EID), entity(EID, equipment, Name)"
        ):
            details['equipment'].append({
                'id': result['EID'],
                'name': result['Name']
            })
        
        # Get contraindications
        for result in self.prolog.query(
            f"fact({exercise_id}, contraindicated_for, CID), entity(CID, condition, Name)"
        ):
            details['contraindications'].append({
                'id': result['CID'],
                'name': result['Name']
            })
        
        # Get substitutions (exercises that can substitute for this one)
        for result in self.prolog.query(
            f"fact(SubID, substitution_for, {exercise_id}), entity(SubID, exercise, Name)"
        ):
            details['substitutions'].append({
                'id': result['SubID'],
                'name': result['Name']
            })
        
        return details
    
    def get_exercises_by_muscle(self, muscle_id: str) -> List[Dict[str, str]]:
        """Get all exercises that use a specific muscle.
        
        Args:
            muscle_id: Muscle ID (e.g., 'm_quadriceps')
            
        Returns:
            List of exercise dictionaries
        """
        exercises = []
        query = f"fact(ID, uses_muscle, {muscle_id}), entity(ID, exercise, Name)"
        for result in self.prolog.query(query):
            exercises.append({
                'id': result['ID'],
                'name': result['Name']
            })
        return exercises
    
    def get_exercises_by_equipment(self, equipment_id: str) -> List[Dict[str, str]]:
        """Get all exercises that require specific equipment.
        
        Args:
            equipment_id: Equipment ID (e.g., 'eq_barbell')
            
        Returns:
            List of exercise dictionaries
        """
        exercises = []
        query = f"fact(ID, requires_equipment, {equipment_id}), entity(ID, exercise, Name)"
        for result in self.prolog.query(query):
            exercises.append({
                'id': result['ID'],
                'name': result['Name']
            })
        return exercises
    
    def get_exercises_by_difficulty(self, difficulty: str) -> List[Dict[str, str]]:
        """Get all exercises of a specific difficulty level.
        
        Args:
            difficulty: Difficulty level ('beginner', 'easy', 'medium', 'hard', 'expert')
            
        Returns:
            List of exercise dictionaries
        """
        exercises = []
        query = f"attr(ID, difficulty, {difficulty}), entity(ID, exercise, Name)"
        for result in self.prolog.query(query):
            exercises.append({
                'id': result['ID'],
                'name': result['Name']
            })
        return exercises
    
    def is_safe_for_condition(self, exercise_id: str, condition_id: str) -> bool:
        """Check if an exercise is safe (not contraindicated) for a condition.
        
        Args:
            exercise_id: Exercise ID
            condition_id: Condition ID (e.g., 'cond_knee')
            
        Returns:
            True if safe (not contraindicated), False otherwise
        """
        query = f"fact({exercise_id}, contraindicated_for, {condition_id})"
        return len(list(self.prolog.query(query))) == 0
    
    def get_substitutions(self, exercise_id: str) -> List[Dict[str, str]]:
        """Get exercises that can substitute for the given exercise.
        
        Args:
            exercise_id: Exercise ID
            
        Returns:
            List of substitute exercise dictionaries
        """
        substitutions = []
        query = f"fact(SubID, substitution_for, {exercise_id}), entity(SubID, exercise, Name)"
        for result in self.prolog.query(query):
            substitutions.append({
                'id': result['SubID'],
                'name': result['Name']
            })
        return substitutions
    
    def find_exercises(
        self,
        muscles: Optional[List[str]] = None,
        equipment: Optional[List[str]] = None,
        difficulty: Optional[str] = None,
        exclude_conditions: Optional[List[str]] = None
    ) -> List[Dict[str, str]]:
        """Find exercises matching multiple criteria.
        
        Args:
            muscles: List of muscle IDs (AND condition)
            equipment: List of acceptable equipment IDs (OR condition)
            difficulty: Difficulty level
            exclude_conditions: List of condition IDs to avoid contraindications
            
        Returns:
            List of matching exercise dictionaries
        """
        # Build query dynamically
        conditions = ["entity(ID, exercise, Name)"]
        
        if muscles:
            for muscle_id in muscles:
                conditions.append(f"fact(ID, uses_muscle, {muscle_id})")
        
        if difficulty:
            conditions.append(f"attr(ID, difficulty, {difficulty})")
        
        query = ", ".join(conditions)
        
        # Get initial results - consume the generator fully before nested queries
        initial_results = list(self.prolog.query(query))
        
        exercises = []
        for result in initial_results:
            ex_id = result['ID']
            
            # Filter by equipment if specified
            if equipment:
                ex_equipment = [eq_result['EID'] 
                               for eq_result in self.prolog.query(f"fact({ex_id}, requires_equipment, EID)")]
                
                # Check if any of the exercise's equipment is in the allowed list
                if not any(eq in equipment for eq in ex_equipment):
                    continue
            
            # Filter by contraindications if specified
            if exclude_conditions:
                is_contraindicated = False
                for cond_id in exclude_conditions:
                    # Check directly without calling another method that uses query
                    contra_query = f"fact({ex_id}, contraindicated_for, {cond_id})"
                    if len(list(self.prolog.query(contra_query))) > 0:
                        is_contraindicated = True
                        break
                
                if is_contraindicated:
                    continue
            
            exercises.append({
                'id': result['ID'],
                'name': result['Name']
            })
        
        return exercises
    
    def get_all_muscles(self) -> List[Dict[str, str]]:
        """Get all muscles in the KB."""
        muscles = []
        for result in self.prolog.query("entity(ID, muscle, Name)"):
            muscles.append({'id': result['ID'], 'name': result['Name']})
        return muscles
    
    def get_all_equipment(self) -> List[Dict[str, str]]:
        """Get all equipment in the KB."""
        equipment = []
        for result in self.prolog.query("entity(ID, equipment, Name)"):
            equipment.append({'id': result['ID'], 'name': result['Name']})
        return equipment
    
    def get_all_conditions(self) -> List[Dict[str, str]]:
        """Get all medical conditions in the KB."""
        conditions = []
        for result in self.prolog.query("entity(ID, condition, Name)"):
            conditions.append({'id': result['ID'], 'name': result['Name']})
        return conditions


# Example usage
if __name__ == '__main__':
    kb = FitnessKB()
    
    print("=== Fitness KB Python API Demo ===\n")
    
    # Get exercise details
    print("1. Exercise details for e_ex_001:")
    details = kb.get_exercise_details("e_ex_001")
    print(f"   Name: {details['name']}")
    print(f"   Difficulty: {details['difficulty']}")
    print(f"   Muscles: {', '.join(m['name'] for m in details['muscles'])}")
    print(f"   Equipment: {', '.join(e['name'] for e in details['equipment'])}")
    print(f"   Contraindications: {', '.join(c['name'] for c in details['contraindications'])}")
    if details['substitutions']:
        print(f"   Substitutions: {', '.join(s['name'] for s in details['substitutions'])}")
    
    # Find exercises
    print("\n2. Finding beginner exercises using chest muscles:")
    exercises = kb.find_exercises(
        muscles=['m_chest'],
        difficulty='easy'
    )
    print(f"   Found {len(exercises)} exercises")
    for ex in exercises[:3]:
        print(f"     - {ex['name']}")
    
    # Safe exercises for knee injury
    print("\n3. Finding exercises safe for knee injury:")
    exercises = kb.find_exercises(
        muscles=['m_chest'],
        exclude_conditions=['cond_knee']
    )
    print(f"   Found {len(exercises)} safe chest exercises")
    
    print("\n✅ Demo completed!")
