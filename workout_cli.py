"""
workout_cli.py

Command-line interface for querying workout planning rules.
Provides interactive and command-line modes for asking questions about
workout scheduling, safety, and recommendations.

Usage:
    # Interactive mode
    python workout_cli.py
    
    # Command-line mode
    python workout_cli.py --user u_healthy_morning --workout e_chest_press --day monday --time morning
    python workout_cli.py --query "What workout for u_healthy_morning on monday?"
"""

import argparse
import sys
from pyswip import Prolog
from typing import List, Dict, Optional

class WorkoutCLI:
    def __init__(self):
        """Initialize Prolog engine and load rules"""
        print("Loading workout planning rules...")
        self.prolog = Prolog()
        
        try:
            # Load main KB and rules
            self.prolog.consult("kb_generated.pl")
            self.prolog.consult("workout_rules.pl")
            self.prolog.consult("tests/test_facts.pl")
            print("✓ Rules loaded successfully\n")
        except Exception as e:
            print(f"✗ Error loading rules: {e}")
            sys.exit(1)
    
    def check_workout_safety(self, workout_id: str, user_id: str) -> Dict:
        """Check if a workout is safe for a user"""
        query = f"safe_for_user({workout_id}, {user_id})"
        results = list(self.prolog.query(query))
        
        is_safe = len(results) > 0
        
        # Get explanation if not safe
        reason = ""
        if not is_safe:
            # Check for injuries
            injury_query = f"user_profile({user_id}, injury(Condition))"
            injuries = list(self.prolog.query(injury_query))
            if injuries:
                conditions = [inj['Condition'] for inj in injuries]
                reason = f"User has injuries: {', '.join(conditions)}"
            else:
                reason = "Unknown safety concern"
        
        return {
            'safe': is_safe,
            'reason': reason
        }
    
    def check_recovery_needs(self, user_id: str) -> Dict:
        """Check how many recovery days a user needs"""
        query = f"needs_recovery({user_id}, Days)"
        results = list(self.prolog.query(query))
        
        if results:
            days = results[0]['Days']
            sleep_query = f"user_profile({user_id}, sleep_hours(Hours))"
            sleep_results = list(self.prolog.query(sleep_query))
            sleep_hours = sleep_results[0]['Hours'] if sleep_results else 'unknown'
            
            return {
                'recovery_days': days,
                'sleep_hours': sleep_hours,
                'recommendation': self._get_recovery_recommendation(days)
            }
        else:
            return {
                'recovery_days': 'unknown',
                'sleep_hours': 'unknown',
                'recommendation': 'Unable to determine recovery needs'
            }
    
    def _get_recovery_recommendation(self, days: int) -> str:
        """Get recommendation based on recovery days needed"""
        if days >= 3:
            return "High recovery need - focus on sleep quality and consider lighter workouts"
        elif days == 2:
            return "Moderate recovery need - balance training with rest days"
        else:
            return "Good recovery - maintain current training schedule"
    
    def check_time_match(self, workout_id: str, user_id: str, time_of_day: str) -> Dict:
        """Check if workout time matches user's energy peak"""
        query = f"optimal_time_match({workout_id}, {user_id}, {time_of_day})"
        results = list(self.prolog.query(query))
        
        is_optimal = len(results) > 0
        
        # Get user's energy peak
        energy_query = f"user_profile({user_id}, energy_peak(Peak))"
        energy_results = list(self.prolog.query(energy_query))
        energy_peak = energy_results[0]['Peak'] if energy_results else 'unknown'
        
        # Get workout difficulty
        diff_query = f"attr({workout_id}, difficulty, Diff)"
        diff_results = list(self.prolog.query(diff_query))
        difficulty = diff_results[0]['Diff'] if diff_results else 'unknown'
        
        return {
            'optimal': is_optimal,
            'energy_peak': energy_peak,
            'workout_difficulty': difficulty,
            'time_of_day': time_of_day,
            'recommendation': f"{'✓ Good match' if is_optimal else '✗ Suboptimal - workout too intense for this time'}"
        }
    
    def suggest_workout(self, user_id: str, day: str, current_schedule: List = None) -> Dict:
        """Suggest a workout for a user on a given day"""
        if current_schedule is None:
            current_schedule = []
        
        # Convert schedule to Prolog format
        schedule_str = str(current_schedule).replace("'", "")
        
        # Query for suitable workouts
        query = f"""
            entity(WorkoutID, exercise, Name),
            safe_for_user(WorkoutID, {user_id}),
            attr(WorkoutID, difficulty, Difficulty)
        """
        
        results = list(self.prolog.query(query))
        
        suitable_workouts = []
        for result in results[:10]:  # Limit to top 10
            workout_id = result['WorkoutID']
            name = result['Name']
            difficulty = result['Difficulty']
            
            suitable_workouts.append({
                'id': workout_id,
                'name': name,
                'difficulty': difficulty
            })
        
        return {
            'suggestions': suitable_workouts,
            'count': len(suitable_workouts)
        }
    
    def interactive_mode(self):
        """Run interactive CLI mode"""
        print("=" * 70)
        print("WORKOUT PLANNING ASSISTANT - Interactive Mode")
        print("=" * 70)
        print("\nCommands:")
        print("  safety <workout_id> <user_id>    - Check if workout is safe")
        print("  recovery <user_id>                - Check recovery needs")
        print("  time <workout_id> <user_id> <time> - Check time-of-day match")
        print("  suggest <user_id> <day>           - Suggest workouts for a day")
        print("  users                             - List available test users")
        print("  workouts                          - List available workouts")
        print("  help                              - Show this help")
        print("  quit                              - Exit")
        print()
        
        while True:
            try:
                command = input("workout> ").strip().lower()
                
                if not command:
                    continue
                
                parts = command.split()
                cmd = parts[0]
                
                if cmd in ['quit', 'exit', 'q']:
                    print("Goodbye!")
                    break
                
                elif cmd == 'help':
                    self.interactive_mode.__doc__
                    continue
                
                elif cmd == 'users':
                    self._list_users()
                
                elif cmd == 'workouts':
                    self._list_workouts()
                
                elif cmd == 'safety' and len(parts) == 3:
                    result = self.check_workout_safety(parts[1], parts[2])
                    print(f"\n{'✓ SAFE' if result['safe'] else '✗ NOT SAFE'}")
                    if result['reason']:
                        print(f"Reason: {result['reason']}")
                    print()
                
                elif cmd == 'recovery' and len(parts) == 2:
                    result = self.check_recovery_needs(parts[1])
                    print(f"\nRecovery Days Needed: {result['recovery_days']}")
                    print(f"Sleep Hours: {result['sleep_hours']}")
                    print(f"Recommendation: {result['recommendation']}\n")
                
                elif cmd == 'time' and len(parts) == 4:
                    result = self.check_time_match(parts[1], parts[2], parts[3])
                    print(f"\n{result['recommendation']}")
                    print(f"Workout Difficulty: {result['workout_difficulty']}")
                    print(f"User Energy Peak: {result['energy_peak']}")
                    print(f"Scheduled Time: {result['time_of_day']}\n")
                
                elif cmd == 'suggest' and len(parts) == 3:
                    result = self.suggest_workout(parts[1], parts[2])
                    print(f"\nFound {result['count']} suitable workouts:")
                    for i, workout in enumerate(result['suggestions'][:5], 1):
                        print(f"  {i}. {workout['name']} ({workout['difficulty']})")
                    print()
                
                else:
                    print("Invalid command. Type 'help' for available commands.\n")
            
            except KeyboardInterrupt:
                print("\nGoodbye!")
                break
            except Exception as e:
                print(f"Error: {e}\n")
    
    def _list_users(self):
        """List available test users"""
        print("\nAvailable Test Users:")
        users = [
            ("u_healthy_morning", "Good sleep, morning energy"),
            ("u_knee_patient", "Knee injury, afternoon energy"),
            ("u_exhausted", "Poor sleep, high fatigue"),
            ("u_evening_injured", "Evening energy, shoulder injury"),
            ("u_traveler", "Traveling, moderate sleep"),
            ("u_overtrainer", "Overtraining risk, poor sleep")
        ]
        for user_id, desc in users:
            print(f"  {user_id:20s} - {desc}")
        print()
    
    def _list_workouts(self):
        """List available test workouts"""
        print("\nAvailable Workouts:")
        workouts = [
            ("e_chest_press", "Chest Press", "hard"),
            ("e_bench_press", "Bench Press", "hard"),
            ("e_squats", "Squats", "hard"),
            ("e_deadlift", "Deadlift", "hard"),
            ("e_bicep_curls", "Bicep Curls", "easy"),
            ("e_shoulder_press", "Shoulder Press", "medium"),
            ("e_lat_pulldown", "Lat Pulldown", "medium"),
            ("e_lunges", "Lunges", "medium"),
            ("e_plank", "Plank", "easy"),
            ("e_yoga_stretch", "Yoga Stretch", "easy")
        ]
        for workout_id, name, difficulty in workouts:
            print(f"  {workout_id:20s} - {name:20s} ({difficulty})")
        print()


def main():
    parser = argparse.ArgumentParser(
        description='Workout Planning Assistant CLI',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument('--user', help='User ID')
    parser.add_argument('--workout', help='Workout ID')
    parser.add_argument('--day', help='Day of week')
    parser.add_argument('--time', help='Time of day (morning/afternoon/evening)')
    parser.add_argument('--action', choices=['safety', 'recovery', 'time', 'suggest'],
                        help='Action to perform')
    
    args = parser.parse_args()
    
    cli = WorkoutCLI()
    
    # Check if command-line mode or interactive mode
    if args.action:
        if args.action == 'safety' and args.workout and args.user:
            result = cli.check_workout_safety(args.workout, args.user)
            print(f"{'SAFE' if result['safe'] else 'NOT SAFE'}")
            if result['reason']:
                print(f"Reason: {result['reason']}")
        
        elif args.action == 'recovery' and args.user:
            result = cli.check_recovery_needs(args.user)
            print(f"Recovery Days: {result['recovery_days']}")
            print(f"Recommendation: {result['recommendation']}")
        
        elif args.action == 'time' and args.workout and args.user and args.time:
            result = cli.check_time_match(args.workout, args.user, args.time)
            print(result['recommendation'])
        
        elif args.action == 'suggest' and args.user and args.day:
            result = cli.suggest_workout(args.user, args.day)
            print(f"Found {result['count']} suitable workouts")
            for workout in result['suggestions'][:5]:
                print(f"  - {workout['name']} ({workout['difficulty']})")
        
        else:
            print("Missing required arguments for action")
            parser.print_help()
            sys.exit(1)
    
    else:
        # Interactive mode
        cli.interactive_mode()


if __name__ == "__main__":
    main()
