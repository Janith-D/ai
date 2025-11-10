/*
 * safety_rules.pl
 * 
 * Safety and injury constraint rules for workout planning.
 * Ensures workouts respect user injuries and contraindications.
 * 
 * Module: safety_rules
 * Exports: safe_for_user/2, injury_affects_muscle/2, workout_avoids_injury/2
 */

:- module(safety_rules, [
    safe_for_user/2,
    injury_affects_muscle/2,
    workout_avoids_injury/2
]).

% safe_for_user(+WorkoutID, +UserID)
% Checks if workout is safe given user's injuries
% 
% Two safety checks:
% 1. Check if workout exercises contraindicated muscles
% 2. Check if user has injuries affecting those muscles
safe_for_user(WorkoutID, UserID) :-
    % Get all muscles used by this workout
    findall(Muscle, fact(WorkoutID, uses_muscle, Muscle), UsedMuscles),
    % Get user's injured areas
    findall(InjuryMuscle, (
        user_profile(UserID, injury(Condition)),
        injury_affects_muscle(Condition, InjuryMuscle)
    ), InjuredMuscles),
    % Check no overlap
    \+ (member(M, UsedMuscles), member(M, InjuredMuscles)).

% safe_for_user succeeds if user has no injuries
safe_for_user(_, UserID) :-
    \+ user_profile(UserID, injury(_)).

% injury_affects_muscle(+Condition, -Muscle)
% Maps injury conditions to affected muscle groups
% 
% Standard condition mappings (can be extended):
injury_affects_muscle(cond_knee, m_quadriceps).
injury_affects_muscle(cond_knee, m_hamstrings).
injury_affects_muscle(cond_knee, m_calves).
injury_affects_muscle(cond_low_back, m_back).
injury_affects_muscle(cond_low_back, m_core).
injury_affects_muscle(cond_shoulder, m_shoulders).
injury_affects_muscle(cond_shoulder, m_chest).
injury_affects_muscle(cond_shoulder, m_biceps).
injury_affects_muscle(cond_shoulder, m_triceps).

% workout_avoids_injury(+WorkoutID, +ConditionID)
% Checks if a workout avoids contraindicated exercises for a condition
workout_avoids_injury(WorkoutID, Condition) :-
    \+ fact(WorkoutID, contraindicated_for, Condition).
