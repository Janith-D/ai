/*
 * rules_master.pl
 * 
 * Master rule file that imports all modular rule sets for workout planning.
 * This provides a single entry point for loading all workout planning rules.
 * 
 * Rule Modules:
 * - recovery_rules: Sleep and rest day management
 * - safety_rules: Injury constraints and contraindications
 * - schedule_rules: Frequency, timing, and split patterns
 * 
 * Usage:
 *   :- consult('rules/rules_master.pl').
 *   ?- needs_recovery(UserID, Days).
 *   ?- safe_for_user(WorkoutID, UserID).
 *   ?- workout_frequency_ok(WorkoutID, Schedule, Result).
 */

% Load all rule modules
:- use_module('recovery_rules').
:- use_module('safety_rules').
:- use_module('schedule_rules').

% ===========================================================================
% High-Level Scheduling Rules (combining multiple rule modules)
% ===========================================================================

% can_schedule_workout(+WorkoutID, +UserID, +Context)
% Master rule that checks if a workout can be scheduled
% 
% Context should contain:
%  - week_schedule([day(N, ExerciseID), ...])
%  - day_of_week(N)
%  - time_of_day(morning|afternoon|evening)
%
% Checks:
% 1. Exercise exists
% 2. Safe for user (injury constraints)
% 3. Optimal time match (energy/intensity)
% 4. Frequency limits not exceeded
% 5. Recovery requirements met
can_schedule_workout(WorkoutID, UserID, Context) :-
    % Extract context
    member(week_schedule(Schedule), Context),
    member(time_of_day(TimeOfDay), Context),
    
    % Check safety
    safety_rules:safe_for_user(WorkoutID, UserID),
    
    % Check time-intensity match
    schedule_rules:optimal_time_match(WorkoutID, UserID, TimeOfDay),
    
    % Check frequency limits
    schedule_rules:workout_frequency_ok(WorkoutID, Schedule, ok),
    
    % Check recovery requirements
    recovery_rules:schedule_has_enough_recovery(UserID, Schedule).

% suggest_workout_day(+WorkoutID, +UserID, +Context)
% Suggests the best day to schedule a workout
% 
% Context should contain:
%  - week_schedule([...])
%  - current_day(N)
%
% Strategy:
% 1. Find available days (not already scheduled)
% 2. Prefer days that maintain upper/lower split
% 3. Ensure recovery requirements met
% 4. Return first suitable day
suggest_workout_day(WorkoutID, UserID, Context) :-
    member(week_schedule(Schedule), Context),
    member(current_day(CurrentDay), Context),
    
    % Find days not yet scheduled
    findall(Day, (
        between(CurrentDay, 7, Day),
        \+ member(day(Day, _), Schedule)
    ), AvailableDays),
    
    % Check if any days available
    AvailableDays \= [],
    
    % Try each day
    member(SuggestedDay, AvailableDays),
    
    % Check if adding workout on this day is valid
    NewSchedule = [day(SuggestedDay, WorkoutID)|Schedule],
    recovery_rules:schedule_has_enough_recovery(UserID, NewSchedule),
    schedule_rules:respects_split_pattern(NewSchedule, balanced).

% ===========================================================================
% Rule Logging and Explanation
% ===========================================================================

% explain_blocked_workout(+WorkoutID, +UserID, +Context, -Reason)
% Explains why a workout cannot be scheduled
% 
% Returns human-readable reason for rejection
explain_blocked_workout(WorkoutID, UserID, Context, 'User has injury affecting workout muscles') :-
    \+ safety_rules:safe_for_user(WorkoutID, UserID),
    !.

explain_blocked_workout(WorkoutID, UserID, Context, 'Workout intensity not optimal for time of day') :-
    member(time_of_day(TimeOfDay), Context),
    \+ schedule_rules:optimal_time_match(WorkoutID, UserID, TimeOfDay),
    !.

explain_blocked_workout(WorkoutID, _UserID, Context, 'Workout trained too frequently this week') :-
    member(week_schedule(Schedule), Context),
    schedule_rules:workout_frequency_ok(WorkoutID, Schedule, too_frequent),
    !.

explain_blocked_workout(_WorkoutID, UserID, Context, 'Insufficient recovery days in schedule') :-
    member(week_schedule(Schedule), Context),
    \+ recovery_rules:schedule_has_enough_recovery(UserID, Schedule),
    !.

explain_blocked_workout(_, _, _, 'Unknown reason').

% ===========================================================================
% Rule Version Information
% ===========================================================================

rule_version('1.0.0').
rule_last_updated('2025-11-11').

rule_changelog('1.0.0', [
    'Initial modular rule system',
    'Split into recovery, safety, and schedule modules',
    'Added master scheduling and explanation predicates'
]).
