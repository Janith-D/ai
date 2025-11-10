% workout_rules.pl - Advanced workout planning rules
%
% This module contains complex workout scheduling and planning rules that
% consider user constraints, recovery, and optimization principles.

:- module(workout_rules, [
    can_schedule_workout/3,
    suggest_workout_day/3,
    needs_recovery/2,
    workout_frequency_ok/3,
    respects_split_pattern/2,
    safe_for_user/2,
    optimal_time_match/3,
    is_upper_body/1,
    is_lower_body/1,
    muscle_group_frequency_ok/3,
    schedule_has_enough_recovery/2,
    injury_affects_muscle/2,
    workout_avoids_injury/2,
    time_intensity_compatible/3,
    has_muscle_overlap/2,
    day_name/2,
    days_between/3
]).

% ============================================================================
% Rule 1: Workout Frequency Control
% Avoid repeating same workout/muscle group more than 2× per week
% ============================================================================

% workout_frequency_ok(+WorkoutType, +WeekSchedule, -Result)
% Checks if adding this workout type would exceed frequency limits
workout_frequency_ok(WorkoutType, WeekSchedule, ok) :-
    findall(W, (member(day(_, W), WeekSchedule), W = WorkoutType), Occurrences),
    length(Occurrences, Count),
    Count < 2.

workout_frequency_ok(WorkoutType, WeekSchedule, too_frequent) :-
    findall(W, (member(day(_, W), WeekSchedule), W = WorkoutType), Occurrences),
    length(Occurrences, Count),
    Count >= 2.

% muscle_group_frequency_ok(+MuscleGroup, +WeekSchedule, +_KB)
% Checks if a muscle group has been trained too frequently
muscle_group_frequency_ok(MuscleGroup, WeekSchedule, _KB) :-
    findall(Day, (
        member(day(Day, WorkoutID), WeekSchedule),
        fact(WorkoutID, uses_muscle, MuscleGroup)
    ), Days),
    length(Days, Count),
    Count < 2.

% ============================================================================
% Rule 2: Upper/Lower Body Split Pattern
% Prefer alternating upper/lower body workouts
% ============================================================================

% is_upper_body(+WorkoutID)
% Classifies workout as upper body based on primary muscles
is_upper_body(WorkoutID) :-
    fact(WorkoutID, uses_muscle, Muscle),
    member(Muscle, [m_chest, m_back, m_shoulders, m_biceps, m_triceps]).

% is_lower_body(+WorkoutID)
% Classifies workout as lower body based on primary muscles
is_lower_body(WorkoutID) :-
    fact(WorkoutID, uses_muscle, Muscle),
    member(Muscle, [m_quadriceps, m_hamstrings, m_glutes, m_calves]).

% respects_split_pattern(+Schedule, -Pattern)
% Validates that schedule alternates upper/lower or returns pattern type
% Returns 'balanced' if all consecutive exercise pairs alternate upper/lower
% Returns 'imbalanced' if any pair has same muscle group

% Base cases - empty or single exercise is balanced
respects_split_pattern([], balanced) :- !.
respects_split_pattern([_], balanced) :- !.

% Skip rest days at start
respects_split_pattern([day(_, rest)|T], Pattern) :-
    !, respects_split_pattern(T, Pattern).

% Skip rest days between exercises
respects_split_pattern([day(_, W1), day(_, rest)|T], Pattern) :-
    W1 \= rest, !,
    respects_split_pattern([day(_, W1)|T], Pattern).

% Check if two consecutive exercises are same muscle group (imbalanced)
respects_split_pattern([day(_, W1), day(_, W2)|T], imbalanced) :-
    W1 \= rest, W2 \= rest,
    ( (is_upper_body(W1), is_upper_body(W2))
    ; (is_lower_body(W1), is_lower_body(W2))
    ), !.

% Check if two consecutive exercises alternate (continue checking rest)
respects_split_pattern([day(_, W1), day(_, W2)|T], Pattern) :-
    W1 \= rest, W2 \= rest,
    ( (is_upper_body(W1), is_lower_body(W2))
    ; (is_lower_body(W1), is_upper_body(W2))
    ), !,
    respects_split_pattern([day(_, W2)|T], Pattern).

% ============================================================================
% Rule 3: Recovery Based on Sleep
% Add recovery days based on sleep quality/hours
% ============================================================================

% needs_recovery(+UserID, -RecoveryDaysPerWeek)
% Determines required recovery days based on sleep patterns
needs_recovery(UserID, 3) :-
    user_profile(UserID, sleep_hours(Hours)),
    Hours < 6.

needs_recovery(UserID, 2) :-
    user_profile(UserID, sleep_hours(Hours)),
    Hours >= 6, Hours < 7.

needs_recovery(UserID, 1) :-
    user_profile(UserID, sleep_hours(Hours)),
    Hours >= 7, Hours < 8.

needs_recovery(UserID, 1) :-
    user_profile(UserID, sleep_hours(Hours)),
    Hours >= 8.

% Default if sleep hours not specified
needs_recovery(UserID, 2) :-
    \+ user_profile(UserID, sleep_hours(_)).

% schedule_has_enough_recovery(+Schedule, +RequiredRecoveryDays)
% Validates schedule has sufficient rest days
schedule_has_enough_recovery(Schedule, RequiredDays) :-
    findall(Day, member(day(Day, rest), Schedule), RestDays),
    length(RestDays, Count),
    Count >= RequiredDays.

% ============================================================================
% Rule 4: Injury Constraints
% Respect injury constraints - no training of injured area
% ============================================================================

% safe_for_user(+WorkoutID, +UserID)
% Checks if workout is safe given user's injuries
safe_for_user(_WorkoutID, UserID) :-
    % First check: If user has no injuries, any workout is safe
    \+ user_profile(UserID, injury(_)),
    !.  % Cut to prevent backtracking

% If user has injuries, check muscle overlap
safe_for_user(WorkoutID, UserID) :-
    user_profile(UserID, injury(_)),  % User must have injuries for this clause
    !,  % Cut after checking user has injuries
    % Get all muscles used by this workout
    findall(Muscle, fact(WorkoutID, uses_muscle, Muscle), UsedMuscles),
    % Ensure workout uses some muscles (otherwise fail safe)
    UsedMuscles \= [],
    % Get user's injured areas
    findall(InjuryMuscle, (
        user_profile(UserID, injury(Condition)),
        injury_affects_muscle(Condition, InjuryMuscle)
    ), InjuredMuscles),
    % Ensure we found injured muscles
    InjuredMuscles \= [],
    % Workout is safe only if there's NO overlap
    \+ has_muscle_overlap(UsedMuscles, InjuredMuscles).

% Helper predicate to check muscle overlap
has_muscle_overlap(UsedMuscles, InjuredMuscles) :-
    member(M, UsedMuscles),
    member(M, InjuredMuscles),
    !.  % Cut after finding first overlap

% injury_affects_muscle(+Condition, -Muscle)
% Maps injury conditions to affected muscle groups
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

% ============================================================================
% Rule 5: Time-of-Day Preferences
% Match workout intensity to user's energy patterns
% ============================================================================

% optimal_time_match(+WorkoutID, +UserID, +TimeOfDay)
% Checks if workout intensity matches user's energy at given time
optimal_time_match(WorkoutID, UserID, TimeOfDay) :-
    user_profile(UserID, energy_peak(UserPeak)),
    attr(WorkoutID, difficulty, Difficulty),
    time_intensity_compatible(TimeOfDay, UserPeak, Difficulty).

% Default: if no preference specified, any time is OK
optimal_time_match(_, UserID, _) :-
    \+ user_profile(UserID, energy_peak(_)).

% time_intensity_compatible(+TimeOfDay, +EnergyPeak, +Difficulty)
% Rules for matching workout intensity to time and user preference
time_intensity_compatible(morning, morning, hard).
time_intensity_compatible(morning, morning, medium).
time_intensity_compatible(morning, evening, easy).
time_intensity_compatible(evening, evening, hard).
time_intensity_compatible(evening, evening, medium).
time_intensity_compatible(evening, morning, easy).
time_intensity_compatible(afternoon, _, medium).
time_intensity_compatible(afternoon, _, easy).
time_intensity_compatible(_, _, beginner).

% ============================================================================
% Combined Rule: Can Schedule Workout
% Master rule combining all constraints
% ============================================================================

% can_schedule_workout(+WorkoutID, +UserID, +Context)
% Context includes: week_schedule, day_of_week, time_of_day
can_schedule_workout(WorkoutID, UserID, Context) :-
    % Extract context
    member(week_schedule(Schedule), Context),
    member(day_of_week(Day), Context),
    member(time_of_day(Time), Context),
    
    % Check all constraints
    entity(WorkoutID, exercise, _),
    safe_for_user(WorkoutID, UserID),
    optimal_time_match(WorkoutID, UserID, Time),
    
    % Check frequency (workout not already used twice this week)
    findall(W, (member(day(_, W), Schedule), W = WorkoutID), Uses),
    length(Uses, UseCount),
    UseCount < 2,
    
    % Check recovery requirements
    needs_recovery(UserID, RequiredRest),
    schedule_has_enough_recovery([day(Day, WorkoutID)|Schedule], RequiredRest).

% ============================================================================
% Helper: Suggest Workout Day
% Suggests best day of week for a workout given constraints
% ============================================================================

% suggest_workout_day(+UserID, +WeekSchedule, -SuggestedDay)
% Analyzes the week and suggests the best day for next workout
suggest_workout_day(UserID, WeekSchedule, SuggestedDay) :-
    % Find days without workouts
    findall(D, between(1, 7, D), AllDays),
    findall(UsedDay, member(day(UsedDay, _), WeekSchedule), UsedDays),
    subtract(AllDays, UsedDays, AvailableDays),
    AvailableDays \= [],
    
    % Prefer day that maintains alternating pattern
    member(SuggestedDay, AvailableDays),
    
    % Check it maintains recovery requirements
    needs_recovery(UserID, RequiredRest),
    % Count rest days in proposed schedule
    findall(R, member(day(_, rest), [day(SuggestedDay, workout)|WeekSchedule]), RestDays),
    length(RestDays, RestCount),
    RestCount >= RequiredRest.

% ============================================================================
% Utility Predicates
% ============================================================================

% day_name(?DayNumber, ?DayName)
day_name(1, monday).
day_name(2, tuesday).
day_name(3, wednesday).
day_name(4, thursday).
day_name(5, friday).
day_name(6, saturday).
day_name(7, sunday).

% days_between(+Day1, +Day2, -Count)
% Calculate days between two day numbers (1-7)
days_between(D1, D2, Count) :-
    D1 =< D2,
    Count is D2 - D1.
days_between(D1, D2, Count) :-
    D1 > D2,
    Count is (7 - D1) + D2.
