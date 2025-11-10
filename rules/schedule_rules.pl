/*
 * schedule_rules.pl
 * 
 * Workout scheduling and frequency management rules.
 * Handles workout frequency limits, upper/lower split patterns,
 * and time-of-day preferences for optimal training.
 * 
 * Module: schedule_rules
 * Exports: workout_frequency_ok/3, respects_split_pattern/2,
 *          optimal_time_match/3, day_name/2, days_between/3
 */

:- module(schedule_rules, [
    workout_frequency_ok/3,
    muscle_group_frequency_ok/3,
    respects_split_pattern/2,
    is_upper_body/1,
    is_lower_body/1,
    optimal_time_match/3,
    time_intensity_compatible/3,
    day_name/2,
    days_between/3
]).

% ===========================================================================
% Frequency Control Rules
% ===========================================================================

% workout_frequency_ok(+WorkoutType, +WeekSchedule, -Result)
% Checks if adding this workout type would exceed frequency limits
% 
% Rule: Same workout should not be done more than 2x per week
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
% 
% Rule: Same muscle group should not be trained more than 2x per week
muscle_group_frequency_ok(MuscleGroup, WeekSchedule, _KB) :-
    findall(Day, (
        member(day(Day, WorkoutID), WeekSchedule),
        fact(WorkoutID, uses_muscle, MuscleGroup)
    ), Days),
    length(Days, Count),
    Count < 2.

% ===========================================================================
% Upper/Lower Split Pattern Rules
% ===========================================================================

% is_upper_body(+WorkoutID)
% Classifies workout as upper body
is_upper_body(WorkoutID) :-
    attr(WorkoutID, body_part, upper).

% is_lower_body(+WorkoutID)
% Classifies workout as lower body
is_lower_body(WorkoutID) :-
    attr(WorkoutID, body_part, lower).

% respects_split_pattern(+WeekSchedule, -Result)
% Checks if schedule alternates upper/lower body appropriately
% 
% Strategies:
% 1. Empty schedule is balanced
% 2. Single workout is balanced
% 3. Check no consecutive same-type workouts
respects_split_pattern([], balanced) :- !.
respects_split_pattern([_], balanced) :- !.
respects_split_pattern([day(_, rest)|T], Result) :-
    !,
    respects_split_pattern(T, Result).
respects_split_pattern([day(_, Ex1), day(_, rest)|T], Result) :-
    !,
    respects_split_pattern([day(_, Ex1)|T], Result).
respects_split_pattern([day(_, Ex1), day(_, Ex2)|T], Result) :-
    (   (is_upper_body(Ex1), is_lower_body(Ex2))
    ;   (is_lower_body(Ex1), is_upper_body(Ex2))
    ;   \+ is_upper_body(Ex1), \+ is_lower_body(Ex1)
    ;   \+ is_upper_body(Ex2), \+ is_lower_body(Ex2)
    ),
    !,
    respects_split_pattern([day(_, Ex2)|T], Result).
respects_split_pattern([day(_, Ex1), day(_, Ex2)|_], imbalanced) :-
    (   (is_upper_body(Ex1), is_upper_body(Ex2))
    ;   (is_lower_body(Ex1), is_lower_body(Ex2))
    ).

% ===========================================================================
% Time-of-Day Preference Rules
% ===========================================================================

% optimal_time_match(+WorkoutID, +UserID, +TimeOfDay)
% Checks if workout intensity matches user's energy peak time
% 
% Rules:
% - Morning person: hard/medium workouts best in morning
% - Evening person: hard/medium workouts best in evening
% - Afternoon: medium/easy workouts OK anytime
% - No preference: any time OK
optimal_time_match(WorkoutID, UserID, TimeOfDay) :-
    attr(WorkoutID, difficulty, Difficulty),
    (   user_profile(UserID, energy_peak(EnergyPeak))
    ->  time_intensity_compatible(EnergyPeak, Difficulty, TimeOfDay)
    ;   true  % No energy peak preference, any time is OK
    ).

% time_intensity_compatible(+EnergyPeak, +Difficulty, +TimeOfDay)
% Determines if workout difficulty is compatible with time and energy
time_intensity_compatible(morning, hard, morning).
time_intensity_compatible(morning, medium, morning).
time_intensity_compatible(morning, easy, _).
time_intensity_compatible(evening, hard, evening).
time_intensity_compatible(evening, medium, evening).
time_intensity_compatible(evening, easy, _).
time_intensity_compatible(afternoon, medium, _).
time_intensity_compatible(afternoon, easy, _).
time_intensity_compatible(none, easy, _).

% ===========================================================================
% Utility Predicates
% ===========================================================================

% day_name(+DayNumber, -DayName)
% Maps day numbers to day names
day_name(1, monday).
day_name(2, tuesday).
day_name(3, wednesday).
day_name(4, thursday).
day_name(5, friday).
day_name(6, saturday).
day_name(7, sunday).

% days_between(+Day1, +Day2, -Days)
% Calculates number of days between two day numbers (1-7)
days_between(Day1, Day2, Days) :-
    Day1 =< Day2,
    Days is Day2 - Day1.

days_between(Day1, Day2, Days) :-
    Day1 > Day2,
    Days is (7 - Day1) + Day2.
