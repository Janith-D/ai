% tests/workout_rules_tests.pl - Unit tests for advanced workout planning rules
%
% Run with: swipl -s tests/workout_rules_tests.pl -g "run_tests,halt."

:- use_module(library(plunit)).
:- consult('../workout_rules.pl').

% ============================================================================
% Test Data Setup
% ============================================================================

% Sample entities for testing
:- dynamic entity/3.
:- dynamic fact/3.
:- dynamic attr/3.
:- dynamic user_profile/2.

% Exercises
entity(e_chest_1, exercise, 'Bench Press').
entity(e_chest_2, exercise, 'Push-ups').
entity(e_legs_1, exercise, 'Squats').
entity(e_legs_2, exercise, 'Lunges').
entity(e_back_1, exercise, 'Pull-ups').
entity(e_shoulders_1, exercise, 'Overhead Press').

% Muscles
entity(m_chest, muscle, 'Chest').
entity(m_back, muscle, 'Back').
entity(m_shoulders, muscle, 'Shoulders').
entity(m_quadriceps, muscle, 'Quadriceps').
entity(m_hamstrings, muscle, 'Hamstrings').
entity(m_core, muscle, 'Core').

% Conditions
entity(cond_knee, condition, 'Knee Injury').
entity(cond_shoulder, condition, 'Shoulder Injury').

% Exercise muscle relationships
fact(e_chest_1, uses_muscle, m_chest).
fact(e_chest_2, uses_muscle, m_chest).
fact(e_legs_1, uses_muscle, m_quadriceps).
fact(e_legs_1, uses_muscle, m_hamstrings).
fact(e_legs_2, uses_muscle, m_quadriceps).
fact(e_back_1, uses_muscle, m_back).
fact(e_shoulders_1, uses_muscle, m_shoulders).

% Exercise contraindications
fact(e_legs_1, contraindicated_for, cond_knee).
fact(e_legs_2, contraindicated_for, cond_knee).
fact(e_shoulders_1, contraindicated_for, cond_shoulder).

% Exercise attributes
attr(e_chest_1, difficulty, hard).
attr(e_chest_2, difficulty, medium).
attr(e_legs_1, difficulty, hard).
attr(e_legs_2, difficulty, medium).
attr(e_back_1, difficulty, hard).
attr(e_shoulders_1, difficulty, medium).

% Test users
user_profile(u_good_sleep, sleep_hours(8)).
user_profile(u_poor_sleep, sleep_hours(5)).
user_profile(u_moderate_sleep, sleep_hours(6.5)).
user_profile(u_knee_injury, injury(cond_knee)).
user_profile(u_morning_person, energy_peak(morning)).
user_profile(u_evening_person, energy_peak(evening)).

% ============================================================================
% Test Suite 1: Workout Frequency Control
% ============================================================================

:- begin_tests(workout_frequency).

test(empty_schedule_allows_workout) :-
    workout_frequency_ok(e_chest_1, [], ok).

test(one_occurrence_allows_second) :-
    Schedule = [day(1, e_chest_1)],
    workout_frequency_ok(e_chest_1, Schedule, ok).

test(two_occurrences_blocks_third) :-
    Schedule = [day(1, e_chest_1), day(3, e_chest_1)],
    workout_frequency_ok(e_chest_1, Schedule, too_frequent).

test(different_workouts_do_not_conflict) :-
    Schedule = [day(1, e_chest_1), day(2, e_legs_1)],
    workout_frequency_ok(e_chest_1, Schedule, ok).

:- end_tests(workout_frequency).

% ============================================================================
% Test Suite 2: Upper/Lower Body Split
% ============================================================================

:- begin_tests(split_pattern).

test(chest_is_upper_body) :-
    is_upper_body(e_chest_1).

test(legs_is_lower_body) :-
    is_lower_body(e_legs_1).

test(empty_schedule_is_balanced) :-
    respects_split_pattern([], balanced).

test(alternating_upper_lower_is_good) :-
    Schedule = [day(1, e_chest_1), day(2, e_legs_1), day(3, e_back_1)],
    respects_split_pattern(Schedule, alternating).

test(same_group_consecutive_detected) :-
    Schedule = [day(1, e_chest_1), day(2, e_back_1)],
    respects_split_pattern(Schedule, same_group).

test(rest_days_do_not_break_pattern) :-
    Schedule = [day(1, e_chest_1), day(2, rest), day(3, e_legs_1)],
    respects_split_pattern(Schedule, Pattern),
    member(Pattern, [alternating, balanced]).

:- end_tests(split_pattern).

% ============================================================================
% Test Suite 3: Recovery Based on Sleep
% ============================================================================

:- begin_tests(recovery).

test(poor_sleep_needs_more_recovery) :-
    needs_recovery(u_poor_sleep, Days),
    Days >= 3.

test(good_sleep_needs_less_recovery) :-
    needs_recovery(u_good_sleep, Days),
    Days =< 2.

test(moderate_sleep_needs_moderate_recovery) :-
    needs_recovery(u_moderate_sleep, Days),
    Days =:= 2.

test(schedule_with_enough_rest) :-
    Schedule = [
        day(1, e_chest_1),
        day(2, rest),
        day(3, e_legs_1),
        day(4, rest),
        day(5, e_back_1)
    ],
    schedule_has_enough_recovery(Schedule, 2).

test(schedule_without_enough_rest_fails) :-
    Schedule = [
        day(1, e_chest_1),
        day(2, e_legs_1),
        day(3, e_back_1)
    ],
    \+ schedule_has_enough_recovery(Schedule, 2).

:- end_tests(recovery).

% ============================================================================
% Test Suite 4: Injury Constraints
% ============================================================================

:- begin_tests(injury_safety).

test(safe_workout_for_healthy_user) :-
    safe_for_user(e_chest_1, u_good_sleep).

test(unsafe_workout_for_knee_injury) :-
    \+ safe_for_user(e_legs_1, u_knee_injury).

test(chest_workout_safe_for_knee_injury) :-
    safe_for_user(e_chest_1, u_knee_injury).

test(injury_affects_correct_muscles) :-
    injury_affects_muscle(cond_knee, m_quadriceps),
    injury_affects_muscle(cond_knee, m_hamstrings).

test(workout_avoids_contraindication) :-
    workout_avoids_injury(e_chest_1, cond_knee).

test(workout_does_not_avoid_contraindication) :-
    \+ workout_avoids_injury(e_legs_1, cond_knee).

:- end_tests(injury_safety).

% ============================================================================
% Test Suite 5: Time-of-Day Preferences
% ============================================================================

:- begin_tests(time_preferences).

test(morning_person_hard_workout_morning) :-
    optimal_time_match(e_chest_1, u_morning_person, morning).

test(morning_person_hard_workout_evening_fails) :-
    \+ optimal_time_match(e_chest_1, u_morning_person, evening).

test(evening_person_hard_workout_evening) :-
    optimal_time_match(e_legs_1, u_evening_person, evening).

test(afternoon_medium_workout_always_ok) :-
    optimal_time_match(e_chest_2, u_morning_person, afternoon),
    optimal_time_match(e_chest_2, u_evening_person, afternoon).

test(user_without_preference_accepts_any_time) :-
    optimal_time_match(e_chest_1, u_good_sleep, morning),
    optimal_time_match(e_chest_1, u_good_sleep, evening).

:- end_tests(time_preferences).

% ============================================================================
% Test Suite 6: Combined Scheduling Logic
% ============================================================================

:- begin_tests(combined_scheduling).

test(can_schedule_valid_workout) :-
    Context = [
        week_schedule([day(1, e_chest_1)]),
        day_of_week(3),
        time_of_day(morning)
    ],
    can_schedule_workout(e_legs_1, u_morning_person, Context).

test(cannot_schedule_too_frequent_workout) :-
    Context = [
        week_schedule([day(1, e_chest_1), day(3, e_chest_1)]),
        day_of_week(5),
        time_of_day(morning)
    ],
    \+ can_schedule_workout(e_chest_1, u_morning_person, Context).

test(cannot_schedule_unsafe_workout) :-
    Context = [
        week_schedule([]),
        day_of_week(1),
        time_of_day(morning)
    ],
    \+ can_schedule_workout(e_legs_1, u_knee_injury, Context).

test(cannot_schedule_wrong_time_high_intensity) :-
    Context = [
        week_schedule([]),
        day_of_week(1),
        time_of_day(evening)
    ],
    \+ can_schedule_workout(e_chest_1, u_morning_person, Context).

:- end_tests(combined_scheduling).

% ============================================================================
% Test Suite 7: Workout Day Suggestions
% ============================================================================

:- begin_tests(day_suggestions).

test(suggest_day_for_new_week) :-
    suggest_workout_day(u_good_sleep, [], Day),
    between(1, 7, Day).

test(suggest_day_respects_existing_schedule) :-
    Schedule = [day(1, e_chest_1), day(2, rest)],
    suggest_workout_day(u_good_sleep, Schedule, Day),
    Day > 2.

test(suggest_day_ensures_recovery) :-
    Schedule = [day(1, e_chest_1)],
    suggest_workout_day(u_poor_sleep, Schedule, Day),
    % Poor sleep user needs 3 rest days, so schedule should accommodate
    Day > 1.

:- end_tests(day_suggestions).

% ============================================================================
% Test Suite 8: Utility Functions
% ============================================================================

:- begin_tests(utilities).

test(day_name_mapping) :-
    day_name(1, monday),
    day_name(7, sunday).

test(days_between_forward) :-
    days_between(1, 5, 4).

test(days_between_wrap_around) :-
    days_between(6, 2, 3).

:- end_tests(utilities).

% ============================================================================
% Run all tests
% ============================================================================

% This will run when the file is consulted with -g "run_tests,halt."
