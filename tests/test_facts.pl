/*
 * test_facts.pl
 * 
 * Controlled test scenarios with 10-30 edge cases for workout planning rules.
 * Tests cover: consecutive training, injuries, fatigue, travel, recovery, timing.
 * 
 * Usage:
 *   swipl -s test_facts.pl
 *   ?- test_scenario(Scenario, Description).
 *   ?- run_edge_case_tests.
 */

:- use_module(library(plunit)).
:- consult('../workout_rules.pl').

% Declare non-contiguous predicates
:- discontiguous entity/3.
:- discontiguous fact/3.
:- discontiguous attr/3.
:- discontiguous user_profile/2.
:- discontiguous injury_affects_muscle/2.

%% Test Entities - Exercises
% These match the workout_rules test data but expanded
entity(e_chest_press, exercise, 'Chest Press').
entity(e_bench_press, exercise, 'Bench Press').
entity(e_squats, exercise, 'Squats').
entity(e_deadlift, exercise, 'Deadlift').
entity(e_bicep_curls, exercise, 'Bicep Curls').
entity(e_shoulder_press, exercise, 'Shoulder Press').
entity(e_lat_pulldown, exercise, 'Lat Pulldown').
entity(e_lunges, exercise, 'Lunges').
entity(e_plank, exercise, 'Plank').
entity(e_yoga_stretch, exercise, 'Yoga Stretch').

%% Exercise Attributes - using 'uses_muscle' for injury checking
fact(e_chest_press, uses_muscle, m_chest).
fact(e_bench_press, uses_muscle, m_chest).
fact(e_squats, uses_muscle, m_quadriceps).
fact(e_deadlift, uses_muscle, m_hamstrings).
fact(e_bicep_curls, uses_muscle, m_biceps).
fact(e_shoulder_press, uses_muscle, m_shoulders).
fact(e_lat_pulldown, uses_muscle, m_back).
fact(e_lunges, uses_muscle, m_quadriceps).
fact(e_plank, uses_muscle, m_core).
fact(e_yoga_stretch, uses_muscle, m_full_body).

attr(e_chest_press, difficulty, hard).
attr(e_bench_press, difficulty, hard).
attr(e_squats, difficulty, hard).
attr(e_deadlift, difficulty, hard).
attr(e_bicep_curls, difficulty, easy).
attr(e_shoulder_press, difficulty, medium).
attr(e_lat_pulldown, difficulty, medium).
attr(e_lunges, difficulty, medium).
attr(e_plank, difficulty, easy).
attr(e_yoga_stretch, difficulty, easy).

attr(e_chest_press, intensity, high).
attr(e_bench_press, intensity, high).
attr(e_squats, intensity, high).
attr(e_deadlift, intensity, high).
attr(e_bicep_curls, intensity, low).
attr(e_shoulder_press, intensity, medium).
attr(e_lat_pulldown, intensity, medium).
attr(e_lunges, intensity, medium).
attr(e_plank, intensity, low).
attr(e_yoga_stretch, intensity, low).

%% Body Part Classification (for split pattern detection)
attr(e_chest_press, body_part, upper).
attr(e_bench_press, body_part, upper).
attr(e_squats, body_part, lower).
attr(e_deadlift, body_part, lower).
attr(e_bicep_curls, body_part, upper).
attr(e_shoulder_press, body_part, upper).
attr(e_lat_pulldown, body_part, upper).
attr(e_lunges, body_part, lower).
attr(e_plank, body_part, core).
attr(e_yoga_stretch, body_part, full_body).

%% Contraindications
fact(e_squats, contraindicated_for, c_knee_injury).
fact(e_deadlift, contraindicated_for, c_back_pain).
fact(e_lunges, contraindicated_for, c_knee_injury).
fact(e_shoulder_press, contraindicated_for, c_shoulder_injury).
fact(e_bench_press, contraindicated_for, c_shoulder_injury).

%% Condition/Entity definitions
entity(c_knee_injury, condition, 'Knee Injury').
entity(c_shoulder_injury, condition, 'Shoulder Injury').
entity(c_back_pain, condition, 'Lower Back Pain').

%% Additional injury-to-muscle mappings for our test conditions
injury_affects_muscle(c_knee_injury, m_quadriceps).
injury_affects_muscle(c_knee_injury, m_hamstrings).
injury_affects_muscle(c_knee_injury, m_calves).
injury_affects_muscle(c_shoulder_injury, m_shoulders).
injury_affects_muscle(c_shoulder_injury, m_chest).
injury_affects_muscle(c_shoulder_injury, m_biceps).
injury_affects_muscle(c_shoulder_injury, m_triceps).
injury_affects_muscle(c_back_pain, m_back).
injury_affects_muscle(c_back_pain, m_core).

%% Test User Profiles
% Format: user_profile(UserID, Attribute) - one fact per attribute

% Scenario 1: Healthy morning person with good sleep
user_profile(u_healthy_morning, sleep_hours(8)).
user_profile(u_healthy_morning, energy_peak(morning)).
user_profile(u_healthy_morning, fitness_level(intermediate)).

% Scenario 2: User with knee injury
user_profile(u_knee_patient, sleep_hours(7)).
user_profile(u_knee_patient, energy_peak(afternoon)).
user_profile(u_knee_patient, injury(c_knee_injury)).
user_profile(u_knee_patient, fitness_level(beginner)).

% Scenario 3: Poor sleep / high fatigue user
user_profile(u_exhausted, sleep_hours(4)).
user_profile(u_exhausted, energy_peak(none)).
user_profile(u_exhausted, fitness_level(intermediate)).

% Scenario 4: Evening person with shoulder injury
user_profile(u_evening_injured, sleep_hours(7)).
user_profile(u_evening_injured, energy_peak(evening)).
user_profile(u_evening_injured, injury(c_shoulder_injury)).
user_profile(u_evening_injured, fitness_level(advanced)).

% Scenario 5: Traveling user (irregular schedule)
user_profile(u_traveler, sleep_hours(6)).
user_profile(u_traveler, energy_peak(afternoon)).
user_profile(u_traveler, fitness_level(intermediate)).
user_profile(u_traveler, special_status(traveling)).

% Scenario 6: Overtraining risk user
user_profile(u_overtrainer, sleep_hours(5)).
user_profile(u_overtrainer, energy_peak(morning)).
user_profile(u_overtrainer, fitness_level(advanced)).
user_profile(u_overtrainer, recent_volume(very_high)).

%% Test Schedules for Edge Cases

% Edge Case 1: Consecutive chest training (3 days in a row)
test_schedule(consecutive_chest, [
    day(1, e_chest_press),
    day(2, e_bench_press),
    day(3, e_chest_press)
]).

% Edge Case 2: Same muscle group trained too frequently
test_schedule(overuse_quads, [
    day(1, e_squats),
    day(2, e_lunges),
    day(4, e_squats),
    day(5, e_lunges)
]).

% Edge Case 3: No recovery days
test_schedule(no_rest, [
    day(1, e_chest_press),
    day(2, e_squats),
    day(3, e_deadlift),
    day(4, e_shoulder_press),
    day(5, e_lat_pulldown),
    day(6, e_lunges),
    day(7, e_plank)
]).

% Edge Case 4: Alternating upper/lower (ideal)
test_schedule(optimal_split, [
    day(1, e_chest_press),
    day(2, e_squats),
    day(3, rest),
    day(4, e_lat_pulldown),
    day(5, e_deadlift),
    day(6, rest),
    day(7, rest)
]).

% Edge Case 5: High intensity every day (poor recovery)
test_schedule(high_intensity_daily, [
    day(1, e_bench_press),
    day(2, e_deadlift),
    day(3, e_squats),
    day(4, e_bench_press),
    day(5, e_deadlift)
]).

% Edge Case 6: Recovery week (low intensity only)
test_schedule(deload_week, [
    day(1, e_yoga_stretch),
    day(3, e_plank),
    day(5, e_bicep_curls),
    day(7, e_yoga_stretch)
]).

% Edge Case 7: User injured mid-week
test_schedule(injury_occurred, [
    day(1, e_chest_press),
    day(2, e_squats),
    day(3, injury_event(c_knee_injury)),
    day(4, e_lunges)  % This should be blocked
]).

%% Test Scenarios with Expected Outcomes

test_scenario(1, 'Healthy user can schedule chest workout in morning', 
    can_schedule_workout(e_chest_press, u_healthy_morning, 
        [week_schedule([]), day_of_week(1), time_of_day(morning)]),
    expected(true)
).

test_scenario(2, 'Knee patient CANNOT do squats',
    safe_for_user(e_squats, u_knee_patient),
    expected(false)
).

test_scenario(3, 'Exhausted user needs 3 recovery days',
    needs_recovery(u_exhausted, Days),
    expected(Days = 3)
).

test_scenario(4, 'Third chest workout in week is blocked',
    workout_frequency_ok(e_chest_press, 
        [day(1, e_chest_press), day(3, e_bench_press)], Result),
    expected(Result = too_frequent)
).

test_scenario(5, 'Evening person should NOT do hard workout in morning',
    optimal_time_match(e_deadlift, u_evening_injured, morning),
    expected(false)
).

test_scenario(6, 'Shoulder injury blocks bench press',
    safe_for_user(e_bench_press, u_evening_injured),
    expected(false)
).

test_scenario(7, 'Shoulder injury allows squats',
    safe_for_user(e_squats, u_evening_injured),
    expected(true)
).

test_scenario(8, 'Schedule with no rest fails recovery check for poor sleep',
    (needs_recovery(u_exhausted, RequiredDays),
     test_schedule(no_rest, Schedule),
     findall(Day, (member(day(Day, _), Schedule)), WorkoutDays),
     length(WorkoutDays, WorkoutCount),
     RestDays is 7 - WorkoutCount,
     RestDays < RequiredDays),
    expected(true)
).

test_scenario(9, 'Optimal split respects upper/lower alternation',
    (test_schedule(optimal_split, Schedule),
     respects_split_pattern(Schedule, Result)),
    expected(Result = balanced)
).

test_scenario(10, 'Consecutive chest training violates frequency rules',
    (test_schedule(consecutive_chest, Schedule),
     member(day(3, LastExercise), Schedule),
     findall(E, (member(day(_, E), Schedule), fact(E, targets, m_chest)), ChestWorkouts),
     length(ChestWorkouts, Count),
     Count > 2),
    expected(true)
).

test_scenario(11, 'Overtrainer with poor sleep needs maximum recovery',
    (user_profile(u_overtrainer, sleep_hours(Hours)),
     Hours < 6,
     needs_recovery(u_overtrainer, Days),
     Days >= 3),
    expected(true)
).

test_scenario(12, 'Traveler with moderate sleep needs moderate recovery',
    needs_recovery(u_traveler, Days),
    expected(Days = 2)
).

test_scenario(13, 'Yoga stretch is safe for all injuries',
    (safe_for_user(e_yoga_stretch, u_knee_patient),
     safe_for_user(e_yoga_stretch, u_evening_injured)),
    expected(true)
).

test_scenario(14, 'Easy exercise OK for exhausted user in afternoon',
    optimal_time_match(e_plank, u_exhausted, afternoon),
    expected(true)
).

test_scenario(15, 'Hard exercise blocked for exhausted user',
    optimal_time_match(e_deadlift, u_exhausted, morning),
    expected(false)
).

%% Plunit Test Suite

:- begin_tests(edge_cases).

test(consecutive_same_muscle, [true(Result = too_frequent)]) :-
    % Should be too_frequent because chest trained 3x in 4 days
    test_schedule(consecutive_chest, Schedule),
    workout_frequency_ok(e_chest_press, Schedule, Result).

test(knee_injury_blocks_squats, [true]) :-
    % Should succeed that squats are NOT safe for knee injury
    \+ safe_for_user(e_squats, u_knee_patient).

test(knee_injury_allows_chest, [true]) :-
    safe_for_user(e_chest_press, u_knee_patient).

test(poor_sleep_needs_recovery, [true(Days >= 3)]) :-
    needs_recovery(u_exhausted, Days).

test(good_sleep_minimal_recovery, [true(Days =< 2)]) :-
    needs_recovery(u_healthy_morning, Days).

test(morning_person_morning_hard, [true]) :-
    optimal_time_match(e_bench_press, u_healthy_morning, morning).

test(morning_person_evening_hard, [true]) :-
    % Should succeed that morning person should NOT do hard workout in evening
    \+ optimal_time_match(e_bench_press, u_healthy_morning, evening).

test(evening_person_evening_hard, [true]) :-
    optimal_time_match(e_deadlift, u_evening_injured, evening).

test(shoulder_injury_blocks_press, [true]) :-
    % Should succeed that press is NOT safe for shoulder injury
    \+ safe_for_user(e_shoulder_press, u_evening_injured).

test(shoulder_injury_allows_legs, [true]) :-
    safe_for_user(e_squats, u_evening_injured).

test(optimal_split_is_balanced, [true]) :-
    test_schedule(optimal_split, Schedule),
    respects_split_pattern(Schedule, balanced).

test(no_rest_schedule_imbalanced, [true]) :-
    test_schedule(no_rest, Schedule),
    \+ respects_split_pattern(Schedule, balanced).

test(overuse_quad_frequency, [true]) :-
    % Check that 4 quad workouts in a week is too much
    test_schedule(overuse_quads, Schedule),
    findall(E, (member(day(_, E), Schedule), fact(E, targets, m_quadriceps)), QuadWorkouts),
    length(QuadWorkouts, Count),
    Count >= 4.

test(deload_week_all_easy, [true]) :-
    test_schedule(deload_week, Schedule),
    forall(
        member(day(_, Exercise), Schedule),
        (attr(Exercise, difficulty, Diff), Diff = easy)
    ).

test(injury_mid_week_blocks_subsequent, [true]) :-
    % Lunges on day 4 should NOT be safe for knee injury
    test_schedule(injury_occurred, Schedule),
    member(day(4, e_lunges), Schedule),
    \+ safe_for_user(e_lunges, u_knee_patient).

:- end_tests(edge_cases).

%% Interactive Test Runner

run_edge_case_tests :-
    writeln('=== Running Edge Case Tests ==='),
    nl,
    run_tests(edge_cases),
    nl,
    writeln('=== Running Scenario Validation ==='),
    nl,
    run_scenario_tests.

run_scenario_tests :-
    forall(
        test_scenario(N, Description, Goal, expected(Expected)),
        (
            format('~n[Scenario ~d] ~w~n', [N, Description]),
            format('  Query: ~w~n', [Goal]),
            format('  Expected: ~w~n', [Expected]),
            (   catch(Goal, Error, (format('  ERROR: ~w~n', [Error]), fail))
            ->  format('  Result: PASS ✓~n', [])
            ;   format('  Result: FAIL ✗~n', [])
            )
        )
    ).

%% Query Helper for Manual Testing

query_helper :-
    writeln('=== Workout Rule Query Helper ==='),
    writeln('Available test users:'),
    writeln('  - u_healthy_morning (good sleep, morning energy)'),
    writeln('  - u_knee_patient (knee injury)'),
    writeln('  - u_exhausted (poor sleep, high fatigue)'),
    writeln('  - u_evening_injured (evening energy, shoulder injury)'),
    writeln('  - u_traveler (traveling, moderate sleep)'),
    writeln('  - u_overtrainer (overtraining risk)'),
    nl,
    writeln('Available exercises:'),
    writeln('  - e_chest_press, e_bench_press (chest)'),
    writeln('  - e_squats, e_lunges (legs)'),
    writeln('  - e_deadlift (hamstrings)'),
    writeln('  - e_shoulder_press, e_lat_pulldown (upper)'),
    writeln('  - e_plank, e_yoga_stretch (easy)'),
    nl,
    writeln('Example queries:'),
    writeln('  ?- safe_for_user(e_squats, u_knee_patient).'),
    writeln('  ?- needs_recovery(u_exhausted, Days).'),
    writeln('  ?- optimal_time_match(e_bench_press, u_healthy_morning, morning).'),
    writeln('  ?- test_schedule(optimal_split, Schedule), respects_split_pattern(Schedule, Result).'),
    nl.

%% Auto-load helper on startup
:- initialization(query_helper, after_load).
