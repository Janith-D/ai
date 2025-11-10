% kb_generated.pl - synthetic fitness KB (200 exercises)
% Generated: synthetic dataset for testing Prolog reasoning

% Suppress warnings about clauses of the same predicate being non-contiguous.
% The generator intentionally emits interleaved fact/3, attr/3 and entity/3
% clauses for readability / incremental emission. These directives are safe
% for a facts-only knowledge base.
:- discontiguous fact/3.
:- discontiguous attr/3.
:- discontiguous entity/3.

% Muscles
entity(m_quadriceps, muscle, 'Quadriceps').
entity(m_glutes, muscle, 'Glutes').
entity(m_hamstrings, muscle, 'Hamstrings').
entity(m_calves, muscle, 'Calves').
entity(m_chest, muscle, 'Pectoralis Major').
entity(m_back, muscle, 'Back').
entity(m_shoulders, muscle, 'Shoulders').
entity(m_biceps, muscle, 'Biceps').
entity(m_triceps, muscle, 'Triceps').
entity(m_core, muscle, 'Core').

% Equipment
entity(eq_barbell, equipment, 'Barbell').
entity(eq_dumbbell, equipment, 'Dumbbell').
entity(eq_leg_press_machine, equipment, 'Leg Press Machine').
entity(eq_kettlebell, equipment, 'Kettlebell').
entity(eq_bench, equipment, 'Bench').
entity(eq_cable, equipment, 'Cable Machine').
entity(eq_none, equipment, 'None').

% Conditions
entity(cond_knee, condition, 'Knee Injury').
entity(cond_low_back, condition, 'Low Back Pain').
entity(cond_shoulder, condition, 'Shoulder Injury').

% Goals
entity(g_goal_strength, goal, 'Increase Strength').
entity(g_goal_endurance, goal, 'Improve Endurance').

% A test user
entity(u_test, user, 'TestUser').
user_profile(u_test, age(30)).
user_profile(u_test, weight(75)).
user_profile(u_test, goal(g_goal_strength)).
user_has_equipment(u_test, eq_barbell).
user_has_equipment(u_test, eq_dumbbell).

% Base exercise types (20 bases) repeated to create 200 variants
% Order: 1 squat,2 lunge,3 deadlift,4 bench_press,5 row,6 shoulder_press,7 biceps_curl,8 triceps_extension,9 plank,10 jump_rope,
% 11 leg_press,12 glute_bridge,13 pushup,14 pullup,15 kettlebell_swing,16 box_jump,17 burpee,18 mountain_climber,19 cycling,20 rowing_machine

% Generate 200 exercises: e_ex_001 .. e_ex_200

% Manually emitted entities and facts for each exercise variant

% Exercise variants (20 base types x 10 variants = 200)

% base mapping helpers (for human readers)
% 1 squat
% 2 lunge
% 3 deadlift
% 4 bench_press
% 5 row
% 6 shoulder_press
% 7 biceps_curl
% 8 triceps_extension
% 9 plank
% 10 jump_rope
% 11 leg_press
% 12 glute_bridge
% 13 pushup
% 14 pullup
% 15 kettlebell_swing
% 16 box_jump
% 17 burpee
% 18 mountain_climber
% 19 cycling
% 20 rowing_machine

% We'll emit each exercise entity and a few associated facts.

% --- Generated exercises -----------------------------------------------------

% helper: zero-padded id formatter used below: e_ex_001 .. e_ex_200

% Variant loop: for variant V = 1..10

% Variant 1..10, Base types 1..20

% We'll now list the 200 exercises (entity + uses_muscle + requires_equipment + attr + some contraindications/substitutions/progression)

% --- Variant 1 (indices 1..20)
entity(e_ex_001, exercise, 'Squat variant 1').
fact(e_ex_001, uses_muscle, m_quadriceps).
fact(e_ex_001, uses_muscle, m_glutes).
fact(e_ex_001, requires_equipment, eq_barbell).
attr(e_ex_001, difficulty, easy).

entity(e_ex_002, exercise, 'Lunge variant 1').
fact(e_ex_002, uses_muscle, m_quadriceps).
fact(e_ex_002, uses_muscle, m_glutes).
fact(e_ex_002, requires_equipment, eq_none).
attr(e_ex_002, difficulty, easy).

entity(e_ex_003, exercise, 'Deadlift variant 1').
fact(e_ex_003, uses_muscle, m_hamstrings).
fact(e_ex_003, uses_muscle, m_glutes).
fact(e_ex_003, requires_equipment, eq_barbell).
attr(e_ex_003, difficulty, easy).

entity(e_ex_004, exercise, 'Bench press variant 1').
fact(e_ex_004, uses_muscle, m_chest).
fact(e_ex_004, uses_muscle, m_triceps).
fact(e_ex_004, requires_equipment, eq_barbell).
attr(e_ex_004, difficulty, easy).

entity(e_ex_005, exercise, 'Row variant 1').
fact(e_ex_005, uses_muscle, m_back).
fact(e_ex_005, uses_muscle, m_biceps).
fact(e_ex_005, requires_equipment, eq_dumbbell).
attr(e_ex_005, difficulty, easy).

entity(e_ex_006, exercise, 'Shoulder press variant 1').
fact(e_ex_006, uses_muscle, m_shoulders).
fact(e_ex_006, uses_muscle, m_triceps).
fact(e_ex_006, requires_equipment, eq_dumbbell).
attr(e_ex_006, difficulty, easy).

entity(e_ex_007, exercise, 'Biceps curl variant 1').
fact(e_ex_007, uses_muscle, m_biceps).
fact(e_ex_007, requires_equipment, eq_dumbbell).
attr(e_ex_007, difficulty, easy).

entity(e_ex_008, exercise, 'Triceps extension variant 1').
fact(e_ex_008, uses_muscle, m_triceps).
fact(e_ex_008, requires_equipment, eq_dumbbell).
attr(e_ex_008, difficulty, easy).

entity(e_ex_009, exercise, 'Plank variant 1').
fact(e_ex_009, uses_muscle, m_core).
fact(e_ex_009, requires_equipment, eq_none).
attr(e_ex_009, difficulty, easy).

entity(e_ex_010, exercise, 'Jump rope variant 1').
fact(e_ex_010, uses_muscle, m_calves).
fact(e_ex_010, requires_equipment, eq_none).
attr(e_ex_010, difficulty, easy).

entity(e_ex_011, exercise, 'Leg press variant 1').
fact(e_ex_011, uses_muscle, m_quadriceps).
fact(e_ex_011, requires_equipment, eq_leg_press_machine).
attr(e_ex_011, difficulty, easy).

entity(e_ex_012, exercise, 'Glute bridge variant 1').
fact(e_ex_012, uses_muscle, m_glutes).
fact(e_ex_012, requires_equipment, eq_none).
attr(e_ex_012, difficulty, easy).

entity(e_ex_013, exercise, 'Push-up variant 1').
fact(e_ex_013, uses_muscle, m_chest).
fact(e_ex_013, uses_muscle, m_triceps).
fact(e_ex_013, requires_equipment, eq_none).
attr(e_ex_013, difficulty, easy).

entity(e_ex_014, exercise, 'Pull-up variant 1').
fact(e_ex_014, uses_muscle, m_back).
fact(e_ex_014, uses_muscle, m_biceps).
fact(e_ex_014, requires_equipment, eq_none).
attr(e_ex_014, difficulty, easy).

entity(e_ex_015, exercise, 'Kettlebell swing variant 1').
fact(e_ex_015, uses_muscle, m_glutes).
fact(e_ex_015, uses_muscle, m_hamstrings).
fact(e_ex_015, requires_equipment, eq_kettlebell).
attr(e_ex_015, difficulty, easy).

entity(e_ex_016, exercise, 'Box jump variant 1').
fact(e_ex_016, uses_muscle, m_quadriceps).
fact(e_ex_016, uses_muscle, m_glutes).
fact(e_ex_016, requires_equipment, eq_none).
attr(e_ex_016, difficulty, easy).

entity(e_ex_017, exercise, 'Burpee variant 1').
fact(e_ex_017, uses_muscle, m_quadriceps).
fact(e_ex_017, uses_muscle, m_chest).
fact(e_ex_017, requires_equipment, eq_none).
attr(e_ex_017, difficulty, easy).

entity(e_ex_018, exercise, 'Mountain climber variant 1').
fact(e_ex_018, uses_muscle, m_core).
fact(e_ex_018, requires_equipment, eq_none).
attr(e_ex_018, difficulty, easy).

entity(e_ex_019, exercise, 'Cycling variant 1').
fact(e_ex_019, uses_muscle, m_quadriceps).
fact(e_ex_019, requires_equipment, eq_none).
attr(e_ex_019, difficulty, easy).

entity(e_ex_020, exercise, 'Rowing machine variant 1').
fact(e_ex_020, uses_muscle, m_back).
fact(e_ex_020, uses_muscle, m_quadriceps).
fact(e_ex_020, requires_equipment, eq_none).
attr(e_ex_020, difficulty, easy).

% --- Variant 2 (indices 21..40)
entity(e_ex_021, exercise, 'Squat variant 2').
fact(e_ex_021, uses_muscle, m_quadriceps).
fact(e_ex_021, uses_muscle, m_glutes).
fact(e_ex_021, requires_equipment, eq_barbell).
attr(e_ex_021, difficulty, medium).

entity(e_ex_022, exercise, 'Lunge variant 2').
fact(e_ex_022, uses_muscle, m_quadriceps).
fact(e_ex_022, uses_muscle, m_glutes).
fact(e_ex_022, requires_equipment, eq_none).
attr(e_ex_022, difficulty, medium).

entity(e_ex_023, exercise, 'Deadlift variant 2').
fact(e_ex_023, uses_muscle, m_hamstrings).
fact(e_ex_023, uses_muscle, m_glutes).
fact(e_ex_023, requires_equipment, eq_barbell).
attr(e_ex_023, difficulty, medium).

entity(e_ex_024, exercise, 'Bench press variant 2').
fact(e_ex_024, uses_muscle, m_chest).
fact(e_ex_024, uses_muscle, m_triceps).
fact(e_ex_024, requires_equipment, eq_barbell).
attr(e_ex_024, difficulty, medium).

entity(e_ex_025, exercise, 'Row variant 2').
fact(e_ex_025, uses_muscle, m_back).
fact(e_ex_025, uses_muscle, m_biceps).
fact(e_ex_025, requires_equipment, eq_dumbbell).
attr(e_ex_025, difficulty, medium).

entity(e_ex_026, exercise, 'Shoulder press variant 2').
fact(e_ex_026, uses_muscle, m_shoulders).
fact(e_ex_026, uses_muscle, m_triceps).
fact(e_ex_026, requires_equipment, eq_dumbbell).
attr(e_ex_026, difficulty, medium).

entity(e_ex_027, exercise, 'Biceps curl variant 2').
fact(e_ex_027, uses_muscle, m_biceps).
fact(e_ex_027, requires_equipment, eq_dumbbell).
attr(e_ex_027, difficulty, medium).

entity(e_ex_028, exercise, 'Triceps extension variant 2').
fact(e_ex_028, uses_muscle, m_triceps).
fact(e_ex_028, requires_equipment, eq_dumbbell).
attr(e_ex_028, difficulty, medium).

entity(e_ex_029, exercise, 'Plank variant 2').
fact(e_ex_029, uses_muscle, m_core).
fact(e_ex_029, requires_equipment, eq_none).
attr(e_ex_029, difficulty, medium).

entity(e_ex_030, exercise, 'Jump rope variant 2').
fact(e_ex_030, uses_muscle, m_calves).
fact(e_ex_030, requires_equipment, eq_none).
attr(e_ex_030, difficulty, medium).

entity(e_ex_031, exercise, 'Leg press variant 2').
fact(e_ex_031, uses_muscle, m_quadriceps).
fact(e_ex_031, requires_equipment, eq_leg_press_machine).
attr(e_ex_031, difficulty, medium).

entity(e_ex_032, exercise, 'Glute bridge variant 2').
fact(e_ex_032, uses_muscle, m_glutes).
fact(e_ex_032, requires_equipment, eq_none).
attr(e_ex_032, difficulty, medium).

entity(e_ex_033, exercise, 'Push-up variant 2').
fact(e_ex_033, uses_muscle, m_chest).
fact(e_ex_033, uses_muscle, m_triceps).
fact(e_ex_033, requires_equipment, eq_none).
attr(e_ex_033, difficulty, medium).

entity(e_ex_034, exercise, 'Pull-up variant 2').
fact(e_ex_034, uses_muscle, m_back).
fact(e_ex_034, uses_muscle, m_biceps).
fact(e_ex_034, requires_equipment, eq_none).
attr(e_ex_034, difficulty, medium).

entity(e_ex_035, exercise, 'Kettlebell swing variant 2').
fact(e_ex_035, uses_muscle, m_glutes).
fact(e_ex_035, uses_muscle, m_hamstrings).
fact(e_ex_035, requires_equipment, eq_kettlebell).
attr(e_ex_035, difficulty, medium).

entity(e_ex_036, exercise, 'Box jump variant 2').
fact(e_ex_036, uses_muscle, m_quadriceps).
fact(e_ex_036, uses_muscle, m_glutes).
fact(e_ex_036, requires_equipment, eq_none).
attr(e_ex_036, difficulty, medium).

entity(e_ex_037, exercise, 'Burpee variant 2').
fact(e_ex_037, uses_muscle, m_quadriceps).
fact(e_ex_037, uses_muscle, m_chest).
fact(e_ex_037, requires_equipment, eq_none).
attr(e_ex_037, difficulty, medium).

entity(e_ex_038, exercise, 'Mountain climber variant 2').
fact(e_ex_038, uses_muscle, m_core).
fact(e_ex_038, requires_equipment, eq_none).
attr(e_ex_038, difficulty, medium).

entity(e_ex_039, exercise, 'Cycling variant 2').
fact(e_ex_039, uses_muscle, m_quadriceps).
fact(e_ex_039, requires_equipment, eq_none).
attr(e_ex_039, difficulty, medium).

entity(e_ex_040, exercise, 'Rowing machine variant 2').
fact(e_ex_040, uses_muscle, m_back).
fact(e_ex_040, uses_muscle, m_quadriceps).
fact(e_ex_040, requires_equipment, eq_none).
attr(e_ex_040, difficulty, medium).

% --- Variant 3 (indices 41..60)
entity(e_ex_041, exercise, 'Squat variant 3').
fact(e_ex_041, uses_muscle, m_quadriceps).
fact(e_ex_041, uses_muscle, m_glutes).
fact(e_ex_041, requires_equipment, eq_barbell).
attr(e_ex_041, difficulty, medium).

entity(e_ex_042, exercise, 'Lunge variant 3').
fact(e_ex_042, uses_muscle, m_quadriceps).
fact(e_ex_042, uses_muscle, m_glutes).
fact(e_ex_042, requires_equipment, eq_none).
attr(e_ex_042, difficulty, medium).

entity(e_ex_043, exercise, 'Deadlift variant 3').
fact(e_ex_043, uses_muscle, m_hamstrings).
fact(e_ex_043, uses_muscle, m_glutes).
fact(e_ex_043, requires_equipment, eq_barbell).
attr(e_ex_043, difficulty, medium).

entity(e_ex_044, exercise, 'Bench press variant 3').
fact(e_ex_044, uses_muscle, m_chest).
fact(e_ex_044, uses_muscle, m_triceps).
fact(e_ex_044, requires_equipment, eq_barbell).
attr(e_ex_044, difficulty, medium).

entity(e_ex_045, exercise, 'Row variant 3').
fact(e_ex_045, uses_muscle, m_back).
fact(e_ex_045, uses_muscle, m_biceps).
fact(e_ex_045, requires_equipment, eq_dumbbell).
attr(e_ex_045, difficulty, medium).

entity(e_ex_046, exercise, 'Shoulder press variant 3').
fact(e_ex_046, uses_muscle, m_shoulders).
fact(e_ex_046, uses_muscle, m_triceps).
fact(e_ex_046, requires_equipment, eq_dumbbell).
attr(e_ex_046, difficulty, medium).

entity(e_ex_047, exercise, 'Biceps curl variant 3').
fact(e_ex_047, uses_muscle, m_biceps).
fact(e_ex_047, requires_equipment, eq_dumbbell).
attr(e_ex_047, difficulty, medium).

entity(e_ex_048, exercise, 'Triceps extension variant 3').
fact(e_ex_048, uses_muscle, m_triceps).
fact(e_ex_048, requires_equipment, eq_dumbbell).
attr(e_ex_048, difficulty, medium).

entity(e_ex_049, exercise, 'Plank variant 3').
fact(e_ex_049, uses_muscle, m_core).
fact(e_ex_049, requires_equipment, eq_none).
attr(e_ex_049, difficulty, medium).

entity(e_ex_050, exercise, 'Jump rope variant 3').
fact(e_ex_050, uses_muscle, m_calves).
fact(e_ex_050, requires_equipment, eq_none).
attr(e_ex_050, difficulty, medium).

entity(e_ex_051, exercise, 'Leg press variant 3').
fact(e_ex_051, uses_muscle, m_quadriceps).
fact(e_ex_051, requires_equipment, eq_leg_press_machine).
attr(e_ex_051, difficulty, medium).

entity(e_ex_052, exercise, 'Glute bridge variant 3').
fact(e_ex_052, uses_muscle, m_glutes).
fact(e_ex_052, requires_equipment, eq_none).
attr(e_ex_052, difficulty, medium).

entity(e_ex_053, exercise, 'Push-up variant 3').
fact(e_ex_053, uses_muscle, m_chest).
fact(e_ex_053, uses_muscle, m_triceps).
fact(e_ex_053, requires_equipment, eq_none).
attr(e_ex_053, difficulty, medium).

entity(e_ex_054, exercise, 'Pull-up variant 3').
fact(e_ex_054, uses_muscle, m_back).
fact(e_ex_054, uses_muscle, m_biceps).
fact(e_ex_054, requires_equipment, eq_none).
attr(e_ex_054, difficulty, medium).

entity(e_ex_055, exercise, 'Kettlebell swing variant 3').
fact(e_ex_055, uses_muscle, m_glutes).
fact(e_ex_055, uses_muscle, m_hamstrings).
fact(e_ex_055, requires_equipment, eq_kettlebell).
attr(e_ex_055, difficulty, medium).

entity(e_ex_056, exercise, 'Box jump variant 3').
fact(e_ex_056, uses_muscle, m_quadriceps).
fact(e_ex_056, uses_muscle, m_glutes).
fact(e_ex_056, requires_equipment, eq_none).
attr(e_ex_056, difficulty, medium).

entity(e_ex_057, exercise, 'Burpee variant 3').
fact(e_ex_057, uses_muscle, m_quadriceps).
fact(e_ex_057, uses_muscle, m_chest).
fact(e_ex_057, requires_equipment, eq_none).
attr(e_ex_057, difficulty, medium).

entity(e_ex_058, exercise, 'Mountain climber variant 3').
fact(e_ex_058, uses_muscle, m_core).
fact(e_ex_058, requires_equipment, eq_none).
attr(e_ex_058, difficulty, medium).

entity(e_ex_059, exercise, 'Cycling variant 3').
fact(e_ex_059, uses_muscle, m_quadriceps).
fact(e_ex_059, requires_equipment, eq_none).
attr(e_ex_059, difficulty, medium).

entity(e_ex_060, exercise, 'Rowing machine variant 3').
fact(e_ex_060, uses_muscle, m_back).
fact(e_ex_060, uses_muscle, m_quadriceps).
fact(e_ex_060, requires_equipment, eq_none).
attr(e_ex_060, difficulty, medium).

% --- Variant 4 (indices 61..80)
entity(e_ex_061, exercise, 'Squat variant 4').
fact(e_ex_061, uses_muscle, m_quadriceps).
fact(e_ex_061, uses_muscle, m_glutes).
fact(e_ex_061, requires_equipment, eq_barbell).
attr(e_ex_061, difficulty, medium).

entity(e_ex_062, exercise, 'Lunge variant 4').
fact(e_ex_062, uses_muscle, m_quadriceps).
fact(e_ex_062, uses_muscle, m_glutes).
fact(e_ex_062, requires_equipment, eq_none).
attr(e_ex_062, difficulty, medium).

entity(e_ex_063, exercise, 'Deadlift variant 4').
fact(e_ex_063, uses_muscle, m_hamstrings).
fact(e_ex_063, uses_muscle, m_glutes).
fact(e_ex_063, requires_equipment, eq_barbell).
attr(e_ex_063, difficulty, medium).

entity(e_ex_064, exercise, 'Bench press variant 4').
fact(e_ex_064, uses_muscle, m_chest).
fact(e_ex_064, uses_muscle, m_triceps).
fact(e_ex_064, requires_equipment, eq_barbell).
attr(e_ex_064, difficulty, medium).

entity(e_ex_065, exercise, 'Row variant 4').
fact(e_ex_065, uses_muscle, m_back).
fact(e_ex_065, uses_muscle, m_biceps).
fact(e_ex_065, requires_equipment, eq_dumbbell).
attr(e_ex_065, difficulty, medium).

entity(e_ex_066, exercise, 'Shoulder press variant 4').
fact(e_ex_066, uses_muscle, m_shoulders).
fact(e_ex_066, uses_muscle, m_triceps).
fact(e_ex_066, requires_equipment, eq_dumbbell).
attr(e_ex_066, difficulty, medium).

entity(e_ex_067, exercise, 'Biceps curl variant 4').
fact(e_ex_067, uses_muscle, m_biceps).
fact(e_ex_067, requires_equipment, eq_dumbbell).
attr(e_ex_067, difficulty, medium).

entity(e_ex_068, exercise, 'Triceps extension variant 4').
fact(e_ex_068, uses_muscle, m_triceps).
fact(e_ex_068, requires_equipment, eq_dumbbell).
attr(e_ex_068, difficulty, medium).

entity(e_ex_069, exercise, 'Plank variant 4').
fact(e_ex_069, uses_muscle, m_core).
fact(e_ex_069, requires_equipment, eq_none).
attr(e_ex_069, difficulty, medium).

entity(e_ex_070, exercise, 'Jump rope variant 4').
fact(e_ex_070, uses_muscle, m_calves).
fact(e_ex_070, requires_equipment, eq_none).
attr(e_ex_070, difficulty, medium).

entity(e_ex_071, exercise, 'Leg press variant 4').
fact(e_ex_071, uses_muscle, m_quadriceps).
fact(e_ex_071, requires_equipment, eq_leg_press_machine).
attr(e_ex_071, difficulty, medium).

entity(e_ex_072, exercise, 'Glute bridge variant 4').
fact(e_ex_072, uses_muscle, m_glutes).
fact(e_ex_072, requires_equipment, eq_none).
attr(e_ex_072, difficulty, medium).

entity(e_ex_073, exercise, 'Push-up variant 4').
fact(e_ex_073, uses_muscle, m_chest).
fact(e_ex_073, uses_muscle, m_triceps).
fact(e_ex_073, requires_equipment, eq_none).
attr(e_ex_073, difficulty, medium).

entity(e_ex_074, exercise, 'Pull-up variant 4').
fact(e_ex_074, uses_muscle, m_back).
fact(e_ex_074, uses_muscle, m_biceps).
fact(e_ex_074, requires_equipment, eq_none).
attr(e_ex_074, difficulty, medium).

entity(e_ex_075, exercise, 'Kettlebell swing variant 4').
fact(e_ex_075, uses_muscle, m_glutes).
fact(e_ex_075, uses_muscle, m_hamstrings).
fact(e_ex_075, requires_equipment, eq_kettlebell).
attr(e_ex_075, difficulty, medium).

entity(e_ex_076, exercise, 'Box jump variant 4').
fact(e_ex_076, uses_muscle, m_quadriceps).
fact(e_ex_076, uses_muscle, m_glutes).
fact(e_ex_076, requires_equipment, eq_none).
attr(e_ex_076, difficulty, medium).

entity(e_ex_077, exercise, 'Burpee variant 4').
fact(e_ex_077, uses_muscle, m_quadriceps).
fact(e_ex_077, uses_muscle, m_chest).
fact(e_ex_077, requires_equipment, eq_none).
attr(e_ex_077, difficulty, medium).

entity(e_ex_078, exercise, 'Mountain climber variant 4').
fact(e_ex_078, uses_muscle, m_core).
fact(e_ex_078, requires_equipment, eq_none).
attr(e_ex_078, difficulty, medium).

entity(e_ex_079, exercise, 'Cycling variant 4').
fact(e_ex_079, uses_muscle, m_quadriceps).
fact(e_ex_079, requires_equipment, eq_none).
attr(e_ex_079, difficulty, medium).

entity(e_ex_080, exercise, 'Rowing machine variant 4').
fact(e_ex_080, uses_muscle, m_back).
fact(e_ex_080, uses_muscle, m_quadriceps).
fact(e_ex_080, requires_equipment, eq_none).
attr(e_ex_080, difficulty, medium).

% --- Variant 5 (indices 81..100)
entity(e_ex_081, exercise, 'Squat variant 5').
fact(e_ex_081, uses_muscle, m_quadriceps).
fact(e_ex_081, uses_muscle, m_glutes).
fact(e_ex_081, requires_equipment, eq_barbell).
attr(e_ex_081, difficulty, hard).

entity(e_ex_082, exercise, 'Lunge variant 5').
fact(e_ex_082, uses_muscle, m_quadriceps).
fact(e_ex_082, uses_muscle, m_glutes).
fact(e_ex_082, requires_equipment, eq_none).
attr(e_ex_082, difficulty, hard).

entity(e_ex_083, exercise, 'Deadlift variant 5').
fact(e_ex_083, uses_muscle, m_hamstrings).
fact(e_ex_083, uses_muscle, m_glutes).
fact(e_ex_083, requires_equipment, eq_barbell).
attr(e_ex_083, difficulty, hard).

entity(e_ex_084, exercise, 'Bench press variant 5').
fact(e_ex_084, uses_muscle, m_chest).
fact(e_ex_084, uses_muscle, m_triceps).
fact(e_ex_084, requires_equipment, eq_barbell).
attr(e_ex_084, difficulty, hard).

entity(e_ex_085, exercise, 'Row variant 5').
fact(e_ex_085, uses_muscle, m_back).
fact(e_ex_085, uses_muscle, m_biceps).
fact(e_ex_085, requires_equipment, eq_dumbbell).
attr(e_ex_085, difficulty, hard).

entity(e_ex_086, exercise, 'Shoulder press variant 5').
fact(e_ex_086, uses_muscle, m_shoulders).
fact(e_ex_086, uses_muscle, m_triceps).
fact(e_ex_086, requires_equipment, eq_dumbbell).
attr(e_ex_086, difficulty, hard).

entity(e_ex_087, exercise, 'Biceps curl variant 5').
fact(e_ex_087, uses_muscle, m_biceps).
fact(e_ex_087, requires_equipment, eq_dumbbell).
attr(e_ex_087, difficulty, hard).

entity(e_ex_088, exercise, 'Triceps extension variant 5').
fact(e_ex_088, uses_muscle, m_triceps).
fact(e_ex_088, requires_equipment, eq_dumbbell).
attr(e_ex_088, difficulty, hard).

entity(e_ex_089, exercise, 'Plank variant 5').
fact(e_ex_089, uses_muscle, m_core).
fact(e_ex_089, requires_equipment, eq_none).
attr(e_ex_089, difficulty, hard).

entity(e_ex_090, exercise, 'Jump rope variant 5').
fact(e_ex_090, uses_muscle, m_calves).
fact(e_ex_090, requires_equipment, eq_none).
attr(e_ex_090, difficulty, hard).

entity(e_ex_091, exercise, 'Leg press variant 5').
fact(e_ex_091, uses_muscle, m_quadriceps).
fact(e_ex_091, requires_equipment, eq_leg_press_machine).
attr(e_ex_091, difficulty, hard).

entity(e_ex_092, exercise, 'Glute bridge variant 5').
fact(e_ex_092, uses_muscle, m_glutes).
fact(e_ex_092, requires_equipment, eq_none).
attr(e_ex_092, difficulty, hard).

entity(e_ex_093, exercise, 'Push-up variant 5').
fact(e_ex_093, uses_muscle, m_chest).
fact(e_ex_093, uses_muscle, m_triceps).
fact(e_ex_093, requires_equipment, eq_none).
attr(e_ex_093, difficulty, hard).

entity(e_ex_094, exercise, 'Pull-up variant 5').
fact(e_ex_094, uses_muscle, m_back).
fact(e_ex_094, uses_muscle, m_biceps).
fact(e_ex_094, requires_equipment, eq_none).
attr(e_ex_094, difficulty, hard).

entity(e_ex_095, exercise, 'Kettlebell swing variant 5').
fact(e_ex_095, uses_muscle, m_glutes).
fact(e_ex_095, uses_muscle, m_hamstrings).
fact(e_ex_095, requires_equipment, eq_kettlebell).
attr(e_ex_095, difficulty, hard).

entity(e_ex_096, exercise, 'Box jump variant 5').
fact(e_ex_096, uses_muscle, m_quadriceps).
fact(e_ex_096, uses_muscle, m_glutes).
fact(e_ex_096, requires_equipment, eq_none).
attr(e_ex_096, difficulty, hard).

entity(e_ex_097, exercise, 'Burpee variant 5').
fact(e_ex_097, uses_muscle, m_quadriceps).
fact(e_ex_097, uses_muscle, m_chest).
fact(e_ex_097, requires_equipment, eq_none).
attr(e_ex_097, difficulty, hard).

entity(e_ex_098, exercise, 'Mountain climber variant 5').
fact(e_ex_098, uses_muscle, m_core).
fact(e_ex_098, requires_equipment, eq_none).
attr(e_ex_098, difficulty, hard).

entity(e_ex_099, exercise, 'Cycling variant 5').
fact(e_ex_099, uses_muscle, m_quadriceps).
fact(e_ex_099, requires_equipment, eq_none).
attr(e_ex_099, difficulty, hard).

entity(e_ex_100, exercise, 'Rowing machine variant 5').
fact(e_ex_100, uses_muscle, m_back).
fact(e_ex_100, uses_muscle, m_quadriceps).
fact(e_ex_100, requires_equipment, eq_none).
attr(e_ex_100, difficulty, hard).

% --- Variant 6 (indices 101..120)
entity(e_ex_101, exercise, 'Squat variant 6').
fact(e_ex_101, uses_muscle, m_quadriceps).
fact(e_ex_101, uses_muscle, m_glutes).
fact(e_ex_101, requires_equipment, eq_barbell).
attr(e_ex_101, difficulty, hard).

entity(e_ex_102, exercise, 'Lunge variant 6').
fact(e_ex_102, uses_muscle, m_quadriceps).
fact(e_ex_102, uses_muscle, m_glutes).
fact(e_ex_102, requires_equipment, eq_none).
attr(e_ex_102, difficulty, hard).

entity(e_ex_103, exercise, 'Deadlift variant 6').
fact(e_ex_103, uses_muscle, m_hamstrings).
fact(e_ex_103, uses_muscle, m_glutes).
fact(e_ex_103, requires_equipment, eq_barbell).
attr(e_ex_103, difficulty, hard).

entity(e_ex_104, exercise, 'Bench press variant 6').
fact(e_ex_104, uses_muscle, m_chest).
fact(e_ex_104, uses_muscle, m_triceps).
fact(e_ex_104, requires_equipment, eq_barbell).
attr(e_ex_104, difficulty, hard).

entity(e_ex_105, exercise, 'Row variant 6').
fact(e_ex_105, uses_muscle, m_back).
fact(e_ex_105, uses_muscle, m_biceps).
fact(e_ex_105, requires_equipment, eq_dumbbell).
attr(e_ex_105, difficulty, hard).

entity(e_ex_106, exercise, 'Shoulder press variant 6').
fact(e_ex_106, uses_muscle, m_shoulders).
fact(e_ex_106, uses_muscle, m_triceps).
fact(e_ex_106, requires_equipment, eq_dumbbell).
attr(e_ex_106, difficulty, hard).

entity(e_ex_107, exercise, 'Biceps curl variant 6').
fact(e_ex_107, uses_muscle, m_biceps).
fact(e_ex_107, requires_equipment, eq_dumbbell).
attr(e_ex_107, difficulty, hard).

entity(e_ex_108, exercise, 'Triceps extension variant 6').
fact(e_ex_108, uses_muscle, m_triceps).
fact(e_ex_108, requires_equipment, eq_dumbbell).
attr(e_ex_108, difficulty, hard).

entity(e_ex_109, exercise, 'Plank variant 6').
fact(e_ex_109, uses_muscle, m_core).
fact(e_ex_109, requires_equipment, eq_none).
attr(e_ex_109, difficulty, hard).

entity(e_ex_110, exercise, 'Jump rope variant 6').
fact(e_ex_110, uses_muscle, m_calves).
fact(e_ex_110, requires_equipment, eq_none).
attr(e_ex_110, difficulty, hard).

entity(e_ex_111, exercise, 'Leg press variant 6').
fact(e_ex_111, uses_muscle, m_quadriceps).
fact(e_ex_111, requires_equipment, eq_leg_press_machine).
attr(e_ex_111, difficulty, hard).

entity(e_ex_112, exercise, 'Glute bridge variant 6').
fact(e_ex_112, uses_muscle, m_glutes).
fact(e_ex_112, requires_equipment, eq_none).
attr(e_ex_112, difficulty, hard).

entity(e_ex_113, exercise, 'Push-up variant 6').
fact(e_ex_113, uses_muscle, m_chest).
fact(e_ex_113, uses_muscle, m_triceps).
fact(e_ex_113, requires_equipment, eq_none).
attr(e_ex_113, difficulty, hard).

entity(e_ex_114, exercise, 'Pull-up variant 6').
fact(e_ex_114, uses_muscle, m_back).
fact(e_ex_114, uses_muscle, m_biceps).
fact(e_ex_114, requires_equipment, eq_none).
attr(e_ex_114, difficulty, hard).

entity(e_ex_115, exercise, 'Kettlebell swing variant 6').
fact(e_ex_115, uses_muscle, m_glutes).
fact(e_ex_115, uses_muscle, m_hamstrings).
fact(e_ex_115, requires_equipment, eq_kettlebell).
attr(e_ex_115, difficulty, hard).

entity(e_ex_116, exercise, 'Box jump variant 6').
fact(e_ex_116, uses_muscle, m_quadriceps).
fact(e_ex_116, uses_muscle, m_glutes).
fact(e_ex_116, requires_equipment, eq_none).
attr(e_ex_116, difficulty, hard).

entity(e_ex_117, exercise, 'Burpee variant 6').
fact(e_ex_117, uses_muscle, m_quadriceps).
fact(e_ex_117, uses_muscle, m_chest).
fact(e_ex_117, requires_equipment, eq_none).
attr(e_ex_117, difficulty, hard).

entity(e_ex_118, exercise, 'Mountain climber variant 6').
fact(e_ex_118, uses_muscle, m_core).
fact(e_ex_118, requires_equipment, eq_none).
attr(e_ex_118, difficulty, hard).

entity(e_ex_119, exercise, 'Cycling variant 6').
fact(e_ex_119, uses_muscle, m_quadriceps).
fact(e_ex_119, requires_equipment, eq_none).
attr(e_ex_119, difficulty, hard).

entity(e_ex_120, exercise, 'Rowing machine variant 6').
fact(e_ex_120, uses_muscle, m_back).
fact(e_ex_120, uses_muscle, m_quadriceps).
fact(e_ex_120, requires_equipment, eq_none).
attr(e_ex_120, difficulty, hard).

% --- Variant 7 (indices 121..140)
entity(e_ex_121, exercise, 'Squat variant 7').
fact(e_ex_121, uses_muscle, m_quadriceps).
fact(e_ex_121, uses_muscle, m_glutes).
fact(e_ex_121, requires_equipment, eq_barbell).
attr(e_ex_121, difficulty, hard).

entity(e_ex_122, exercise, 'Lunge variant 7').
fact(e_ex_122, uses_muscle, m_quadriceps).
fact(e_ex_122, uses_muscle, m_glutes).
fact(e_ex_122, requires_equipment, eq_none).
attr(e_ex_122, difficulty, hard).

entity(e_ex_123, exercise, 'Deadlift variant 7').
fact(e_ex_123, uses_muscle, m_hamstrings).
fact(e_ex_123, uses_muscle, m_glutes).
fact(e_ex_123, requires_equipment, eq_barbell).
attr(e_ex_123, difficulty, hard).

entity(e_ex_124, exercise, 'Bench press variant 7').
fact(e_ex_124, uses_muscle, m_chest).
fact(e_ex_124, uses_muscle, m_triceps).
fact(e_ex_124, requires_equipment, eq_barbell).
attr(e_ex_124, difficulty, hard).

entity(e_ex_125, exercise, 'Row variant 7').
fact(e_ex_125, uses_muscle, m_back).
fact(e_ex_125, uses_muscle, m_biceps).
fact(e_ex_125, requires_equipment, eq_dumbbell).
attr(e_ex_125, difficulty, hard).

entity(e_ex_126, exercise, 'Shoulder press variant 7').
fact(e_ex_126, uses_muscle, m_shoulders).
fact(e_ex_126, uses_muscle, m_triceps).
fact(e_ex_126, requires_equipment, eq_dumbbell).
attr(e_ex_126, difficulty, hard).

entity(e_ex_127, exercise, 'Biceps curl variant 7').
fact(e_ex_127, uses_muscle, m_biceps).
fact(e_ex_127, requires_equipment, eq_dumbbell).
attr(e_ex_127, difficulty, hard).

entity(e_ex_128, exercise, 'Triceps extension variant 7').
fact(e_ex_128, uses_muscle, m_triceps).
fact(e_ex_128, requires_equipment, eq_dumbbell).
attr(e_ex_128, difficulty, hard).

entity(e_ex_129, exercise, 'Plank variant 7').
fact(e_ex_129, uses_muscle, m_core).
fact(e_ex_129, requires_equipment, eq_none).
attr(e_ex_129, difficulty, hard).

entity(e_ex_130, exercise, 'Jump rope variant 7').
fact(e_ex_130, uses_muscle, m_calves).
fact(e_ex_130, requires_equipment, eq_none).
attr(e_ex_130, difficulty, hard).

entity(e_ex_131, exercise, 'Leg press variant 7').
fact(e_ex_131, uses_muscle, m_quadriceps).
fact(e_ex_131, requires_equipment, eq_leg_press_machine).
attr(e_ex_131, difficulty, hard).

entity(e_ex_132, exercise, 'Glute bridge variant 7').
fact(e_ex_132, uses_muscle, m_glutes).
fact(e_ex_132, requires_equipment, eq_none).
attr(e_ex_132, difficulty, hard).

entity(e_ex_133, exercise, 'Push-up variant 7').
fact(e_ex_133, uses_muscle, m_chest).
fact(e_ex_133, uses_muscle, m_triceps).
fact(e_ex_133, requires_equipment, eq_none).
attr(e_ex_133, difficulty, hard).

entity(e_ex_134, exercise, 'Pull-up variant 7').
fact(e_ex_134, uses_muscle, m_back).
fact(e_ex_134, uses_muscle, m_biceps).
fact(e_ex_134, requires_equipment, eq_none).
attr(e_ex_134, difficulty, hard).

entity(e_ex_135, exercise, 'Kettlebell swing variant 7').
fact(e_ex_135, uses_muscle, m_glutes).
fact(e_ex_135, uses_muscle, m_hamstrings).
fact(e_ex_135, requires_equipment, eq_kettlebell).
attr(e_ex_135, difficulty, hard).

entity(e_ex_136, exercise, 'Box jump variant 7').
fact(e_ex_136, uses_muscle, m_quadriceps).
fact(e_ex_136, uses_muscle, m_glutes).
fact(e_ex_136, requires_equipment, eq_none).
attr(e_ex_136, difficulty, hard).

entity(e_ex_137, exercise, 'Burpee variant 7').
fact(e_ex_137, uses_muscle, m_quadriceps).
fact(e_ex_137, uses_muscle, m_chest).
fact(e_ex_137, requires_equipment, eq_none).
attr(e_ex_137, difficulty, hard).

entity(e_ex_138, exercise, 'Mountain climber variant 7').
fact(e_ex_138, uses_muscle, m_core).
fact(e_ex_138, requires_equipment, eq_none).
attr(e_ex_138, difficulty, hard).

entity(e_ex_139, exercise, 'Cycling variant 7').
fact(e_ex_139, uses_muscle, m_quadriceps).
fact(e_ex_139, requires_equipment, eq_none).
attr(e_ex_139, difficulty, hard).

entity(e_ex_140, exercise, 'Rowing machine variant 7').
fact(e_ex_140, uses_muscle, m_back).
fact(e_ex_140, uses_muscle, m_quadriceps).
fact(e_ex_140, requires_equipment, eq_none).
attr(e_ex_140, difficulty, hard).

% --- Variant 8 (indices 141..160)
entity(e_ex_141, exercise, 'Squat variant 8').
fact(e_ex_141, uses_muscle, m_quadriceps).
fact(e_ex_141, uses_muscle, m_glutes).
fact(e_ex_141, requires_equipment, eq_barbell).
attr(e_ex_141, difficulty, hard).

entity(e_ex_142, exercise, 'Lunge variant 8').
fact(e_ex_142, uses_muscle, m_quadriceps).
fact(e_ex_142, uses_muscle, m_glutes).
fact(e_ex_142, requires_equipment, eq_none).
attr(e_ex_142, difficulty, hard).

entity(e_ex_143, exercise, 'Deadlift variant 8').
fact(e_ex_143, uses_muscle, m_hamstrings).
fact(e_ex_143, uses_muscle, m_glutes).
fact(e_ex_143, requires_equipment, eq_barbell).
attr(e_ex_143, difficulty, hard).

entity(e_ex_144, exercise, 'Bench press variant 8').
fact(e_ex_144, uses_muscle, m_chest).
fact(e_ex_144, uses_muscle, m_triceps).
fact(e_ex_144, requires_equipment, eq_barbell).
attr(e_ex_144, difficulty, hard).

entity(e_ex_145, exercise, 'Row variant 8').
fact(e_ex_145, uses_muscle, m_back).
fact(e_ex_145, uses_muscle, m_biceps).
fact(e_ex_145, requires_equipment, eq_dumbbell).
attr(e_ex_145, difficulty, hard).

entity(e_ex_146, exercise, 'Shoulder press variant 8').
fact(e_ex_146, uses_muscle, m_shoulders).
fact(e_ex_146, uses_muscle, m_triceps).
fact(e_ex_146, requires_equipment, eq_dumbbell).
attr(e_ex_146, difficulty, hard).

entity(e_ex_147, exercise, 'Biceps curl variant 8').
fact(e_ex_147, uses_muscle, m_biceps).
fact(e_ex_147, requires_equipment, eq_dumbbell).
attr(e_ex_147, difficulty, hard).

entity(e_ex_148, exercise, 'Triceps extension variant 8').
fact(e_ex_148, uses_muscle, m_triceps).
fact(e_ex_148, requires_equipment, eq_dumbbell).
attr(e_ex_148, difficulty, hard).

entity(e_ex_149, exercise, 'Plank variant 8').
fact(e_ex_149, uses_muscle, m_core).
fact(e_ex_149, requires_equipment, eq_none).
attr(e_ex_149, difficulty, hard).

entity(e_ex_150, exercise, 'Jump rope variant 8').
fact(e_ex_150, uses_muscle, m_calves).
fact(e_ex_150, requires_equipment, eq_none).
attr(e_ex_150, difficulty, hard).

entity(e_ex_151, exercise, 'Leg press variant 8').
fact(e_ex_151, uses_muscle, m_quadriceps).
fact(e_ex_151, requires_equipment, eq_leg_press_machine).
attr(e_ex_151, difficulty, hard).

entity(e_ex_152, exercise, 'Glute bridge variant 8').
fact(e_ex_152, uses_muscle, m_glutes).
fact(e_ex_152, requires_equipment, eq_none).
attr(e_ex_152, difficulty, hard).

entity(e_ex_153, exercise, 'Push-up variant 8').
fact(e_ex_153, uses_muscle, m_chest).
fact(e_ex_153, uses_muscle, m_triceps).
fact(e_ex_153, requires_equipment, eq_none).
attr(e_ex_153, difficulty, hard).

entity(e_ex_154, exercise, 'Pull-up variant 8').
fact(e_ex_154, uses_muscle, m_back).
fact(e_ex_154, uses_muscle, m_biceps).
fact(e_ex_154, requires_equipment, eq_none).
attr(e_ex_154, difficulty, hard).

entity(e_ex_155, exercise, 'Kettlebell swing variant 8').
fact(e_ex_155, uses_muscle, m_glutes).
fact(e_ex_155, uses_muscle, m_hamstrings).
fact(e_ex_155, requires_equipment, eq_kettlebell).
attr(e_ex_155, difficulty, hard).

entity(e_ex_156, exercise, 'Box jump variant 8').
fact(e_ex_156, uses_muscle, m_quadriceps).
fact(e_ex_156, uses_muscle, m_glutes).
fact(e_ex_156, requires_equipment, eq_none).
attr(e_ex_156, difficulty, hard).

entity(e_ex_157, exercise, 'Burpee variant 8').
fact(e_ex_157, uses_muscle, m_quadriceps).
fact(e_ex_157, uses_muscle, m_chest).
fact(e_ex_157, requires_equipment, eq_none).
attr(e_ex_157, difficulty, hard).

entity(e_ex_158, exercise, 'Mountain climber variant 8').
fact(e_ex_158, uses_muscle, m_core).
fact(e_ex_158, requires_equipment, eq_none).
attr(e_ex_158, difficulty, hard).

entity(e_ex_159, exercise, 'Cycling variant 8').
fact(e_ex_159, uses_muscle, m_quadriceps).
fact(e_ex_159, requires_equipment, eq_none).
attr(e_ex_159, difficulty, hard).

entity(e_ex_160, exercise, 'Rowing machine variant 8').
fact(e_ex_160, uses_muscle, m_back).
fact(e_ex_160, uses_muscle, m_quadriceps).
fact(e_ex_160, requires_equipment, eq_none).
attr(e_ex_160, difficulty, hard).

% --- Variant 9 (indices 161..180)
entity(e_ex_161, exercise, 'Squat variant 9').
fact(e_ex_161, uses_muscle, m_quadriceps).
fact(e_ex_161, uses_muscle, m_glutes).
fact(e_ex_161, requires_equipment, eq_barbell).
attr(e_ex_161, difficulty, hard).

entity(e_ex_162, exercise, 'Lunge variant 9').
fact(e_ex_162, uses_muscle, m_quadriceps).
fact(e_ex_162, uses_muscle, m_glutes).
fact(e_ex_162, requires_equipment, eq_none).
attr(e_ex_162, difficulty, hard).

entity(e_ex_163, exercise, 'Deadlift variant 9').
fact(e_ex_163, uses_muscle, m_hamstrings).
fact(e_ex_163, uses_muscle, m_glutes).
fact(e_ex_163, requires_equipment, eq_barbell).
attr(e_ex_163, difficulty, hard).

entity(e_ex_164, exercise, 'Bench press variant 9').
fact(e_ex_164, uses_muscle, m_chest).
fact(e_ex_164, uses_muscle, m_triceps).
fact(e_ex_164, requires_equipment, eq_barbell).
attr(e_ex_164, difficulty, hard).

entity(e_ex_165, exercise, 'Row variant 9').
fact(e_ex_165, uses_muscle, m_back).
fact(e_ex_165, uses_muscle, m_biceps).
fact(e_ex_165, requires_equipment, eq_dumbbell).
attr(e_ex_165, difficulty, hard).

entity(e_ex_166, exercise, 'Shoulder press variant 9').
fact(e_ex_166, uses_muscle, m_shoulders).
fact(e_ex_166, uses_muscle, m_triceps).
fact(e_ex_166, requires_equipment, eq_dumbbell).
attr(e_ex_166, difficulty, hard).

entity(e_ex_167, exercise, 'Biceps curl variant 9').
fact(e_ex_167, uses_muscle, m_biceps).
fact(e_ex_167, requires_equipment, eq_dumbbell).
attr(e_ex_167, difficulty, hard).

entity(e_ex_168, exercise, 'Triceps extension variant 9').
fact(e_ex_168, uses_muscle, m_triceps).
fact(e_ex_168, requires_equipment, eq_dumbbell).
attr(e_ex_168, difficulty, hard).

entity(e_ex_169, exercise, 'Plank variant 9').
fact(e_ex_169, uses_muscle, m_core).
fact(e_ex_169, requires_equipment, eq_none).
attr(e_ex_169, difficulty, hard).

entity(e_ex_170, exercise, 'Jump rope variant 9').
fact(e_ex_170, uses_muscle, m_calves).
fact(e_ex_170, requires_equipment, eq_none).
attr(e_ex_170, difficulty, hard).

entity(e_ex_171, exercise, 'Leg press variant 9').
fact(e_ex_171, uses_muscle, m_quadriceps).
fact(e_ex_171, requires_equipment, eq_leg_press_machine).
attr(e_ex_171, difficulty, hard).

entity(e_ex_172, exercise, 'Glute bridge variant 9').
fact(e_ex_172, uses_muscle, m_glutes).
fact(e_ex_172, requires_equipment, eq_none).
attr(e_ex_172, difficulty, hard).

entity(e_ex_173, exercise, 'Push-up variant 9').
fact(e_ex_173, uses_muscle, m_chest).
fact(e_ex_173, uses_muscle, m_triceps).
fact(e_ex_173, requires_equipment, eq_none).
attr(e_ex_173, difficulty, hard).

entity(e_ex_174, exercise, 'Pull-up variant 9').
fact(e_ex_174, uses_muscle, m_back).
fact(e_ex_174, uses_muscle, m_biceps).
fact(e_ex_174, requires_equipment, eq_none).
attr(e_ex_174, difficulty, hard).

entity(e_ex_175, exercise, 'Kettlebell swing variant 9').
fact(e_ex_175, uses_muscle, m_glutes).
fact(e_ex_175, uses_muscle, m_hamstrings).
fact(e_ex_175, requires_equipment, eq_kettlebell).
attr(e_ex_175, difficulty, hard).

entity(e_ex_176, exercise, 'Box jump variant 9').
fact(e_ex_176, uses_muscle, m_quadriceps).
fact(e_ex_176, uses_muscle, m_glutes).
fact(e_ex_176, requires_equipment, eq_none).
attr(e_ex_176, difficulty, hard).

entity(e_ex_177, exercise, 'Burpee variant 9').
fact(e_ex_177, uses_muscle, m_quadriceps).
fact(e_ex_177, uses_muscle, m_chest).
fact(e_ex_177, requires_equipment, eq_none).
attr(e_ex_177, difficulty, hard).

entity(e_ex_178, exercise, 'Mountain climber variant 9').
fact(e_ex_178, uses_muscle, m_core).
fact(e_ex_178, requires_equipment, eq_none).
attr(e_ex_178, difficulty, hard).

entity(e_ex_179, exercise, 'Cycling variant 9').
fact(e_ex_179, uses_muscle, m_quadriceps).
fact(e_ex_179, requires_equipment, eq_none).
attr(e_ex_179, difficulty, hard).

entity(e_ex_180, exercise, 'Rowing machine variant 9').
fact(e_ex_180, uses_muscle, m_back).
fact(e_ex_180, uses_muscle, m_quadriceps).
fact(e_ex_180, requires_equipment, eq_none).
attr(e_ex_180, difficulty, hard).

% --- Variant 10 (indices 181..200)
entity(e_ex_181, exercise, 'Squat variant 10').
fact(e_ex_181, uses_muscle, m_quadriceps).
fact(e_ex_181, uses_muscle, m_glutes).
fact(e_ex_181, requires_equipment, eq_barbell).
attr(e_ex_181, difficulty, hard).

entity(e_ex_182, exercise, 'Lunge variant 10').
fact(e_ex_182, uses_muscle, m_quadriceps).
fact(e_ex_182, uses_muscle, m_glutes).
fact(e_ex_182, requires_equipment, eq_none).
attr(e_ex_182, difficulty, hard).

entity(e_ex_183, exercise, 'Deadlift variant 10').
fact(e_ex_183, uses_muscle, m_hamstrings).
fact(e_ex_183, uses_muscle, m_glutes).
fact(e_ex_183, requires_equipment, eq_barbell).
attr(e_ex_183, difficulty, hard).

entity(e_ex_184, exercise, 'Bench press variant 10').
fact(e_ex_184, uses_muscle, m_chest).
fact(e_ex_184, uses_muscle, m_triceps).
fact(e_ex_184, requires_equipment, eq_barbell).
attr(e_ex_184, difficulty, hard).

entity(e_ex_185, exercise, 'Row variant 10').
fact(e_ex_185, uses_muscle, m_back).
fact(e_ex_185, uses_muscle, m_biceps).
fact(e_ex_185, requires_equipment, eq_dumbbell).
attr(e_ex_185, difficulty, hard).

entity(e_ex_186, exercise, 'Shoulder press variant 10').
fact(e_ex_186, uses_muscle, m_shoulders).
fact(e_ex_186, uses_muscle, m_triceps).
fact(e_ex_186, requires_equipment, eq_dumbbell).
attr(e_ex_186, difficulty, hard).

entity(e_ex_187, exercise, 'Biceps curl variant 10').
fact(e_ex_187, uses_muscle, m_biceps).
fact(e_ex_187, requires_equipment, eq_dumbbell).
attr(e_ex_187, difficulty, hard).

entity(e_ex_188, exercise, 'Triceps extension variant 10').
fact(e_ex_188, uses_muscle, m_triceps).
fact(e_ex_188, requires_equipment, eq_dumbbell).
attr(e_ex_188, difficulty, hard).

entity(e_ex_189, exercise, 'Plank variant 10').
fact(e_ex_189, uses_muscle, m_core).
fact(e_ex_189, requires_equipment, eq_none).
attr(e_ex_189, difficulty, hard).

entity(e_ex_190, exercise, 'Jump rope variant 10').
fact(e_ex_190, uses_muscle, m_calves).
fact(e_ex_190, requires_equipment, eq_none).
attr(e_ex_190, difficulty, hard).

entity(e_ex_191, exercise, 'Leg press variant 10').
fact(e_ex_191, uses_muscle, m_quadriceps).
fact(e_ex_191, requires_equipment, eq_leg_press_machine).
attr(e_ex_191, difficulty, hard).

entity(e_ex_192, exercise, 'Glute bridge variant 10').
fact(e_ex_192, uses_muscle, m_glutes).
fact(e_ex_192, requires_equipment, eq_none).
attr(e_ex_192, difficulty, hard).

entity(e_ex_193, exercise, 'Push-up variant 10').
fact(e_ex_193, uses_muscle, m_chest).
fact(e_ex_193, uses_muscle, m_triceps).
fact(e_ex_193, requires_equipment, eq_none).
attr(e_ex_193, difficulty, hard).

entity(e_ex_194, exercise, 'Pull-up variant 10').
fact(e_ex_194, uses_muscle, m_back).
fact(e_ex_194, uses_muscle, m_biceps).
fact(e_ex_194, requires_equipment, eq_none).
attr(e_ex_194, difficulty, hard).

entity(e_ex_195, exercise, 'Kettlebell swing variant 10').
fact(e_ex_195, uses_muscle, m_glutes).
fact(e_ex_195, uses_muscle, m_hamstrings).
fact(e_ex_195, requires_equipment, eq_kettlebell).
attr(e_ex_195, difficulty, hard).

entity(e_ex_196, exercise, 'Box jump variant 10').
fact(e_ex_196, uses_muscle, m_quadriceps).
fact(e_ex_196, uses_muscle, m_glutes).
fact(e_ex_196, requires_equipment, eq_none).
attr(e_ex_196, difficulty, hard).

entity(e_ex_197, exercise, 'Burpee variant 10').
fact(e_ex_197, uses_muscle, m_quadriceps).
fact(e_ex_197, uses_muscle, m_chest).
fact(e_ex_197, requires_equipment, eq_none).
attr(e_ex_197, difficulty, hard).

entity(e_ex_198, exercise, 'Mountain climber variant 10').
fact(e_ex_198, uses_muscle, m_core).
fact(e_ex_198, requires_equipment, eq_none).
attr(e_ex_198, difficulty, hard).

entity(e_ex_199, exercise, 'Cycling variant 10').
fact(e_ex_199, uses_muscle, m_quadriceps).
fact(e_ex_199, requires_equipment, eq_none).
attr(e_ex_199, difficulty, hard).

entity(e_ex_200, exercise, 'Rowing machine variant 10').
fact(e_ex_200, uses_muscle, m_back).
fact(e_ex_200, uses_muscle, m_quadriceps).
fact(e_ex_200, requires_equipment, eq_none).
attr(e_ex_200, difficulty, hard).

% --- Substitutions and contraindications -------------------------------------
% Leg press substitutes for squat (matching variant numbers)
fact(e_ex_011, substitution_for, e_ex_001).
fact(e_ex_031, substitution_for, e_ex_021).
fact(e_ex_051, substitution_for, e_ex_041).
fact(e_ex_071, substitution_for, e_ex_061).
fact(e_ex_091, substitution_for, e_ex_081).
fact(e_ex_111, substitution_for, e_ex_101).
fact(e_ex_131, substitution_for, e_ex_121).
fact(e_ex_151, substitution_for, e_ex_141).
fact(e_ex_171, substitution_for, e_ex_161).
fact(e_ex_191, substitution_for, e_ex_181).

% Contraindications: knee for squat, lunge, box_jump (all variants)
% deadlift -> low_back; bench_press and shoulder_press -> shoulder
:- multifile fact/3.
fact(e_ex_001, contraindicated_for, cond_knee).
fact(e_ex_002, contraindicated_for, cond_knee).
fact(e_ex_016, contraindicated_for, cond_knee).
fact(e_ex_021, contraindicated_for, cond_knee).
fact(e_ex_022, contraindicated_for, cond_knee).
fact(e_ex_036, contraindicated_for, cond_knee).
fact(e_ex_041, contraindicated_for, cond_knee).
fact(e_ex_042, contraindicated_for, cond_knee).
fact(e_ex_056, contraindicated_for, cond_knee).
fact(e_ex_061, contraindicated_for, cond_knee).
fact(e_ex_062, contraindicated_for, cond_knee).
fact(e_ex_076, contraindicated_for, cond_knee).
fact(e_ex_081, contraindicated_for, cond_knee).
fact(e_ex_082, contraindicated_for, cond_knee).
fact(e_ex_096, contraindicated_for, cond_knee).
fact(e_ex_101, contraindicated_for, cond_knee).
fact(e_ex_102, contraindicated_for, cond_knee).
fact(e_ex_116, contraindicated_for, cond_knee).
fact(e_ex_121, contraindicated_for, cond_knee).
fact(e_ex_122, contraindicated_for, cond_knee).
fact(e_ex_136, contraindicated_for, cond_knee).
fact(e_ex_141, contraindicated_for, cond_knee).
fact(e_ex_142, contraindicated_for, cond_knee).
fact(e_ex_156, contraindicated_for, cond_knee).
fact(e_ex_161, contraindicated_for, cond_knee).
fact(e_ex_162, contraindicated_for, cond_knee).
fact(e_ex_176, contraindicated_for, cond_knee).
fact(e_ex_181, contraindicated_for, cond_knee).
fact(e_ex_182, contraindicated_for, cond_knee).
fact(e_ex_196, contraindicated_for, cond_knee).

% deadlift contraindicated for low back
fact(e_ex_003, contraindicated_for, cond_low_back).
fact(e_ex_023, contraindicated_for, cond_low_back).
fact(e_ex_043, contraindicated_for, cond_low_back).
fact(e_ex_063, contraindicated_for, cond_low_back).
fact(e_ex_083, contraindicated_for, cond_low_back).
fact(e_ex_103, contraindicated_for, cond_low_back).
fact(e_ex_123, contraindicated_for, cond_low_back).
fact(e_ex_143, contraindicated_for, cond_low_back).
fact(e_ex_163, contraindicated_for, cond_low_back).
fact(e_ex_183, contraindicated_for, cond_low_back).

% bench press and shoulder press -> shoulder contraindication (some variants)
fact(e_ex_004, contraindicated_for, cond_shoulder).
fact(e_ex_024, contraindicated_for, cond_shoulder).
fact(e_ex_044, contraindicated_for, cond_shoulder).
fact(e_ex_064, contraindicated_for, cond_shoulder).
fact(e_ex_006, contraindicated_for, cond_shoulder).
fact(e_ex_026, contraindicated_for, cond_shoulder).
fact(e_ex_046, contraindicated_for, cond_shoulder).
fact(e_ex_066, contraindicated_for, cond_shoulder).

% --- Progression links (variant N -> variant N+1 for the same base type)
% For each i from 1..180, progression_to e_ex_{i} -> e_ex_{i+20}
progression_to(e_ex_001, e_ex_021).
progression_to(e_ex_002, e_ex_022).
progression_to(e_ex_003, e_ex_023).
progression_to(e_ex_004, e_ex_024).
progression_to(e_ex_005, e_ex_025).
progression_to(e_ex_006, e_ex_026).
progression_to(e_ex_007, e_ex_027).
progression_to(e_ex_008, e_ex_028).
progression_to(e_ex_009, e_ex_029).
progression_to(e_ex_010, e_ex_030).
progression_to(e_ex_011, e_ex_031).
progression_to(e_ex_012, e_ex_032).
progression_to(e_ex_013, e_ex_033).
progression_to(e_ex_014, e_ex_034).
progression_to(e_ex_015, e_ex_035).
progression_to(e_ex_016, e_ex_036).
progression_to(e_ex_017, e_ex_037).
progression_to(e_ex_018, e_ex_038).
progression_to(e_ex_019, e_ex_039).
progression_to(e_ex_020, e_ex_040).

progression_to(e_ex_021, e_ex_041).
progression_to(e_ex_022, e_ex_042).
progression_to(e_ex_023, e_ex_043).
progression_to(e_ex_024, e_ex_044).
progression_to(e_ex_025, e_ex_045).
progression_to(e_ex_026, e_ex_046).
progression_to(e_ex_027, e_ex_047).
progression_to(e_ex_028, e_ex_048).
progression_to(e_ex_029, e_ex_049).
progression_to(e_ex_030, e_ex_050).
progression_to(e_ex_031, e_ex_051).
progression_to(e_ex_032, e_ex_052).
progression_to(e_ex_033, e_ex_053).
progression_to(e_ex_034, e_ex_054).
progression_to(e_ex_035, e_ex_055).
progression_to(e_ex_036, e_ex_056).
progression_to(e_ex_037, e_ex_057).
progression_to(e_ex_038, e_ex_058).
progression_to(e_ex_039, e_ex_059).
progression_to(e_ex_040, e_ex_060).

progression_to(e_ex_041, e_ex_061).
progression_to(e_ex_042, e_ex_062).
progression_to(e_ex_043, e_ex_063).
progression_to(e_ex_044, e_ex_064).
progression_to(e_ex_045, e_ex_065).
progression_to(e_ex_046, e_ex_066).
progression_to(e_ex_047, e_ex_067).
progression_to(e_ex_048, e_ex_068).
progression_to(e_ex_049, e_ex_069).
progression_to(e_ex_050, e_ex_070).
progression_to(e_ex_051, e_ex_071).
progression_to(e_ex_052, e_ex_072).
progression_to(e_ex_053, e_ex_073).
progression_to(e_ex_054, e_ex_074).
progression_to(e_ex_055, e_ex_075).
progression_to(e_ex_056, e_ex_076).
progression_to(e_ex_057, e_ex_077).
progression_to(e_ex_058, e_ex_078).
progression_to(e_ex_059, e_ex_079).
progression_to(e_ex_060, e_ex_080).

progression_to(e_ex_061, e_ex_081).
progression_to(e_ex_062, e_ex_082).
progression_to(e_ex_063, e_ex_083).
progression_to(e_ex_064, e_ex_084).
progression_to(e_ex_065, e_ex_085).
progression_to(e_ex_066, e_ex_086).
progression_to(e_ex_067, e_ex_087).
progression_to(e_ex_068, e_ex_088).
progression_to(e_ex_069, e_ex_089).
progression_to(e_ex_070, e_ex_090).
progression_to(e_ex_071, e_ex_091).
progression_to(e_ex_072, e_ex_092).
progression_to(e_ex_073, e_ex_093).
progression_to(e_ex_074, e_ex_094).
progression_to(e_ex_075, e_ex_095).
progression_to(e_ex_076, e_ex_096).
progression_to(e_ex_077, e_ex_097).
progression_to(e_ex_078, e_ex_098).
progression_to(e_ex_079, e_ex_099).
progression_to(e_ex_080, e_ex_100).

progression_to(e_ex_081, e_ex_101).
progression_to(e_ex_082, e_ex_102).
progression_to(e_ex_083, e_ex_103).
progression_to(e_ex_084, e_ex_104).
progression_to(e_ex_085, e_ex_105).
progression_to(e_ex_086, e_ex_106).
progression_to(e_ex_087, e_ex_107).
progression_to(e_ex_088, e_ex_108).
progression_to(e_ex_089, e_ex_109).
progression_to(e_ex_090, e_ex_110).
progression_to(e_ex_091, e_ex_111).
progression_to(e_ex_092, e_ex_112).
progression_to(e_ex_093, e_ex_113).
progression_to(e_ex_094, e_ex_114).
progression_to(e_ex_095, e_ex_115).
progression_to(e_ex_096, e_ex_116).
progression_to(e_ex_097, e_ex_117).
progression_to(e_ex_098, e_ex_118).
progression_to(e_ex_099, e_ex_119).
progression_to(e_ex_100, e_ex_120).

progression_to(e_ex_101, e_ex_121).
progression_to(e_ex_102, e_ex_122).
progression_to(e_ex_103, e_ex_123).
progression_to(e_ex_104, e_ex_124).
progression_to(e_ex_105, e_ex_125).
progression_to(e_ex_106, e_ex_126).
progression_to(e_ex_107, e_ex_127).
progression_to(e_ex_108, e_ex_128).
progression_to(e_ex_109, e_ex_129).
progression_to(e_ex_110, e_ex_130).
progression_to(e_ex_111, e_ex_131).
progression_to(e_ex_112, e_ex_132).
progression_to(e_ex_113, e_ex_133).
progression_to(e_ex_114, e_ex_134).
progression_to(e_ex_115, e_ex_135).
progression_to(e_ex_116, e_ex_136).
progression_to(e_ex_117, e_ex_137).
progression_to(e_ex_118, e_ex_138).
progression_to(e_ex_119, e_ex_139).
progression_to(e_ex_120, e_ex_140).

progression_to(e_ex_121, e_ex_141).
progression_to(e_ex_122, e_ex_142).
progression_to(e_ex_123, e_ex_143).
progression_to(e_ex_124, e_ex_144).
progression_to(e_ex_125, e_ex_145).
progression_to(e_ex_126, e_ex_146).
progression_to(e_ex_127, e_ex_147).
progression_to(e_ex_128, e_ex_148).
progression_to(e_ex_129, e_ex_149).
progression_to(e_ex_130, e_ex_150).
progression_to(e_ex_131, e_ex_151).
progression_to(e_ex_132, e_ex_152).
progression_to(e_ex_133, e_ex_153).
progression_to(e_ex_134, e_ex_154).
progression_to(e_ex_135, e_ex_155).
progression_to(e_ex_136, e_ex_156).
progression_to(e_ex_137, e_ex_157).
progression_to(e_ex_138, e_ex_158).
progression_to(e_ex_139, e_ex_159).
progression_to(e_ex_140, e_ex_160).

progression_to(e_ex_141, e_ex_161).
progression_to(e_ex_142, e_ex_162).
progression_to(e_ex_143, e_ex_163).
progression_to(e_ex_144, e_ex_164).
progression_to(e_ex_145, e_ex_165).
progression_to(e_ex_146, e_ex_166).
progression_to(e_ex_147, e_ex_167).
progression_to(e_ex_148, e_ex_168).
progression_to(e_ex_149, e_ex_169).
progression_to(e_ex_150, e_ex_170).
progression_to(e_ex_151, e_ex_171).
progression_to(e_ex_152, e_ex_172).
progression_to(e_ex_153, e_ex_173).
progression_to(e_ex_154, e_ex_174).
progression_to(e_ex_155, e_ex_175).
progression_to(e_ex_156, e_ex_176).
progression_to(e_ex_157, e_ex_177).
progression_to(e_ex_158, e_ex_178).
progression_to(e_ex_159, e_ex_179).
progression_to(e_ex_160, e_ex_180).

progression_to(e_ex_161, e_ex_181).
progression_to(e_ex_162, e_ex_182).
progression_to(e_ex_163, e_ex_183).
progression_to(e_ex_164, e_ex_184).
progression_to(e_ex_165, e_ex_185).
progression_to(e_ex_166, e_ex_186).
progression_to(e_ex_167, e_ex_187).
progression_to(e_ex_168, e_ex_188).
progression_to(e_ex_169, e_ex_189).
progression_to(e_ex_170, e_ex_190).
progression_to(e_ex_171, e_ex_191).
progression_to(e_ex_172, e_ex_192).
progression_to(e_ex_173, e_ex_193).
progression_to(e_ex_174, e_ex_194).
progression_to(e_ex_175, e_ex_195).
progression_to(e_ex_176, e_ex_196).
progression_to(e_ex_177, e_ex_197).
progression_to(e_ex_178, e_ex_198).
progression_to(e_ex_179, e_ex_199).
progression_to(e_ex_180, e_ex_200).

% --- Simple sample user capability and schedule
capability(u_test, max_rep(e_ex_021, 80, 5)).
schedule(u_test, date(2025,11,15), time(18,0), activity(e_ex_021), 45).

% File end


