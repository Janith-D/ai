:- discontiguous fact/3.
:- discontiguous attr/3.
:- discontiguous entity/3.
:- discontiguous uses_muscle/2.
:- discontiguous requires_equipment/2.
:- discontiguous difficulty/2.
:- discontiguous contraindicated_for/2.
:- discontiguous substitution_for/2.

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
uses_muscle(e_ex_001,m_quadriceps).
uses_muscle(e_ex_001,m_glutes).
requires_equipment(e_ex_001,eq_barbell).
difficulty(e_ex_001,easy).

entity(e_ex_002, exercise, 'Lunge variant 1').
uses_muscle(e_ex_002,m_quadriceps).
uses_muscle(e_ex_002,m_glutes).
requires_equipment(e_ex_002,eq_none).
difficulty(e_ex_002,easy).

entity(e_ex_003, exercise, 'Deadlift variant 1').
uses_muscle(e_ex_003,m_hamstrings).
uses_muscle(e_ex_003,m_glutes).
requires_equipment(e_ex_003,eq_barbell).
difficulty(e_ex_003,easy).

entity(e_ex_004, exercise, 'Bench press variant 1').
uses_muscle(e_ex_004,m_chest).
uses_muscle(e_ex_004,m_triceps).
requires_equipment(e_ex_004,eq_barbell).
difficulty(e_ex_004,easy).

entity(e_ex_005, exercise, 'Row variant 1').
uses_muscle(e_ex_005,m_back).
uses_muscle(e_ex_005,m_biceps).
requires_equipment(e_ex_005,eq_dumbbell).
difficulty(e_ex_005,easy).

entity(e_ex_006, exercise, 'Shoulder press variant 1').
uses_muscle(e_ex_006,m_shoulders).
uses_muscle(e_ex_006,m_triceps).
requires_equipment(e_ex_006,eq_dumbbell).
difficulty(e_ex_006,easy).

entity(e_ex_007, exercise, 'Biceps curl variant 1').
uses_muscle(e_ex_007,m_biceps).
requires_equipment(e_ex_007,eq_dumbbell).
difficulty(e_ex_007,easy).

entity(e_ex_008, exercise, 'Triceps extension variant 1').
uses_muscle(e_ex_008,m_triceps).
requires_equipment(e_ex_008,eq_dumbbell).
difficulty(e_ex_008,easy).

entity(e_ex_009, exercise, 'Plank variant 1').
uses_muscle(e_ex_009,m_core).
requires_equipment(e_ex_009,eq_none).
difficulty(e_ex_009,easy).

entity(e_ex_010, exercise, 'Jump rope variant 1').
uses_muscle(e_ex_010,m_calves).
requires_equipment(e_ex_010,eq_none).
difficulty(e_ex_010,easy).

entity(e_ex_011, exercise, 'Leg press variant 1').
uses_muscle(e_ex_011,m_quadriceps).
requires_equipment(e_ex_011,eq_leg_press_machine).
difficulty(e_ex_011,easy).

entity(e_ex_012, exercise, 'Glute bridge variant 1').
uses_muscle(e_ex_012,m_glutes).
requires_equipment(e_ex_012,eq_none).
difficulty(e_ex_012,easy).

entity(e_ex_013, exercise, 'Push-up variant 1').
uses_muscle(e_ex_013,m_chest).
uses_muscle(e_ex_013,m_triceps).
requires_equipment(e_ex_013,eq_none).
difficulty(e_ex_013,easy).

entity(e_ex_014, exercise, 'Pull-up variant 1').
uses_muscle(e_ex_014,m_back).
uses_muscle(e_ex_014,m_biceps).
requires_equipment(e_ex_014,eq_none).
difficulty(e_ex_014,easy).

entity(e_ex_015, exercise, 'Kettlebell swing variant 1').
uses_muscle(e_ex_015,m_glutes).
uses_muscle(e_ex_015,m_hamstrings).
requires_equipment(e_ex_015,eq_kettlebell).
difficulty(e_ex_015,easy).

entity(e_ex_016, exercise, 'Box jump variant 1').
uses_muscle(e_ex_016,m_quadriceps).
uses_muscle(e_ex_016,m_glutes).
requires_equipment(e_ex_016,eq_none).
difficulty(e_ex_016,easy).

entity(e_ex_017, exercise, 'Burpee variant 1').
uses_muscle(e_ex_017,m_quadriceps).
uses_muscle(e_ex_017,m_chest).
requires_equipment(e_ex_017,eq_none).
difficulty(e_ex_017,easy).

entity(e_ex_018, exercise, 'Mountain climber variant 1').
uses_muscle(e_ex_018,m_core).
requires_equipment(e_ex_018,eq_none).
difficulty(e_ex_018,easy).

entity(e_ex_019, exercise, 'Cycling variant 1').
uses_muscle(e_ex_019,m_quadriceps).
requires_equipment(e_ex_019,eq_none).
difficulty(e_ex_019,easy).

entity(e_ex_020, exercise, 'Rowing machine variant 1').
uses_muscle(e_ex_020,m_back).
uses_muscle(e_ex_020,m_quadriceps).
requires_equipment(e_ex_020,eq_none).
difficulty(e_ex_020,easy).

% --- Variant 2 (indices 21..40)
entity(e_ex_021, exercise, 'Squat variant 2').
uses_muscle(e_ex_021,m_quadriceps).
uses_muscle(e_ex_021,m_glutes).
requires_equipment(e_ex_021,eq_barbell).
difficulty(e_ex_021,medium).

entity(e_ex_022, exercise, 'Lunge variant 2').
uses_muscle(e_ex_022,m_quadriceps).
uses_muscle(e_ex_022,m_glutes).
requires_equipment(e_ex_022,eq_none).
difficulty(e_ex_022,medium).

entity(e_ex_023, exercise, 'Deadlift variant 2').
uses_muscle(e_ex_023,m_hamstrings).
uses_muscle(e_ex_023,m_glutes).
requires_equipment(e_ex_023,eq_barbell).
difficulty(e_ex_023,medium).

entity(e_ex_024, exercise, 'Bench press variant 2').
uses_muscle(e_ex_024,m_chest).
uses_muscle(e_ex_024,m_triceps).
requires_equipment(e_ex_024,eq_barbell).
difficulty(e_ex_024,medium).

entity(e_ex_025, exercise, 'Row variant 2').
uses_muscle(e_ex_025,m_back).
uses_muscle(e_ex_025,m_biceps).
requires_equipment(e_ex_025,eq_dumbbell).
difficulty(e_ex_025,medium).

entity(e_ex_026, exercise, 'Shoulder press variant 2').
uses_muscle(e_ex_026,m_shoulders).
uses_muscle(e_ex_026,m_triceps).
requires_equipment(e_ex_026,eq_dumbbell).
difficulty(e_ex_026,medium).

entity(e_ex_027, exercise, 'Biceps curl variant 2').
uses_muscle(e_ex_027,m_biceps).
requires_equipment(e_ex_027,eq_dumbbell).
difficulty(e_ex_027,medium).

entity(e_ex_028, exercise, 'Triceps extension variant 2').
uses_muscle(e_ex_028,m_triceps).
requires_equipment(e_ex_028,eq_dumbbell).
difficulty(e_ex_028,medium).

entity(e_ex_029, exercise, 'Plank variant 2').
uses_muscle(e_ex_029,m_core).
requires_equipment(e_ex_029,eq_none).
difficulty(e_ex_029,medium).

entity(e_ex_030, exercise, 'Jump rope variant 2').
uses_muscle(e_ex_030,m_calves).
requires_equipment(e_ex_030,eq_none).
difficulty(e_ex_030,medium).

entity(e_ex_031, exercise, 'Leg press variant 2').
uses_muscle(e_ex_031,m_quadriceps).
requires_equipment(e_ex_031,eq_leg_press_machine).
difficulty(e_ex_031,medium).

entity(e_ex_032, exercise, 'Glute bridge variant 2').
uses_muscle(e_ex_032,m_glutes).
requires_equipment(e_ex_032,eq_none).
difficulty(e_ex_032,medium).

entity(e_ex_033, exercise, 'Push-up variant 2').
uses_muscle(e_ex_033,m_chest).
uses_muscle(e_ex_033,m_triceps).
requires_equipment(e_ex_033,eq_none).
difficulty(e_ex_033,medium).

entity(e_ex_034, exercise, 'Pull-up variant 2').
uses_muscle(e_ex_034,m_back).
uses_muscle(e_ex_034,m_biceps).
requires_equipment(e_ex_034,eq_none).
difficulty(e_ex_034,medium).

entity(e_ex_035, exercise, 'Kettlebell swing variant 2').
uses_muscle(e_ex_035,m_glutes).
uses_muscle(e_ex_035,m_hamstrings).
requires_equipment(e_ex_035,eq_kettlebell).
difficulty(e_ex_035,medium).

entity(e_ex_036, exercise, 'Box jump variant 2').
uses_muscle(e_ex_036,m_quadriceps).
uses_muscle(e_ex_036,m_glutes).
requires_equipment(e_ex_036,eq_none).
difficulty(e_ex_036,medium).

entity(e_ex_037, exercise, 'Burpee variant 2').
uses_muscle(e_ex_037,m_quadriceps).
uses_muscle(e_ex_037,m_chest).
requires_equipment(e_ex_037,eq_none).
difficulty(e_ex_037,medium).

entity(e_ex_038, exercise, 'Mountain climber variant 2').
uses_muscle(e_ex_038,m_core).
requires_equipment(e_ex_038,eq_none).
difficulty(e_ex_038,medium).

entity(e_ex_039, exercise, 'Cycling variant 2').
uses_muscle(e_ex_039,m_quadriceps).
requires_equipment(e_ex_039,eq_none).
difficulty(e_ex_039,medium).

entity(e_ex_040, exercise, 'Rowing machine variant 2').
uses_muscle(e_ex_040,m_back).
uses_muscle(e_ex_040,m_quadriceps).
requires_equipment(e_ex_040,eq_none).
difficulty(e_ex_040,medium).

% --- Variant 3 (indices 41..60)
entity(e_ex_041, exercise, 'Squat variant 3').
uses_muscle(e_ex_041,m_quadriceps).
uses_muscle(e_ex_041,m_glutes).
requires_equipment(e_ex_041,eq_barbell).
difficulty(e_ex_041,medium).

entity(e_ex_042, exercise, 'Lunge variant 3').
uses_muscle(e_ex_042,m_quadriceps).
uses_muscle(e_ex_042,m_glutes).
requires_equipment(e_ex_042,eq_none).
difficulty(e_ex_042,medium).

entity(e_ex_043, exercise, 'Deadlift variant 3').
uses_muscle(e_ex_043,m_hamstrings).
uses_muscle(e_ex_043,m_glutes).
requires_equipment(e_ex_043,eq_barbell).
difficulty(e_ex_043,medium).

entity(e_ex_044, exercise, 'Bench press variant 3').
uses_muscle(e_ex_044,m_chest).
uses_muscle(e_ex_044,m_triceps).
requires_equipment(e_ex_044,eq_barbell).
difficulty(e_ex_044,medium).

entity(e_ex_045, exercise, 'Row variant 3').
uses_muscle(e_ex_045,m_back).
uses_muscle(e_ex_045,m_biceps).
requires_equipment(e_ex_045,eq_dumbbell).
difficulty(e_ex_045,medium).

entity(e_ex_046, exercise, 'Shoulder press variant 3').
uses_muscle(e_ex_046,m_shoulders).
uses_muscle(e_ex_046,m_triceps).
requires_equipment(e_ex_046,eq_dumbbell).
difficulty(e_ex_046,medium).

entity(e_ex_047, exercise, 'Biceps curl variant 3').
uses_muscle(e_ex_047,m_biceps).
requires_equipment(e_ex_047,eq_dumbbell).
difficulty(e_ex_047,medium).

entity(e_ex_048, exercise, 'Triceps extension variant 3').
uses_muscle(e_ex_048,m_triceps).
requires_equipment(e_ex_048,eq_dumbbell).
difficulty(e_ex_048,medium).

entity(e_ex_049, exercise, 'Plank variant 3').
uses_muscle(e_ex_049,m_core).
requires_equipment(e_ex_049,eq_none).
difficulty(e_ex_049,medium).

entity(e_ex_050, exercise, 'Jump rope variant 3').
uses_muscle(e_ex_050,m_calves).
requires_equipment(e_ex_050,eq_none).
difficulty(e_ex_050,medium).

entity(e_ex_051, exercise, 'Leg press variant 3').
uses_muscle(e_ex_051,m_quadriceps).
requires_equipment(e_ex_051,eq_leg_press_machine).
difficulty(e_ex_051,medium).

entity(e_ex_052, exercise, 'Glute bridge variant 3').
uses_muscle(e_ex_052,m_glutes).
requires_equipment(e_ex_052,eq_none).
difficulty(e_ex_052,medium).

entity(e_ex_053, exercise, 'Push-up variant 3').
uses_muscle(e_ex_053,m_chest).
uses_muscle(e_ex_053,m_triceps).
requires_equipment(e_ex_053,eq_none).
difficulty(e_ex_053,medium).

entity(e_ex_054, exercise, 'Pull-up variant 3').
uses_muscle(e_ex_054,m_back).
uses_muscle(e_ex_054,m_biceps).
requires_equipment(e_ex_054,eq_none).
difficulty(e_ex_054,medium).

entity(e_ex_055, exercise, 'Kettlebell swing variant 3').
uses_muscle(e_ex_055,m_glutes).
uses_muscle(e_ex_055,m_hamstrings).
requires_equipment(e_ex_055,eq_kettlebell).
difficulty(e_ex_055,medium).

entity(e_ex_056, exercise, 'Box jump variant 3').
uses_muscle(e_ex_056,m_quadriceps).
uses_muscle(e_ex_056,m_glutes).
requires_equipment(e_ex_056,eq_none).
difficulty(e_ex_056,medium).

entity(e_ex_057, exercise, 'Burpee variant 3').
uses_muscle(e_ex_057,m_quadriceps).
uses_muscle(e_ex_057,m_chest).
requires_equipment(e_ex_057,eq_none).
difficulty(e_ex_057,medium).

entity(e_ex_058, exercise, 'Mountain climber variant 3').
uses_muscle(e_ex_058,m_core).
requires_equipment(e_ex_058,eq_none).
difficulty(e_ex_058,medium).

entity(e_ex_059, exercise, 'Cycling variant 3').
uses_muscle(e_ex_059,m_quadriceps).
requires_equipment(e_ex_059,eq_none).
difficulty(e_ex_059,medium).

entity(e_ex_060, exercise, 'Rowing machine variant 3').
uses_muscle(e_ex_060,m_back).
uses_muscle(e_ex_060,m_quadriceps).
requires_equipment(e_ex_060,eq_none).
difficulty(e_ex_060,medium).

% --- Variant 4 (indices 61..80)
entity(e_ex_061, exercise, 'Squat variant 4').
uses_muscle(e_ex_061,m_quadriceps).
uses_muscle(e_ex_061,m_glutes).
requires_equipment(e_ex_061,eq_barbell).
difficulty(e_ex_061,medium).

entity(e_ex_062, exercise, 'Lunge variant 4').
uses_muscle(e_ex_062,m_quadriceps).
uses_muscle(e_ex_062,m_glutes).
requires_equipment(e_ex_062,eq_none).
difficulty(e_ex_062,medium).

entity(e_ex_063, exercise, 'Deadlift variant 4').
uses_muscle(e_ex_063,m_hamstrings).
uses_muscle(e_ex_063,m_glutes).
requires_equipment(e_ex_063,eq_barbell).
difficulty(e_ex_063,medium).

entity(e_ex_064, exercise, 'Bench press variant 4').
uses_muscle(e_ex_064,m_chest).
uses_muscle(e_ex_064,m_triceps).
requires_equipment(e_ex_064,eq_barbell).
difficulty(e_ex_064,medium).

entity(e_ex_065, exercise, 'Row variant 4').
uses_muscle(e_ex_065,m_back).
uses_muscle(e_ex_065,m_biceps).
requires_equipment(e_ex_065,eq_dumbbell).
difficulty(e_ex_065,medium).

entity(e_ex_066, exercise, 'Shoulder press variant 4').
uses_muscle(e_ex_066,m_shoulders).
uses_muscle(e_ex_066,m_triceps).
requires_equipment(e_ex_066,eq_dumbbell).
difficulty(e_ex_066,medium).

entity(e_ex_067, exercise, 'Biceps curl variant 4').
uses_muscle(e_ex_067,m_biceps).
requires_equipment(e_ex_067,eq_dumbbell).
difficulty(e_ex_067,medium).

entity(e_ex_068, exercise, 'Triceps extension variant 4').
uses_muscle(e_ex_068,m_triceps).
requires_equipment(e_ex_068,eq_dumbbell).
difficulty(e_ex_068,medium).

entity(e_ex_069, exercise, 'Plank variant 4').
uses_muscle(e_ex_069,m_core).
requires_equipment(e_ex_069,eq_none).
difficulty(e_ex_069,medium).

entity(e_ex_070, exercise, 'Jump rope variant 4').
uses_muscle(e_ex_070,m_calves).
requires_equipment(e_ex_070,eq_none).
difficulty(e_ex_070,medium).

entity(e_ex_071, exercise, 'Leg press variant 4').
uses_muscle(e_ex_071,m_quadriceps).
requires_equipment(e_ex_071,eq_leg_press_machine).
difficulty(e_ex_071,medium).

entity(e_ex_072, exercise, 'Glute bridge variant 4').
uses_muscle(e_ex_072,m_glutes).
requires_equipment(e_ex_072,eq_none).
difficulty(e_ex_072,medium).

entity(e_ex_073, exercise, 'Push-up variant 4').
uses_muscle(e_ex_073,m_chest).
uses_muscle(e_ex_073,m_triceps).
requires_equipment(e_ex_073,eq_none).
difficulty(e_ex_073,medium).

entity(e_ex_074, exercise, 'Pull-up variant 4').
uses_muscle(e_ex_074,m_back).
uses_muscle(e_ex_074,m_biceps).
requires_equipment(e_ex_074,eq_none).
difficulty(e_ex_074,medium).

entity(e_ex_075, exercise, 'Kettlebell swing variant 4').
uses_muscle(e_ex_075,m_glutes).
uses_muscle(e_ex_075,m_hamstrings).
requires_equipment(e_ex_075,eq_kettlebell).
difficulty(e_ex_075,medium).

entity(e_ex_076, exercise, 'Box jump variant 4').
uses_muscle(e_ex_076,m_quadriceps).
uses_muscle(e_ex_076,m_glutes).
requires_equipment(e_ex_076,eq_none).
difficulty(e_ex_076,medium).

entity(e_ex_077, exercise, 'Burpee variant 4').
uses_muscle(e_ex_077,m_quadriceps).
uses_muscle(e_ex_077,m_chest).
requires_equipment(e_ex_077,eq_none).
difficulty(e_ex_077,medium).

entity(e_ex_078, exercise, 'Mountain climber variant 4').
uses_muscle(e_ex_078,m_core).
requires_equipment(e_ex_078,eq_none).
difficulty(e_ex_078,medium).

entity(e_ex_079, exercise, 'Cycling variant 4').
uses_muscle(e_ex_079,m_quadriceps).
requires_equipment(e_ex_079,eq_none).
difficulty(e_ex_079,medium).

entity(e_ex_080, exercise, 'Rowing machine variant 4').
uses_muscle(e_ex_080,m_back).
uses_muscle(e_ex_080,m_quadriceps).
requires_equipment(e_ex_080,eq_none).
difficulty(e_ex_080,medium).

% --- Variant 5 (indices 81..100)
entity(e_ex_081, exercise, 'Squat variant 5').
uses_muscle(e_ex_081,m_quadriceps).
uses_muscle(e_ex_081,m_glutes).
requires_equipment(e_ex_081,eq_barbell).
difficulty(e_ex_081,hard).

entity(e_ex_082, exercise, 'Lunge variant 5').
uses_muscle(e_ex_082,m_quadriceps).
uses_muscle(e_ex_082,m_glutes).
requires_equipment(e_ex_082,eq_none).
difficulty(e_ex_082,hard).

entity(e_ex_083, exercise, 'Deadlift variant 5').
uses_muscle(e_ex_083,m_hamstrings).
uses_muscle(e_ex_083,m_glutes).
requires_equipment(e_ex_083,eq_barbell).
difficulty(e_ex_083,hard).

entity(e_ex_084, exercise, 'Bench press variant 5').
uses_muscle(e_ex_084,m_chest).
uses_muscle(e_ex_084,m_triceps).
requires_equipment(e_ex_084,eq_barbell).
difficulty(e_ex_084,hard).

entity(e_ex_085, exercise, 'Row variant 5').
uses_muscle(e_ex_085,m_back).
uses_muscle(e_ex_085,m_biceps).
requires_equipment(e_ex_085,eq_dumbbell).
difficulty(e_ex_085,hard).

entity(e_ex_086, exercise, 'Shoulder press variant 5').
uses_muscle(e_ex_086,m_shoulders).
uses_muscle(e_ex_086,m_triceps).
requires_equipment(e_ex_086,eq_dumbbell).
difficulty(e_ex_086,hard).

entity(e_ex_087, exercise, 'Biceps curl variant 5').
uses_muscle(e_ex_087,m_biceps).
requires_equipment(e_ex_087,eq_dumbbell).
difficulty(e_ex_087,hard).

entity(e_ex_088, exercise, 'Triceps extension variant 5').
uses_muscle(e_ex_088,m_triceps).
requires_equipment(e_ex_088,eq_dumbbell).
difficulty(e_ex_088,hard).

entity(e_ex_089, exercise, 'Plank variant 5').
uses_muscle(e_ex_089,m_core).
requires_equipment(e_ex_089,eq_none).
difficulty(e_ex_089,hard).

entity(e_ex_090, exercise, 'Jump rope variant 5').
uses_muscle(e_ex_090,m_calves).
requires_equipment(e_ex_090,eq_none).
difficulty(e_ex_090,hard).

entity(e_ex_091, exercise, 'Leg press variant 5').
uses_muscle(e_ex_091,m_quadriceps).
requires_equipment(e_ex_091,eq_leg_press_machine).
difficulty(e_ex_091,hard).

entity(e_ex_092, exercise, 'Glute bridge variant 5').
uses_muscle(e_ex_092,m_glutes).
requires_equipment(e_ex_092,eq_none).
difficulty(e_ex_092,hard).

entity(e_ex_093, exercise, 'Push-up variant 5').
uses_muscle(e_ex_093,m_chest).
uses_muscle(e_ex_093,m_triceps).
requires_equipment(e_ex_093,eq_none).
difficulty(e_ex_093,hard).

entity(e_ex_094, exercise, 'Pull-up variant 5').
uses_muscle(e_ex_094,m_back).
uses_muscle(e_ex_094,m_biceps).
requires_equipment(e_ex_094,eq_none).
difficulty(e_ex_094,hard).

entity(e_ex_095, exercise, 'Kettlebell swing variant 5').
uses_muscle(e_ex_095,m_glutes).
uses_muscle(e_ex_095,m_hamstrings).
requires_equipment(e_ex_095,eq_kettlebell).
difficulty(e_ex_095,hard).

entity(e_ex_096, exercise, 'Box jump variant 5').
uses_muscle(e_ex_096,m_quadriceps).
uses_muscle(e_ex_096,m_glutes).
requires_equipment(e_ex_096,eq_none).
difficulty(e_ex_096,hard).

entity(e_ex_097, exercise, 'Burpee variant 5').
uses_muscle(e_ex_097,m_quadriceps).
uses_muscle(e_ex_097,m_chest).
requires_equipment(e_ex_097,eq_none).
difficulty(e_ex_097,hard).

entity(e_ex_098, exercise, 'Mountain climber variant 5').
uses_muscle(e_ex_098,m_core).
requires_equipment(e_ex_098,eq_none).
difficulty(e_ex_098,hard).

entity(e_ex_099, exercise, 'Cycling variant 5').
uses_muscle(e_ex_099,m_quadriceps).
requires_equipment(e_ex_099,eq_none).
difficulty(e_ex_099,hard).

entity(e_ex_100, exercise, 'Rowing machine variant 5').
uses_muscle(e_ex_100,m_back).
uses_muscle(e_ex_100,m_quadriceps).
requires_equipment(e_ex_100,eq_none).
difficulty(e_ex_100,hard).

% --- Variant 6 (indices 101..120)
entity(e_ex_101, exercise, 'Squat variant 6').
uses_muscle(e_ex_101,m_quadriceps).
uses_muscle(e_ex_101,m_glutes).
requires_equipment(e_ex_101,eq_barbell).
difficulty(e_ex_101,hard).

entity(e_ex_102, exercise, 'Lunge variant 6').
uses_muscle(e_ex_102,m_quadriceps).
uses_muscle(e_ex_102,m_glutes).
requires_equipment(e_ex_102,eq_none).
difficulty(e_ex_102,hard).

entity(e_ex_103, exercise, 'Deadlift variant 6').
uses_muscle(e_ex_103,m_hamstrings).
uses_muscle(e_ex_103,m_glutes).
requires_equipment(e_ex_103,eq_barbell).
difficulty(e_ex_103,hard).

entity(e_ex_104, exercise, 'Bench press variant 6').
uses_muscle(e_ex_104,m_chest).
uses_muscle(e_ex_104,m_triceps).
requires_equipment(e_ex_104,eq_barbell).
difficulty(e_ex_104,hard).

entity(e_ex_105, exercise, 'Row variant 6').
uses_muscle(e_ex_105,m_back).
uses_muscle(e_ex_105,m_biceps).
requires_equipment(e_ex_105,eq_dumbbell).
difficulty(e_ex_105,hard).

entity(e_ex_106, exercise, 'Shoulder press variant 6').
uses_muscle(e_ex_106,m_shoulders).
uses_muscle(e_ex_106,m_triceps).
requires_equipment(e_ex_106,eq_dumbbell).
difficulty(e_ex_106,hard).

entity(e_ex_107, exercise, 'Biceps curl variant 6').
uses_muscle(e_ex_107,m_biceps).
requires_equipment(e_ex_107,eq_dumbbell).
difficulty(e_ex_107,hard).

entity(e_ex_108, exercise, 'Triceps extension variant 6').
uses_muscle(e_ex_108,m_triceps).
requires_equipment(e_ex_108,eq_dumbbell).
difficulty(e_ex_108,hard).

entity(e_ex_109, exercise, 'Plank variant 6').
uses_muscle(e_ex_109,m_core).
requires_equipment(e_ex_109,eq_none).
difficulty(e_ex_109,hard).

entity(e_ex_110, exercise, 'Jump rope variant 6').
uses_muscle(e_ex_110,m_calves).
requires_equipment(e_ex_110,eq_none).
difficulty(e_ex_110,hard).

entity(e_ex_111, exercise, 'Leg press variant 6').
uses_muscle(e_ex_111,m_quadriceps).
requires_equipment(e_ex_111,eq_leg_press_machine).
difficulty(e_ex_111,hard).

entity(e_ex_112, exercise, 'Glute bridge variant 6').
uses_muscle(e_ex_112,m_glutes).
requires_equipment(e_ex_112,eq_none).
difficulty(e_ex_112,hard).

entity(e_ex_113, exercise, 'Push-up variant 6').
uses_muscle(e_ex_113,m_chest).
uses_muscle(e_ex_113,m_triceps).
requires_equipment(e_ex_113,eq_none).
difficulty(e_ex_113,hard).

entity(e_ex_114, exercise, 'Pull-up variant 6').
uses_muscle(e_ex_114,m_back).
uses_muscle(e_ex_114,m_biceps).
requires_equipment(e_ex_114,eq_none).
difficulty(e_ex_114,hard).

entity(e_ex_115, exercise, 'Kettlebell swing variant 6').
uses_muscle(e_ex_115,m_glutes).
uses_muscle(e_ex_115,m_hamstrings).
requires_equipment(e_ex_115,eq_kettlebell).
difficulty(e_ex_115,hard).

entity(e_ex_116, exercise, 'Box jump variant 6').
uses_muscle(e_ex_116,m_quadriceps).
uses_muscle(e_ex_116,m_glutes).
requires_equipment(e_ex_116,eq_none).
difficulty(e_ex_116,hard).

entity(e_ex_117, exercise, 'Burpee variant 6').
uses_muscle(e_ex_117,m_quadriceps).
uses_muscle(e_ex_117,m_chest).
requires_equipment(e_ex_117,eq_none).
difficulty(e_ex_117,hard).

entity(e_ex_118, exercise, 'Mountain climber variant 6').
uses_muscle(e_ex_118,m_core).
requires_equipment(e_ex_118,eq_none).
difficulty(e_ex_118,hard).

entity(e_ex_119, exercise, 'Cycling variant 6').
uses_muscle(e_ex_119,m_quadriceps).
requires_equipment(e_ex_119,eq_none).
difficulty(e_ex_119,hard).

entity(e_ex_120, exercise, 'Rowing machine variant 6').
uses_muscle(e_ex_120,m_back).
uses_muscle(e_ex_120,m_quadriceps).
requires_equipment(e_ex_120,eq_none).
difficulty(e_ex_120,hard).

% --- Variant 7 (indices 121..140)
entity(e_ex_121, exercise, 'Squat variant 7').
uses_muscle(e_ex_121,m_quadriceps).
uses_muscle(e_ex_121,m_glutes).
requires_equipment(e_ex_121,eq_barbell).
difficulty(e_ex_121,hard).

entity(e_ex_122, exercise, 'Lunge variant 7').
uses_muscle(e_ex_122,m_quadriceps).
uses_muscle(e_ex_122,m_glutes).
requires_equipment(e_ex_122,eq_none).
difficulty(e_ex_122,hard).

entity(e_ex_123, exercise, 'Deadlift variant 7').
uses_muscle(e_ex_123,m_hamstrings).
uses_muscle(e_ex_123,m_glutes).
requires_equipment(e_ex_123,eq_barbell).
difficulty(e_ex_123,hard).

entity(e_ex_124, exercise, 'Bench press variant 7').
uses_muscle(e_ex_124,m_chest).
uses_muscle(e_ex_124,m_triceps).
requires_equipment(e_ex_124,eq_barbell).
difficulty(e_ex_124,hard).

entity(e_ex_125, exercise, 'Row variant 7').
uses_muscle(e_ex_125,m_back).
uses_muscle(e_ex_125,m_biceps).
requires_equipment(e_ex_125,eq_dumbbell).
difficulty(e_ex_125,hard).

entity(e_ex_126, exercise, 'Shoulder press variant 7').
uses_muscle(e_ex_126,m_shoulders).
uses_muscle(e_ex_126,m_triceps).
requires_equipment(e_ex_126,eq_dumbbell).
difficulty(e_ex_126,hard).

entity(e_ex_127, exercise, 'Biceps curl variant 7').
uses_muscle(e_ex_127,m_biceps).
requires_equipment(e_ex_127,eq_dumbbell).
difficulty(e_ex_127,hard).

entity(e_ex_128, exercise, 'Triceps extension variant 7').
uses_muscle(e_ex_128,m_triceps).
requires_equipment(e_ex_128,eq_dumbbell).
difficulty(e_ex_128,hard).

entity(e_ex_129, exercise, 'Plank variant 7').
uses_muscle(e_ex_129,m_core).
requires_equipment(e_ex_129,eq_none).
difficulty(e_ex_129,hard).

entity(e_ex_130, exercise, 'Jump rope variant 7').
uses_muscle(e_ex_130,m_calves).
requires_equipment(e_ex_130,eq_none).
difficulty(e_ex_130,hard).

entity(e_ex_131, exercise, 'Leg press variant 7').
uses_muscle(e_ex_131,m_quadriceps).
requires_equipment(e_ex_131,eq_leg_press_machine).
difficulty(e_ex_131,hard).

entity(e_ex_132, exercise, 'Glute bridge variant 7').
uses_muscle(e_ex_132,m_glutes).
requires_equipment(e_ex_132,eq_none).
difficulty(e_ex_132,hard).

entity(e_ex_133, exercise, 'Push-up variant 7').
uses_muscle(e_ex_133,m_chest).
uses_muscle(e_ex_133,m_triceps).
requires_equipment(e_ex_133,eq_none).
difficulty(e_ex_133,hard).

entity(e_ex_134, exercise, 'Pull-up variant 7').
uses_muscle(e_ex_134,m_back).
uses_muscle(e_ex_134,m_biceps).
requires_equipment(e_ex_134,eq_none).
difficulty(e_ex_134,hard).

entity(e_ex_135, exercise, 'Kettlebell swing variant 7').
uses_muscle(e_ex_135,m_glutes).
uses_muscle(e_ex_135,m_hamstrings).
requires_equipment(e_ex_135,eq_kettlebell).
difficulty(e_ex_135,hard).

entity(e_ex_136, exercise, 'Box jump variant 7').
uses_muscle(e_ex_136,m_quadriceps).
uses_muscle(e_ex_136,m_glutes).
requires_equipment(e_ex_136,eq_none).
difficulty(e_ex_136,hard).

entity(e_ex_137, exercise, 'Burpee variant 7').
uses_muscle(e_ex_137,m_quadriceps).
uses_muscle(e_ex_137,m_chest).
requires_equipment(e_ex_137,eq_none).
difficulty(e_ex_137,hard).

entity(e_ex_138, exercise, 'Mountain climber variant 7').
uses_muscle(e_ex_138,m_core).
requires_equipment(e_ex_138,eq_none).
difficulty(e_ex_138,hard).

entity(e_ex_139, exercise, 'Cycling variant 7').
uses_muscle(e_ex_139,m_quadriceps).
requires_equipment(e_ex_139,eq_none).
difficulty(e_ex_139,hard).

entity(e_ex_140, exercise, 'Rowing machine variant 7').
uses_muscle(e_ex_140,m_back).
uses_muscle(e_ex_140,m_quadriceps).
requires_equipment(e_ex_140,eq_none).
difficulty(e_ex_140,hard).

% --- Variant 8 (indices 141..160)
entity(e_ex_141, exercise, 'Squat variant 8').
uses_muscle(e_ex_141,m_quadriceps).
uses_muscle(e_ex_141,m_glutes).
requires_equipment(e_ex_141,eq_barbell).
difficulty(e_ex_141,hard).

entity(e_ex_142, exercise, 'Lunge variant 8').
uses_muscle(e_ex_142,m_quadriceps).
uses_muscle(e_ex_142,m_glutes).
requires_equipment(e_ex_142,eq_none).
difficulty(e_ex_142,hard).

entity(e_ex_143, exercise, 'Deadlift variant 8').
uses_muscle(e_ex_143,m_hamstrings).
uses_muscle(e_ex_143,m_glutes).
requires_equipment(e_ex_143,eq_barbell).
difficulty(e_ex_143,hard).

entity(e_ex_144, exercise, 'Bench press variant 8').
uses_muscle(e_ex_144,m_chest).
uses_muscle(e_ex_144,m_triceps).
requires_equipment(e_ex_144,eq_barbell).
difficulty(e_ex_144,hard).

entity(e_ex_145, exercise, 'Row variant 8').
uses_muscle(e_ex_145,m_back).
uses_muscle(e_ex_145,m_biceps).
requires_equipment(e_ex_145,eq_dumbbell).
difficulty(e_ex_145,hard).

entity(e_ex_146, exercise, 'Shoulder press variant 8').
uses_muscle(e_ex_146,m_shoulders).
uses_muscle(e_ex_146,m_triceps).
requires_equipment(e_ex_146,eq_dumbbell).
difficulty(e_ex_146,hard).

entity(e_ex_147, exercise, 'Biceps curl variant 8').
uses_muscle(e_ex_147,m_biceps).
requires_equipment(e_ex_147,eq_dumbbell).
difficulty(e_ex_147,hard).

entity(e_ex_148, exercise, 'Triceps extension variant 8').
uses_muscle(e_ex_148,m_triceps).
requires_equipment(e_ex_148,eq_dumbbell).
difficulty(e_ex_148,hard).

entity(e_ex_149, exercise, 'Plank variant 8').
uses_muscle(e_ex_149,m_core).
requires_equipment(e_ex_149,eq_none).
difficulty(e_ex_149,hard).

entity(e_ex_150, exercise, 'Jump rope variant 8').
uses_muscle(e_ex_150,m_calves).
requires_equipment(e_ex_150,eq_none).
difficulty(e_ex_150,hard).

entity(e_ex_151, exercise, 'Leg press variant 8').
uses_muscle(e_ex_151,m_quadriceps).
requires_equipment(e_ex_151,eq_leg_press_machine).
difficulty(e_ex_151,hard).

entity(e_ex_152, exercise, 'Glute bridge variant 8').
uses_muscle(e_ex_152,m_glutes).
requires_equipment(e_ex_152,eq_none).
difficulty(e_ex_152,hard).

entity(e_ex_153, exercise, 'Push-up variant 8').
uses_muscle(e_ex_153,m_chest).
uses_muscle(e_ex_153,m_triceps).
requires_equipment(e_ex_153,eq_none).
difficulty(e_ex_153,hard).

entity(e_ex_154, exercise, 'Pull-up variant 8').
uses_muscle(e_ex_154,m_back).
uses_muscle(e_ex_154,m_biceps).
requires_equipment(e_ex_154,eq_none).
difficulty(e_ex_154,hard).

entity(e_ex_155, exercise, 'Kettlebell swing variant 8').
uses_muscle(e_ex_155,m_glutes).
uses_muscle(e_ex_155,m_hamstrings).
requires_equipment(e_ex_155,eq_kettlebell).
difficulty(e_ex_155,hard).

entity(e_ex_156, exercise, 'Box jump variant 8').
uses_muscle(e_ex_156,m_quadriceps).
uses_muscle(e_ex_156,m_glutes).
requires_equipment(e_ex_156,eq_none).
difficulty(e_ex_156,hard).

entity(e_ex_157, exercise, 'Burpee variant 8').
uses_muscle(e_ex_157,m_quadriceps).
uses_muscle(e_ex_157,m_chest).
requires_equipment(e_ex_157,eq_none).
difficulty(e_ex_157,hard).

entity(e_ex_158, exercise, 'Mountain climber variant 8').
uses_muscle(e_ex_158,m_core).
requires_equipment(e_ex_158,eq_none).
difficulty(e_ex_158,hard).

entity(e_ex_159, exercise, 'Cycling variant 8').
uses_muscle(e_ex_159,m_quadriceps).
requires_equipment(e_ex_159,eq_none).
difficulty(e_ex_159,hard).

entity(e_ex_160, exercise, 'Rowing machine variant 8').
uses_muscle(e_ex_160,m_back).
uses_muscle(e_ex_160,m_quadriceps).
requires_equipment(e_ex_160,eq_none).
difficulty(e_ex_160,hard).

% --- Variant 9 (indices 161..180)
entity(e_ex_161, exercise, 'Squat variant 9').
uses_muscle(e_ex_161,m_quadriceps).
uses_muscle(e_ex_161,m_glutes).
requires_equipment(e_ex_161,eq_barbell).
difficulty(e_ex_161,hard).

entity(e_ex_162, exercise, 'Lunge variant 9').
uses_muscle(e_ex_162,m_quadriceps).
uses_muscle(e_ex_162,m_glutes).
requires_equipment(e_ex_162,eq_none).
difficulty(e_ex_162,hard).

entity(e_ex_163, exercise, 'Deadlift variant 9').
uses_muscle(e_ex_163,m_hamstrings).
uses_muscle(e_ex_163,m_glutes).
requires_equipment(e_ex_163,eq_barbell).
difficulty(e_ex_163,hard).

entity(e_ex_164, exercise, 'Bench press variant 9').
uses_muscle(e_ex_164,m_chest).
uses_muscle(e_ex_164,m_triceps).
requires_equipment(e_ex_164,eq_barbell).
difficulty(e_ex_164,hard).

entity(e_ex_165, exercise, 'Row variant 9').
uses_muscle(e_ex_165,m_back).
uses_muscle(e_ex_165,m_biceps).
requires_equipment(e_ex_165,eq_dumbbell).
difficulty(e_ex_165,hard).

entity(e_ex_166, exercise, 'Shoulder press variant 9').
uses_muscle(e_ex_166,m_shoulders).
uses_muscle(e_ex_166,m_triceps).
requires_equipment(e_ex_166,eq_dumbbell).
difficulty(e_ex_166,hard).

entity(e_ex_167, exercise, 'Biceps curl variant 9').
uses_muscle(e_ex_167,m_biceps).
requires_equipment(e_ex_167,eq_dumbbell).
difficulty(e_ex_167,hard).

entity(e_ex_168, exercise, 'Triceps extension variant 9').
uses_muscle(e_ex_168,m_triceps).
requires_equipment(e_ex_168,eq_dumbbell).
difficulty(e_ex_168,hard).

entity(e_ex_169, exercise, 'Plank variant 9').
uses_muscle(e_ex_169,m_core).
requires_equipment(e_ex_169,eq_none).
difficulty(e_ex_169,hard).

entity(e_ex_170, exercise, 'Jump rope variant 9').
uses_muscle(e_ex_170,m_calves).
requires_equipment(e_ex_170,eq_none).
difficulty(e_ex_170,hard).

entity(e_ex_171, exercise, 'Leg press variant 9').
uses_muscle(e_ex_171,m_quadriceps).
requires_equipment(e_ex_171,eq_leg_press_machine).
difficulty(e_ex_171,hard).

entity(e_ex_172, exercise, 'Glute bridge variant 9').
uses_muscle(e_ex_172,m_glutes).
requires_equipment(e_ex_172,eq_none).
difficulty(e_ex_172,hard).

entity(e_ex_173, exercise, 'Push-up variant 9').
uses_muscle(e_ex_173,m_chest).
uses_muscle(e_ex_173,m_triceps).
requires_equipment(e_ex_173,eq_none).
difficulty(e_ex_173,hard).

entity(e_ex_174, exercise, 'Pull-up variant 9').
uses_muscle(e_ex_174,m_back).
uses_muscle(e_ex_174,m_biceps).
requires_equipment(e_ex_174,eq_none).
difficulty(e_ex_174,hard).

entity(e_ex_175, exercise, 'Kettlebell swing variant 9').
uses_muscle(e_ex_175,m_glutes).
uses_muscle(e_ex_175,m_hamstrings).
requires_equipment(e_ex_175,eq_kettlebell).
difficulty(e_ex_175,hard).

entity(e_ex_176, exercise, 'Box jump variant 9').
uses_muscle(e_ex_176,m_quadriceps).
uses_muscle(e_ex_176,m_glutes).
requires_equipment(e_ex_176,eq_none).
difficulty(e_ex_176,hard).

entity(e_ex_177, exercise, 'Burpee variant 9').
uses_muscle(e_ex_177,m_quadriceps).
uses_muscle(e_ex_177,m_chest).
requires_equipment(e_ex_177,eq_none).
difficulty(e_ex_177,hard).

entity(e_ex_178, exercise, 'Mountain climber variant 9').
uses_muscle(e_ex_178,m_core).
requires_equipment(e_ex_178,eq_none).
difficulty(e_ex_178,hard).

entity(e_ex_179, exercise, 'Cycling variant 9').
uses_muscle(e_ex_179,m_quadriceps).
requires_equipment(e_ex_179,eq_none).
difficulty(e_ex_179,hard).

entity(e_ex_180, exercise, 'Rowing machine variant 9').
uses_muscle(e_ex_180,m_back).
uses_muscle(e_ex_180,m_quadriceps).
requires_equipment(e_ex_180,eq_none).
difficulty(e_ex_180,hard).

% --- Variant 10 (indices 181..200)
entity(e_ex_181, exercise, 'Squat variant 10').
uses_muscle(e_ex_181,m_quadriceps).
uses_muscle(e_ex_181,m_glutes).
requires_equipment(e_ex_181,eq_barbell).
difficulty(e_ex_181,hard).

entity(e_ex_182, exercise, 'Lunge variant 10').
uses_muscle(e_ex_182,m_quadriceps).
uses_muscle(e_ex_182,m_glutes).
requires_equipment(e_ex_182,eq_none).
difficulty(e_ex_182,hard).

entity(e_ex_183, exercise, 'Deadlift variant 10').
uses_muscle(e_ex_183,m_hamstrings).
uses_muscle(e_ex_183,m_glutes).
requires_equipment(e_ex_183,eq_barbell).
difficulty(e_ex_183,hard).

entity(e_ex_184, exercise, 'Bench press variant 10').
uses_muscle(e_ex_184,m_chest).
uses_muscle(e_ex_184,m_triceps).
requires_equipment(e_ex_184,eq_barbell).
difficulty(e_ex_184,hard).

entity(e_ex_185, exercise, 'Row variant 10').
uses_muscle(e_ex_185,m_back).
uses_muscle(e_ex_185,m_biceps).
requires_equipment(e_ex_185,eq_dumbbell).
difficulty(e_ex_185,hard).

entity(e_ex_186, exercise, 'Shoulder press variant 10').
uses_muscle(e_ex_186,m_shoulders).
uses_muscle(e_ex_186,m_triceps).
requires_equipment(e_ex_186,eq_dumbbell).
difficulty(e_ex_186,hard).

entity(e_ex_187, exercise, 'Biceps curl variant 10').
uses_muscle(e_ex_187,m_biceps).
requires_equipment(e_ex_187,eq_dumbbell).
difficulty(e_ex_187,hard).

entity(e_ex_188, exercise, 'Triceps extension variant 10').
uses_muscle(e_ex_188,m_triceps).
requires_equipment(e_ex_188,eq_dumbbell).
difficulty(e_ex_188,hard).

entity(e_ex_189, exercise, 'Plank variant 10').
uses_muscle(e_ex_189,m_core).
requires_equipment(e_ex_189,eq_none).
difficulty(e_ex_189,hard).

entity(e_ex_190, exercise, 'Jump rope variant 10').
uses_muscle(e_ex_190,m_calves).
requires_equipment(e_ex_190,eq_none).
difficulty(e_ex_190,hard).

entity(e_ex_191, exercise, 'Leg press variant 10').
uses_muscle(e_ex_191,m_quadriceps).
requires_equipment(e_ex_191,eq_leg_press_machine).
difficulty(e_ex_191,hard).

entity(e_ex_192, exercise, 'Glute bridge variant 10').
uses_muscle(e_ex_192,m_glutes).
requires_equipment(e_ex_192,eq_none).
difficulty(e_ex_192,hard).

entity(e_ex_193, exercise, 'Push-up variant 10').
uses_muscle(e_ex_193,m_chest).
uses_muscle(e_ex_193,m_triceps).
requires_equipment(e_ex_193,eq_none).
difficulty(e_ex_193,hard).

entity(e_ex_194, exercise, 'Pull-up variant 10').
uses_muscle(e_ex_194,m_back).
uses_muscle(e_ex_194,m_biceps).
requires_equipment(e_ex_194,eq_none).
difficulty(e_ex_194,hard).

entity(e_ex_195, exercise, 'Kettlebell swing variant 10').
uses_muscle(e_ex_195,m_glutes).
uses_muscle(e_ex_195,m_hamstrings).
requires_equipment(e_ex_195,eq_kettlebell).
difficulty(e_ex_195,hard).

entity(e_ex_196, exercise, 'Box jump variant 10').
uses_muscle(e_ex_196,m_quadriceps).
uses_muscle(e_ex_196,m_glutes).
requires_equipment(e_ex_196,eq_none).
difficulty(e_ex_196,hard).

entity(e_ex_197, exercise, 'Burpee variant 10').
uses_muscle(e_ex_197,m_quadriceps).
uses_muscle(e_ex_197,m_chest).
requires_equipment(e_ex_197,eq_none).
difficulty(e_ex_197,hard).

entity(e_ex_198, exercise, 'Mountain climber variant 10').
uses_muscle(e_ex_198,m_core).
requires_equipment(e_ex_198,eq_none).
difficulty(e_ex_198,hard).

entity(e_ex_199, exercise, 'Cycling variant 10').
uses_muscle(e_ex_199,m_quadriceps).
requires_equipment(e_ex_199,eq_none).
difficulty(e_ex_199,hard).

entity(e_ex_200, exercise, 'Rowing machine variant 10').
uses_muscle(e_ex_200,m_back).
uses_muscle(e_ex_200,m_quadriceps).
requires_equipment(e_ex_200,eq_none).
difficulty(e_ex_200,hard).

% --- Substitutions and contraindications -------------------------------------
% Leg press substitutes for squat (matching variant numbers)
substitution_for(e_ex_011,e_ex_001).
substitution_for(e_ex_031,e_ex_021).
substitution_for(e_ex_051,e_ex_041).
substitution_for(e_ex_071,e_ex_061).
substitution_for(e_ex_091,e_ex_081).
substitution_for(e_ex_111,e_ex_101).
substitution_for(e_ex_131,e_ex_121).
substitution_for(e_ex_151,e_ex_141).
substitution_for(e_ex_171,e_ex_161).
substitution_for(e_ex_191,e_ex_181).

% Contraindications: knee for squat, lunge, box_jump (all variants)
% deadlift -> low_back; bench_press and shoulder_press -> shoulder
:- multifile fact/3.
contraindicated_for(e_ex_001,cond_knee).
contraindicated_for(e_ex_002,cond_knee).
contraindicated_for(e_ex_016,cond_knee).
contraindicated_for(e_ex_021,cond_knee).
contraindicated_for(e_ex_022,cond_knee).
contraindicated_for(e_ex_036,cond_knee).
contraindicated_for(e_ex_041,cond_knee).
contraindicated_for(e_ex_042,cond_knee).
contraindicated_for(e_ex_056,cond_knee).
contraindicated_for(e_ex_061,cond_knee).
contraindicated_for(e_ex_062,cond_knee).
contraindicated_for(e_ex_076,cond_knee).
contraindicated_for(e_ex_081,cond_knee).
contraindicated_for(e_ex_082,cond_knee).
contraindicated_for(e_ex_096,cond_knee).
contraindicated_for(e_ex_101,cond_knee).
contraindicated_for(e_ex_102,cond_knee).
contraindicated_for(e_ex_116,cond_knee).
contraindicated_for(e_ex_121,cond_knee).
contraindicated_for(e_ex_122,cond_knee).
contraindicated_for(e_ex_136,cond_knee).
contraindicated_for(e_ex_141,cond_knee).
contraindicated_for(e_ex_142,cond_knee).
contraindicated_for(e_ex_156,cond_knee).
contraindicated_for(e_ex_161,cond_knee).
contraindicated_for(e_ex_162,cond_knee).
contraindicated_for(e_ex_176,cond_knee).
contraindicated_for(e_ex_181,cond_knee).
contraindicated_for(e_ex_182,cond_knee).
contraindicated_for(e_ex_196,cond_knee).

% deadlift contraindicated for low back
contraindicated_for(e_ex_003,cond_low_back).
contraindicated_for(e_ex_023,cond_low_back).
contraindicated_for(e_ex_043,cond_low_back).
contraindicated_for(e_ex_063,cond_low_back).
contraindicated_for(e_ex_083,cond_low_back).
contraindicated_for(e_ex_103,cond_low_back).
contraindicated_for(e_ex_123,cond_low_back).
contraindicated_for(e_ex_143,cond_low_back).
contraindicated_for(e_ex_163,cond_low_back).
contraindicated_for(e_ex_183,cond_low_back).

% bench press and shoulder press -> shoulder contraindication (some variants)
contraindicated_for(e_ex_004,cond_shoulder).
contraindicated_for(e_ex_024,cond_shoulder).
contraindicated_for(e_ex_044,cond_shoulder).
contraindicated_for(e_ex_064,cond_shoulder).
contraindicated_for(e_ex_006,cond_shoulder).
contraindicated_for(e_ex_026,cond_shoulder).
contraindicated_for(e_ex_046,cond_shoulder).
contraindicated_for(e_ex_066,cond_shoulder).

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


