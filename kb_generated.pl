% kb_generated.pl - synthetic fitness KB (200 exercises)
% Generated: synthetic dataset for testing Prolog reasoning

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

