% kb.pl - fitness domain knowledge base (sample)
% Entities: exercises (e_), muscles (m_), equipment (eq_), conditions (cond_), goals (g_), users (u_)

:- module(kb, [entity/3, fact/3, attr/3, user_profile/2, capability/2, schedule/5, user_has_equipment/2]).

%% --- Entities ----------------------------------------------------------------
entity(e_ex_squat, exercise, 'Barbell Back Squat').
entity(e_ex_box_squat, exercise, 'Box Squat').
entity(e_ex_leg_press, exercise, 'Leg Press').
entity(e_ex_glute_bridge, exercise, 'Glute Bridge').
entity(e_ex_jump_rope, exercise, 'Jump Rope').
entity(e_ex_pushup, exercise, 'Push-up').

entity(m_quadriceps, muscle, 'Quadriceps').
entity(m_glutes, muscle, 'Glutes').
entity(m_hamstrings, muscle, 'Hamstrings').
entity(m_chest, muscle, 'Pectoralis Major').

entity(eq_barbell, equipment, 'Barbell').
entity(eq_leg_press_machine, equipment, 'Leg Press Machine').
entity(eq_none, equipment, 'None').

entity(cond_knee, condition, 'Knee Injury').
entity(cond_low_back, condition, 'Low Back Pain').

entity(g_goal_strength, goal, 'Increase Strength').
entity(g_goal_endurance, goal, 'Improve Endurance').

entity(u_alex, user, 'Alex').

%% --- Facts (triples) ---------------------------------------------------------
% fact(Subject, Predicate, Object).
fact(e_ex_squat, uses_muscle, m_quadriceps).
fact(e_ex_squat, uses_muscle, m_glutes).
fact(e_ex_squat, requires_equipment, eq_barbell).

fact(e_ex_box_squat, uses_muscle, m_quadriceps).
fact(e_ex_box_squat, uses_muscle, m_glutes).
fact(e_ex_box_squat, requires_equipment, eq_barbell).
fact(e_ex_box_squat, substitution_for, e_ex_squat).

fact(e_ex_leg_press, uses_muscle, m_quadriceps).
fact(e_ex_leg_press, requires_equipment, eq_leg_press_machine).
fact(e_ex_leg_press, substitution_for, e_ex_squat).

fact(e_ex_glute_bridge, uses_muscle, m_glutes).
fact(e_ex_glute_bridge, requires_equipment, eq_none).

% contraindications
fact(e_ex_squat, contraindicated_for, cond_knee).

% goal targets (map goals to modalities or targets)
fact(g_goal_strength, prefers_modality, strength).
fact(g_goal_endurance, prefers_modality, endurance).

%% --- Attributes --------------------------------------------------------------
% attr(Entity, Attribute, Value).
attr(e_ex_squat, difficulty, hard).
attr(e_ex_box_squat, difficulty, medium).
attr(e_ex_leg_press, difficulty, medium).
attr(e_ex_glute_bridge, difficulty, easy).

%% --- User profiles & capabilities --------------------------------------------
% user_profile(UserId, Attr).
user_profile(u_alex, age(35)).
user_profile(u_alex, weight(85)).
user_profile(u_alex, injury(cond_knee)).
user_profile(u_alex, goal(g_goal_strength)).

% capability(User, max_rep(ExerciseId, WeightKg, Reps)).
capability(u_alex, max_rep(e_ex_box_squat, 60, 5)).

% equipment ownership
user_has_equipment(u_alex, eq_barbell).

% schedule(User, date(Y,M,D), time(H,Min), activity(ActivityId), duration_minutes).
schedule(u_alex, date(2025,11,10), time(18,0), activity(e_ex_box_squat), 30).

%% --- Confidence / provenance (simple example) --------------------------------
% confidence(Statement, Source, Score).
confidence(fact(e_ex_squat, contraindicated_for, cond_knee), 'manual_review', 0.95).

%% --- Rule metadata (example) -------------------------------------------------
% rule_meta(RuleId, Author, Purpose, Priority).
rule_meta(r1, 'system', 'basic contraindication check', 10).

%% --- Helper predicates & rules -----------------------------------------------
% contraindicated(User, Exercise) - true if user's injury matches exercise contraindication
contraindicated(User, Exercise) :-
    user_profile(User, injury(Condition)),
    fact(Exercise, contraindicated_for, Condition).

% user_has_equipment(User, Equipment) - defined via facts above or default false
% available_equipment(User, Exercise) - true if exercise needs none or user owns required equipment
available_equipment(User, Exercise) :-
    fact(Exercise, requires_equipment, eq_none), !.
available_equipment(User, Exercise) :-
    fact(Exercise, requires_equipment, Eq),
    user_has_equipment(User, Eq).

% matches_goal(Exercise, User) - simplistic: if exercise uses muscles that align with goal modality
matches_goal(Exercise, User) :-
    user_profile(User, goal(Goal)),
    fact(Goal, prefers_modality, Modality),
    modality_matches_exercise(Modality, Exercise).

% modality_matches_exercise examples (expand in KB)
modality_matches_exercise(strength, Exercise) :-
    fact(Exercise, uses_muscle, _AnyMuscle).
modality_matches_exercise(endurance, Exercise) :-
    fact(Exercise, uses_muscle, _AnyMuscle).

% recommend(User, Exercise) - main high-level recommendation rule
recommend(User, Exercise) :-
    entity(Exercise, exercise, _Label),
    matches_goal(Exercise, User),
    \+ contraindicated(User, Exercise),
    available_equipment(User, Exercise).

% recommend_alternative(User, Exercise, Alternative)
recommend_alternative(User, Exercise, Alternative) :-
    ( fact(Exercise, substitution_for, Alternative)
    ; fact(Alternative, substitution_for, Exercise)
    ),
    \+ contraindicated(User, Alternative),
    available_equipment(User, Alternative).

% progression_to facts (examples)
fact(e_ex_box_squat, progression_to, e_ex_squat).

% progress_candidate(User, Exercise, Next)
progress_candidate(User, Exercise, Next) :-
    capability(User, max_rep(Exercise, Weight, Reps)),
    Reps >= 5,
    fact(Exercise, progression_to, Next),
    \+ contraindicated(User, Next).

% warmup rules
warmup_for_muscle(m_quadriceps, e_ex_leg_swing) :- entity(e_ex_leg_swing, exercise, _), !.
warmup_for_muscle(m_glutes, e_ex_bodyweight_glute_bridge) :- entity(e_ex_bodyweight_glute_bridge, exercise, _), !.

needs_warmup(Exercise, Warmup) :-
    fact(Exercise, uses_muscle, Muscle),
    warmup_for_muscle(Muscle, Warmup).

% scheduling conflict: avoid same primary muscle groups on consecutive days (simplified)
conflicts_with_schedule(User, date(Y,M,D), Exercise) :-
    schedule(User, date(Y,M,DOther), _, activity(Other), _),
    DOther =:= D - 1,
    fact(Other, uses_muscle, Muscle),
    fact(Exercise, uses_muscle, Muscle).

% explanation helper: collect simple reasons for recommendation
recommend_with_reason(User, Exercise, Reason) :-
    recommend(User, Exercise),
    ( contraindicated(User, Exercise) -> Reason = 'contraindicated' ; Reason = 'ok' ).

% small utility: list exercises for debugging
list_exercises(List) :- findall(Label-Id, entity(Id, exercise, Label), List).
