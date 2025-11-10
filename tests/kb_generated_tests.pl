% plunit tests for kb_generated.pl
% Load KB into the user module *before* starting the test module to avoid
% plunit warnings about clauses being discontiguous inside the test module.
:- consult('../kb_generated.pl').
:- begin_tests(kb_generated_tests).

test(entity_count) :-
    findall(ID, entity(ID, exercise, _), L), length(L, N), assertion(N == 200).

test(has_substitution) :-
    % there should be at least one substitution/link
    fact(_, substitution_for, _).

test(knee_contra) :-
    % at least one exercise contraindicated for knee
    fact(_, contraindicated_for, cond_knee).

:- end_tests(kb_generated_tests).
