% queries.pl - example queries and plunit tests for kb.pl

:- begin_tests(kb_tests).

:- use_module(kb).

test(list_exercises) :-
    list_exercises(List),
    is_list(List),
    List \= [].

test(contraindicated_true) :-
    contraindicated(u_alex, e_ex_squat).

test(recommend_alternative_for_contraindicated) :-
    recommend_alternative(u_alex, e_ex_squat, Alt),
    Alt \= e_ex_squat.

test(progress_candidate_example) :-
    progress_candidate(u_alex, e_ex_box_squat, Next),
    Next = e_ex_squat.

:- end_tests(kb_tests).

% helper to run tests
run_tests :- run_tests([kb_tests]).

% interactive example queries
example_queries :-
    findall(X, recommend(u_alex, X), Recs),
    format('Recommendations for ~w: ~w~n', [u_alex, Recs]),
    ( recommend_alternative(u_alex, e_ex_squat, Alt) -> format('Alt for squat: ~w~n', [Alt]) ; true ).
