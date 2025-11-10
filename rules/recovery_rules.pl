/*
 * recovery_rules.pl
 * 
 * Recovery and rest day management rules for workout planning.
 * Determines recovery needs based on sleep quality, training intensity,
 * and other fatigue indicators.
 * 
 * Module: recovery_rules
 * Exports: needs_recovery/2, schedule_has_enough_recovery/2
 */

:- module(recovery_rules, [
    needs_recovery/2,
    schedule_has_enough_recovery/2
]).

% needs_recovery(+UserID, -Days)
% Calculates required rest days per week based on sleep quality
% 
% Sleep < 6h → 3 rest days/week (high recovery need)
% Sleep 6-7h → 2 rest days/week (moderate recovery)
% Sleep 7-8h → 1 rest day/week (normal recovery)
% Sleep 8+h → 1 rest day/week (optimal recovery)
needs_recovery(UserID, 3) :-
    user_profile(UserID, sleep_hours(Hours)),
    Hours < 6.

needs_recovery(UserID, 2) :-
    user_profile(UserID, sleep_hours(Hours)),
    Hours >= 6,
    Hours < 7.

needs_recovery(UserID, 1) :-
    user_profile(UserID, sleep_hours(Hours)),
    Hours >= 7,
    Hours < 8.

needs_recovery(UserID, 1) :-
    user_profile(UserID, sleep_hours(Hours)),
    Hours >= 8.

% schedule_has_enough_recovery(+UserID, +WeekSchedule)
% Checks if schedule contains sufficient rest days
schedule_has_enough_recovery(UserID, WeekSchedule) :-
    needs_recovery(UserID, RequiredDays),
    findall(Day, member(day(Day, _), WeekSchedule), WorkoutDays),
    length(WorkoutDays, WorkoutCount),
    RestDays is 7 - WorkoutCount,
    RestDays >= RequiredDays.
