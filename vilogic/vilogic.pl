
:- use_module(library(clpfd)).

% State = (Pos, Text)
% Pos = index in Text string (not line/col)

% cases where out of space to move
move_pos_char(_, 0, 0, -1) :- !.
move_pos_char(Text, Pos, Pos, 1) :-
    length(Text, TextLength),
    Pos #= TextLength - 1, !.
% case where newline is encountered, so position doesn't actually change
move_pos_char(Text, Pos, Pos, Delta) :-
    member(Delta, [-1, 1]),
    Pos2 #= Pos + Delta,
    % ascii 10 is newline
    nth0(Pos2, Text, 10), !.
move_pos_char(Text, Pos1, Pos2, Delta) :-
    member(Delta, [-1, 1]),
    Pos2 #= Pos1 + Delta, !.

% todo
move_pos_line(_, Pos, Pos, 0).
move_pos_line(Text, Pos1, Pos2, Delta).

get_text_to_insert(TextInserted) :-
    read_string(user_input, "$", [], _, Input),
    string_codes(Input, TextInserted).

insert_text(TextInserted, (PosPrior, TextPrior), (PosAfter, TextAfter)) :-
    split_at(PosPrior, TextPrior, TextLeft, TextRight),
    append(TextLeft, TextInserted, TextLeft2),
    append(TextLeft2, TextRight, TextAfter),
    length(TextInserted, PosDelta),
    PosAfter #= PosPrior + PosDelta.

% normal_cmd(cmd, State1, State2).

% press h from normal mode: move left
normal_cmd(move_left, (Pos, Text), (Pos2, Text)) :-
    move_pos_char(Text, Pos, Pos2, -1).
% press j from normal mode: move down
normal_cmd(move_down, (Pos, Text), (Pos2, Text)) :-
    move_pos_line(Text, Pos, Pos2, 1).
% press k from normal mode: move up
normal_cmd(move_up, (Pos, Text), (Pos2, Text)) :-
    move_pos_line(Text, Pos, Pos2, -1).
% press l from normal mode: move right
normal_cmd(move_right, (Pos, Text), (Pos2, Text)) :-
    move_pos_char(Text, Pos, Pos2, 1).

% press i from normal mode: insert before char
normal_cmd(insert_before_cursor, State1, State2) :-
    get_text_to_insert(TextInserted),
    insert_text(TextInserted, State1, State2).
% press I from normal mode: insert before line
normal_cmd(insert_before_line, (Pos, TextPrior), (Pos3, TextAfter)) :-
    move_pos_line(TextPrior, Pos, Pos2, -1),
    get_text_to_insert(TextInserted),
    insert_text(TextInserted, (Pos2, TextPrior), (Pos3, TextAfter)).
% press a from normal mode: insert after char
normal_cmd(append_after_cursor, (Pos, TextPrior), (Pos3, TextAfter)) :-
    move_pos_char(TextPrior, Pos, Pos2, 1),
    get_text_to_insert(TextInserted),
    insert_text(TextInserted, (Pos2, TextPrior), (Pos3, TextAfter)).
% press A from normal mode: insert after line
normal_cmd(append_after_line, (Pos, TextPrior), (Pos3, TextAfter)) :-
    move_pos_line(TextPrior, Pos, Pos2, 1),
    get_text_to_insert(TextInserted),
    insert_text(TextInserted, (Pos2, TextPrior), (Pos3, TextAfter)).

grammar_normal(State1, State2) --> grammar_normal_cmd(Cmd),
    { normal_cmd(Cmd, State1, State2) }.

grammar_normal_cmd(Cmd) --> grammar_movement(Cmd).
grammar_normal_cmd(Cmd) --> grammar_insert(Cmd).

grammar_movement(move_left) --> "h".
grammar_movement(move_down) --> "j".
grammar_movement(move_up) --> "k".
grammar_movement(move_right) --> "l".

grammar_insert(insert_before_cursor) --> "i".
grammar_insert(insert_before_line) --> "I".
grammar_insert(append_after_cursor) --> "a".
grammar_insert(append_after_line) --> "A".


% split_at from: https://github.com/mndrix/list_util/blob/master/prolog/list_util.pl
split_at(N,Xs,Take,Rest) :-
    split_at_(Xs,N,Take,Rest).
split_at_(Rest, 0, [], Rest) :- !. % optimization
split_at_([], 0, [], []).
split_at_([X|Xs], N, [X|Take], Rest) :-
    N > 0,
    succ(N0, N),
    split_at_(Xs, N0, Take, Rest).
