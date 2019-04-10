%% File: syllogisms.pl
%% Name: Noah-Vincenz Noeh
%% Date: 15/11/2018
%%
%% This program is a solution to Prolog 531 Assessed Exercise 5 'Syllogisms'
%% The exercise is to develop a parser and meta-interpreter for syllogistic
%% sentences, and use these to build a tool to determine the validity of a
%% syllogistic argument.

%% ---------------------------- Step 1 ---------------------------------------%%

%% opposite(+L, -Opp)
opposite([no|Tail], [some|Tail]) :- !.
opposite([some, Thing1, is, not, a, Thing2], [a, Thing1, is, a, Thing2]) :- !.
opposite([some, Thing1, is, not, Thing2], [a, Thing1, is, Thing2]) :- !.
opposite([some|Tail], [no|Tail]) :- !.
% for 'a' and 'every'
opposite([_, Thing1, is, a, Thing2], [some, Thing1, is, not, a, Thing2]).
opposite([_, Thing1, is, Thing2], [some, Thing1, is, not, Thing2]).


%% ---------------------------- Step 2 ---------------------------------------%%

%% Stage 2.1 - This is the suggested way to develop the solution.
%% Once Stage 2.2 is complete you can delete or comment out this code.
%% syllogism/0

/*
syllogism --> article, [B], is_, optional_article, [C].
syllogism --> some, [B], is_, not, optional_article, [C].
syllogism --> no_, [B], is_, optional_article, [C].
syllogism --> some, [B], is_, optional_article, [C].
*/

%% Stage 2.2
%% syllogism(-Clauses)

syllogism(Clauses) --> article, [B], is_, optional_article, [C], {
    Term1 =.. [C, X],
    Term2 =.. [B, X],
    Clauses = [(Term1:-Term2)]
}.

syllogism(Clauses) --> some, [B], is_, not, optional_article, [C], {
    Term1 =.. [not, C],
    Term2 =.. [some, B, Term1],
    Term3 =.. [B, Term2],
    Term4 =   (Term3:-true),
    Term5 =.. [C, Term2],
    Term6 =   (false:-Term5),
    Clauses = [Term4,Term6]
}.

syllogism(Clauses) --> no_, [B], is_, optional_article, [C], {
    Term1 =.. [B, X],
    Term2 =.. [C, X],
    Clauses = [(false:-Term1, Term2)]
}.

syllogism(Clauses) --> some, [B], is_, optional_article, [C], {
    Term1 =.. [some, B, C],
    Term2 =.. [B, Term1],
    Term3 =   (Term2:-true),
    Term4 =.. [C, Term1],
    Term5 =   (Term4:-true),
    Clauses = [Term3, Term5]
}.


article --> [a].
article --> [every].
some --> [some].
not --> [not].
no_ --> [no].
is_ --> [is].
optional_article --> [a].
optional_article --> [].



%% ---------------------------- Step 3 ---------------------------------------%%

%% translate(+N)
%% finds all (2) premises in the arguments file, gets the corresponding conclusion, builds its opposite
%% and gets the corresponding clauses by calling the syllogism predicate
%% and finally asserts all clauses

translate(N) :-
	findall(Premise, p(N, Premise), List),
  write(List),
  nl,
	c(N, Conclusion),
	opposite(Conclusion, Opposite),
	get_clauses([Opposite|List], [], Phrases),
  write(Phrases),
  assertall(N, Phrases).

get_clauses([], Acc, Acc).
get_clauses([X|Tail], Acc, Phrases) :-
	phrase(syllogism(Clauses), X), % returns semantics as clauses
  append(Clauses, Acc, NewAcc),
	get_clauses(Tail, NewAcc, Phrases).

%% ---------------------------- Step 4 ---------------------------------------%%

%% eval(+N, +Calls)
%% succeeds if each condition in Calls (=(P,Q) or P) can be derived using just the clauses
%% recorded as cl(N,H,B) facts by checking if there is a cl/3 clause with index N whose head is equal to the
%% element in the round bracket list; if there is, recursively evaluate the body of the clause.

eval(_, true) :- !.

eval(N, (P, Q)) :-
  eval(N, P),!,
  !,
  eval(N, P),
  eval(N, Q).

eval(N, P) :-
  cl(N, P, Body),
  eval(N, Body).


%% valid(?N)

valid(N) :-
  eval(N, false).


%% invalid(?N)

invalid(N) :-
  \+ eval(N, false).


%% ---------------------------- Step 5 ---------------------------------------%%

%% test(+N)


test(N) :-
  write('syllogism '), write(N), write(':'),
  nl,
  findall(Premise, p(N, Premise), PremiseList),
  printList(PremiseList),
  nl,
  write('   =>'),
  nl,
  c(N, Conclusion),
  write('   '),
  printListElems(Conclusion),
  nl,
  nl,
  write('Premises and opposite of conclusion converted to clauses:'),
  nl,
  show_clauses(N),
  nl,
  print_validity(N).

%% there are either a single list item or two sublists (two list items) - for the first we
%% simply print the single list's list items. For the latter we first print the first
%% sublists item followed by the second list items
printList([X,Y]) :-
  !,
  write('   '),
  printListElems(X),!,
  nl,
  write('   '),
  printListElems(Y).

printListElems([X|Tail]) :-
  write(X), write(' '),
  printListElems(Tail).

printListElems([]).


print_validity(N) :-
  valid(N),!,
  write('false can be derived, syllogism '), write(N), write(' is valid.'),
  nl.

print_validity(N) :-
  write('false cannot be derived, syllogism '), write(N), write(' is invalid.'),
  nl.
