

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAC/MCSS/MRes
%% 531 Prolog
%% Assessed Exercise 5 - Syllogisms 
%% utilities.pl
%% Consult this file in order to use the programs below.
%% Your submitted answer should not include a consult declaration
%% for either provided file. This file will be consulted for you during testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the following is built-in in some Prologs but not Sicstus

forall(C1,C2) :- \+ (call(C1), \+ call(C2)).


% the following can be used by your test/1 program to display the clauses 
% you generate for the N'th syllogism stored as facts of the form cl(N,Clause).


show_clauses(N) :-
  forall(
    cl(N,H,B),
    (
      write('   '),
      write((H:-B)),
      write('.'),
      nl
    )
  ).

/* The above could also be written using format/2 (see manual)

show_clauses(N) :-
  forall(cl(N,H,B),
         format("   ~w.~n",[(H:-B)] ).

*/


% The following can be used to assert each clause (H:-B) of the list of clauses 
% produced by parsing a syllogism sentence list number N  as facts 
% of the form cl(N,H,B)

assertcl(N,(H:-B)) :- !, assert(cl(N,H,B)).
assertcl(N,H) :- assert(cl(N,H,true)). 

assertall(_,[]).  
assertall(N,[C|Cs]) :-
  assertcl(N,C),
  assertall(N,Cs).


% You might find the following useful

writeL([]).
writeL([W|Ws]) :-
  write(W),
  write(' '),
  writeL(Ws).


