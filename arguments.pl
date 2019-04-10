


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAC/MCSS/MRes
%% 531 Prolog
%% Assessed Exercise 5 - Syllogisms
%% arguments.pl  (Test syllogism arguments)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


p(1,[a,robin,is,a,bird]).    %    bird(X):-robin(X)
p(1,[no,bird,is,a,reptile]). %  false:-bird(X),reptile(X)




p(2,[a,human,is,a,mammal]).       % mammal(X):-human(X))
p(2,[a,mammal,is,warm_blooded]).  % warm_blooded(X):-mammal(X)




p(3,[a,human,is,a,mammal]).  % mammal(X):-human(X)
p(3,[some,human,is,deaf]).
    %  human(some(human,deaf):-true
    %  deaf(some(human,deaf)):-true


p(4,[a,robin,is,a,bird]).   % bird(X):-robin(X)
p(4,[no,bird,is,a,reptile]).  % false:-bird(X),reptile(X)



p(5,[every, philosopher, is, a, logician]).  % logician(X):-philosopher(X)
p(5,[every, philosopher, is, a, professor]).   % professor(X):-philosopher(X)


p(6,[no,vegetarian,is,a,meat_eater]).  % false:-meat_eater(X),vegetarian(X)
p(6,[some,vegetarian,is,a,policeman]).
    % vegetarian(some(vegetarian,policeman)):-true
    % policeman(some(vegetarian,policeman)):-true


p(7,[some,student,is,rich]).
   %  student(some(student,rich)):- true
   % rich(some(student,rich)):- true
p(7,[some,girl,is,not,a,student]).
   % girl(some(girl,not(student))):-true
   % false:-student(some(girl,not(student))

p(8,[some,man,is,a,clown]).
   % man(some(man,clown)):-true
   % clown(some(man,clown)):-true
p(8,[some,man,is,bald]).
   % man(some(man,bald)):-true
   % bald(some(man,bald):-true



c(1,[no,robin,is,a,reptile]).
%     Opposite is:  [some,robin,is,a,reptile]).
%     robin(some(robin,reptile)):-true
%     reptile(some(robin,reptile):-true
% false can be derived, syllogism is valid


c(2,[a,human,is,warm_blooded]).
%     Opposite is: [some,human,is,not,warm_blooded]).
%     human(some(human,not(warm_blooded))):-true
%     false:-warm_blooded(some(human,not(warm_blooded)))
% false can be derived, syllogism is valid


c(3,[some,mammal,is,deaf]).
%      Opposite is: [no,mammal,is,deaf]).
%      false:-mammal(X),deaf(X))
% false can be derived, syllogism is valid


c(4,[no,reptile,is,a,robin]).
    %  Opposite is: [some,reptile,is,a,robin]).
    % robin(some(reptile,robin)):-true
        % reptile(some(reptile,robin)):-true
% false can be derived, syllogism is valid

c(5,[every,logician,is,a,professor]).
      % Opposite is [some,logician,is,not,a,professor]
      % logician(some(logician,not(professor))):-true
      % false:-professor(some(logician,not(professor)))
% False cannot be derived, syllogism is invalid

c(6,[some,policeman,is,not,a,meat_eater]).
  % Negated conclusion is: [a,policeman,is,a,meat_eater]
  % meat_eater(X):-policeman(X)
% false can be derived, syllogism is valid


c(7,[some,girl,is,not,rich]).
  %  Opposites: [a,girl,is,rich]
  % rich(X):-girl(X)
% false cannot be derived, syllogism is invalid

c(8,[some,clown,is,bald]).
  % Opposite is [no,clown,is,bald]
  % false:-clown(X),bald(X)
% false cannot be derived, syllogism is invalid
