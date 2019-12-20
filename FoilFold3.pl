%input
%knowledge base
knowledgeRule(example1).
knowledgeRule(example2).
knowledgeRule(example3).
knowledgeRule(example4).
%handling negative knowledge base rules implying positive examples as exception.
knowledgeRule(example1):- 
	knowledgeRule(example4).
%positive examples
positiveexamples(example1).
positiveexamples(example2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   BASIC implementation of FOIL                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%code
%To calculate propositions.

propositions([],[]).
propositions([rule(X, Y)|T],Propositions):-
	append(Y,[X],Listone),
	propositions(T,Restlists),
	append(Listone,Restlists,List),
	flatten(List,FlattenList),
	sort(FlattenList,Propositions).

log(X,Y) :- 
	Y is log(X).

checksets([], []).
checksets([H1|R1], [H2|R2]):-
    H1 = H2,
    checksets(R1, R2).

%Basic flatten function

flatten([], []) :- !.
flatten([L|Ls], FlatL) :-
    !,
    flatten(L, NewL),
    flatten(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten(L, [L]).

%To check if first list is subset of second list.

checksubset([], []).
checksubset([H|T], [H|T1]):-
  checksubset(T, T1).
checksubset([_|T], T1):-
  checksubset(T, T1).

% Function to remove duplicates from a list.
compress([], []).
compress([H], [H]).
compress([H,H|T], L):-		
	compress([H|T], L).
compress([H1,H2|T], [H1|L]):-
	compress([H2|T], L).

%Generic append function.
append([],L,L).
append([H|T],L,[H|T2]):-
	append(T,L,T2).

%FOIL and FOLD

%Filter out positive literals
extractonepositive(Empty,List):-
	positiveexamples(H),
	append(Empty,H,List).

extractpositives(O):-
	%we have to pick examples on the basis of information gain function.
	%currently working only for base condition, not for clause.
	findall(Y,extractonepositive([],Y),Z),
	compress(Z,O),
	!.

extractOneKBExample(Empty,List):-
	knowledgeRule(H),
	append(Empty,H,List).

extractKBExamples(O):-
	findall(Y,extractOneKBExample([],Y),Z),
	compress(Z,O),
	!.

ismember(X,[X|_]).
ismember(X,[Y|T]):- 
	ismember(X,T).

%handling exceptions
exception((A :- B), Xs, Xs1) :-
	findall(A, ( ismember(A,Xs), \+( \+ B ) ), Xs1).

exception((A:-B), Xs, Xs1) :-
	findall(A, ( ismember(A, Xs), \+ B ), Xs1 ).

%Pxs is PositiveExamples, Nxs is NegativeExamples 
info_value(Clause, PositiveExamples, NegativeExamples, Value) :-
	append(Clause, PositiveExamples, Ptuples),
	length(Ptuples, P),
	( P =:= 0 ->
	      Value = 0
	; append(Clause, NegativeExamples, Ntuples),
	  length(Ntuples, N),
	  Temp is P / (P + N),
	  log(Temp, Temp1),
	  Value is Temp1 * -1.442695
	).

%To calculate information gain.
informationGain(NegativeExamples, PositiveExamples, Info, AllExamples, Gain) :-
	exception(AllExamples, PositiveExamples, RetainedPositiveExamples),
	length(RetainedPositiveExamples, L),
	( L =:= 0 ->
	      Gain = 0
	; info_value(AllExamples, PositiveExamples, NegativeExamples, Info1),
	  Gain is L * (Info - Info1)
	).


%Filter out negative examples
returnNegExamples([],PositiveExamples,[]).
returnNegExamples([H|T],PositiveExamples,[H|NegativeExamples]):-
	\+ ismember(H,PositiveExamples),
	returnNegExamples(T,PositiveExamples,NegativeExamples),
	!.
returnNegExamples([H|T],PositiveExamples,NegativeExamples):-
	returnNegExamples(T,PositiveExamples,NegativeExamples).

createRule([],[]).
createRule([H|T],[not(knowledgeRule(H))|Rules]):-
	createRule(T,Rules).


%recursion check taken refenence from John Zelle website (NorthWestern)
recursionDecision(G, Pred, Arity, Flag) :-
	( functor(G, Pred, Arity) ->
	      Flag = true
	; Flag = false
	).

%main FOIL rule
createFOILrule(Rule):-
	extractKBExamples(AllExamples),
	extractpositives(PositiveExamples),
	returnNegExamples(AllExamples,PositiveExamples,NegativeExamples),
	createRule(NegativeExamples,Rule).
	

