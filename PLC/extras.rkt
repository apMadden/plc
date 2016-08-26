

%one([]).
%one([H|T]):-
%index(E, [], N).
%index(E, [H|T], N):-



/**
 * predicate(-Arg:type) is nondet
 *
 *
 * L - unsorted list
 * S - sorted list
 */

insertion_sort(L,S):-sort(L, [], S).
         
sort([],A,A).
sort([H|T],A,S):-insert(H, A, newA),sort(T, newA, S).

insert(E,[H|T],[H|T2]) :-E>H,insert(E,T,T2).
insert(E,[H|T],[E,H|T]):-X=<Y.
insert(E,[],[E]).

insert(E,[H|Sorted],[H|Sorted1]):-X>Y,!,insert(X,Sorted,Sorted1).        
%!insert(X,Sorted,[X|Sorted]).