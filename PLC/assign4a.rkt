male(tony).
male(ethan).
male(chris).
male(ben).
male(nathan).
female(sim).
female(helen).
female(sophie).
female(olivia).
parents(helen, sim, tony).
parents(chris, sim, tony).
parents(ben, sim, tony).
parents(sophie, helen, ethan).
parents(olivia, helen, ethan).
parents(nathan, helen, ethan).
member(X, [X | T]).
member(X, [H | T]) :- member(X, T).
append([], L, L).
append([H | T], L, [H | T2]) :- append(T, L, T2).
rev([], []).
rev([H | T], R) :- rev(T, RT), append(RT, [H], R).
reva([], A, A).
reva([H | T], A, R) :- reva(T, [H | A], R).
len([], 0).
len([H | T], N) :- len(T, N1), N is N1+1.
lena([], A, A).
lena([H | T], A, N) :- A1 is A+1, lena(T, A1, N).
/*********************************************************/
/*
 * sister_of(X, Y)
 *
 * X - element (the sister)
 * Y - element (the sibling)
 */        
sister_of(X, Y) :-
    female(X),
    parents(Y, M, D),
    parents(X, M, D),
    X\=Y. 
        
/*
 * second(L, E)
 *
 * L - A list
 * E - A list element
 */
second([H,E|Tail],E).    %If the element E is the Head of the Tail of the list, return yes.

/*
 * one(L)
 *
 * L - a List
 */
one(L):-
    len(L,N),        % N = the length of list L.
    N=1.             % If N=1, return yes.
        
         
/*
 * insertion_sort(L, S)
 *
 * L - a List of numbers
 * S - a List of numbers, which is the sorted version of L
 */
insertion_sort([],[]).              % Base Case - Two empty lists are already sorted.
insertion_sort([H|T],S):-
                insertion_sort(T,W), % Sort the Tail of the list L.
                insert(H,W,S).       % Insert the Head of L

insert(X,[],[X]).
insert(X,[H|T],[X|T2]):-
                X=<H,               % If the element is less than the Head
                insert(H,T,T2).     % Insert the Head into the Tails
insert(X,[H|T],[H|T2]):-
                X>H,                 % If the element is less than the Head
                insert(X,T,T2).      % Insert the Element into the Tails

/*
 * index(E, L, N)
 *
 * E - a symbol
 * L - a List of symbols
 * N - a number
 */
index(E, [E|T], 0).       % The element is at the Head of the list parameter L.
index(E, [H|T], N):-      % Element isn't at the head of the list parameter.
    index(E, T, N1),      % Look for the element in the tail of the list.
    N is N1+1.            % Incremement the index counter


%sister_of(sim, N).
%sister_of(helen, N).
%sister_of(sophie, N).
%sister_of(olivia, N).
%sister_of(tony, N).
%sister_of(ethan, N).
%sister_of(chris, N).
%sister_of(ben, N).
%sister_of(nathan, N).
%sister_of(olivia, sophie).
%sister_of(olivia, nathan).
%sister_of(helen, sophie).
%sister_of(helen, chris).

%second([], E).
%second([a], E).
%second([a,b], E).
%second([a,b,c], E).
%second([d,s,z,a], E).
%second([d,s,z,v,d],z).
%second([d,s,z,v,d],s).

%one(a).
%one([]).
%one([a]).
%one([a,b]).

%insertion_sort([],[]).
%insertion_sort([1,2,3,4,5],N).
%insertion_sort([1,2,3,4,5],[1,2,3,4,5]).
%insertion_sort([4,5,2,3,1,3,2],N).
%insertion_sort([4,5,2,3,1,3,2],[1,2,2,3,3,4,5]).


%index(x,[x],N).
%index(x,[x],0).
%index(x,[x],1).
%index(x,[q,d,s,x],N).
%index(x,[q,d,s,x,a,s,d,a,x],N).