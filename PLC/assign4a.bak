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
sister_of(X, Y) :-
    female(X),
    parents(Y, M, D),
    parents(X, M, D),
    X\=Y. 
        
/*********************************************************/
one(A):-len(A,N),N=1.
        
/*********************************************************/
second([H,H2|Tail],H2).
         
/*********************************************************/
insertion_sort([],[]).
insertion_sort([H|T],S):-insertion_sort(T,W), insert(H,W,S).

insert(X,[],[X]).
insert(X,[H|T],[X|T2]):-X=<H, insert(H,T,T2). % If the element is less than the Head
insert(X,[H|T],[H|T2]):-X>H,insert(X,T,T2).         

/*********************************************************/
 %E is a symbol, L is a list of symbols, N is the index.
index(E, [E|T], 0).       % The element is at the Head of the list parameter L.
index(E, [H|T], N):-      % Element isn't at the head of the list parameter.
    index(E, T, N1),      % Look for the element in the tail of the list.
    N is N1+1.            % Incremement the index counter


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