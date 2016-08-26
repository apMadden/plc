quicksort([], []).
quicksort([H | T], S) :-
   partition(T, H, Less, Same, Greater),
   quicksort(Less, SLess),
   quicksort(Greater, SGreater),
   append(SLess, [H | Same], SGreater, S).

append([], L, L).
append([H | T], L, [H | A]) :- append(T, L, A).

append(L1, L2, L3, A) :- append(L2, L3, L23), append(L1, L23, A).