/** FLP 2020
Toto je ukazkovy soubor zpracovani vstupu v prologu.
Tento soubor muzete v projektu libovolne pouzit.

autor: Martin Hyrs, ihyrs@fit.vutbr.cz
preklad: swipl -q -g start -o flp19-log -c input2.pl
*/
:- dynamic edge/2, vert/1.

add_vert(A) :-
    clause(vert(A), true),
    !.
add_vert(A) :-
    assertz(vert(A)).

add_edge(A, B) :-
    clause(edge(A, B), true),
    !.
add_edge(A, B) :-
    assertz(edge(A, B)).

create_edges([]).
create_edges([[[A],[B]]|T]) :-
    add_vert(A),
    add_vert(B),
    add_edge(A,B),
    add_edge(B,A),
    create_edges(T).

rotate_list(List, Rotated) :-
    append(Prefix, Suffix, List),
    append(Suffix, Prefix, Rotated).

vert_used([], _) :- true.
vert_used([H|T], L) :-
    (member(H:_, L);
    member(_:H, L)),
    !,
    vert_used(T, L).

all_verts_used(Y, L) :- 
    bagof(X, vert(X), XR),
    vert_used(XR, [Y:Y|L]).

path_one(X, L, R) :-
    edge(X, Y),
    not(memberchk(Y:X, L)),
    not(memberchk(X:_, L)),
    not(memberchk(_:Y, L)),
    memberchk(Y:_, L),
    all_verts_used(Y, L),
    !,
    reverse([X:Y|L], R).

path_one(X, L, R) :-
    edge(X, Y),
    not(memberchk(X:_, L)),
    not(memberchk(X:Y, L)),
    not(memberchk(Y:X, L)),
    path_one(Y, [X:Y|L], R).


all_paths(R):-
    vert(X),
    path_one(X, [], R).

change_edge_order([], []).
change_edge_order([X:Y|T], [Y:X|R]) :-
    change_edge_order(T, R).

list_intersection([], _) :- false.
list_intersection(_, []) :- false.
list_intersection([H|T], List2) :-
    member(H, List2);
    list_intersection(T, List2).

edge_intersection(X, L) :-
    findall(Y, rotate_list(X, Y), R1),
    change_edge_order(X, RX),
    reverse(RX, RRX),
    findall(Y, rotate_list(RRX, Y), R2),
    append(R1, R2, RR),
    list_intersection(RR, L),
    !.

clean_paths([], [], []).

clean_paths([], [H|T], [H|R]) :-
    clean_paths([], T, R).
    
clean_paths([H|T], L, R) :-
    edge_intersection(H, L) ->
    clean_paths(T, L, R);
    clean_paths(T, [H|L], R).

main :- 
    consult('input2.pl'),
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,S),
    create_edges(S),
    findall(X, all_paths(X), R),
    clean_paths(R, [], Res),
    write(Res),
    halt.

