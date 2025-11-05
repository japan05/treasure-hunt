% -------------------
% Dynamic predicates
% -------------------

:- dynamic wumpus/2.
:- dynamic pit/2.
:- dynamic gold/2.
:- dynamic visited/2.
:- dynamic start/2.

% -------------------
% World Setup
% -------------------

%grid size grid_max(4) = 5x5
grid_max(6).





% --------------------------
% Safe object placement
% --------------------------
setup_objects(0, _, _) :- !.  % base case

setup_objects(N, Max, Type) :-
    random_between(0, Max, X),
    random_between(0, Max, Y),
    % Ensures no overlap with a current entity
    \+ wumpus(X, Y),
    \+ pit(X, Y),
    \+ gold(X, Y), 
    \+ start(X, Y),
    Obj =.. [Type, X, Y], % create entity fact
    assertz(Obj),
    N1 is N - 1,
    setup_objects(N1, Max, Type).
setup_objects(N, Max, Type) :-    % Retry if cell was occupied
    setup_objects(N, Max, Type).

% --------------------------
% World Setup
% --------------------------
setup_world :-
    % Clear fact
    retractall(wumpus(_, _)),
    retractall(pit(_, _)), 
    retractall(gold(_, _)),
    retractall(start(_, _)),

    grid_max(Max),

    % Random Start Position
    repeat,
        random_between(0, Max, SX),
        random_between(0, Max, SY),
        % just in case
        \+ wumpus(SX,SY),
        \+ pit(SX,SY),
        \+ gold(SX,SY),
        assertz(start(SX,SY)),
    !,

    % Random Wumpus Positions
    random_between(2, 3, NumWumpus),
    setup_objects(NumWumpus, Max, wumpus),

    % Random Pit Positions
    random_between(3, 5, NumPits),
    setup_objects(NumPits, Max, pit),

    % Random Gold Position 
    repeat,
        random_between(0, Max, GX),
        random_between(0, Max, GY),
        \+ wumpus(GX,GY),
        \+ pit(GX,GY),
        \+ gold(GX,GY),
        assertz(gold(GX,GY)),
    !,

    % Print final world setup
    print_world.



% Print world

print_world :-
    grid_max(Max),
    nl, write('World Grid:'), nl,
    forall(between(0, Max, Y),
        (forall(between(0, Max, X),
            print_cell(X,Y)),nl)), nl.

print_cell(X,Y) :-
    ( wumpus(X,Y) -> write(' W ')
    ; pit(X,Y)    -> write(' P ')
    ; gold(X,Y)   -> write(' G ')
    ; start(X,Y)  -> write(' S ')
    ; write(' . ')
    ).

% -------------------
% Helpers
% -------------------
adjacent(X,Y,X1,Y1) :- X1 is X+1, Y1 is Y.
adjacent(X,Y,X1,Y1) :- X1 is X-1, Y1 is Y.
adjacent(X,Y,X1,Y1) :- X1 is X, Y1 is Y+1.
adjacent(X,Y,X1,Y1) :- X1 is X, Y1 is Y-1.

in_bounds(X,Y) :- grid_max(Max), X >= 0, Y >= 0, X =< Max, Y =< Max.
safe(X,Y) :- in_bounds(X,Y), \+ wumpus(X,Y), \+ pit(X,Y).
heuristic([X,Y], H) :-
    gold(GX,GY),
    H is abs(X-GX) + abs(Y-GY).

move_cost(X,Y,1000) :- wumpus(X,Y), !.
move_cost(X,Y,1000) :- pit(X,Y), !.
move_cost(X,Y,10)   :- \+ safe(X,Y), !.
move_cost(X,Y,1)    :- safe(X,Y), !.
move_cost(_,_,1).

% -------------------
% BFS 
% -------------------

% 
bfs([[ [X,Y] | Path ] | _], [[X,Y] | Path], [[X,Y]]) :-
    gold(X,Y),
    write('BFS Gold found at: '), write([X,Y]), nl, !.

bfs([[ [X,Y] | Path ] | Rest], FinalPath, Iterations) :-
    % find all new unvistied path and add the coordinate
    findall([ [NX,NY] | [ [X,Y] | Path ] ],
            (adjacent(X,Y,NX,NY), safe(NX,NY), \+ visited(NX,NY)),
            NextNodes),
    % add visited fact for each NX,NY
    forall(member([ [NX,NY] | _], NextNodes), assertz(visited(NX,NY))),
    % FIFO add newest coordinates to the end of queue
    append(Rest, NextNodes, NewQueue),
    % add NX NY to Iter
    findall([NX,NY], member([ [NX,NY] | _], NextNodes), VisitedNow),
    bfs(NewQueue, FinalPath, IterRest),
    append(VisitedNow, IterRest, Iterations).

bfs([], [], []).

find_gold_path_bfs(P, I) :-
    retractall(visited(_, _)),
    start(SX, SY),
    assertz(visited(SX, SY)),
    bfs([[[SX, SY]]], RevPath, IterRest),
    ( RevPath \= [] ->
        reverse(RevPath, P),                % reverse path
        I = [[SX, SY] | IterRest]
    ;
        write('No path found!'), nl,
        P = [],
        I = [[SX, SY] | IterRest]
    ).


% -------------------
% DFS 
% -------------------

dfs([[X,Y]|Path], [[X,Y]|Path], [[X,Y]]) :-
    gold(X,Y),
    !.

dfs([[X,Y]|Path], FinalPath, Iterations) :-
    findall([NX,NY],
            (adjacent(X,Y,NX,NY),
             safe(NX,NY),
             \+ visited(NX,NY)),
            NextNodes),
    try_dfs_children(NextNodes, [[X,Y]|Path], FinalPath, Iterations).

try_dfs_children([], _Path, [], []).  % no path found
try_dfs_children([[NX,NY]|Rest], Path, FinalPath, Iterations) :-
    assertz(visited(NX,NY)),
    dfs([[NX,NY]|Path], FP, IterChild),
    ( FP \= [] ->
        FinalPath = FP,
        Iterations = [[NX,NY]|IterChild]
    ;
        try_dfs_children(Rest, Path, FinalPath, IterRest),
        Iterations = [[NX,NY]|IterRest]
    ).

find_gold_path_dfs(P, I) :-
    retractall(visited(_, _)),
    start(SX, SY),
    assertz(visited(SX, SY)),
    dfs([[SX, SY]], RevPath, IterRest),
    ( RevPath \= [] ->
        reverse(RevPath, P),                % ✅ Fix: reverse to show start → goal
        I = [[SX, SY] | IterRest]
    ;
        write('No path found!'), nl,
        P = [],
        I = [[SX, SY] | IterRest]
    ).


% -------------------
% IDS (Iterative Deepening Search)
% -------------------


dls([[X,Y]|Path], _Limit, [[X,Y]|Path]) :-
    gold(X,Y),
    !.

dls([[X,Y]|Path], Limit, FinalPath) :-
    Limit > 0,
    NewLimit is Limit - 1,
    adjacent(X,Y,NX,NY),
    safe(NX,NY),
    \+ member([NX,NY], [[X,Y]|Path]),   % avoid cycles
    dls([[NX,NY]|[[X,Y]|Path]], NewLimit, FinalPath).


ids(Start, FinalPath) :-
    between(1, 20, DepthLimit),
    write('Trying depth limit: '), write(DepthLimit), nl,
    dls(Start, DepthLimit, FinalPath),
    !.  % stop when first success found

find_gold_path_ids(P) :-
    retractall(visited(_,_)),
    start(SX, SY),
    assertz(visited(SX, SY)),
    ids([[SX,SY]], RevPath),
    reverse(RevPath, P).

% -------------------
% UCS (Uniform Cost Search)
% -------------------

find_gold_path_ucs(PathWithG, IterationsWithG) :-
    start(SX, SY),
    ucs([[0,[SX,SY],[[SX,SY,0]]]], [], PathWithG, IterationsWithG),
    ( PathWithG \= [] ->
    nl
    ; write('No path!'), nl ).

ucs([], Visited, [], Visited) :- !.

ucs(Open, VisitedSoFar, FinalPath, Iterations) :-
    sort(Open, [[G,[X,Y],PathWithG]|Rest]),  % pick lowest cost
    CurrentNodeG = [X,Y,G],
    ( gold(X,Y) ->
        reverse(PathWithG, FinalPath),
        append(VisitedSoFar, [CurrentNodeG], Iterations)
    ;
        findall([NewG,[NX,NY],[[NX,NY,NewG]|PathWithG]],
            (adjacent(X,Y,NX,NY),
             in_bounds(NX,NY),
             \+ pit(NX,NY),
             \+ wumpus(NX,NY),
             \+ member([NX,NY,_], PathWithG),   % avoid loops
             move_cost(NX,NY,Step),
             NewG is G + Step
            ), Children),
        append(Rest, Children, NewOpen),
        append(VisitedSoFar, [CurrentNodeG], NewVisited),
        ucs(NewOpen, NewVisited, FinalPath, Iterations)
    ).


% -------------------
% A* Weighted
% -------------------

find_gold_path_astar_weighted(W, PathWithGH, IterationsWithGH) :-
    start(SX, SY),
    astar_weighted(W, [[0,0,[SX,SY],[[SX,SY,0,0]]]], [], PathWithGH, IterationsWithGH),
    ( PathWithGH \= [] ->
        nl
    ; write('No path!'), nl ).

astar_weighted(_W, [], Visited, [], Visited) :- !.

astar_weighted(W, Open, VisitedSoFar, FinalPath, Iterations) :-
    sort(Open, [[F,G,[X,Y],PathWithGH]|Rest]),
    heuristic([X,Y], H),
    CurrentNodeGH = [X,Y,G,H],
    ( gold(X,Y) ->
        reverse(PathWithGH, FinalPath),
        append(VisitedSoFar, [CurrentNodeGH], Iterations)
    ;
        findall([NewF, NewG, [NX,NY], [[NX,NY,NewG,NewH]|PathWithGH]],
            (adjacent(X,Y,NX,NY),
             in_bounds(NX,NY),
             \+ pit(NX,NY),
             \+ wumpus(NX,NY),
             \+ member([NX,NY,_,_], PathWithGH),   % avoid loops
             NewG is G + 1,
             heuristic([NX,NY], NewH),
             NewF is NewG + W*NewH
            ), Children),
        append(Rest, Children, NewOpen),
        append(VisitedSoFar, [CurrentNodeGH], NewVisited),
        astar_weighted(W, NewOpen, NewVisited, FinalPath, Iterations)
    ).

% -------------------
% Print path function for prolog_bridge.py compatibility
% -------------------
print_path([]) :- nl.
print_path([X,Y|Rest]) :-
    write('['), write(X), write(','), write(Y), write('] → '),
    print_path(Rest).

% -------------------
% Simple algorithm entry points for prolog_bridge.py compatibility
% -------------------

% Simple BFS entry point
find_gold_path_bfs(Path) :-
    write('BFS: Starting search...'), nl,
    ( start(SX, SY) ->
        write('BFS: Starting from '), write([SX,SY]), nl
    ;
        write('BFS: No start position found, using (0,0)'), nl,
        assertz(start(0,0)),
        SX = 0, SY = 0
    ),
    ( gold(GX, GY) ->
        write('BFS: Goal is at '), write([GX,GY]), nl
    ;
        write('BFS: No gold found!'), nl,
        Path = [],
        !
    ),
    find_gold_path_bfs(PathWithIterations, _),
    extract_simple_coordinates(PathWithIterations, Path),
    write('BFS: Found path with '), length(Path, Len), write(' steps'), nl.

% Simple DFS entry point
find_gold_path_dfs(Path) :-
    write('DFS: Starting search...'), nl,
    ( start(SX, SY) ->
        write('DFS: Starting from '), write([SX,SY]), nl
    ;
        write('DFS: No start position found, using (0,0)'), nl,
        assertz(start(0,0))
    ),
    ( gold(GX, GY) ->
        write('DFS: Goal is at '), write([GX,GY]), nl
    ;
        write('DFS: No gold found!'), nl,
        Path = [],
        !
    ),
    find_gold_path_dfs(PathWithIterations, _),
    extract_simple_coordinates(PathWithIterations, Path),
    write('DFS: Found path with '), length(Path, Len), write(' steps'), nl.

% Simple IDS entry point (already correct signature, just add debug)
find_gold_path_ids(Path) :-
    write('IDS: Starting search...'), nl,
    ( start(SX, SY) ->
        write('IDS: Starting from '), write([SX,SY]), nl
    ;
        write('IDS: No start position found, using (0,0)'), nl,
        assertz(start(0,0))
    ),
    ( gold(GX, GY) ->
        write('IDS: Goal is at '), write([GX,GY]), nl
    ;
        write('IDS: No gold found!'), nl,
        Path = [],
        !
    ),
    retractall(visited(_,_)),
    start(SX2, SY2),
    assertz(visited(SX2, SY2)),
    ids([[SX2,SY2]], RevPath),
    reverse(RevPath, TempPath),
    extract_simple_coordinates(TempPath, Path),
    write('IDS: Found path with '), length(Path, Len), write(' steps'), nl.

% Simple UCS entry point  
find_gold_path_ucs(Path) :-
    write('UCS: Starting search...'), nl,
    ( start(SX, SY) ->
        write('UCS: Starting from '), write([SX,SY]), nl
    ;
        write('UCS: No start position found, using (0,0)'), nl,
        assertz(start(0,0))
    ),
    ( gold(GX, GY) ->
        write('UCS: Goal is at '), write([GX,GY]), nl
    ;
        write('UCS: No gold found!'), nl,
        Path = [],
        !
    ),
    find_gold_path_ucs(PathWithIterations, _),
    extract_coordinates(PathWithIterations, Path),
    write('UCS: Found path with '), length(Path, Len), write(' steps'), nl.

% Simple A* entry point
find_gold_path_astar(Path) :-
    write('A*: Starting search...'), nl,
    ( start(SX, SY) ->
        write('A*: Starting from '), write([SX,SY]), nl
    ;
        write('A*: No start position found, using (0,0)'), nl,
        assertz(start(0,0))
    ),
    ( gold(GX, GY) ->
        write('A*: Goal is at '), write([GX,GY]), nl
    ;
        write('A*: No gold found!'), nl,
        Path = [],
        !
    ),
    find_gold_path_astar_weighted(1, PathWithGH, _),
    extract_coordinates(PathWithGH, Path),
    write('A*: Found path with '), length(Path, Len), write(' steps'), nl.

% Simple PRM entry point (uses BFS for simplicity)
find_gold_path_prm(Path) :-
    write('PRM: Using BFS implementation...'), nl,
    find_gold_path_bfs(Path).

% -------------------
% Helper functions for coordinate extraction
% -------------------

% Extract simple coordinates from path with extra data
extract_simple_coordinates([], []).
extract_simple_coordinates([[X,Y]|Rest], [X,Y|RestCoords]) :-
    extract_simple_coordinates(Rest, RestCoords).

% Extract coordinates from path with G,H values
extract_coordinates([], []).
extract_coordinates([[X,Y,_,_]|Rest], [X,Y|RestCoords]) :-
    extract_coordinates(Rest, RestCoords).
extract_coordinates([[X,Y,_]|Rest], [X,Y|RestCoords]) :-
    extract_coordinates(Rest, RestCoords).
extract_coordinates([[X,Y]|Rest], [X,Y|RestCoords]) :-
    extract_coordinates(Rest, RestCoords).

% -------------------
% Debug utility functions
% -------------------

% Print current world facts for debugging
debug_world_state :-
    write('=== DEBUG WORLD STATE ==='), nl,
    forall(start(X,Y), (write('Start: '), write([X,Y]), nl)),
    forall(wumpus(X,Y), (write('Wumpus: '), write([X,Y]), nl)),
    forall(pit(X,Y), (write('Pit: '), write([X,Y]), nl)),
    forall(gold(X,Y), (write('Gold: '), write([X,Y]), nl)),
    write('========================'), nl.






