% -----------------------------
% Dynamic predicates
% -----------------------------
:- dynamic visited/2.
:- dynamic safe/2.
:- dynamic possible_wumpus/2.
:- dynamic possible_pit/2.
:- dynamic agent/2.
:- dynamic wumpus/2.
:- dynamic pit/2.
:- dynamic gold/2.
:- dynamic percept_at/3.
:- dynamic found_wumpus/2.
:- dynamic found_pit/2.
:- discontiguous print_full_map/0.
:- dynamic found_gold/2.
:- dynamic grid_size/1.
:- dynamic score/1.

% -----------------------------
% Run the world
% -----------------------------
run(Steps,Score) :-
    clean_world,
    init_world,
    init_score,
    main_loop_steps([], Steps),
    score(Score).
grid_size(6).
% -----------------------------
% Cleanup world
% -----------------------------
clean_world :-
    retractall(visited(_, _)),
    retractall(percept_at(_, _, _)),
    retractall(safe(_, _)),
    retractall(possible_wumpus(_, _)),
    retractall(possible_pit(_, _)),
    retractall(agent(_, _)),
    retractall(wumpus(_, _)),
    retractall(pit(_, _)),
    retractall(gold(_, _)),
    retractall(found_wumpus(_, _)),
    retractall(found_pit(_, _)).

% -----------------------------
% Initialize world
% -----------------------------
% -----------------------------
% Initialize world with random hazards
% -----------------------------
init_world :-
    assert(score(0)),
    clean_world,  
    grid_size(X),
    GridSize = X,
    write(GridSize),
    % Place 1 Wumpus randomly
    random_between(1, GridSize, WX),
    random_between(1, GridSize, WY),
    assert(wumpus(WX, WY)),

    % Place 1 Gold randomly, avoid Wumpus cell
    repeat,
        random_between(1, GridSize, GX),
        random_between(1, GridSize, GY),
        (GX \= WX ; GY \= WY),
        assert(gold(GX, GY)), !,

    % Place 3 pits randomly, avoid Wumpus, Gold, start (1,1)
    place_random_pits(3, GridSize, WX, WY, GX, GY, 1, 1),

    % Initialize agent
    assert(agent(1,1)),
    assert(safe(1,1)),
    assert(visited(1,1)),

    format('--- Starting Wumpus World ---~n', []),
    print_grid.

% -----------------------------
% Place N random pits avoiding forbidden cells
% -----------------------------
place_random_pits(0, _, _, _, _, _, _, _) :- !.
place_random_pits(N, GridSize, WX, WY, GX, GY, SX, SY) :-
    random_between(1, GridSize, PX),
    random_between(1, GridSize, PY),
    % Not overlapping Wumpus, Gold, Start, or other pits
    \+ wumpus(PX, PY),
    \+ gold(PX, PY),
    \+ pit(PX, PY),
    (PX \= SX ; PY \= SY),
    assert(pit(PX, PY)),
    N1 is N - 1,
    place_random_pits(N1, GridSize, WX, WY, GX, GY, SX, SY), !.
place_random_pits(N, GridSize, WX, WY, GX, GY, SX, SY) :-
    % Retry if invalid
    place_random_pits(N, GridSize, WX, WY, GX, GY, SX, SY).

% -----------------------------
% Grid visualization (safe printing)
% -----------------------------
print_grid :-
    format('~nCurrent Grid (A=Agent, .=Safe, ?=Unknown, P?=Possible Pit, W?=Possible Wumpus, v=Visited):~n'),
    print_rows(4).

print_rows(0) :- !.  % done
print_rows(Y) :-
    print_row(Y, 1),
    nl,
    Y1 is Y - 1,
    print_rows(Y1).

print_row(_, 5) :- !.  % done with columns
print_row(Y, X) :-
    print_cell(X,Y),
    X1 is X + 1,
    print_row(Y, X1).

print_cell(X,Y) :-
    ( agent(X,Y) -> write(' A ')
    ; safe(X,Y) -> write(' . ')
    ; possible_wumpus(X,Y) -> write(' W?')
    ; possible_pit(X,Y) -> write(' P?')
    ; visited(X,Y) -> write(' v ')
    ; write(' ? ')
    ).
print_full_map :-
    format('~nFull Grid (A=Agent, W=Wumpus, G=Gold, P=Pit, .=Safe, v=Visited):~n'),
    print_full_rows(4).

print_full_rows(0) :- !.
print_full_rows(Y) :-
    print_full_row(Y, 1),
    nl,
    Y1 is Y - 1,
    print_full_rows(Y1).

print_full_row(_, 5) :- !.
print_full_row(Y, X) :-
    print_full_cell(X,Y),
    X1 is X + 1,
    print_full_row(Y, X1).

print_full_cell(X,Y) :-
    ( agent(X,Y) -> write(' A ')
    ; wumpus(X,Y) -> write(' W ')
    ; gold(X,Y) -> write(' G ')
    ; pit(X,Y) -> write(' P ')
    ; safe(X,Y) -> write(' . ')
    ; visited(X,Y) -> write(' v ')
    ; write(' ? ')
    ).
print_full_map.

% -----------------------------
% Main loop 
% -----------------------------
main_loop_steps(Acc, Steps) :-
    agent(X, Y),
    perceive(X, Y, Percepts),
    assertz(visited(X,Y)),

    % Run inference
    infer_from_percepts(X, Y, Percepts),
    rerun_inference,

    print_grid,       
    print_full_map,

    % Get current knowledge as a step
    build_step(Step),
    append(Acc, [Step], NewAcc),

    % Check if gold found
    ( member(glitter, Percepts) -> 
        assert_found_gold(X,Y),
        format('Gold found at (~w,~w)! Exploration ends.~n', [X,Y]),
        % Remove all stench knowledge after gold is found
        retractall(percept_at(_,_,_)),  % clear old percepts
        forall((percept_at(A,B,OldPs), member(stench,OldPs)),
               (select(stench, OldPs, NewPs),
                retract(percept_at(A,B,OldPs)),
                assertz(percept_at(A,B,NewPs)))),
        Steps = NewAcc
    ;
        % No safe moves left? shoot if Wumpus known
        next_safe_move(NX, NY),
        ( NX = none ->
            ( found_wumpus(WX, WY) ->
                shoot_wumpus(WX,WY),
                % After shooting, check for safe moves again
                next_safe_move(NX2, NY2),
                ( NX2 = none -> Steps = NewAcc
                    ; retractall(agent(_, _)), assert(agent(NX2,NY2)),update_score(-1), 
                    main_loop_steps(NewAcc, Steps)
                )
                ; Steps = NewAcc
            )

        ;
            % Move to next safe cell
            retractall(agent(_, _)),
            assert(agent(NX, NY)),
            main_loop_steps(NewAcc, Steps)
        )
    ).



% -----------------------------
% Perception
% -----------------------------
perceive(X, Y, Percepts) :-
    percept_at(X, Y, Percepts), !.  % Already known
perceive(X, Y, Percepts) :-
    findall(P, infer_percept_from_env(X, Y, P), PerceptsList),
    list_to_set(PerceptsList, Percepts),
    assertz(percept_at(X, Y, Percepts)),
    format('Scanned (~w,~w): ~w~n', [X, Y, Percepts]).

infer_percept_from_env(X, Y, stench) :- adjacent(X, Y, WX, WY), wumpus(WX, WY).
infer_percept_from_env(X, Y, breeze) :- adjacent(X, Y, PX, PY), pit(PX, PY).
infer_percept_from_env(X, Y, glitter) :- gold(X, Y).

% -----------------------------
% Grid printing
% -----------------------------


% -----------------------------
% Inference rules
% -----------------------------
infer_from_percepts(X, Y, Percepts) :-
    (Percepts = [] -> infer_safe_neighbors(X,Y) ; true),
    (member(stench, Percepts) -> infer_possible_wumpus(X,Y) ; true),
    (member(breeze, Percepts) -> infer_possible_pit(X,Y) ; true),
    (member(glitter, Percepts) -> assert_found_gold(X,Y) ; true),
    infer_safe_by_contradiction,
    cross_infer_wumpus,
    cross_infer_pit.

infer_safe_neighbors(X,Y) :-
    neighbors(X,Y,Ns),
    forall(member((NX,NY), Ns),
        (\+ visited(NX,NY), \+ safe(NX,NY) -> assertz(safe(NX,NY)), format('Inferred safe: (~w,~w)~n',[NX,NY]) ; true)).

assert_found_gold(X,Y) :-
    (found_gold(X,Y) -> true ; assertz(found_gold(X,Y)),
    update_score(1000),      % Getting gold gives 1000 points 
    format('Found Gold at (~w,~w)!~n',[X,Y])).

infer_possible_pit(X,Y) :-
    neighbors(X,Y,Ns),
    forall(member((NX,NY), Ns),
        (\+ safe(NX,NY), \+ visited(NX,NY), \+ possible_pit(NX,NY) -> assertz(possible_pit(NX,NY)), format('Possible pit at (~w,~w)~n',[NX,NY]) ; true)).

infer_possible_wumpus(X,Y) :-
    neighbors(X,Y,Ns),
    forall(member((NX,NY), Ns),
        (\+ safe(NX,NY), \+ visited(NX,NY), \+ possible_wumpus(NX,NY) -> assertz(possible_wumpus(NX,NY)), format('Possible Wumpus at (~w,~w)~n',[NX,NY]) ; true)).

cross_infer_wumpus :-
    % Collect all cells that have a 'stench' in their percepts
    findall((X, Y),
            (percept_at(X, Y, Ps), member(stench, Ps)),
            StenchCells),
    % For each pair of distinct stench cells, find their common neighbor
    forall(
        (member((X1, Y1), StenchCells),
         member((X2, Y2), StenchCells),
         (X1, Y1) \= (X2, Y2),
         common_neighbor(X1, Y1, X2, Y2, NX, NY)),
        (
            % Only assert a new Wumpus if the cell is not already safe, visited, or known as a Wumpus
            (   \+ safe(NX, NY),
                \+ visited(NX, NY),
                \+ found_wumpus(NX, NY),
                \+ found_pit(NX, NY)

            ->  ( assertz(found_wumpus(NX, NY)),rerun_inference,
                  format(' Confirmed Wumpus at (~w,~w)!~n', [NX, NY]) )
            ;   true
            )
        )
    ).

cross_infer_pit :-
    findall((X, Y),
            (percept_at(X, Y, Ps), member(breeze, Ps)),
            BreezeCells),

    % For each pair of distinct stench cells, find their common neighbor
    forall(
        (member((X1, Y1), BreezeCells),
         member((X2, Y2), BreezeCells),
         (X1, Y1) \= (X2, Y2),
         common_neighbor(X1, Y1, X2, Y2, NX, NY)),
        (
            (   \+ safe(NX, NY),
                \+ visited(NX, NY),
                \+ found_pit(NX, NY),
                \+ found_wumpus(NX, NY)
            ->  ( assertz(found_pit(NX, NY)),rerun_inference,
                  format(' Confirmed pit at (~w,~w)!~n', [NX, NY]) )
            ;   true
            )
        )
    ).

% -----------------------------
% Contradiction inference
% -----------------------------
infer_safe_by_contradiction :-
    forall(
        ( percept_at(X1,Y1,P1),
          percept_at(X2,Y2,P2),
          (X1 \= X2 ; Y1 \= Y2),
          perpendicular_neighbors(X1,Y1,X2,Y2,MX,MY),
          ((member(stench,P1), \+ member(stench,P2));
           (member(breeze,P1), \+ member(breeze,P2));
           (member(stench,P2), \+ member(stench,P1));
           (member(breeze,P2), \+ member(breeze,P1))),
          adjacent(MX,MY,SX,SY), safe(SX,SY),
          \+ safe(MX,MY),
          \+ visited(MX,MY),
          \+ found_wumpus(MX,MY),
          \+ found_pit(MX,MY)
        ),
        ( format(' Inference from (~w,~w) and (~w,~w) → (~w,~w) marked SAFE.~n',
                  [X1,Y1,X2,Y2,MX,MY]),
          assertz(safe(MX,MY))
        )
    ).

% -----------------------------
% Re-run inference for all visited
% -----------------------------
rerun_inference :-
    findall((X,Y), visited(X,Y), VisitedList),
    forall(member((VX,VY), VisitedList),
        (percept_at(VX,VY,P), infer_from_percepts(VX,VY,P))).

% -----------------------------
% Neighbors
% -----------------------------

% Find a common neighbor of two stench cells
common_neighbor(X1, Y1, X2, Y2, NX, NY) :-
    neighbors(X1, Y1, N1),
    neighbors(X2, Y2, N2),
    member((NX, NY), N1),
    member((NX, NY), N2),
    NX >= 1, NX =< 4, NY >= 1, NY =< 4.




neighbors(X,Y,Ns) :-
    findall((NX,NY), (adjacent(X,Y,NX,NY), NX>0,NY>0,NX=<3,NY=<3), Ns).

adjacent(X,Y,NX,Y) :- NX is X+1.
adjacent(X,Y,NX,Y) :- NX is X-1.
adjacent(X,Y,X,NY) :- NY is Y+1.
adjacent(X,Y,X,NY) :- NY is Y-1.

perpendicular_neighbors(X1,Y1,X2,Y2,MX,MY) :-
    DX is abs(X1 - X2),
    DY is abs(Y1 - Y2),
    DX =:= 1,
    DY =:= 1,
    MX is X1,
    MY is Y2.

perpendicular_neighbors(X1,Y1,X2,Y2,MX,MY) :-
    DX is abs(X1 - X2),
    DY is abs(Y1 - Y2),
    DX =:= 1,
    DY =:= 1,
    MX is X2,
    MY is Y1.
% -----------------------------
% Next safe move
% -----------------------------
next_safe_move(NX,NY) :-
    safe(NX,NY),
    \+ visited(NX,NY), !.
next_safe_move(none, none).




shoot_wumpus(WX,WY) :-
    format(' Shooting Wumpus at (~w,~w)!~n', [WX, WY]),
    retract(found_wumpus(WX,WY)),
    assertz(wumpus_dead(WX,WY)),
    update_score(-10),        % Shooting arrow costs 10 points
    % Remove stench from all cells adjacent to Wumpus
    neighbors(WX,WY,Adj),
    forall(member((AX,AY),Adj),
        (percept_at(AX,AY,Ps) ->
            select(stench, Ps, NewPs),
            retract(percept_at(AX,AY,Ps)),
            assertz(percept_at(AX,AY,NewPs))
        ; true
        )
    ).


% Build a step with percepts included
build_step(Step) :-
    findall([X,Y,Val,Percepts],
        (   determined_cell(X,Y,Val),
            cell_percepts(X,Y,Percepts)
        ),
        Step).

% Get percepts at a cell (if none, return empty list)
cell_percepts(X,Y,Percepts) :-
    ( percept_at(X,Y,Ps) -> Percepts = Ps ; Percepts = [] ).

% Determine cell value
determined_cell(X,Y,'A') :- agent(X,Y).       % Current agent
determined_cell(X,Y,'.') :- safe(X,Y), \+ visited(X,Y), \+ agent(X,Y).
determined_cell(X,Y,'W') :- found_wumpus(X,Y), \+ visited(X,Y), \+ agent(X,Y).
determined_cell(X,Y,'P') :- found_pit(X,Y), \+ visited(X,Y), \+ agent(X,Y).
determined_cell(X,Y,'G') :- found_gold(X,Y).
determined_cell(X,Y,'v') :- visited(X,Y), \+ agent(X,Y), \+ found_gold(X,Y), \+ found_pit(X,Y), \+ found_wumpus(X,Y), \+ safe(X,Y).


update_score(Delta) :-
    retract(score(S)),
    S1 is S + Delta,
    assert(score(S1)),
    format('Current Score: ~w~n', [S1]).

init_score :-
    retractall(score(_)),
    assertz(score(0)).
