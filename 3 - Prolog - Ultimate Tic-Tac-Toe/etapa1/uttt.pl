:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% state(Boards, UboardState, Player, NextMove).

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards(state(Boards, UboardState, Player, NextMove), Boards).

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT, 
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
% getBoard(state(Boards, UboardState, Player, NextMove), UPos, Board) :- positions(PosList), nth0(Index, PosList, UPos), nth0(Index, Boards, Board, Rest).
getBoard(state(Boards, UboardState, Player, NextMove), UPos, Board) :- getBoard(Boards, UPos, Board), !.
getBoard(Boards, UPos, Board) :- positions(PosList), nth0(Index, PosList, UPos), nth0(Index, Boards, Board, Rest), !.

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.
getPos(Board, Pos, Cell) :- positions(PosList), nth0(Index, PosList, Pos), nth0(Index, Board, Cell, Rest).

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos(S, UPos, Pos, Cell) :- getBoard(S, UPos, Board), getPos(Board, Pos, Cell).

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
check_r(Board) :- \+ member('', Board),
				  \+ player_wins(x, Board),
				  \+ player_wins(0, Board).

getBoardResult(Board, Winner) :- player_wins(Winner, Board), !.
getBoardResult(Board, r) :- check_r(Board), !.
getBoardResult(Board, '').

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
status_UBoard(Boards, [S_nw, S_n, S_ne, S_w, S_c, S_e, S_sw, S_s, S_se]) :- getBoard(Boards, nw, B_nw), getBoardResult(B_nw, S_nw),
																			getBoard(Boards, n, B_n), getBoardResult(B_n, S_n),
																			getBoard(Boards, ne, B_ne), getBoardResult(B_ne, S_ne),
																			getBoard(Boards, w, B_w), getBoardResult(B_w, S_w),
																			getBoard(Boards, c, B_c), getBoardResult(B_c, S_c),
																			getBoard(Boards, e, B_e), getBoardResult(B_e, S_e),
																			getBoard(Boards, sw, B_sw), getBoardResult(B_sw, S_sw),
																			getBoard(Boards, s, B_s), getBoardResult(B_s, S_s),
																			getBoard(Boards, se, B_se), getBoardResult(B_se, S_se).

count([], X, 0).
count([X|T], X, S1) :- count(T, X, S), S1 is S + 1, !.
count([H|T], X, S) :- count(T, X, S).

sumX(Boards, S) :- findall(SumX,(positions(PosList), memb(X, PosList), getBoard(Boards, X, Board), count(Board, x, SumX)), Res), sum_list(Res, S).
sum0(Boards, S) :- findall(SumX,(positions(PosList), memb(X, PosList), getBoard(Boards, X, Board), count(Board, 0, SumX)), Res), sum_list(Res, S).

sum(X, Y, S) :- S is X + Y.

computePlayer(Boards, x) :- sumX(Boards, SX), sum0(Boards, S0), sum(SX, S0, S), S mod 2 =:= 0, !.
computePlayer(Boards, 0).

buildState(Boards, PreviousPos, state(Boards, UboardState, Player, PreviousPos)) :- status_UBoard(Boards, UboardState), computePlayer(Boards, Player).
% buildState(Boards, PreviousPos, CurrPlayer, state(Boards, UboardState, NextPlayer, PreviousPos)) :- status_UBoard(Boards, UboardState), nextPlayer(CurrPlayer, NextPlayer).


% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
% initialState(([B, B, B, B, B, B, B, B, B] nothing]) :- empty_board(B)
initialState(S) :- empty_board(B), buildState([B, B, B, B, B, B, B, B, B], nothing, S).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
getUBoard(state(Boards, UboardState, Player, NextMove), UboardState).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0).
getNextPlayer(state(Boards, UboardState, Player, NextMove), NextPlayer) :- computePlayer(Boards, NextPlayer).

% getNextPlayer(state(Boards, UboardState, Player, nothing), Player) :- !.
% getNextPlayer(state(Boards, UboardState, Player, NextMove), NextPlayer) :- nextPlayer(Player, NextPlayer).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.
is_available(Board) :- \+ player_wins(x, Board), \+ player_wins(0, Board), \+ check_r(Board).
is_available(Boards, Pos) :- getBoard(Boards, Pos, Board), !, is_available(Board).


addToAvailabilityList(Boards, Pos, InitList, List) :- getBoard(Boards, Pos, Board), is_available(Board), !, append(InitList, [Pos], List).
addToAvailabilityList(Boards, Pos, InitList, InitList).

memb(H, [H|_]).
memb(X, [_|T]) :- memb(X, T).

availabilityIndexList(UboardState, Res ):- findall(X,(between(0, 8, X), nth0(X, UboardState, '')), Res).

availabilityList(UboardState, Res) :- findall(Direction,(positions(PosList), availabilityIndexList(UboardState, Indexes), memb(X, Indexes), nth0(X, PosList, Direction)), Res).

getNextAvailableBoards(state(Boards, UboardState, Player, NextMove),  [NextMove]) :- is_available(Boards, NextMove), !.
getNextAvailableBoards(state(Boards, UboardState, Player, NextMove),  NextBoardsPoss) :- availabilityList(UboardState, NextBoardsPoss).

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
validMove(state(Boards, UboardState, Player, NextMove), Move) :- \+ memb('', UboardState), !, fail.
validMove(state(Boards, UboardState, Player, NextMove), Move) :-
	getNextAvailableBoards(state(Boards, UboardState, Player, NextMove),  NextBoardsPoss), length(NextBoardsPoss, Len),
	Len =:= 1, !, getBoard(Boards, NextMove, Board), getPos(Board, Move, Cell), Cell == '', !.
validMove(state(Boards, UboardState, Player, NextMove), (UPos, Pos)) :-
	getNextAvailableBoards(state(Boards, UboardState, Player, NextMove),  NextBoardsPoss), memb(UPos, NextBoardsPoss),
	getBoard(Boards, UPos, Board), getPos(Board, Pos, Cell), Cell == '', !.

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.

excludeBoard(Boards, Pos, Res) :- findall(Board, (positions(PosList), memb(X, PosList), X \== Pos, getBoard(Boards, X, Board)), Res).
addBoard(Boards, Board, Pos, NewBoards) :- positions(PosList), nth0(Index, PosList, Pos), nth0(Index, NewBoards, Board, Boards).

excludeCell(Board, Pos, Res) :- findall(Cell, (positions(PosList), memb(X, PosList), X \== Pos, getPos(Board, X, Cell)), Res).
addCell(Board, Cell, Pos, NewBoard) :- positions(PosList), nth0(Index, PosList, Pos), nth0(Index, NewBoard, Cell, Board).
makeMoveInBoard(Board, Player, Move, NewBoard) :- excludeCell(Board, Move, Res), addCell(Res, Player, Move, NewBoard).

makeMove(state(Boards, UboardState, Player, NextMove), (UPos, Pos), NewState) :- validMove(state(Boards, UboardState, Player, NextMove), (UPos, Pos)),
	getBoard(Boards, UPos, Board),
	excludeBoard(Boards, UPos, Res),
	makeMoveInBoard(Board, Player, Pos, NewBoard),
	addBoard(Res, NewBoard, UPos, NewBoards),
	buildState(NewBoards, Pos, NewState), !.

makeMove(state(Boards, UboardState, Player, NextMove), Move, NewState) :- validMove(state(Boards, UboardState, Player, NextMove), Move), !,
	getBoard(Boards, NextMove, Board),
	excludeBoard(Boards, NextMove, Res),
	makeMoveInBoard(Board, Player, Move, NewBoard),
	addBoard(Res, NewBoard, NextMove, NewBoards),
	buildState(NewBoards, Move, NewState), !.

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first(state(Boards, UboardState, Player, NextMove), Move) :- getNextAvailableBoards(state(Boards, UboardState, Player, NextMove),  [Pos|Rest]),
	length(Rest, Len), Len =:= 0, !, getBoard(Boards, Pos, Board), availabilityList(Board, [Move|_]), !.
dummy_first(state(Boards, UboardState, Player, NextMove), (Pos, Move)) :- getNextAvailableBoards(state(Boards, UboardState, Player, NextMove),  [Pos|Rest]),
	length(Rest, Len), Len =\= 0, !, getBoard(Boards, Pos, Board), availabilityList(Board, [Move|_]), !.


% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).
dummy_last(state(Boards, UboardState, Player, NextMove), Move) :- getNextAvailableBoards(state(Boards, UboardState, Player, NextMove),  [Pos|Rest]),
	length(Rest, Len), Len =:= 0, !, getBoard(Boards, Pos, Board), availabilityList(Board, List), last(List, Move), !.
dummy_last(state(Boards, UboardState, Player, NextMove), (Pos, Move)) :- getNextAvailableBoards(state(Boards, UboardState, Player, NextMove),  [_|Rest]), last(Rest, Pos),
	length(Rest, Len), Len =\= 0, !, getBoard(Boards, Pos, Board), availabilityList(Board, List), last(List, Move), !.
