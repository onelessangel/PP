

% empty_board/1
% empty_board(-Board)
% Reprezentare a unei table goale.
empty_board(['', '', '', '', '', '', '', '', '']).

% player/1
% player(-Value)
% Simbolurile posibile cu care poate juca un jucător.
player(x).
player(0).

% nextPlayer/2
% nextPlayer(-Player, -NextPlayer)
% Întoarce simbolul următorului jucător în NextPlayer.
nextPlayer(x, 0).
nextPlayer(0, x).

% positions/1
% positions(+List)
% Lista de poziții posibile pe o tablă de joc.
positions([nw, n, ne, w, c, e, sw, s, se]).

% player_wins/2
% player_wins(-Player, +Board)
% Este adevărat dacă tabla Board este câștigată de jucătorul P (P poate fi x sau 0).
% Pentru remize și pentru table în care jocul este încă în derulare întoarce fals.
% Predicatul folosește predicatul getPos/3.
% Predicatul funcționează corect doar pentru tablele corecte (câștigate de cel mult un jucător).
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, n, ne]), getPos(Board, Pos, P)). 
player_wins(P, Board) :- player(P), forall(member(Pos, [w, c, e]),   getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [sw, s, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, w, sw]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [n, c, s]),   getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [ne, e, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, c, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [ne, c, sw]), getPos(Board, Pos, P)).



% print_cell/1
% print_cell(+Cell)
% Afișează o celulă (o poziție).
print_cell('', Sep) :- format('~w~w', ['-', Sep]).
print_cell(Cell, Sep) :- format('~w~w', [Cell, Sep]).

% print_board_line/2
% print_board_line(+SubRow, +Board)
% Afișează o linie dintr-o tablă individuală.
print_board_line(SubRow, Board) :-
    IdxStart is SubRow * 3,
    IdxEnd is (SubRow + 1) * 3 - 1,
    forall(
        between(IdxStart, IdxEnd, Idx),
        (nth0(Idx, Board, Cell),
         print_cell(Cell, " "))
    ).

% print_uttt_line/2
% print_uttt_line(+Row, +UtttBoard, +UBoard)
% Afișează un rând din tabla mare de UTTT.
print_uttt_line(Row, UtttBoard, UB) :-
    IdxStart is Row * 3,
    IdxEnd is (Row + 1) * 3 - 1,
    forall(
        between(0, 2, SubRow),
        (   forall(
                between(IdxStart, IdxEnd, Idx),
                (nth0(Idx, UtttBoard, Board),
                 write("   "),
                 (SubRow == 1 -> 
                 	(nth0(Idx, UB, R),
                 	print_cell(R, "| ")) ;
                 	write("   ")),
                 print_board_line(SubRow, Board)
                 )),
            write("\n")
        )).

% printBoards/1
% printBoards(+State)
% Afișează tabla de Ultimate Tic Tac Toe.
% Este necesar ca getBoards și getUBoard să fie deja implementate.
printBoards(State) :-
	getBoards(State, Boards),
	getUBoard(State, UB),
    forall(
        between(0, 2, Row),
        (print_uttt_line(Row, Boards, UB),
         write("\n"))
    ).

% printBoard/1
% printBoard(+Board)
% Afișează tabla de Ultimate Tic Tac Toe.
printBoard(Board) :-
	forall(
		between(0, 2, Row),
		(print_board_line(Row, Board), write("\n"))
		).