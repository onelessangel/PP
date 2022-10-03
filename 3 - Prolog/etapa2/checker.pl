
%% Decomentați linia de mai jos pentru testare mai detaliată.
%% ATENȚIE: pe vmchecker linia este comentată.
%detailed_mode_disabled :- !, fail.

tt(getBoards, [
       exp("initialState(S), getBoards(S, Y)",
           ['Y', [['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
				  ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
				  ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', '']]]),
       exp("uttt(0, S), getBoards(S, Y)",
	['Y', [['', '', '', x, '', '', '', '', ''], ['', '', '', '', '', '', '', '', x], ['', '', '', '', '', '', '', '', ''],
			   ['', 0, '', x, '', 0, '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', x, ''],
			   ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', 0, '', ''], ['', '', '', 0, '', '', '', '', '']]]),
       exp("uttt(1, S), getBoards(S, Y)",
	['Y', [['', x, '', x, '', '', '', '', ''], ['', '', '', '', '', '', '', 0, x], ['', '', '', '', '', '', '', '', ''],
			   [0, 0, 0, x, '', 0, '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', x, ''],
			   ['', '', '', x, '', '', '', '', ''], ['', '', '', x, '', '', 0, '', ''], ['', '', '', 0, '', '', '', '', '']]]),
       exp("uttt(2, S), getBoards(S, Y)",
	['Y', [['', x, '', x, '', '', '', '', ''], ['', '', '', '', '', '', '', 0, x], ['', '', '', x, '', '', '', '', ''],
			   [0, 0, 0, x, '', 0, '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', x, ''],
			   ['', '', '', x, '', '', '', '', ''], ['', '', '', x, '', '', 0, '', ''], ['', '', '', 0, '', '', '', '', '']]]),
       exp("uttt(3, S), getBoards(S, Y)",
	['Y', [['', x, '', x, '', '', '', '', ''], ['', '', '', '', '', 0, '', 0, x], ['', '', '', x, '', '', 0, '', ''],
			   [0, 0, 0, x, '', 0, '', '', ''], ['', '', '', '', '', x, '', '', ''], ['', x, x, 0, 0, 0, '', x, ''],
			   ['', '', '', x, '', x, '', '', ''], ['', '', '', x, '', '', 0, '', ''], ['', '', '', 0, '', '', '', '', '']]]),
       exp("uttt(4, S), getBoards(S, Y)",
	['Y', [['', x, '', x, x, '', '', '', ''], ['', '', '', '', '', 0, '', 0, x], ['', '', '', x, x, '', 0, '', ''],
			   [0, 0, 0, x, '', 0, '', '', ''], ['', '', '', '', 0, x, '', 0, x], ['', x, x, 0, 0, 0, '', x, ''],
			   ['', '', '', x, '', x, '', '', ''], ['', '', '', x, '', '', 0, '', ''], ['', '', 0, 0, x, '', '', '', '']]])
    ]).


tt(getBoard, [
       exp("initialState(S), getBoard(S, n, Y)",
           ['Y',  ['', '', '', '', '', '', '', '', '']]),
       exp("uttt(0, S), getBoard(S, s, Y)",
	['Y', ['', '', '', '', '', '', 0, '', '']]),
       exp("uttt(1, S), getBoard(S, w, Y)",
	['Y', [0, 0, 0, x, '', 0, '', '', '']]),
       exp("uttt(2, S), getBoard(S, nw, Y)",
	['Y', ['', x, '', x, '', '', '', '', '']]),
       exp("uttt(3, S), getBoard(S, e, Y)",
	['Y', ['', x, x, 0, 0, 0, '', x, '']]),
       exp("uttt(4, S), getBoard(S, c, Y)",
	['Y', ['', '', '', '', 0, x, '', 0, x]])

   ]).

tt(getUBoard, [
       exp("initialState(S), getUBoard(S, Y)",
           ['Y', ['', '', '', '', '', '', '', '', '']]),
       exp("uttt(0, S), getUBoard(S, Y)",
	['Y', ['', '', '', '', '', '', '', '', '']])
   ]).

tt(getPos4, [
       ech("initialState(S), positions(PS), member(UPos, PS), member(Pos, PS)",
	   ["getPos(S, UPos, Pos, Cell), Cell==''"]),
       exp("uttt(4, S), getPos(S, e,  ne, Cell)", ['Cell', x]),
       exp("uttt(4, S), getPos(S, c,  s,  Cell)", ['Cell', 0]),
       exp("uttt(4, S), getPos(S, sw, c,  Cell)", ['Cell', '']),
       exp("uttt(4, S), getPos(S, se, se, Cell)", ['Cell', ''])
   ]).

tt(getPos3, [
       2, ech("initialState(S), positions(PS), member(UPos, PS), getBoard(S, UPos, B), member(Pos, PS)",
	      ["getPos(B, Pos, Cell), Cell==''"]),
       exp("uttt(3, S), getBoard(S, e, B), getPos(B, ne, Cell)", ['Cell', x]),
       exp("uttt(3, S), getBoard(S, s, B), getPos(B, sw, Cell)", ['Cell', 0]),
       exp("uttt(3, S), getBoard(S, s, B), getPos(B, se, Cell)", ['Cell', '']),
       exp("uttt(3, S), getUBoard(S, UB), getPos(UB, e, Cell)", ['Cell', 0]),
       exp("uttt(3, S), getUBoard(S, UB), getPos(UB, w, Cell)", ['Cell', 0]),
       exp("uttt(3, S), getUBoard(S, UB), getPos(UB, n, Cell)", ['Cell', ''])
   ]).

tt(getNextPlayer, [
       exp("initialState(S), getNextPlayer(S, Y)",
           ['Y', x]),
       exp("uttt(4, S), getNextPlayer(S, Y)",
           ['Y', 0])
    ]).

tt(getNextAvailableBoards, [
       exp("initialState(S), getNextAvailableBoards(S, Y)",
           ['Y', [nw, n, ne, w, c, e, sw, s, se]]),
       exp("uttt(0, S), getNextAvailableBoards(S,Y)",
           ['Y', [sw]]),
       exp("uttt(1, S), getNextAvailableBoards(S,Y)",
           ['Y', [ne]]),
       exp("uttt(2, S), getNextAvailableBoards(S,Y)",
           ['Y', [nw, n, ne, c, e, sw, s, se]]),
       exp("uttt(3, S), getNextAvailableBoards(S,Y)",
           ['Y', [nw, n, ne, c, sw, s, se]]),
       exp("uttt(4, S), getNextAvailableBoards(S,Y)",
           ['Y', [c]])

    ]).

tt(getBoardResult, [
       ech("initialState(S), positions(PS), member(UPos, PS), getBoard(S, UPos, B)",
	   ["getBoardResult(B, Res), Res == ''"]),
       exp("getBoardResult([x,x,x,0,'',0,'',0,''], R)", ['R', x]),
       exp("getBoardResult([x,'',x,0,x,0,'',0,''], R)", ['R', '']),
       exp("getBoardResult([x,0,x,x,0,0,0,x,x], R)", ['R', r]),
       exp("getBoardResult([0,x,0,'',0,x,0,x,x], R)", ['R', 0])
	]).

tt(validMove, [
       ech("initialState(S), positions(PS), member(UPos, PS), member(Pos, PS)",
	   ["validMove(S, (Upos, Pos))"]),
       exp("uttt(4, S)", [cond("validMove(S, nw)"), cond("validMove(S, n)"), cond("validMove(S, ne)"),
			  cond("validMove(S, w)"), cond("validMove(S, sw)")]),
       exp("uttt(4, S)", [cond("\\+ validMove(S, se)"), cond("\\+ validMove(S, s)"),
			  cond("\\+ validMove(S, c)"), cond("\\+ validMove(S, e)")]),
       ech("uttt(3, S), positions(PS), member(Pos, PS)", ["\\+ validMove(S, Pos)"]),
       ech("uttt(3, S), member(UPos, [nw, n, ne, c, sw, s, se])", ["validMove(S, (UPos, c))"]),
       ech("uttt(3, S), member(UPos, [nw, ne, c, sw, s, se])", ["validMove(S, (UPos, se))"]),
       ech("uttt(3, S), member(UPos, [e,w]), positions(PS), member(Pos, PS)",
	   ["\\+ validMove(S, (UPos, Pos))"]),
       0.5, ech("uttt(5, S), positions(PS), member(UPos, PS), member(Pos, PS)",
	   ["\\+ validMove(S, (UPos, Pos))"]),
       0.5, ech("uttt(5, S), positions(PS), member(Pos, PS)",
	   ["\\+ validMove(S, Pos)"])
	]).

tt(makeMove, [
       exp("initialState(S), makeMove(S, (nw, w), Y), getBoards(Y, Boards), getUBoard(Y, UBoard)",
           ['Boards', [['', '', '', x, '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
			 ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
			 ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
			 ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
			 ['', '', '', '', '', '', '', '', '']],
            'UBoard', ['', '', '', '', '', '', '', '', '']]),
       exp("initialState(S), makeMove(S, (nw, w), Y), getNextPlayer(Y, Player), getNextAvailableBoards(Y, NB)",
           ['Player', 0, set('NB', [w])]),

       exp("uttt(1, S), makeMove(S, w, Y), getBoards(Y, Boards), getUBoard(Y, UBoard)",
           ['Boards', [['', x, '', x, '', '', '', '', ''], ['', '', '', '', '', '', '', 0, x],
			 ['', '', '', x, '', '', '', '', ''], [0, 0, 0, x, '', 0, '', '', ''],
		 ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', x, ''],
		 ['', '', '', x, '', '', '', '', ''], ['', '', '', x, '', '', 0, '', ''],
		 ['', '', '', 0, '', '', '', '', '']],
            'UBoard', ['', '', '', 0, '', '', '', '', '']]),
       exp("uttt(1, S), makeMove(S, w, Y), getNextPlayer(Y, Player), getNextAvailableBoards(Y, NB)",
           ['Player', 0, set('NB', [nw,n,ne,c,e,sw,s,se])]),

       exp("uttt(2, S), makeMove(S, (e, c), Y), getBoards(Y, Boards), getUBoard(Y, UBoard)",
           ['Boards', [['', x, '', x, '', '', '', '', ''], ['', '', '', '', '', '', '', 0, x],
			 ['', '', '', x, '', '', '', '', ''], [0, 0, 0, x, '', 0, '', '', ''],
			 ['', '', '', '', '', '', '', '', ''], ['', '', '', '', 0, '', '', x, ''],
			 ['', '', '', x, '', '', '', '', ''], ['', '', '', x, '', '', 0, '', ''],
			 ['', '', '', 0, '', '', '', '', '']],
            'UBoard', ['', '', '', 0, '', '', '', '', '']]),
       exp("uttt(2, S), makeMove(S, (e, c), Y), getNextPlayer(Y, Player), getNextAvailableBoards(Y, NB)",
           ['Player', x, set('NB', [c])]),

       exp("initialState(S), moves1(Moves), apply_moves(S, Moves, Y), getUBoard(Y, Q)",
	['Q', ['', '', '', 0, 0, 0, '', '', '']]),
       exp("initialState(S), moves1(Moves), apply_moves(S, Moves, Y), getUBoard(Y, UB), getBoardResult(UB, P)",
        ['P', 0]),

       0.5, exp("initialState(S), moves2(Moves), apply_moves(S, Moves, Y), getUBoard(Y, UB)",
	['UB', ['', x, x, 0, x, 0, x, 0, x]]),
       0.5, exp("initialState(S), moves2(Moves), apply_moves(S, Moves, Y), getUBoard(Y, UB), getBoardResult(UB, P)",
	['UB', ['', x, x, 0, x, 0, x, 0, x], 'P', x]),

       exp("initialState(S), moves3a(Moves), apply_moves(S, Moves, Y), getBoards(Y, BY), uttt(2, U2), getBoards(U2, BU2)",
	[cond('BY == BU2')])

    ]).


tt(dummy_first, [
       exp("initialState(S), dummy_first(S, Move)",
	   ['Move', (nw, nw), cond('validMove(S, Move)')]),
       exp("initialState(S), dummy_first(S, M), makeMove(S, M, Y), dummy_first(Y, N)",
	   ['N', n, cond('validMove(Y, N)')]),
       exp("initialState(S), moves2a(Moves), apply_moves(S, Moves, Y), dummy_first(Y, Move)",
	   ['Move', (nw, n), cond('validMove(Y, Move)')]),
       exp("initialState(S), moves2a(Moves), apply_moves(S, Moves, Y), dummy_first(Y, Move), makeMove(Y, Move, Z), dummy_first(Z, N)",
	   ['N', (nw, ne), cond('validMove(Z, N)')])
	]).

tt(dummy_last, [
       exp("initialState(S), dummy_last(S, Move)",
	   ['Move', (se, se), cond('validMove(S, Move)')]),
       exp("initialState(S), dummy_last(S, M), makeMove(S, M, Y), dummy_last(Y, N)",
	   ['N', s, cond('validMove(Y, N)')]),
       exp("initialState(S), moves2a(Moves), apply_moves(S, Moves, Y), dummy_last(Y, Move)",
	   ['Move', (ne, se), cond('validMove(Y, Move)')]),
       exp("initialState(S), moves2a(Moves), apply_moves(S, Moves, Y), dummy_last(Y, Move), makeMove(Y, Move, Z), dummy_last(Z, N)",
	   ['N', (ne, s), cond('validMove(Z, N)')])
	]).


tt(movePriority, [
       exp("uttt(5, S), getBoard(S, nw, B), movePriority(x, B, ne, P)",
     ['P', 0]),
       exp("uttt(5, S), getBoard(S, nw, B), movePriority(0, B, ne, P)",
     ['P', 1]),
       exp("initialState(S), getBoard(S, e, B), movePriority(x, B, se, P)",
     ['P', 2]),
       exp("initialState(S), getBoard(S, e, B), movePriority(x, B, c, P)",
     ['P', 3]),
       exp("uttt(4, S), getBoard(S, nw, B), movePriority(x, B, sw, P)",
     ['P', 4]),
       exp("uttt(4, S), getBoard(S, s, B), movePriority(x, B, e, P)",
     ['P', 4])
  ]).


tt(bestIndividualMoves, [
       exp("initialState(S), getBoard(S, w, B), getNextPlayer(S, P), bestIndividualMoves(P, B, Moves)",
     ['Moves', [nw, ne, sw, se, c, n, w, e, s]]),
       exp("uttt(4, S), getBoard(S, nw, B), getNextPlayer(S, P), bestIndividualMoves(P, B, Moves)",
     ['Moves', [e, s, nw, ne, sw, se]]),
       exp("uttt(4, S), getBoard(S, c, B), getNextPlayer(S, P), bestIndividualMoves(P, B, Moves)",
     ['Moves', [n, ne, nw, w, sw]]),
       exp("uttt(4, S), getBoard(S, ne, B), getNextPlayer(S, P), bestIndividualMoves(P, B, Moves)",
     ['Moves', [e, s, se, nw, ne, n]]),
       exp("uttt(5, S), getBoard(S, nw, B), getNextPlayer(S, P), bestIndividualMoves(P, B, Moves)",
     ['Moves', [ne, n, c]]),
       exp("uttt(5, S), getBoard(S, ne, B), getNextPlayer(S, P), bestIndividualMoves(P, B, Moves)",
     ['Moves', [n, ne, s, se]])
  ]).

tt(narrowGreedy, [
       exp("initialState(S), narrowGreedy(S, M)", ['M', (nw, nw)]),
       exp("uttt(2,S), narrowGreedy(S, M)", ['M', (nw, c)]),
       exp("uttt(3,S), narrowGreedy(S, M)", ['M', (c, ne)]),
       exp("uttt(4,S), narrowGreedy(S, M)", ['M', n])
   ]).

tt(greedy, [
       exp("initialState(S), greedy(S, M)", ['M', (nw, nw)]),
       exp("uttt(1, S), bestMoves(S, Mvs)", ['Mvs', [nw, ne, sw, c, e, se, n, s, w]]),
       exp("uttt(2, S), greedy(S, M)", [cond("(M == (nw, c) ; M == (c, se))")]),
       exp("uttt(3, S), greedy(S, M)", ['M', (c, nw)]),
       exp("uttt(4, S), bestMoves(S, Mvs)", ['Mvs', [n, ne, nw, sw, w]])
   ]).

tt(strats, [
       wait, exp("play(narrowGreedy, narrowGreedy, _, W)", ['W', r]),
       wait, exp("play(narrowGreedy, greedy, _, W)", ['W', 0]),
       wait, exp("play(greedy, narrowGreedy, _, W)", ['W', x])
   ]).
