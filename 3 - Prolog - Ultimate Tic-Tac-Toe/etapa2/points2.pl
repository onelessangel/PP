
vmpoints(Test, Points):-
        member(Test:Points,
               [
               movePriority:20,
               bestIndividualMoves:20,
               narrowGreedy:10,
               greedy:40,
               strats:10
              ]).
