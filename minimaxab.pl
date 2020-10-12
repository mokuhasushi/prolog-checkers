

swap_node_type(min, max).
swap_node_type(max, min).

best_score_and_board(max, Board1, Score1, Board2, Score2, BestBoard, BestScore) :-
	(Score1 >= Score2, !, BestBoard = Board1, BestScore = Score1) ;
	(Score1  < Score2, BestBoard = Board2, BestScore = Score2).
best_score_and_board(min, Board1, Score1, Board2, Score2, BestBoard, BestScore) :-
	(Score1 =< Score2, !, BestBoard = Board1, BestScore = Score1) ;
	(Score1  > Score2, BestBoard = Board2, BestScore = Score2).

alpha_beta_prune(max, _, Beta, Value)  :- Value > Beta.
alpha_beta_prune(min, Alpha, _, Value) :- Value < Alpha.

update_alpha_beta(max, Alpha, Beta, Value, NewAlpha, Beta) :- 
	(Value > Alpha, !, NewAlpha = Value) ;
	(NewAlpha = Alpha).
update_alpha_beta(min, Alpha, Beta, Value, Alpha, NewBeta) :- 
	(Value < Beta,  !, NewBeta = Value) ;
	(NewBeta = Beta).

find_best_move(_, [BestBoard], BestBoard, BestScore, 0, _, _) :-
	utility(BestBoard, BestScore), !.

find_best_move(NodeType, [Board1 | Tail], BestBoard, BestScore, 0, Alpha, Beta) :-
	utility(Board1, Score1),
	((alpha_beta_prune(NodeType, Alpha, Beta, Score1), !,
	BestBoard = Board1, BestScore = Score1);
	(find_best_move(NodeType, Tail, Board2, Score2, 0, Alpha, Beta),
	best_score_and_board(NodeType, Board1, Score1, Board2, Score2, BestBoard, BestScore))).

find_best_move(NodeType, [BestBoard], BestBoard, BestScore, Depth, Alpha, Beta) :-
	Depth > 0,
	NewDepth is (Depth - 1),
	swap_node_type(NodeType, NextNodeType),
	minmax(NextNodeType, BestBoard, _, BestScore, NewDepth, Alpha, Beta), !.

find_best_move(NodeType, [Board1 | Tail], BestBoard, BestScore, Depth, Alpha, Beta) :-
	Depth > 0,
	NewDepth is (Depth - 1),
	swap_node_type(NodeType, NextNodeType),
	minmax(NextNodeType, Board1, _, Score1, NewDepth, Alpha, Beta),
	((alpha_beta_prune(NodeType, Alpha, Beta, Score1), !, 
	BestBoard = Board1, BestScore = Score1) ;
	(update_alpha_beta(NodeType, Alpha, Beta, Score1, NewAlpha, NewBeta),
	find_best_move(NodeType, Tail, Board2, Score2, Depth, NewAlpha, NewBeta),
	best_score_and_board(NodeType, Board1, Score1, Board2, Score2, BestBoard, BestScore))).

find_best_movetrace(_, [BestBoard], BestBoard, BestScore, 0, _, _) :-
	utility(BestBoard, BestScore), !.

find_best_movetrace(NodeType, [Board1 | Tail], BestBoard, BestScore, 0, Alpha, Beta) :-
	utility(Board1, Score1),
	((alpha_beta_prune(NodeType, Alpha, Beta, Score1), !,
	BestBoard = Board1, BestScore = Score1);
	(find_best_move(NodeType, Tail, Board2, Score2, 0, Alpha, Beta),
	best_score_and_board(NodeType, Board1, Score1, Board2, Score2, BestBoard, BestScore))).

find_best_movetrace(NodeType, [BestBoard], BestBoard, BestScore, Depth, Alpha, Beta) :-
	Depth > 0,
	NewDepth is (Depth - 1),
	swap_node_type(NodeType, NextNodeType),
	minmaxtrace(NextNodeType, BestBoard, _, BestScore, NewDepth, Alpha, Beta), !.

find_best_movetrace(NodeType, [Board1 | Tail], BestBoard, BestScore, Depth, Alpha, Beta) :-
	Depth > 0,
	NewDepth is (Depth - 1),
	swap_node_type(NodeType, NextNodeType),
	minmaxtrace(NextNodeType, Board1, _, Score1, NewDepth, Alpha, Beta),
	((alpha_beta_prune(NodeType, Alpha, Beta, Score1), !, 
	BestBoard = Board1, BestScore = Score1) ;
	(update_alpha_beta(NodeType, Alpha, Beta, Score1, NewAlpha, NewBeta),
	find_best_movetrace(NodeType, Tail, Board2, Score2, Depth, NewAlpha, NewBeta),
	best_score_and_board(NodeType, Board1, Score1, Board2, Score2, BestBoard, BestScore))).


minmaxtrace(NodeType, Board, BestBoard, BestScore, Depth, Alpha, Beta) :-
	trovamosse(Board, PossibleMoves),
	find_best_movetrace(NodeType, PossibleMoves, BestBoard, BestScore, Depth, Alpha, Beta),forall(between(1,Depth,_), write(" * ")),write(BestScore),write("\n").

minmaxtrace(_, Board, Board, Score, Depth, _, _) :-
	utility(Board, Score),forall(between(1,Depth,_), write(" * ")),write("leafnode, "),write(Score),write("\n"),!.

minmax(NodeType, Board, BestBoard, BestScore, Depth, Alpha, Beta) :-
	trovamosse(Board, PossibleMoves),
	find_best_move(NodeType, PossibleMoves, BestBoard, BestScore, Depth, Alpha, Beta),!.

minmax(_, Board, Board, Score, _, _, _) :-
	utility(Board, Score).

