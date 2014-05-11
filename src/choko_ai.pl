%the random module is used to insert random errors into the AIs computations (to prevent them from getting stuck in an infinite loop against eachother
%it is also used when the AI has to choose between two moves with the same worth (to prevent repetitive games)
?- use_module(library(random)).

%----------------------------------------------------------------SECTION: AI INTERFACE---------------------------------------------------------------
%this section contains predicates to handle an AI player's moves.

%in this case, the AI is forced to pass his turn due to having no moves available and no pieces to drop
ai_turn(Player,Board,DropInitiative,Player,Board,DropInitiative):-
	player(Turn,0,_,_,_) = Player,
	\+ player_can_move(Board, Turn),
	print_board(Board),nl,write('AI has no moves available!'),nl,
	wait_for_input,
	!.
%in this case, the AI is forced to move a piece due to having none left to drop (if there were also none on the board the game would be over already)
ai_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative):-
	player(Turn,0,_,_,_) = Player,
	nl,write('AI has no pieces left to drop'),nl,
	move_piece_AI(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative),
	!.
%in this case, the AI is forced to drop a piece either due to lack of pieces on the board, opponent having initiative or all pieces being blocked
ai_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative):-
	player(Turn,_,_,_,_) = Player,
	(get_opponent_piece(Turn,DropInitiative), nl, write('Opponent has the drop initiative'), nl; 
	\+ has_piece(Board,Turn), nl, write('AI has no pieces left on the board'), nl; 
	\+ player_can_move(Board, Turn),nl,write('All of the AI´s pieces are blocked!'),nl),
	drop_piece_AI(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative),
	!.
%in this case, the AI can drop a piece or perform a move
ai_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative):-
	player(Turn,_,_,_,_) = Player,
	perform_move_AI(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative),
	!.
	
%the AI calculates all possible drops and chooses the best option (or not deppending on difficulty and 'luck')
drop_piece_AI(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative):-
	player(Turn,NumPieces,_,Pred,Difficulty) = Player,
	choose_drop_AI(Board,Turn,ChosenC,ChosenL,Difficulty,DropInitiative),
	letter_to_num(Col, ChosenC),
	write('AI is dropping a piece at position '), write(Col), write(ChosenL), nl,
	wait_for_input,
	drop_piece(Board,ChosenC,ChosenL,Turn,ResultingBoard),
	!,
	NewNumPieces is NumPieces-1,
	ResultingPlayer = player(Turn,NewNumPieces,comp,Pred,Difficulty),
	(DropInitiative = e, ResultingDropInitiative = Turn;
	ResultingDropInitiative = DropInitiative).
	
%the AI calculates all possible piece movements and chooses the best option (or not deppending on difficulty and 'luck')	
move_piece_AI(Player,Board,Turn,ResultingBoard,Player, DropInitiative, ResultingDropInitiative):-
	player(Turn,_,_,_,Difficulty) = Player,
	get_opponent_piece(Turn,OppPiece),
	choose_movement_AI(Board,Turn,IniC,IniL,DestC,DestL,RemC,RemL,Difficulty,DropInitiative),
	letter_to_num(ColI, IniC),letter_to_num(ColD, DestC),
	write('AI is moving a piece from '), write(ColI),write(IniL),write(' to '), write(ColD),write(DestL),
	(letter_to_num(ColR, RemC), write(' and removing opponent´s piece from '), write(ColR), write(RemL), nl;
	nl),
	wait_for_input,
	move_piece(Board,IniC,IniL,DestC,DestL,Turn,TempBoard,ai_remove(TempBoard,OppPiece,RemC,RemL,TempBoard2)),
	!,
	(var(TempBoard2),ResultingBoard = TempBoard;
	ResultingBoard = TempBoard2),
	(DropInitiative = Turn, ResultingDropInitiative = e;
	ResultingDropInitiative = DropInitiative).
	
%the AI calculates all drops and possible piece movements and chooses the best option (or not deppending on difficulty and 'luck')	
perform_move_AI(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative):-
	player(Turn,NumPieces,_,Pred,Difficulty) = Player,
	get_opponent_piece(Turn,OppPiece),
	choose_move_AI(Board,Turn,ChosenMove,Difficulty,DropInitiative),
	(ChosenMove = IniC/IniL-DestC/DestL-RemC/RemL, 
		%movement action occurred
		letter_to_num(ColI, IniC),letter_to_num(ColD, DestC),
		write('AI is moving a piece from '), write(ColI),write(IniL),write(' to '), write(ColD),write(DestL),
		(letter_to_num(ColR, RemC), write(' and removing opponent´s piece from '), write(ColR), write(RemL), nl;
		nl),
		wait_for_input,
		move_piece(Board,IniC,IniL,DestC,DestL,Turn,TempBoard,ai_remove(TempBoard,OppPiece,RemC,RemL,TempBoard2)),
		!,
		ResultingPlayer = Player,
		(var(TempBoard2),ResultingBoard = TempBoard;
		ResultingBoard = TempBoard2),
		(DropInitiative = Turn, ResultingDropInitiative = e;
		ResultingDropInitiative = DropInitiative);
	ChosenMove = DropC/DropL,
		%drop action occurred
		letter_to_num(Col, DropC),
		write('AI is dropping a piece at position '), write(Col), write(DropL), nl,
		wait_for_input,
		drop_piece(Board,DropC,DropL,Turn,ResultingBoard),
		!,
		NewNumPieces is NumPieces-1,
		ResultingPlayer = player(Turn,NewNumPieces,comp,Pred,Difficulty),
		(DropInitiative = e, ResultingDropInitiative = Turn;
		ResultingDropInitiative = DropInitiative)
	).
	
	
%piece removal predicate for the AI. It simply removes the piece from the Board
	%no piece is to be removed
ai_remove(Board,_,0,0,Board):-!.
	%piece at RemC/RemL will be removed
ai_remove(Board,OppPiece,RemC,RemL,ResultingBoard):-
	rp(Board,RemC,RemL,OppPiece,ResultingBoard).

%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------


%------------------------------------------------------------SECTION: AI AUXILIARY PREDICATES--------------------------------------------------------
%this section contains auxiliary predicates for the AI's computations

%initial values of each board position for when the AI is calculating the board's total value
board_position_values([
	[5,2,2,2,5],
	[2,0,0,0,2],
	[2,0,0,0,2],
	[2,0,0,0,2],
	[5,2,2,2,5]]).
	

%AI generates all possible drops for the board and calls evaluate_and_choose to choose the appropriate board
%the appropriate board is usually the best one but random 'miscalculations' are generated with frequency deppending on the AI's difficulty
choose_drop_AI(Board,Turn,ChosenC,ChosenL,Difficulty,DropInitiative):-
	get_drop_moves(Board,DropMoves),
	(Difficulty = easy, RandomLimit is 6;
	Difficulty = medium, RandomLimit is 51;
	Difficulty = hard, RandomLimit is 201),
	evaluate_and_choose(Board,Turn,DropMoves,[],RandomLimit,ChosenMove,DropInitiative),
	ChosenMove = ChosenC/ChosenL.

%AI generates all possible movements (and piece removals after capture movements) for the board and calls evaluate_and_choose to choose the appropriate board
%the appropriate board is usually the best one but random 'miscalculations' are generated with frequency deppending on the AI's difficulty	
choose_movement_AI(Board,Turn,IniC,IniL,DestC,DestL,RemC,RemL,Difficulty,DropInitiative):-
	get_movements(Board,Turn,Movements),
	(Difficulty = easy, RandomLimit is 6;
	Difficulty = medium, RandomLimit is 51;
	Difficulty = hard, RandomLimit is 201),
	evaluate_and_choose(Board,Turn,[],Movements,RandomLimit,ChosenMove,DropInitiative),
	ChosenMove = IniC/IniL-DestC/DestL-RemC/RemL.

%AI generates all possible drops and movements for the board and calls evaluate_and_choose to choose the appropriate board
%the appropriate board is usually the best one but random 'miscalculations' are generated with frequency deppending on the AI's difficulty
choose_move_AI(Board,Turn,ChosenMove,Difficulty,DropInitiative):-
	get_drop_moves(Board,DropMoves),
	get_movements(Board,Turn,Movements),
	(Difficulty = easy, RandomLimit is 6;
	Difficulty = medium, RandomLimit is 51;
	Difficulty = hard, RandomLimit is 201),
	evaluate_and_choose(Board,Turn,DropMoves,Movements,RandomLimit,ChosenMove,DropInitiative).
	
%selects the first drop move as the best one (or first movement if there are no drops) and uses an auxiliary predicates to check all the remaining moves
	%and compare them to the one currently selected as the best move
evaluate_and_choose(Board,Turn,DropMoves,Movements,RandomLimit,ChosenMove,DropInitiative):-
	get_opponent_piece(Turn,OppPiece),
	board_position_values(PosValues),
	%drop list is not empty, first drop is chosen as best move
	(DropMoves = [DropC/DropL|RemainingDrops], drop_piece(Board,DropC,DropL,Turn,CurrentBestBoard),RemainingMovements = Movements,
		CurrentBestMove = DropC/DropL,
		(get_opponent_piece(Turn,DropInitiative), NewDropInitiative = DropInitiative;
		NewDropInitiative = Turn),
		get_piece(PosValues,DropC,DropL,PosValue), %value of having a piece in that position
		EatValue is 0;
	%drop list is empty, first movement is chosen as best move
	Movements = [IniC/IniL-DestC/DestL-RemC/RemL | RemainingMovements],
		move_piece(Board, IniC, IniL, DestC, DestL, Turn, TempBoard, ai_remove(TempBoard,OppPiece,RemC,RemL,TempBoard2)),
		(var(TempBoard2),CurrentBestBoard = TempBoard, EatValue is 0;
		CurrentBestBoard = TempBoard2,EatValue is 20), %capture occurred, move value is increased
		CurrentBestMove = IniC/IniL-DestC/DestL-RemC/RemL,
		RemainingDrops = DropMoves,
		(DropInitiative = Turn, NewDropInitiative = e;
		NewDropInitiative = DropInitiative),
		PosValue is 0),
	evaluate_board(CurrentBestBoard,Turn,EValue,NewDropInitiative),
	CurrentValue is EValue+PosValue,
	%remaining moves are evaluated
	eac_aux(Board,Turn,RemainingDrops,RemainingMovements,RandomLimit,CurrentBestMove,CurrentValue,ChosenMove,DropInitiative).
	
%base case for the evaluation auxiliary predicate, both lists of moves are empty, final result is the current best move
eac_aux(_,_,[],[],_,CurrentBestMove,_,CurrentBestMove,_):-!.
%drop moves are being evaluated and compared to the current best move
eac_aux(Board,Turn,[DropC/DropL | RemainingDrops],RemainingMovements,RandomLimit,CurrentBestMove,CurrentValue,ChosenMove,DropInitiative):-
	drop_piece(Board,DropC,DropL,Turn,ResultingBoard),
	board_position_values(PosValues),
	get_piece(PosValues,DropC,DropL,PosValue),
	(get_opponent_piece(Turn,DropInitiative), NewDropInitiative = DropInitiative;
	NewDropInitiative = Turn),
	evaluate_board(ResultingBoard,Turn,EValue,NewDropInitiative),
	RBValue is EValue+PosValue,
	random(1,RandomLimit,DoSomethingStupid), %randomly inserts errors into the AI computation deppending on the 'RandomLimit' (calculated from the AI difficulty)
	(DoSomethingStupid =\= 1, %in this case, the AI is choosing wisely
		(RBValue > CurrentValue, NewBestMove = DropC/DropL, NewValue is RBValue;
			%if the move has the same value as the current best move, the AI randomly chooses between the two (to prevent repetitive games)
			RBValue =:= CurrentValue, random(1,3,I),(I =:=1, NewBestMove = DropC/DropL, NewValue is RBValue;
												NewBestMove = CurrentBestMove, NewValue is CurrentValue);
			NewBestMove = CurrentBestMove, NewValue is CurrentValue);
		%AI makes a 'mistake'. swaps moves but not values to prevent further iterations from 'correcting' the mistake
		(RBValue >= CurrentValue, NewBestMove = CurrentBestMove, NewValue is RBValue;
			NewBestMove = DropC/DropL, NewValue is CurrentValue)
	),
	!,
	%remaining moves are evaluated
	eac_aux(Board,Turn,RemainingDrops,RemainingMovements,RandomLimit,NewBestMove,NewValue,ChosenMove,DropInitiative).
%movements (and possible piece removals) are being evaluated and compared to the current best move (no drops left to evaluate)
%functions similarly to the evaluation of drop moves
eac_aux(Board,Turn,[],[IniC/IniL-DestC/DestL-RemC/RemL | RemainingMovements],RandomLimit,CurrentBestMove,CurrentValue,ChosenMove,DropInitiative):-
	get_opponent_piece(Turn,OppPiece),
	move_piece(Board, IniC, IniL, DestC, DestL, Turn, TempBoard, ai_remove(TempBoard,OppPiece,RemC,RemL,TempBoard2)),
	(var(TempBoard2),ResultingBoard = TempBoard,AcrescValue is 0; %checks if TempBoard2 (after removal) was initialized or not (whether a capture move or standard move occurred)
	ResultingBoard = TempBoard2,AcrescValue is 20), %assigns the appropriate current board
	
	(DropInitiative = Turn, NewDropInitiative = e;
	NewDropInitiative = DropInitiative),
	evaluate_board(ResultingBoard,Turn,EValue,NewDropInitiative),
	RBValue is EValue + AcrescValue,
	random(1,RandomLimit,DoSomethingStupid),
	(DoSomethingStupid =\= 1,
		(RBValue > CurrentValue, NewBestMove = IniC/IniL-DestC/DestL-RemC/RemL , NewValue is RBValue;
			RBValue =:= CurrentValue, random(1,3,I), (I =:=1, NewBestMove = IniC/IniL-DestC/DestL-RemC/RemL , NewValue is RBValue;
												NewBestMove = CurrentBestMove, NewValue is CurrentValue);
			NewBestMove = CurrentBestMove, NewValue is CurrentValue);
			%swap moves but not values to prevent further iterations from 'correcting' the mistake
		(RBValue >= CurrentValue, NewBestMove = CurrentBestMove, NewValue is RBValue;
			NewBestMove = IniC/IniL-DestC/DestL-RemC/RemL, NewValue is CurrentValue)
	),
	!,
	eac_aux(Board,Turn,[],RemainingMovements,RandomLimit,NewBestMove,NewValue,ChosenMove,DropInitiative).
	
%calculates all possible movements for a given player on the specified board
get_movements(Board,Turn,Movements):-
	gm_aux(Board,Turn,1,5,Movements,Board).
	
%auxiliary predicate for calculating all possible movements for a player
	%checks the board for every piece belonging to that player and calculates all possible moves for that piece
gm_aux([],_,_,_,[],_):-!.
gm_aux([[]|Rb],Turn,_,PosL,Movements,OB):-
	NewC is 1,
	NewL is PosL-1,
	gm_aux(Rb,Turn,NewC,NewL,Movements,OB).
gm_aux([[Turn|Rl]|Rb],Turn,PosC,PosL,Movements,OriginalBoard):-
	!,
	get_piece_movements(OriginalBoard,Turn,PosC,PosL,PieceMovements),
	NewC is PosC+1,
	gm_aux([Rl|Rb],Turn,NewC,PosL,RestOfMovements,OriginalBoard),
	append(PieceMovements,RestOfMovements,Movements).
gm_aux([[_|Rl]|Rb],Turn,PosC,PosL,Movements,OriginalBoard):-
	NewC is PosC+1,
	gm_aux([Rl|Rb],Turn,NewC,PosL,Movements,OriginalBoard).
	
%calculates all possible movements for a piece of the board at the specified position
get_piece_movements(Board,Piece,PosC,PosL,PieceMovements):-
	get_opponent_piece(Piece,OppPiece),
	%checks for possible normal movements
	(DestC is PosC+1, DestL is PosL, move_piece(Board, PosC, PosL, DestC, DestL, Piece, _, fake_remove), NormalMoveRight=[PosC/PosL-DestC/DestL-0/0]; NormalMoveRight=[]),
	(DestC2 is PosC-1, DestL2 is PosL, move_piece(Board, PosC, PosL, DestC2, DestL2, Piece, _, fake_remove), NormalMoveLeft=[PosC/PosL-DestC2/DestL2-0/0]; NormalMoveLeft=[]),
	(DestL3 is PosL+1, DestC3 is PosC, move_piece(Board, PosC, PosL, DestC3, DestL3, Piece, _, fake_remove), NormalMoveDown=[PosC/PosL-DestC3/DestL3-0/0]; NormalMoveDown=[]),
	(DestL4 is PosL-1, DestC4 is PosC, move_piece(Board, PosC, PosL, DestC4, DestL4, Piece, _, fake_remove), NormalMoveUp=[PosC/PosL-DestC4/DestL4-0/0]; NormalMoveUp=[]),
	%checks for possible capture moves (uses piece removal predicate list_removes which stores all possible moves combined with all possible removes in a list)
	(DestC5 is PosC+2, DestL5 is PosL, move_piece(Board, PosC, PosL, DestC5, DestL5, Piece, RB, list_removes(RB,PosC,PosL,DestC5,DestL5,OppPiece,CaptureMoveRight)); CaptureMoveRight=[]),
	(DestC6 is PosC-2, DestL6 is PosL, move_piece(Board, PosC, PosL, DestC6, DestL6, Piece, RB2,list_removes(RB2,PosC,PosL,DestC6,DestL6,OppPiece,CaptureMoveLeft)); CaptureMoveLeft=[]),
	(DestL7 is PosL+2, DestC7 is PosC, move_piece(Board, PosC, PosL, DestC7, DestL7, Piece, RB3,list_removes(RB3,PosC,PosL,DestC7,DestL7,OppPiece,CaptureMoveDown)); CaptureMoveDown=[]),
	(DestL8 is PosL-2, DestC8 is PosC, move_piece(Board, PosC, PosL, DestC8, DestL8, Piece, RB4,list_removes(RB4,PosC,PosL,DestC8,DestL8,OppPiece,CaptureMoveUp)); CaptureMoveUp=[]),
	%combines all the produced results
	append(NormalMoveRight,NormalMoveLeft,Temp1),
	append(NormalMoveDown,NormalMoveUp,Temp2),
	append(CaptureMoveUp,CaptureMoveDown,Temp3),
	append(CaptureMoveRight,CaptureMoveLeft,Temp4),
	append(Temp1,Temp2,TP),
	append(Temp3,Temp4,TP2),
	append(TP,TP2,PieceMovements),!.

%when called, a capture move has occurred for a piece moving from PosC/PosL to DestC/DestL
%it checks all possible piece removals and creates a list with elements containing all of the moves' information
	%in this case there are no pieces to be removed
list_removes(Board,PosC,PosL,DestC,DestL,OppPiece,[PosC/PosL-DestC/DestL-0/0]):-
	\+ has_piece(Board,OppPiece),!.
	%in this case an auxiliary predicate is used to check the board for all of the opponent's pieces
list_removes(Board,PosC,PosL,DestC,DestL,OppPiece,List):-
	lr_aux(Board,PosC,PosL,DestC,DestL,1,5,OppPiece,List).

%goes through the entire board, listing positions where OppPieces are in the format PosC/PosL-DestC/DestL-CurC/CurL where 
	%CurC/CurL is the current position of the board being checked and PosC/PosL-DestC/DestL are provided before-hand (capture move format)
lr_aux([],_,_,_,_,_,_,_,[]):- !.
lr_aux([[]|Rb],PosC,PosL,DestC,DestL,_,CurL,OppPiece,RList):-
	NewC is 1,
	NewL is CurL-1,
	lr_aux(Rb,PosC,PosL,DestC,DestL,NewC,NewL,OppPiece,RList).
lr_aux([[OppPiece|Rl]|Rb],PosC,PosL,DestC,DestL,CurC,CurL,OppPiece,[PosC/PosL-DestC/DestL-CurC/CurL | RList]):-
	!,
	NewC is CurC+1,
	lr_aux([Rl|Rb],PosC,PosL,DestC,DestL,NewC,CurL,OppPiece,RList).
lr_aux([[_|Rl]|Rb],PosC,PosL,DestC,DestL,CurC,CurL,OppPiece,RList):-
	NewC is CurC+1,
	lr_aux([Rl|Rb],PosC,PosL,DestC,DestL,NewC,CurL,OppPiece,RList).
	
%gets all possible drop moves on the board (does not deppend on piece type)
get_drop_moves(Board,Moves):-
	gdm_aux(Board,1,5,Moves).

%goes through the board saving the positions of empty cells in a list in the format PosC/PosL
gdm_aux([],_,_,[]):-!.
gdm_aux([[]|Rb],_,PosL,Moves):-
	NewC is 1,
	NewL is PosL-1,
	gdm_aux(Rb,NewC,NewL,Moves).
gdm_aux([[e|Rl]|Rb],PosC,PosL,[PosC/PosL | RMoves]):-
	NewC is PosC+1,
	gdm_aux([Rl|Rb],NewC,PosL,RMoves).
gdm_aux([[_|Rl]|Rb],PosC,PosL,Moves):-
	NewC is PosC+1,
	gdm_aux([Rl|Rb],NewC,PosL,Moves).
	
%receives a board and a piece type and calculates the board's 'worth' for that piece type
evaluate_board(Board,Turn,Value,DropInitiative):-
	IniValue is 0,
	ev(Board,Turn,IniValue,1,5,Board,Value,DropInitiative).
	
%this predicates checks the whole board, decreasing the final value by 5 whenever an opponent's piece is found or increasing by 5 when a piece of type 'Turn' is found
%when a piece of type turn is found it calculates the piece's value (using calculate_piece_value/5)
%the piece's value depends on what pieces it can eat or by which pieces it can be eaten (danger and offensive values)
ev([],_,FValue,_,_,_,FValue,_):- !.
ev([[]|Rb],Turn,Value,_,PosL,OriginalBoard,FValue,DropInitiative):-
	NewC is 1,
	NewL is PosL-1,
	ev(Rb,Turn,Value,NewC,NewL,OriginalBoard,FValue,DropInitiative).
ev([[Turn|Rl]|Rb],Turn,Value,PosC,PosL,OriginalBoard,FValue,DropInitiative):-
	!,
	NewValue is Value+5,
	calculate_piece_value(OriginalBoard,Turn,PosC,PosL,PValue,DropInitiative),
	NewValue2 is NewValue+PValue,
	NewC is PosC+1,
	ev([Rl|Rb],Turn,NewValue2,NewC,PosL,OriginalBoard,FValue,DropInitiative).
ev([[Piece|Rl]|Rb],Turn,Value,PosC,PosL,OriginalBoard,FValue,DropInitiative):-
	NewC is PosC+1,
	(get_opponent_piece(Piece,Turn), NewValue is Value - 5;
	Piece = e, NewValue is Value),
	ev([Rl|Rb],Turn,NewValue,NewC,PosL,OriginalBoard,FValue,DropInitiative).
	
%this predicate calculates the value of a given piece of PieceType on the specified position of the board
%this value consists of danger value plus offensive value (danger value might be negative)
calculate_piece_value(Board,PieceType,PosC,PosL,PValue,DropInitiative):-
	calculate_danger_value(Board,PieceType,PosC,PosL,DValue,DropInitiative),
	calculate_offensive_value(Board,PieceType,PosC,PosL,OValue,DropInitiative),
	PValue is DValue+OValue.
	
%this predicate calculates a danger value for a piece on the board. The value is -18*(number of pieces that can eat the specified piece) if opponent can
%capture on the next turn (depends on drop initiative), -2*(number of pieces that can eat the specified piece) otherwise.
%(since after this turn, the opponent will have his turn, being in danger of losing the piece nullifies any 'eat moves' that piece can do
%	since any eat moves are worth only 6 points and if the piece can be eaten it can only eat 3 pieces).
calculate_danger_value(Board,PieceType,PosC,PosL,Value,DropInitiative):-
	(DropInitiative = PieceType, Penalizer is -2; 
	Penalizer is -18),
	CLeft is PosC-1, CRight is PosC+1, LUp is PosL+1, LDown is PosL-1,
	get_opponent_piece(PieceType,Piece),
	(capture_move(Board, PosC, LUp, PosC, LDown, Piece, _,fake_remove), UpValue is Penalizer;UpValue is 0),
	(capture_move(Board, PosC, LDown, PosC, LUp, Piece, _,fake_remove), DownValue is Penalizer;DownValue is 0),
	(capture_move(Board, CLeft, PosL, CRight, PosL, Piece, _,fake_remove),LeftValue is Penalizer;LeftValue is 0),
	(capture_move(Board, CRight, PosL, CLeft, PosL, Piece, _,fake_remove), RightValue is Penalizer;RightValue is 0),
	Value is UpValue+DownValue+LeftValue+RightValue.

%this predicate calculates an offensive value for a piece on the board. The value is 6*(number of pieces that the specified piece can eat) if the opponent
% does not have the drop initiative, 2*(number of pieces that the specified piece can eat) otherwise.
calculate_offensive_value(Board,PieceType,PosC,PosL,Value,DropInitiative):-
	(get_opponent_piece(PieceType,DropInitiative), Valorizer is 2;
	Valorizer is 6),
	CLeft is PosC-2, CRight is PosC+2, LUp is PosL+2, LDown is PosL-2,
	(capture_move(Board, PosC, PosL, PosC, LUp, PieceType, _,fake_remove), UpValue is Valorizer; UpValue is 0),
	(capture_move(Board, PosC, PosL, PosC, LDown, PieceType, _,fake_remove), DownValue is Valorizer; DownValue is 0),
	(capture_move(Board, PosC, PosL, CLeft, PosL, PieceType, _,fake_remove), LeftValue is Valorizer; LeftValue is 0),
	(capture_move(Board, PosC, PosL, CRight, PosL, PieceType, _,fake_remove), RightValue is Valorizer; RightValue is 0),
	Value is UpValue+DownValue+LeftValue+RightValue.

%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------
