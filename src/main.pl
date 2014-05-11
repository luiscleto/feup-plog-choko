%this file contains predicates used both for printing the board and converting board elements into the symbols used to represent each of the elements
%it also contains predicates to print other aspects of the game's status such as current turn, player info, etc.
?- ensure_loaded('status_printing.pl').
%this file contains predicates used to interact with the user and human players
?- ensure_loaded('user_interface.pl').
%this file contains predicates used to generate the AI's decisions
?- ensure_loaded('choko_ai.pl').


%--------------------------------------------------SECTION: PREDICATES TO INITIALIZE GAME VARIABLES--------------------------------------------------
%this sections contains several predicates used to initialize game variables such as the board and players

%used to create a board with only empty cells (initial situation)
initial_board([
	[e,e,e,e,e],
	[e,e,e,e,e],
	[e,e,e,e,e],
	[e,e,e,e,e],
	[e,e,e,e,e]]).
	
%initializes a human player of type player(Piece,NumPiecesInHand,TypeOfPlayer,TurnPredicate,Difficulty) with pieces of type Piece, 12 pieces in hand and the specified type (human, computer)
init_player(Type,Piece,P):-
	Type = human,
	P = player(Piece,12,Type,human_turn,_).
init_player(Type,Piece,Difficulty,P):-
	Type = comp,
	P = player(Piece,12,Type,ai_turn,Difficulty).
	
%initializes the players for a game of human vs human.
init_humans(P1, P2):-
	init_player(human,w,P1),
	init_player(human,b,P2).
	
%initializes an AI player
init_ai_player(Difficulty,Piece,P):-
	init_player(comp,Piece,Difficulty,P).
%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------

	
%---------------------------------------------------------------SECTION: GAME INTERFACE--------------------------------------------------------------
%this section contains several predicates used as the game interface including the 'main' predicate play/0.

%initializes the board, calls the initial menu to set up the players (human or AI (and AI's difficulty) and starts the game using play/7
%when the game is over it also declares the winner
play:-
	initial_board(B),
	menu(P1,P2),
	!,
	%init_humans(P1,P2),
	player(DropInitiative,_,_,_,_) = P1,
	play(P1,P2,B,DropInitiative,DropInitiative,0,Winner),
	player(WinPiece,_,_,_,_) = Winner,
	piece_to_player(WinPiece,Win),
	write(Win),write(' is Victorious!'),nl.

%checks condition for game over
	%P1 has no pieces in hand or on the board, P2 wins
play(player(Piece,0,_,_,_),P2,Board,_,_,_,P2):-
	\+ has_piece(Board,Piece),!,
	print_board(Board).
	%P2 has no pieces in hand or on the board, P1 wins
play(P1,player(Piece,0,_,_,_),Board,_,_,_,P1):-
	\+ has_piece(Board,Piece),!,
	print_board(Board).
%the last piece (24th piece) has just been dropped, P2 has his turn
play(P1,P2,Board,_,DropInitiative,24,Winner):-
	NewInitiative = e,
	player(Turn,_,_,TurnPred,_) = P2, Pred =.. [TurnPred,P2,Board,NewInitiative,NewP2,NewBoard,NewInitiative], NewP1 = P1,
	write('»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»'),nl,
	write('»All pieces have been dropped, player 2 has his turn.»'),nl,
	write('»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»'),nl,
	print_board(Board),
	show_player_status(P2,DropInitiative),
	Pred,
	NewNumPiecesDropped is 25, %prevents this predicate from ever happening again
	get_opponent_piece(Turn,NextTurn),
	play(NewP1,NewP2,NewBoard,NextTurn,NewInitiative,NewNumPiecesDropped,Winner),
	!.
%normal turn occurring, active player is chosen and the turn predicate is created
%player information is printed on the screen along with the board and the turn predicate is executed
%another play predicate is called at the end with the resulting board, initiative, players, active turn and number of pieces dropped
play(P1,P2,Board,Turn,DropInitiative,NumPiecesDropped,Winner):-
	(player(Turn,_,_,TurnPred,_) = P1, Pred =.. [TurnPred,P1,Board,DropInitiative,NewP1,NewBoard,NewInitiative], NewP2 = P2, Player = P1;
	player(Turn,_,_,TurnPred,_) = P2, Pred =.. [TurnPred,P2,Board,DropInitiative,NewP2,NewBoard,NewInitiative], NewP1 = P1, Player = P2), !,
	piece_to_player(Turn,PText),
	write('»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»'),nl,
	write('»                   '),write(PText),write('´s turn                  »'),nl,
	write('»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»'),nl,
	print_board(Board),
	show_player_status(Player,DropInitiative),
	Pred,
	%----calculating total number of pieces dropped----
	player(_,IniPieces1,_,_,_) = P1, player(_,IniPieces2,_,_,_) = P2,
	player(_,FinalPieces1,_,_,_) = NewP1, player(_,FinalPieces2,_,_,_) = NewP2,
	PiecesDropped is IniPieces1-FinalPieces1+IniPieces2-FinalPieces2,
	NewNumPiecesDropped is NumPiecesDropped+PiecesDropped,
	%--------------------calculated--------------------
	get_opponent_piece(Turn,NextTurn),
	play(NewP1,NewP2,NewBoard,NextTurn,NewInitiative,NewNumPiecesDropped,Winner).
%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------
	
%------------------------------------------------------SECTION: CHOKO'S CORE LOGIC PREDICATES--------------------------------------------------------
%this section contains the predicates that handle the game's core logic functions

%gets the piece type of the opponent's piece
get_opponent_piece(w,b).
get_opponent_piece(b,w).
	
%drops a piece of type Piece on the Board, in the specified position (if and only if that position is vacant)
%the result is stored in ResultingBoard
%(Piece type cannot be an empty cell (e))
drop_piece(Board, PosC, PosL, Piece, ResultingBoard):-
	Piece \= e,
	dp(Board, PosC, PosL, Piece, ResultingBoard).
%searches the board for the specified position and checks that it is empty, replacing the corresponding position on the resulting board by Piece
%end case is (1,5) and not (1,1) since the board's lines are numbered from 5 to 1 (decreasing for every line checked)
dp([[e|Rs]|Bs],1,5,Piece,[[Piece|Rs]|Bs]).
dp([[R|Rs]|Bs], PosC, 5, Piece, [[R|RBs]|Bs]) :-
	PosC > 1,
	C1 is PosC-1,
	dp([Rs|Bs],C1,5,Piece,[RBs|Bs]).
dp([B|Bs], PosC, PosL, Piece, [B|RBs]):-
	PosL<5,
	L1 is PosL+1,
	dp(Bs,PosC,L1,Piece,RBs).
	
%removes a piece from the board if the piece in the specified position corresponds to the type of piece to be removed
%(Piece type cannot be an empty cell (e) )
remove_piece(Board, PosC, PosL, Piece, ResultingBoard):-
	Piece \= e,
	letter_to_num(PosC,CNumber),
	rp(Board, CNumber, PosL, Piece, ResultingBoard).
%functions int the same way as the predicate dp/5 except this one verifies that the specified position in the board corresponds to the expected Piece
%and replaces it by an empty cell on the resulting board
rp([[Piece|Rs]|Bs],1,5,Piece,[[e|Rs]|Bs]).
rp([[R|Rs]|Bs], PosC, 5, Piece, [[R|RBs]|Bs]) :-
	PosC > 1,
	C1 is PosC-1,
	rp([Rs|Bs],C1,5,Piece,[RBs|Bs]).
rp([B|Bs], PosC, PosL, Piece, [B|RBs]):-
	PosL<5,
	L1 is PosL+1,
	rp(Bs,PosC,L1,Piece,RBs).
%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------


%-----------------------------------------------------------SECTION: MOVEMENT PREDICATES-------------------------------------------------------------
%contains predicates that allow piece movements to occurr (both standard and capture movements)
%its essentially a specialized extension to the core logic section

%attempts to move a piece from (InitialC,InitialL) to (DestC,DestL) by checking if it is a valid standard move or a valid capture move
%it stores the type of move that occured in TypeOfMove
move_piece(Board, InitialC, InitialL, DestC, DestL, Piece, ResultingBoard, PieceRemovalPredicate) :-
	InitialC > 0, InitialC < 6, InitialL > 0, InitialL < 6,
	DestC > 0, DestC < 6, DestL > 0, DestL < 6,
	(standard_move(Board, InitialC, InitialL, DestC, DestL, Piece, ResultingBoard);
	capture_move(Board, InitialC, InitialL, DestC, DestL, Piece, ResultingBoard,PieceRemovalPredicate)).

%checks if a given move is a valid standard move (Piece is in the initial position, moves only one space and final position is an empty cell)
%resulting board is storing in the variable ResultingBoard
standard_move(Board,InitialC, InitialL, DestC, DestL, Piece, ResultingBoard):-
	DiffC is DestC-InitialC,
	DiffL is DestL-InitialL,
	(1 =:= abs(DiffC), 0 =:= DiffL;
	1 =:= abs(DiffL), 0 =:= DiffC),
	rp(Board,InitialC,InitialL,Piece,NB2),
	dp(NB2,DestC,DestL,Piece,ResultingBoard).

%checks if a given move is a valid capture move
%(Piece is in the initial position, moves exactly two spaces (in a straight line), final position is an empty cell and the cell inbetween contains a piece of the opponent's type)
%resulting board is storing in the variable ResultingBoard
%the PieceRemovalPredicate is called after the capture move occurs successfully
capture_move(Board, InitialC, InitialL, DestC, DestL, Piece, ResultingBoard,PieceRemovalPredicate):-
	valid_capture_move_positions(InitialC, InitialL, DestC, DestL, MedC, MedL),
	rp(Board, InitialC, InitialL, Piece, TempB),
	get_opponent_piece(Piece, OppPiece),
	rp(TempB, MedC, MedL, OppPiece, TempB2),
	dp(TempB2,DestC, DestL,Piece,ResultingBoard),
	PieceRemovalPredicate.
%checks if the given positions are valid for a capture movement (moving either 2 lines or 2 columns)
%calculates the position that the moving piece will jump over (position (MedC,MedL))
valid_capture_move_positions(InitialC, InitialL, DestC, DestL, MedC, MedL):- %case where the piece moves two lines (up or down)
	InitialC > 0, InitialC < 6, InitialL > 0, InitialL < 6,
	DestC > 0, DestC < 6, DestL > 0, DestL < 6,
	InitialC =:= DestC,
	MedC is InitialC,
	(DestL =:= InitialL+2, MedL is InitialL+1;
	DestL =:= InitialL-2, MedL is InitialL-1).
valid_capture_move_positions(InitialC, InitialL, DestC, DestL, MedC, MedL):- %case where the piece moves two columns (left or right)
	InitialC > 0, InitialC < 6, InitialL > 0, InitialL < 6,
	DestC > 0, DestC < 6, DestL > 0, DestL < 6,
	InitialL =:= DestL,
	MedL is InitialL,
	(DestC =:= InitialC+2, MedC is InitialC+1;
	DestC =:= InitialC-2, MedC is InitialC-1).
%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------
	
	
%----------------------------------------------------SECTION: BOARD HANDLING AUXILIARY FUNCTIONS-----------------------------------------------------
%this section contains predicates useful for checking the board status (which kind of pieces remain, which piece is in which cell, etc)

%retrieves a piece from the specified board in the position indicated py PosC (column number) and PosL (line number)
%can also be used to check if a given piece is in the specified board in the indicated position
get_piece(Board,PosC,PosL,Piece):-
	get_line(Board,PosL,Line),
	get_line_element(Line,PosC,Piece).
%retrieves a line from the board (remmember: lines are numbered from 5 to 1, therefore line 5 would correspond to the first line)
%uses a counter to check when the desired line has been reached (increments the counter until it reaches 5, moving on to the following line with each increment)
get_line([Line|_],5,Line):- !.
get_line([_|Bs],L,Line):-
	L < 5,
	L1 is L+1,
	get_line(Bs,L1,Line).
%retrieves an element from a line
%uses a counter to check when the desired line has been reached (decreases the counter until it reaches 1, moving on to the following element with each decrement)
get_line_element([Piece|_], 1, Piece):- !.
get_line_element([_|Ls],N,Piece):-
	N > 1,
	N1 is N-1,
	get_line_element(Ls,N1,Piece).
	
%checks if a given board still has pieces of the type piece
%it starts by checking if a line has that piece and if it does not, it moves on to the next line of the board
has_piece([B|Bs],Piece):-
	(line_has_piece(B,Piece);
	has_piece(Bs,Piece)).
%checks if a given line contains a piece of the the type Piece
%stops upon finding that type of piece or after having checked all elements of the line
line_has_piece([Piece|_],Piece):- !.
line_has_piece([_|Rs],Piece):- line_has_piece(Rs,Piece).

%checks if a player has any piece on the board which he can move
player_can_move(Board, Piece):-
	pcm_aux(Board, 1, 5, Piece, Board), !.

%iterates through the board checking for every piece of type Piece if that can move. If it can, the predicate is successfull
pcm_aux([[]|Rb],_, PosL, Piece, CompleteBoard):-
	NewL is PosL-1,
	NewC is 1,
	pcm_aux(Rb, NewC, NewL, Piece, CompleteBoard).
pcm_aux([[Piece|Ls]|Rb],PosC, PosL, Piece, CompleteBoard):-
	(piece_can_move(CompleteBoard, PosC, PosL, Piece);
	NewC is PosC+1, pcm_aux([Ls|Rb],NewC, PosL, Piece, CompleteBoard)).
pcm_aux([[_|Ls]|Rb], PosC, PosL, Piece, CompleteBoard):-
	NewC is PosC+1, pcm_aux([Ls|Rb],NewC, PosL, Piece, CompleteBoard).
	
%stub for removing pieces from the board. to be used only when you dont actually want to let the capture_move remove a piece from the board
fake_remove.
%checks if the expected piece is in the specified position of the board and if it can move
piece_can_move(Board, PosC, PosL, Piece):-
	(DestC is PosC+1, DestL is PosL; DestC is PosC-1, DestL is PosL; DestC is PosC+2, DestL is PosL; DestC is PosC-2, DestL is PosL;
	DestL is PosL+1, DestC is PosC; DestL is PosL-1, DestC is PosC; DestL is PosL+2, DestC is PosC; DestL is PosL-2, DestC is PosC),
	move_piece(Board, PosC, PosL, DestC, DestL, Piece, _, fake_remove),
	!.

%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------