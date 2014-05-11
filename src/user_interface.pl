%---------------------------------------------------------SECTION: HUMAN INTERFACE - MENUS-----------------------------------------------------------
%this section contains the game menus used when the game is initialized	

%allows the user to choose the game type (human vs human, human vs ai or ai vs ai) and calls the apropriate menu for their choice
menu(P1,P2):-
	write('Choose game type:'),nl,nl,
	write('1- Human vs Human'),nl,
	write('2- Human vs AI'),nl,
	write('3- AI vs AI'),nl,
	(get_user_choice(1,3,Choice),
		(Choice =:= 1, init_humans(P1,P2); %no menu needed since there are no difficulties or player orders to choose
		Choice =:= 2, menu_human_vs_ai(P1,P2);
		Choice =:= 3, menu_ai_vs_ai(P1,P2));
	nl,write('Invalid choice!'),nl,nl,menu(P1,P2)).

%allows the user to choose who goes first (human or AI) and choose the AI's difficulty mode
menu_human_vs_ai(P1, P2):-
	nl,write('Choose player order:'),nl,nl,
	write('1- Human goes first'),nl,
	write('2- AI goes first'),nl,
	(get_user_choice(1,2,Choice),
		(Choice =:= 1, init_player(human,w,P1), choose_ai_difficulty(P2,b);
		Choice =:= 2, init_player(human,b,P2), choose_ai_difficulty(P1,w));
	nl,write('Invalid choice!'),nl,nl,menu_human_vs_ai(P1,P2)).

%asks the user to choose the difficulty for the AI players
menu_ai_vs_ai(P1,P2):-
	nl,write('Player 1:'),nl,
	choose_ai_difficulty(P1,w),
	nl,write('Player 2:'),nl,
	choose_ai_difficulty(P2,b).
	
%asks the user to choose a difficulty mode for the AI (easy, medium or hard) and initializes an AI player with that difficulty and the specified piece type
choose_ai_difficulty(AIplayer,PieceType):-
	write('AI difficulty level:'),nl,nl,
	write('1- easy'),nl,
	write('2- medium'),nl,
	write('3- hard'),nl,
	(get_user_choice(1,3,Choice),
		(Choice =:= 1, init_ai_player(easy,PieceType, AIplayer);
		Choice =:= 2, init_ai_player(medium,PieceType, AIplayer);
		Choice =:= 3, init_ai_player(hard,PieceType, AIplayer));
	nl,write('Invalid choice!'),nl,nl,choose_ai_difficulty(AIplayer,PieceType)).
	
%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------
	
	
%-------------------------------------------------------------SECTION: HUMAN INTERFACE---------------------------------------------------------------
%this section contains interface predicates specific to human players

%checks if a given position parameters are valid and belong to the board
valid_position_parameters(ChosenC,ChosenL):-
	integer(ChosenL),
	nonvar(ChosenC),
	letter_to_num(ChosenC,_),
	ChosenL > 0, ChosenL < 6.

%waits for user input before continuing
wait_for_input:-
	write('Enter anything to continue'),nl,
	read(_), 
	!.
%reads user input corresponding to a single cell position and validates it
read_and_validate_position(ChosenC/ChosenL):-
	read(ChosenC/ChosenL),
	valid_position_parameters(ChosenC,ChosenL).
%reads user input corresponding to two cell positions and validates them
read_and_validate_position(IniC/IniL,DestC/DestL):-
	read(IniC/IniL-DestC/DestL),
	valid_position_parameters(IniC,IniL),
	valid_position_parameters(DestC,DestL).
	
%reads and validates user input corresponding to a choice between two integers
get_user_choice(ChoiceMin,ChoiceMax, Input):-
	read(Input),
	integer(Input),
	Input >= ChoiceMin,
	Input =< ChoiceMax.
	
%in this case, the player is forced to pass his turn due to having no moves available and no pieces to drop
human_turn(Player,Board,DropInitiative,Player,Board,DropInitiative):-
	player(Turn,0,_,_,_) = Player,
	\+ player_can_move(Board, Turn),
	print_board(Board),nl,write('No moves available!'),nl,
	wait_for_input,
	!.
%in this case, the player is forced to move a piece due to having none left to drop (if there were also none on the board the game would be over already)
human_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative):-
	player(Turn,0,_,_,_) = Player,
	nl,write('No pieces left to drop'),nl,
	(move_piece_human(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative);
	print_board(Board),write('Invalid move!'),nl,
	human_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative)),
	!.
%in this case, the player is forced to drop a piece either due to lack of pieces on the board, opponent having initiative or all pieces being blocked
human_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative):-
	player(Turn,_,_,_,_) = Player,
	(get_opponent_piece(Turn,DropInitiative), nl, write('Opponent has the drop initiative'), nl; 
	\+ has_piece(Board,Turn), nl, write('You have no pieces left on the board'), nl; 
	\+ player_can_move(Board, Turn),nl,write('All your pieces are blocked!'),nl),
	!,
	(drop_piece_human(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative);
	print_board(Board),write('You can not drop your piece there!'),nl,
	human_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative)),
	!.
%in this case, the player can choose to drop a piece or perform a move
human_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative):-
	player(Turn,_,_,_,_) = Player,
	write('1- Drop piece'),nl,write('2- Move piece'),nl,
	(get_user_choice(1,2,Input), 
		(Input = 1,!,(print_board(Board),drop_piece_human(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative);
					print_board(Board),write('You can not drop your piece there!'),nl,
					human_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative));
		Input = 2,!,(print_board(Board),move_piece_human(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative);
					print_board(Board),write('Invalid move!'),nl,
					human_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative)));
	write('Invalid choice!'),nl,!,human_turn(Player,Board,DropInitiative,ResultingPlayer,ResultingBoard,ResultingDropInitiative)),
	!.

%asks the player to choose where to drop the piece and attempts to execute the drop
move_piece_human(Player,Board,Turn,ResultingBoard,Player, DropInitiative, ResultingDropInitiative):-
	player(Turn,_,_,_,_) = Player,
	get_opponent_piece(Turn,OppPiece),
	write('Choose movement (IniColumn/IniRow-DestColumn/DestRow)'),nl,
	read_and_validate_position(IniC/IniL,DestC/DestL),letter_to_num(IniC,COrig),letter_to_num(DestC,CFinal),
	move_piece(Board,COrig,IniL,CFinal,DestL,Turn,TempBoard,remove_piece_after_capture_human(TempBoard,OppPiece,TempBoard2)),
	!,
	(var(TempBoard2),ResultingBoard = TempBoard;
	ResultingBoard = TempBoard2),
	(DropInitiative = Turn, ResultingDropInitiative = e;
	ResultingDropInitiative = DropInitiative).

%asks the player to choose where to drop the piece and attempts to execute the drop
drop_piece_human(Player,Board,Turn,ResultingBoard,ResultingPlayer, DropInitiative, ResultingDropInitiative):-
	player(Turn,NumPieces,_,Pred,_) = Player,
	write('Choose location to drop piece in (Column/Row)'),nl,
	read_and_validate_position(ChosenC/ChosenL),letter_to_num(ChosenC,CPos),drop_piece(Board,CPos,ChosenL,Turn,ResultingBoard),
	!,
	NewNumPieces is NumPieces-1,
	ResultingPlayer = player(Turn,NewNumPieces,human,Pred,_),
	(DropInitiative = e, ResultingDropInitiative = Turn;
	ResultingDropInitiative = DropInitiative).
	
%asks the player which piece to remove after the capture in case there are any available to remove
	%opponent has no pieces on the board. nothing happens
remove_piece_after_capture_human(Board,OppPiece,Board):-
	\+ has_piece(Board,OppPiece), !.
	%user is asked to choose a piece to remove (an invalid choice originates an error message and the predicate is called again)
remove_piece_after_capture_human(Board,OppPiece,ResultingBoard):-
	print_board(Board),
	get_opponent_piece(OppPiece,Turn),
	piece_to_player(Turn, PText), sign(Turn, Symbol),
	write(PText), write(' (You play with '), write(Symbol), write(')'),nl,
	write('Choose an opponent´s piece to remove (Column/Row) '),nl,
	(read_and_validate_position(ChosenC/ChosenL),letter_to_num(ChosenC,CNum),rp(Board,CNum,ChosenL,OppPiece,ResultingBoard);
	print_board(Board),write('You can´t remove anything from there!'),nl,remove_piece_after_capture_human(Board,OppPiece,ResultingBoard)).
%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------

