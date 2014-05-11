%------------------------------------------------------SECTION: PREDICATES FOR PRINTING THE BOARD-----------------------------------------------------
%this section of the code contains predicates used both for printing the board and converting board elements into the symbols used to represent each
%of the elements

%converts the piece type into a corresponding symbol to be displayed on screen
sign( b, 'X'). %b indicates a black piece type and is represented on screen by an X
sign( w, 'O'). %w indicates a white piece type and is represented on screen by an O
sign( e, ' '). %e indicates an empty cell (absence of piece). When used as a value to represent the drop initiative, it indicates no player currently has the initiative.

%prints the column indexes and the top and bottom borders, calling an auxiliary recursive function in-between to print the contents of the board
print_board(B):-
	nl,nl,
	print('                     a b c d e  '),nl,
	print('                    |---------|'),nl,
	length(B,N),
	pb(B,N),
	print('                    |---------|'),nl, nl.
%recursively prints each line of the board followed by a line separator until there is only one element left (in this case no separator is printed).
pb([B], N) :-
	print('                   '),
	print(N),
	print_line(B), !.
pb([B|Bs], N) :-
	print('                   '),
	print(N),
	print_line(B),
	print('                    |-+-+-+-+-|'),nl,
	N1 is N-1,
	pb(Bs,N1).
%recursively prints a vertical separator and the on-screen symbol corresponding to the piece of the current line element.
%When the line has been fully printed, an additional vertical separator is printed (right border of the board).
print_line([]) :-
	print('|'),nl.
print_line([L|Ls]):-
	sign(L,Symbol),
	print('|'),
	print(Symbol),
	print_line(Ls).
%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------


%----------------------------------------------SECTION: AUXILIARY PREDICATES FOR VISUALIZING GAME STATUS---------------------------------------------
%this section contains predicates used to validate user input
%it also contains predicates used to convert several game elements into 'human friendly' descriptions that will be printed on screen
%these predicates do not interact directly with the user (they don't read user input)

%gets the player 'name' corresponding to a piece type (used to represent turns)
piece_to_player(w,'Player 1').
piece_to_player(b,'Player 2').
piece_to_player(e,'Noone').

%converts letters to the corresponding column numbers
letter_to_num(a,1).
letter_to_num(b,2).
letter_to_num(c,3).
letter_to_num(d,4).
letter_to_num(e,5).


%prints the status of the specified player on screen (used when a player has to take an action)
show_player_status(Player):-
	player(Turn,NumPieces,_,_,_) = Player,
	piece_to_player(Turn,PText),
	sign(Turn,Symbol),
	write(PText), write('´s turn.'), write(' (You play with '), write(Symbol), write(')'),
	nl,
	write('You have '),write(NumPieces), write(' pieces in hand.'),nl.
%prints the status of the specified player on screen (used at the beginning of his turn) as well as who has the drop initiative
show_player_status(Player, DropInitiative):-
	player(Turn,NumPieces,_,_,_) = Player,
	piece_to_player(Turn,PText),
	sign(Turn,Symbol),
	piece_to_player(DropInitiative,PInitiative),
	write(PText), write('´s turn.'), write(' (You play with '), write(Symbol), write(')'),
	nl,
	write('You have '),write(NumPieces), write(' pieces in hand.'),nl,
	write(PInitiative),write(' has the drop initiative.'),nl, nl.
%-------------------------------------------------------------------END OF SECTION-------------------------------------------------------------------