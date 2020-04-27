:-use_module(library(lists)).
:-dynamic current/2.
%Some useful booleans
king(wk).
king(bk).
next_player(white, black).
next_player(black, white).
next_player(b, w).
next_player(w, b).
player_piece(white, w).
player_piece(white, wk).
player_piece(black, b).
player_piece(black, bk).

%Who's winning?
value(1, 0). % Blank spaces on the board
value(0, 0).
value(b, 1). % The computer is the black player
value(bk, 4). %May change to higher?
value(w, -1).
value(wk, 4).

% Let's see how this goes with score alone
% https://scholarworks.uark.edu/cgi/viewcontent.cgi?referer=https://www.google.com/&httpsredir=1&article=1031&context=csceuht

%Let's make a board container
%Board with uninitialized positions

board(game_board(A,B,C,D,E,F,G,H)):-
    % Make each line
    functor(A, line, 8),
    functor(B, line, 8),
    functor(C, line, 8),
    functor(D, line, 8),
    functor(E, line, 8),
    functor(F, line, 8),
    functor(G, line, 8),
    functor(H, line, 8).

%White is 0, 1 is black (squares)
empty_board(
    game_board(
        line(0, 1, 0, 1, 0, 1, 0, 1),
        line(0, 1, 0, 1, 0, 1, 0, 1),
        line(0, 1, 0, 1, 0, 1, 0, 1),
        line(0, 1, 0, 1, 0, 1, 0, 1),
        line(0, 1, 0, 1, 0, 1, 0, 1),
        line(0, 1, 0, 1, 0, 1, 0, 1),
        line(0, 1, 0, 1, 0, 1, 0, 1),
        line(0, 1, 0, 1, 0, 1, 0, 1)
    )).

game_init(game_board(A,B,C,D,E,F,G,H)):-
    board(game_board(A,B,C,D,E,F,G,H)),
    board_init_black_first(A, b),
    board_init_white_first(B, b),
    board_init_black_first(C, b),
    board_init_white_first(D),
    board_init_black_first(E),
    board_init_white_first(F, w),
    board_init_black_first(G, w),
    board_init_white_first(H, w).

%initializes a black line (black, white, black, white . . .)
board_init_black_first(Line, Player):-
    arg(1, Line, 0), arg(2, Line, Player), arg(3, Line, 0), arg(4, Line, Player),
    arg(5, Line, 0), arg(6, Line, Player), arg(7, Line, 0), arg(8, Line, Player).
%For the empty lines
board_init_black_first(Line):-
    arg(1, Line, 0), arg(2, Line, 0), arg(3, Line, 0), arg(4, Line, 0),
    arg(5, Line, 0), arg(6, Line, 0), arg(7, Line, 0), arg(8, Line, 0).
%intializes a white line (white, black, white, black . . .)
board_init_white_first(Line, Player):-
    arg(1, Line, Player), arg(2, Line, 0), arg(3, Line, Player),
    arg(4, Line, 0), arg(5, Line, Player), arg(6, Line, 0), arg(7, Line, Player),
    arg(8, Line, 0).
board_init_white_first(Line):-
    arg(1, Line, 0), arg(2, Line, 0), arg(3, Line, 0), arg(4, Line, 0),
    arg(5, Line, 0), arg(6, Line, 0), arg(7, Line, 0), arg(8, Line, 0).

print_board(game_board(A,B,C,D,E,F,G,H)):-
    % I don't know how to align, so this is slightly off.
        %Here we print the header: 1 2 3 4 5 6 7 8
	tab(3),print(1), tab(2),print(2), tab(2),
	print(3), tab(2),print(4), tab(2), print(5),
        tab(2),print(6), tab(2), print(7), tab(2),
        print(8), tab(2), nl,
        %Here we print line by line
	print(1), tab(2),
        board_print_line(A),
	print(2), tab(2),
        board_print_line(B),
	print(3), tab(2),
        board_print_line(C),
	print(4), tab(2),
        board_print_line(D),
	print(5), tab(2),
        board_print_line(E),
	print(6), tab(2),
        board_print_line(F),
	print(7), tab(2),
        board_print_line(G),
	print(8), tab(2),
        board_print_line(H).

% prints a line of the board
board_print_line(Line):-
    print_line_element(Line, 1),
    print_line_element(Line, 2),
    print_line_element(Line, 3),
    print_line_element(Line, 4),
    print_line_element(Line, 5),
    print_line_element(Line, 6),
    print_line_element(Line, 7),
    print_line_element(Line, 8),
    nl.
%First line is different.s
print_line_element(Line, Index):-
    arg(Index, Line, E),
    E == 1, !,
    format('~c', [95]), tab(2).

%Prints a white space
print_line_element(Line, Index):-
    arg(Index, Line, E),
    E == 0, !, tab(3).

%If the piece is white
print_line_element(Line, Index):-

    arg(Index, Line, E),
    E == w, !,
    format('~c', [9817]), tab(2).
    %9817 is unicode for the white pawn

%If the piece is black
print_line_element(Line, Index):-
    arg(Index, Line, E),
    E == b, !,
    format('~c', [9823]), tab(2).
    %9823 is code for a black pawn

% If the piece is a white king
print_line_element(Line, Index):-
    arg(Index, Line, E),
    E == wk, !,
    format('~c', [9812]), tab(2).
    %9812 is unicode for a white King

% If the piece is a black king
print_line_element(Index, Line, E):-
    arg(Index, Line, E),
    E == bk, !,
    format('~c', [9818]), tab(2).
    %9818 is unicode for a black king

%!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% returns what's at the position (X, Y). Either board color or the piece
pos(Board, X, Y, E):-
    arg(Y, Board, T),
    arg(X, T, E).


% Places or replaces the element at (X, Y) with the new Element and
% returns the board New_board
replace(Board, X, Y, Element, New_board):-
    functor(New_board, game_board, 8),
    replace_board(Board, X, Y, Element, New_board, 1).


%For use with replace:
% copies everything from Board onto New_board with the exception of the
% position (X, Y) which is replaced with Element
replace_board(_,_,_,_,_,Iterator):-Iterator > 8, !.
replace_board(Board, X, Y, Element, New_board, Iterator):-
    Iterator == Y, !,
    arg(Y, Board, Line),
    functor(New_line, line, 8),
    replace_line(Line, X, Element, New_line, 1),
    arg(Iterator, New_board, New_line),
    Next_iter is Iterator + 1,
    replace_board(Board, X, Y, Element, New_board, Next_iter).

replace_board(Board, X, Y, Element, New_board, Iterator):-
    arg(Iterator, Board, Line),
    arg(Iterator, New_board, Line),
    Next_iter is Iterator + 1,
    replace_board(Board, X, Y, Element, New_board, Next_iter).

% Replaces the element at index X in this line with Element. Copies
% everything else from Line and returns New_line
% Row by Row, hence only needing 'X'
replace_line(_,_,_,_,Iterator):- Iterator > 8, !.
replace_line(Line, X, Element, New_line, Iterator):-
    Iterator == X, !,
    arg(X, New_line, Element),
    Next_iter is Iterator + 1,
    replace_line(Line, X, Element, New_line, Next_iter).
replace_line(Line, X, Element, New_line, Iterator):-
    arg(Iterator, Line, Old_line),
    arg(Iterator, New_line, Old_line),
    Next_iter is Iterator + 1,
    replace_line(Line, X, Element, New_line, Next_iter).

% Remves the piece at position (X, Y) from the board. Returns New_board
remove(Board, X, Y, New_board):-
    empty_board(Empty_Board),
    pos(Empty_Board, X, Y, Place),
    replace(Board, X, Y, Place, New_board).

% Moves a piece on the board (X1, Y1 to X2, Y2). Returns a new board
% with the piece moved
/* move(Board, _, _, _, _, New_board).*/
move(Board, X1, Y1, X2, Y2, New_board):-
    pos(Board, X1, Y1, Piece),
    remove(Board, X1, Y1, Temp_board),
    promote(Y1, Piece, New_piece),
    %There's already an if statement in promote to only promote pieces at the end of the board so we don't need one here.
    replace(Temp_board, X2, Y2, New_piece, New_board).
% Promotes a piece to a king if it reaches the end of the board or
% returns the Piece
promote(1, w, wk):- !.
promote(8, b, bk):- !.
promote(_, Piece, Piece).

%Boolean for if a position (X, Y) is occupied
is_occupied(Board, X, Y):-
    pos(Board, X, Y, Element),
    %not(number(Element)).
    \+number(Element).

% Checks if the position contains an enemy of the player. (E.g. Succeeds
% if the Player is Black and the piece is white. Otherwise Fails).
is_enemy(Board, X, Y, Player):-
    pos(Board, X, Y, Piece),
    next_player(Player, N),
    player_piece(N, Piece), !.

% !
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Here we detail how to take pieces

%How to hop over a single piece for regular pieces

%Looking right for white player
next_take(Board, w, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 7, Y_init > 2, %Can't go past the edge of the board
    Y_next is Y_init - 1,
    X_next is X_init + 1,
    is_enemy(Board, X_next, Y_next, w),
    X_fin is X_init + 2, Y_fin is Y_init - 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%Looking left for white player
next_take(Board, w, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init > 2, Y_init > 2, %Can't eat past the edge of the board
    X_next is X_init - 1, Y_next is Y_init - 1,
    is_enemy(Board, X_next, Y_next, w),
    X_fin is X_init - 2, Y_fin is Y_init - 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%Looking right for the black player
next_take(Board, b, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 7, Y_init < 7, %Can't pass the edge of the
    X_next is X_init + 1, Y_next is Y_init + 1,
    is_enemy(Board, X_next, Y_next, b),
    X_fin is X_init + 2, Y_fin is Y_init + 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%Looking left for the black player
next_take(Board, b, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init > 2, Y_init < 7,
    X_next is X_init - 1, Y_next is Y_init + 1,
    is_enemy(Board, X_next, Y_next, b),
    X_fin is X_init - 2, Y_fin is X_init + 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

% Now Let's focus on the kings taking pieces. They have four directions
% each

%White king tries to take a piece in forward right
next_take(Board, wk, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 7, Y_init > 2,
    X_next is X_init + 1, Y_next is Y_init - 1,
    is_enemy(Board, X_next, Y_next, w),
    X_fin is X_init + 2, Y_fin is Y_init - 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%White king tries to take a piece in forward left
next_take(Board, wk, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init > 2, Y_init > 2,
    X_next is X_init - 1, Y_next is Y_init - 1,
    is_enemy(Board, X_next, Y_next, white),
    X_fin is X_init - 2, Y_fin is Y_init - 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%White king tries to take a piece in backwards right
next_take(Board, wk, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 7, Y_init < 7,
    X_next is X_init + 1, Y_next is Y_init + 1,
    is_enemy(Board, X_next, Y_next, white),
    X_fin is X_init + 2, Y_fin is Y_init + 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%White king tries to take a piece in backwards left
next_take(Board, bk, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init > 2, Y_init < 7,
    X_next is X_init - 1, Y_next is Y_init + 1,
    is_enemy(Board, X_next, Y_next, white),
    X_fin is X_init - 2, Y_fin is Y_init + 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%Black king tries to take a piece in forward right
next_take(Board, bk, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 7, Y_init < 7,
    X_next is X_init + 1, Y_next is Y_init + 1,
    is_enemy(Board, X_next, Y_next, black),
    X_fin is X_init + 2, Y_fin is Y_init + 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%Black king tries to take a piece in forward left
next_take(Board, bk, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init > 2, Y_init < 7,
    X_next is X_init - 1, Y_next is Y_init + 1,
    is_enemy(Board, X_next, Y_next, black),
    X_fin is X_init - 2, Y_fin is Y_init + 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%Black king tries to take a piece in backwards right
next_take(Board, bk, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 7, Y_init > 2,
    X_next is X_init + 1, Y_next is Y_init - 1,
    is_enemy(Board, X_next, Y_next, black),
    X_fin is X_init + 2, Y_fin is Y_init - 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%Black king tries to take a piece in backwards left
next_take(Board, bk, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 2, Y_init < 2,
    X_next is X_init - 1, Y_next is Y_init - 1,
    is_enemy(Board, X_next, Y_next, black),
    X_fin is X_init - 2, Y_fin is Y_init - 2,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, Temp_board),
    remove(Temp_board, X_init, Y_init, New_board).

%How to move around the board without taking a piece (If possible)
%
% A white piece moving right
next_move(Board, w, X_init, Y_init, m(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 8, Y_init > 1,
    X_fin is X_init + 1, Y_fin is Y_init - 1,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, New_board).

% A white piece moving left
next_move(Board, w, X_init, Y_init, m(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init > 1, Y_init > 1,
    X_fin is X_init - 1, Y_fin is Y_init - 1,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, New_board).

% Black piece moving right
next_move(Board, b, X_init, Y_init, m(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init < 8, Y_init < 8,
    X_fin is X_init - 1, Y_fin is Y_init + 1,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, New_board).

% Black piece moving left
next_move(Board, b, X_init, Y_init, m(X_init, Y_init, X_fin, Y_fin, New_board)):-
    X_init > 1, Y_init < 8,
    X_fin is X_init - 1, Y_fin is Y_init - 1,
    \+is_occupied(Board, X_fin, Y_fin),
    move(Board, X_init, Y_init, X_fin, Y_fin, New_board).

%Kings can move forward and back. But other pieces cannot move back

%King moving up and right
next_move(Board, Piece, X_init, Y_init, m(X_init, Y_init, X_fin, Y_fin, New_board)):-
          king(Piece),
          %I don't want to write this 8 times. I don't need to check the color of the piece as the piece can move in any direction
          X_init < 8, Y_init > 1,
          X_fin is X_init - 1, Y_fin is Y_init - 1,
          \+is_occupied(Board, X_fin, Y_fin),
          move(Board, X_init, Y_init, X_fin, Y_fin, New_board).

%King moving up and left
next_move(Board, Piece, X_init, Y_init, m(X_init, Y_init, X_fin, Y_fin, New_board)):-
          king(Piece),
          X_init > 1, Y_init > 1,
          X_fin is X_init + 1, Y_fin is Y_init -1,
          \+is_occupied(Board, X_fin, Y_fin),
          move(Board, X_init, Y_init, X_fin, Y_fin, New_board).

%King moving down and right
next_move(Board, Piece, X_init, Y_init, m(X_init, Y_init, X_fin, Y_fin, New_board)):-
          king(Piece),
          X_init < 8, Y_init < 8,
          X_fin is X_init + 1, Y_fin is Y_init + 1,
          \+is_occupied(Board, X_fin, Y_fin),
          move(Board, X_init, Y_init, X_fin, Y_fin, New_board).

%King moving down and left
next_move(Board, Piece, X_init, Y_init, m(X_init, Y_init, X_fin, Y_fin, New_board)):-
          king(Piece),
          X_init > 1, Y_init < 8,
          X_fin is X_init - 1, Y_fin is Y_init + 1,
          \+is_occupied(Board, X_fin, Y_fin),
          move(Board, X_init, Y_init, X_fin, Y_fin, New_board).

% Returns a list of all available pieces that you can take. If there are
% no available pieces, makes moves instead.
list_available_moves(Board, Player, Moves):-
    list_all_pieces(Board, Player, Pieces),
    list_available_moves_helper(Board, Pieces, Moves),
    Moves \= [].

% The rules of checkers are that if you need to take a piece, you must
% take said piece. Hence the cut.
list_available_moves_helper(Board, Pieces, Moves):-
    list_all_takes(Board, Pieces, Moves),
    Moves \= [], !.
list_available_moves_helper(Board, Pieces, Moves):-
    list_possible_moves(Board, Pieces, Moves).

%bagof is https://www.swi-prolog.org/pldoc/man?predicate=bagof/3
%Lists all possible pieces a piece can take from a position

%Base Case
list_all_takes(_,[],[]):- !.
list_all_takes(Board, [p(T,X,Y) | Positions], Moves):-
    % Any moves where you can take multiple pieces?
    bagof(M,chain(Board, p(T,X,Y),M),Move), !,
    %What about taking just one?
    list_all_takes(Board, Positions, Move_2),
    append(Move, Move_2, Move_3),
    remove_empty_space(Move_3, Moves).
list_all_takes(Board, [_|Positions], Moves):-
    list_all_takes(Board, Positions, Moves).


%If there's no piece to take, list the possible moves of the piece
list_possible_moves(_,[],[]):- !.
list_possible_moves(Board, [p(P, X, Y)|Positions], Moves):-
    bagof(M,next_move(Board,P,X,Y,M), Move), !,
    list_possible_moves(Board, Positions, Other_moves),
    append(Move, Other_moves, Moves).
list_possible_moves(Board, [_|Pieces], Moves):-
    list_possible_moves(Board, Pieces, Moves).

%Lists all pieces of the player

list_all_pieces(Board, Player, Pieces):-
    bagof(Place, list_all_pieces_helper(Board, Player, Place), Pieces).
list_all_pieces_helper(Board, Player, p(T,X,Y)):-
    member(X, [1,2,3,4,5,6,7,8]), %Is X and/or Y on the board?
    member(Y, [1,2,3,4,5,6,7,8]),
    pos(Board, X, Y, T),
    player_piece(Player, T).


remove_empty_space([], []):- !.
remove_empty_space([[]|S], S1):- !, remove_empty_space(S, S1).
remove_empty_space([X|S], [X|S1]):- remove_empty_space(S, S1).


chain(Board, p(T,X_init,Y_init), [t(X_init, Y_init, X_fin, Y_fin, New_board)|Moves]):-
    next_take(Board, T, X_init, Y_init, t(X_init, Y_init, X_fin, Y_fin, New_board)),
    pos(New_board, X_fin, Y_fin, T_fin),
    chain(New_board, p(T_fin, X_fin, Y_fin),Moves).
chain(Board, p(T, X, Y), []):-
    \+next_take(Board, T, X, Y, _).


%Now humans can play.


%!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Let's try some AI



%What if we can hop over more than one piece?
%Return a list with all positions hopped over starting at Start.
%Backtrack over all posibilities starting at Start

evaluate(Board, 300,_):-
    \+list_available_moves(Board,white,_),
    list_available_moves(Board,black,_), !.
evaluate(Board, -300, _):-
    \+list_available_moves(Board,black,_),
    list_available_moves(Board, white, _), !.
evaluate(Board, 0, Iterator):- Iterator > 8, !. % each rank is 8 across
evaluate(Board, Val, Iterator):-
    arg(Iterator, Board, Line), !,
    eval_line(Line, Line_eval, Iterator, 1),
    Next_iter is Iterator + 1,
    evaluate(Board, Remainder, Next_iter),
    Val is Line_eval + Remainder.

eval_line(Line, 0, _, Col):- Col > 8, !. % 8 X 8 board
eval_line(Line, Eval, Row, Col):-
    arg(Col, Line, Piece), !,
    value(Piece, Value),
    Next_iter is Col + 1,
    eval_line(Line, Remainder, Row, Next_iter),
    Eval is Remainder + Value. %Should make some rows weighted more than others

%The end of a "Move" is the new board, so we seperate them.
move_less(m(_,_,_,_, Board), Board). %Change to change_board throughout program
move_less(t(_,_,_,_, Board), Board).

%Base case
find_move([Move], Move).
%last returns the last member of the list
find_move([Move|Moves], Last_move):- last(Moves, Last_move).
find_move(Move, Move).

%The player is always white hence the computer is always black.
maximize(black).
minimize(white).

%What's the best move out of the ones we have?
% This was originally for my minimax function, but I refitted it for
% alphabeta pruning
better_of(Player, Pos_move, Pos_Val, Other_move, Other_val, Pos_Move, Pos_val):-
    maximize(Player),
    Pos_Val >= Other_val, !.

better_of(Player, Pos_move, Pos_val, Other_move, Other_val, Other_move, Other_val):-
    maximize(Player),
    Other_val >= Pos_val, !.

better_of(Player, Pos_move, Pos_val, Other_move, Other_val, Pos_move, Pos_val):-
    minimize(Player),
    Pos_val =< Other_val, !.

better_of(Player, Pos_move, Pos_val, Other_move, Other_val, Other_move, Other_val):-
    minimize(Player),
    Other_val =< Pos_val, !.

% Way faster than minimax, so we can increase the depth.
alphabeta(Alpha, Beta, Depth, Player, Board, Val, Next_move):-
    Depth < 25,
    New_depth is Depth + 1,
    list_available_moves(Board, Player, Moves),
    print(Moves), nl, halt,
    temp_name(Alpha, Beta, Depth, Player, Moves, Next_move, Val), !.
alphabeta(Alpha, Beta, Depth, Player, Board, Val, Next_move):-
    print(Board),
    evaluate(Board, Val, 1), !.

temp_name(Alpha, Beta, Depth, Player, [Move|Moves], Best, Best_val):-
    find_move(Move, Next),
    move_less(Next, Board),
    next_player(Player, Other),
    alphabeta(Alpha, Beta, Depth, Other, Board, Val, _),
    better_move(Alpha, Beta, Depth, Player, Moves, Next, Val, Best, Best_val).

% Base case: No more moves to look through.
better_move(_, _, Depth, Player, [], Move, Val, Move, Val, Depth):- !.

% We come up with the best move for us that limits the opponents
% counterplay
better_move(Alpha, Beta, Depth, Player, _, Move, Val, Move, Val):-
    minimize(Player), Val > Alpha, !.

better_move(Alpha, Beta, Depth, Player, _, Move, Val, Move, Val):-
    maximize(Player), Val < Beta, !.

better_move(Alpha, Beta, Depth, Player, Moves, Move, Val, Best_move, Best_val):-
    change_alphabeta(Alpha, Beta, Player, Val, New_alpha, New_beta),
    temp_name(New_alpha, New_beta, Depth, Player, Moves, Next_move, Next_val),
    % Move this down a bit
    better_of(Player, Move, Val, Next_move, Next_val, Best_move, Best_val).


change_alphabeta(Alpha, Beta, Player, Val, Val, Beta):-
    minimize(Player), Val > Alpha, !.
change_alphabeta(Alpha, Beta, Player, Val, Alpha, Val):-
    maximize(Player), Val < Beta, !.


play:-
    current(Player, Board),
    write(Player), nl,
    print_board(Board),
    play(Player, Board).


play(black, Board):-
    print("Black move"),nl,
    alphabeta(-1000, 1000, 0, black,  Board, Eval, Best),
    nonvar(Best), !,
    write('Black (computer) turn'),nl,
    write('Evaluation: '), write(Eval), nl,
    print_move(Best), nl,
    move_less(Best, New_board),
    assert(current(white, New_board)),
    print("Asserted"), nl,
    print_board(New_board),
    play(white, New_board).

play(black, Board):-
    list_available_moves(Board, white, _), !,
    % If white can't move after black plays, then black wins.
    write('Black wins the game.').
% I guess this can happen.
play(black, _):-
    write('Draw').

play(white, Board):-
    print_board(Board),
    list_available_moves(Board, white, Moves), !,
    write('White (player) turn.'), nl,
    print_possible(1, Moves),
    repeat, %In case of an invalid move
    read(Option),
    nth1(Option, Moves, Move), % Is Move a part of Moves?
    find_move(Move, New_move),
    move_less(New_move, New_board),
    assert(current(black, New_board)), % It's black's turn
    print_board(New_board),
    play(black, New_board).

    % If the black player cannot win after the white player plays, white wins
play(white, Board):-
    list_available_moves(Board, white, _), !,
    write('White player wins!').
play(white, _):-
    write('Draw').

print_move(m(X_start, Y_start, X_end, Y_end, _)):- !,
    write(X_start), write(','), write(Y_start), write(' -> '),
    write(X_end), write(','), write(Y_end), nl.
print_move(t(X_start, Y_start, X_end, Y_end, _)):-
    write(X_start), write(','), write(Y_start), write(' -> '),
    write(X_end), write(','), write(Y_end), nl, !.

%It didn't work with the cut at the end. Not sure why.
print_hop_hop([t(X_start, Y_start, X_end, Y_end, _)]) :- !,
    write(X_start), write(','), write(Y_start), write('->'),
    write(X_end), write(','), write(Y_end), nl.
print_hop_hop([t(X_start, Y_start, X_end, Y_end, _)|Hopped]) :- !,
    write(X_start), write(','), write(Y_start), write('->'),
    write(X_end), write(','), write(Y_end), write('->'),
    print_hop_hop(Hopped).

%Base Case
print_possible(_, []) :- !, nl.
%General case
print_possible(Num, [m(X_start, Y_start, X_end, Y_end, _)|Moves]):- !,
    write(Num), write(':'), write(X_start), write(','), write(Y_start),
    write('->'), write(X_end), write(','), write(Y_end), nl,
    Next_num is Num + 1,
    print_possible(Next_num, Moves).
% Chained hops
print_possible(Num, [Hop_hop|Moves]):- !,
    write(Num), write(':'),
    print_hop_hop(Hop_hop),
    Next is Num + 1,
    print_possible(Next, Moves).

main_ :-
    game_init(Board),
    assert(current(white, Board)),
    write("Beginning checkers"), nl,
    play.







