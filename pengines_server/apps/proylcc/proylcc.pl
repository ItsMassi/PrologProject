:- module(proylcc,
	[  
		put/8
	]).

:-use_module(library(lists)).
:- use_module(library(clpfd)). %This library helps with the management of lists and columns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%

put(Content, [RowN, ColN], _RowsClues, _ColsClues, Grid, NewGrid, 0, 0):-
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).			
	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Content
		;
	replace(_Cell, ColN, Content, Row, NewRow)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%check a row of characters against a list of clues, incrementing a counter for each "#" character
%and resetting it when encountering an "X" or "_" character, depending on the match of the current clue and counter.

check([], [Clue|Clues], Counter):- % Base case
    ((Counter =:= Clue, Clues = []) ->
        writeln('List Correctly checked'),
        true
    ;
    writeln('List Not Correctly checked'),    
    fail % fails
    )
    . % Base case: empty lists

check(["X"|Rs], [Clue|Clues], Counter):-% Works for "X" or "_"
    (Counter =:= Clue ->
        check(Rs, Clues, 0) % Reset counter and continue with the rest of the lists
    ;
        check(Rs, [Clue|Clues], Counter) % Continue without resetting the counter
    ).

check(["#"|Rs], [Clue|Clues], Counter):-
    NewCounter is Counter + 1,
    check(Rs, [Clue|Clues], NewCounter). % Increment counter and continue