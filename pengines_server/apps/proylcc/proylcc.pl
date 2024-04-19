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

put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat):-
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).			
	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Content
		;
	replace(_Cell, ColN, Content, Row, NewRow)),


    copy_term(NewGrid,GridCopy), % This is for not instantiating the annonymous variables
    checkClues(GridCopy,RowN,ColN,RowsClues,ColsClues,RowSat, ColSat)
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%check a row of characters against a list of clues, incrementing a counter for each "#" character
%and resetting it when encountering an "X" or "_" character, depending on the match of the current clue and counter.

% Base case: If the list is empty and there are no more clues, the list is correctly checked.
check([], [], _).
check([], [Counter], Counter).

% Case for "X": If the counter matches the current clue, reset the counter and continue checking the rest of the list.
check(["X"|Rs], [Clue|Clues], Counter):-
    (Counter =:= Clue ->
        check(Rs, Clues, 0)
    ;
        !, fail % Cut here to prevent backtracking if Counter =:= Clue is false
    ).

% Case for "#": Increment the counter and continue checking the rest of the list.
check(["#"|Rs], [Clue|Clues], Counter):-
    NewCounter is Counter + 1,
    check(Rs, [Clue|Clues], NewCounter).

% Case for "_": Reset the counter and continue checking the rest of the list.
check(["_"|Rs], [Clue|Clues], _):-
    check(Rs, [Clue|Clues], 0).

% CheckClues Checks if the clues of certain Row and Column are complete
checkClues([R|Rs],RowNum, ColumnNum,RClues, CClues,RowSat,ColSat):-
    transpose([R|Rs],[C|Cs]),
    nth0(RowNum, [R|Rs], Row), nth0(RowNum, RClues, RClue),
    nth0(ColumnNum, [C|Cs], Column), nth0(ColumnNum, CClues, CClue),
    (check(Row, RClue, 0) -> RowSat is 1 ; RowSat is 0), % If row check is correct, RowSat is 1, otherwise 0
    (check(Column,CClue,0) -> ColSat is 1 ; ColSat is 0). % If column check is correct, ColSat is 1, otherwise 0
    
copyList(L,R) :- copyListAccumulator(L,R).
copyListAccumulator([],[]).
copyListAccumulator([H|T1],[H|T2]) :- copyListAccumulator(T1,T2).    
    