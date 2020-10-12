:- consult(dama).

:- dynamic (vdama/3).
:- dynamic (vped/3).
:- dynamic(board/1).


load_board(K) :-
	char_board(K,SC),
	retractall(vped(Name,_,_)),
	retractall(board(Name)),
	load_rows(K, 0, SC),
	assert(board(K)).

% METODI PER CONSULTARE LA RAPPRESENTAZIONE TRAMITE LISTA (DAMA.PL)

traduci(sc(L1,L2,_),K) :-
	retractall(vped(_,_,_)),
	retractall(vdama(_,_,_)),
	retractall(board(_)),
	traducirosso(L1,K),
	traduciblu(L2,K),
	assert(board(K)).

traducirosso([ped(X,Y)|L],K) :-
	assert(vped(K,point(X,Y),red)),
	traducirosso(L,K).
traducirosso([dama(X,Y)|L],K) :-
	assert(vdama(K,point(X,Y),red)),
	traducirosso(L,K).
traducirosso([],_).


traduciblu([ped(X,Y)|L],K) :-
	assert(vped(K,point(X,Y),blue)),
	traduciblu(L,K).
traduciblu([dama(X,Y)|L],K) :-
	assert(vdama(K,point(X,Y),blue)),
	traduciblu(L,K).
traduciblu([],_).





% METODI PER LEGGERE UNA SCACCHIERA DA CARATTERI

load_rows(K, RN, [R|Rows]) :-
	atom_chars(R, LR),
	load_row(K, RN, 0, LR),
	RN1 is RN + 1,
	load_rows(K, RN1, Rows).

load_rows(_,_,[]).

load_row(K,RN,CN,[Ch|Chars]) :-
	(Ch = 'O' -> assert(vped(K, point(RN,CN),red));
	Ch = 'X' -> assert(vped(K, point(RN,CN),blue));
	true),
	CN1 is CN + 1,
	load_row(K,RN,CN1,Chars).

load_row(_,_,_,[]).


char_board(default,[
' X X X X',
'X X X X ',
' X X X X',
'        ',
'        ',
'O O O O ',
' O O O O',
'O O O O ']).
