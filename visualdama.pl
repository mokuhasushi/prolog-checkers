
:- consult(read_board).

:- dynamic(picture/5).
:- dynamic(fig/4).
:- dynamic(current_board/1).


set_board(SC) :-
	board(SC),!,
	retractall(current_board(_)),
	assert(current_board(SC)),
	new_board(SC),
	forall(between(0,7,Row),
		forall(between(0,7,Col),
			(vped(SC, point(Row,Col),C) ->
			draw_vped(SC,C,point(Row,Col))
			;vdama(SC,point(Row,Col),C)->
			draw_vdama(SC,C,point(Row,Col));true)
		)
	).

new_board(Name):-
	retractall(board(_)),
	Ncol is 8, Nrow is 8, Size is 70,
	retractall(picture(_,Name,_,_,_)),
	retractall(fig(_,Name,_,_)),
	Width is Ncol * Size,
	Height is Nrow * Size,
	new(Pic, picture(Name, size(Width, Height))),
	send(Pic, display, new(_,box(Width,Height))),
	forall(between(0,7,D),
		(is_even(D),
		new_row1(Pic,Size,D));
		(is_odd(D),
		new_row2(Pic,Size,D))),
	send(Pic,open),!,
	assert(picture(Pic,Name,Ncol,Nrow,Size)),
	assert(board(Name)).

update_board(Name):-
	retractall(fig(_,Name,circ(45,_),point(_,_))),
	retractall(fig(_,Name,sqaure(45,_),point(_,_))),
	forall(between(0,7,Row),
		forall(between(0,7,Col),
			(vped(SC, point(Row,Col),C) ->
			draw_vped(SC,C,point(Row,Col))
			;vdama(SC,point(Row,Col),C)->
			draw_vdama(SC,C,point(Row,Col));true)
		)
	).

draw_vped(Name,Coulor,point(Row,Col)):-
	picture(Pic,Name,_,_,Size),
	X is Col*Size +12,
	Y is Row*Size +12,
	new_fig(circ(45,[col(Coulor)]),F),
	send(Pic, display(F, point(X,Y))),
%	send(F, displayed,@on),			%Prima funzionava con questo decommentato
	send(Pic, redraw),!,
	assert(fig(F,Name,circ(45,[col(Coulor)]),point(Row,Col))).

draw_vdama(Name,Coulor,point(Row,Col)):-
	picture(Pic,Name,_,_,Size),
	X is Col*Size +12,
	Y is Row*Size +12,
	new_fig(square(45,[col(Coulor)]),F),
	send(Pic, display(F, point(X,Y))),
%	send(F, displayed,@on),			%Problema dopo aggiornamento
	send(Pic, redraw),!,
	assert(fig(F,Name,square(45,[col(Coulor)]),point(Row,Col))).


new_fig(circ(Size, Param), B) :-
	new(B, circle(Size)),
	send_fig(B, Param).
new_fig(square(Size,Param),B) :-
	new(B, box(Size,Size)),
	send_fig(B, Param).

new_row1(Pic,Size,D) :-
	forall(between(0,7,N),
		((is_odd(N),
		C is N * Size,
		E is D * Size,
		new(Q, box(Size,Size)),
		send(Q, fill_pattern, colour(black)),
		send(Pic,display(Q, point(C,E)))));true).

new_row2(Pic,Size,D) :-
	forall(between(0,7,N),
		((is_even(N),
		C is N * Size,
		E is D * Size,
		new(Q, box(Size,Size)),
		send(Q, fill_pattern, colour(black)),
		send(Pic,display(Q, point(C,E)))));true).


send_fig(_,[]) :- !.
send_fig(B, [col(C)]) :-!,
	send(B, fill_pattern, colour(C)).

is_even(N):-
	C is N mod 2,
	C = 0.
is_odd(N):-
	C is N mod 2,
	C = 1.

del_vped(Name, point(Col,Row)) :-
	picture(Pic, Name, _, _, _),
	fig(F, Name, Fig, point(Col, Row)),
	send(F, destroy),
	send(Pic,redraw),!,
	retract(fig(F, Name, Fig, point(Col, Row))).

move_vped(Name, C, P1, point(X,Y)) :-
	(X = 7 ; X = 0),
	del_vped(Name,P1),!,
	draw_vdama(Name,C,point(X,Y)),
	retract(vped(K,point(X,Y),P)),
	assert(vdama(K,point(X,Y),P)).

move_vped(Name,C,P1,P2) :-
	del_vped(Name,P1),!,
	draw_vped(Name,C,P2).

move_vdama(Name,C,P1,P2) :-
	del_vped(Name,P1),
	draw_vdama(Name,C,P2).

del_fig(Name, Fig, point(Col,Row)) :-
	picture(Pic, Name, _, _, _),
	fig(F, Name, Fig, point(Col, Row)),
	send(F, destroy),
	send(Pic,redraw),!,
	retract(fig(F, Name, Fig, point(Col, Row))).







