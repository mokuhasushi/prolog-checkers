:- consult(read_board).
:- consult(visualdama).
:- consult(dama).
:- consult(scacchiere).
:- consult(minimaxab).

:-dynamic(lunghezza/1).

start(D) :-
	scacchiera(nuova,_,SC),
	traduci(SC,nuovas),
	set_board(nuovas),
	giocapre(red,SC,D).

startgame(S,D) :-
	scacchiera(S,P,SC),
	traduci(SC,scacc),
	set_board(scacc),
	giocapre(P,SC,D).

starttrace(D) :-
	scacchiera(nuova,_,SC),
	traduci(SC,nuovas),
	set_board(nuovas),
	giocapretrace(red,SC,D).

startgametrace(S,D) :-
	scacchiera(S,P,SC),
	traduci(SC,scacc),
	set_board(scacc),
	giocapretrace(P,SC,D).

giocapre(P,S,D):-
	board(K),!,
	gioca([S,P,play],K,D).

giocapretrace(P,S,D):-
	board(K),!,
	giocatrace([S,P,play],K,D).

gioca([S,red,play],K,D):- !,
	mossa(K,red,S,NS),
	((fine(NS,red),(write("vince: "),write(red)));
	(gioca([NS,blue,play],K,D))).

gioca([S,blue,play],K,D):- !,
	bestMove([S,blue,play],[NS,red,STATE],D),
	traduci(NS,K),
	set_board(K),
	((STATE = win -> (write("vince: "),write(blue)));
	gioca([NS,red,play],K,D)).
	
giocatrace([S,red,play],K,D):- !,
	mossa(K,red,S,NS),
	((fine(NS,red),(write("vince: "),write(red)));
	(giocatrace([NS,blue,play],K,D))).

giocatrace([S,blue,play],K,D):- !,
	bestMovetrace([S,blue,play],[NS,red,STATE],D),
	traduci(NS,K),
	set_board(K),
	((STATE = win -> (write("vince: "),write(blue)));
	giocatrace([NS,red,play],K,D)).



nextplayer(P,blue) :-
	P = red.
nextplayer(P,red) :-
	P = blue.

fine(sc(L,_,_),blue):-
	length(L,0).
fine(sc(_,L,_),red):-
	length(L,0).


mossa(K,P,S,NS) :-
	write(" 'a' per abortire, qualsiasi altro tasto per continuare\n"),
	readln(L),(
		(L = [a|_], abort);
		(write("gioca"),write(P),write("\n"),getcord(X,Y),getcord(A,B))),
	(mossamangia(K,P,S,NS,X,Y,A,B),!;
	mossamuovi(K,P,S,NS,X,Y,A,B),!;
	(write("mossa non valida\n"),
	mossa(K,P,S,NS))).
%mossa, ma obbligatorio mangiare

simangia(red,sc(LB,LN,LV)):-
	member(X,LB),
	mangiap(sc(LB,LN,LV),X,_,_).
simangia(blue,sc(LB,LN,LV)):-
	member(X,LN),
	mangiap(sc(LB,LN,LV),X,_,_).

mossamuovi(_,P,S,_,_,_,_,_) :-
	simangia(P,S),!,
	write("devi mangiare"),
	fail.

mossamuovi(K,P,S,NS,X,Y,A,B) :-
	muovi(S,ped(X,Y),(A,B),NS),
	retract(vped(K,point(X,Y),P)),
	assert(vped(K,point(A,B),P)),
	move_vped(K,P,point(X,Y),point(A,B)).

mossamuovi(K,P,S,NS,X,Y,A,B) :- 
	muovi(S,dama(X,Y),(A,B),NS),
	retract(vdama(K,point(X,Y),P)),
	assert(vdama(K,point(A,B),P)),
	move_vdama(K,P,point(X,Y),point(A,B)).

mossamangia(K,P,S,NNS,X,Y,A,B) :-
	mangiap(S,ped(X,Y),(A,B),NS),
	aggiornamangia(ped(X,Y),A,B,P,K),
	mangiap(NS,ped(A,B),_,_), 
	!,
	getcord(X1,Y1),
	mossamangia(K,P,NS,NNS,A,B,X1,Y1).
% In caso non si possa più mangiare ho già aggiornato la parte visuale, devo restituire true ripetendo il controllo.
mossamangia(_,_,S,NS,X,Y,A,B) :-
	mangiap(S,ped(X,Y),(A,B),NS).

mossamangia(K,P,S,NNS,X,Y,A,B) :-
	mangiap(S,dama(X,Y),(A,B),NS),
	aggiornamangia(dama(X,Y),A,B,P,K),
	mangiap(NS,dama(A,B),_,_),
	!,
	getcord(X1,Y1),
	mossamangia(K,P,NS,NNS,A,B,X1,Y1).

mossamangia(_,_,S,NS,X,Y,A,B) :-
	mangiap(S,dama(X,Y),(A,B),NS).

aggiornamangia(ped(X,Y),A,B,P,K):-
	retract(vped(K,point(X,Y),P)),
	assert(vped(K,point(A,B),P)),
	C is (A + X) / 2,
	D is (B + Y) / 2,
	del_vped(K,point(C,D)),
	move_vped(K,P,point(X,Y),point(A,B)).

aggiornamangia(dama(X,Y),A,B,P,K):-
	retract(vdama(K,point(X,Y),P)),
	assert(vdama(K,point(A,B),P)),
	C is (A + X) / 2,
	D is (B + Y) / 2,
	del_vped(K,point(C,D)),
	move_vdama(K,P,point(X,Y),point(A,B)).


getcord(X,Y) :-
	readln([X]),
	readln([Y]).



%METODI DA DEFINIRE PER MINIMAX

%trovamosse([sc(LB,LN,LV),red,play], PossibleMoves) :-	(bagof(NS,trovamangia(sc(LB,LN,LV),LB,NS),ListaMangia);identita([],ListaMangia)),
%	((length(ListaMangia,0),!, bagof(NSm,trovamuovi(sc(LB,LN,LV),LB,NSm),L));
%	(length(ListaMangia,1),!,identita(ListaMangia,L));
%	(getLongest(ListaMangia,L))),
%	leggimossered(L,PossibleMoves).
%
%trovamosse([sc(LB,LN,LV),blue,play], PossibleMoves) :- 	(bagof(NS,trovamangia(sc(LB,LN,LV),LN,NS),ListaMangia);identita([],ListaMangia)),
%	((length(ListaMangia,0),!, bagof(NSm,trovamuovi(sc(LB,LN,LV),LN,NSm),L));
%	(length(ListaMangia,1),!,identita(ListaMangia,L));
%	(getLongest(ListaMangia,L))),
%	leggimosseblue(L,PossibleMoves).

trovamosse([sc(LB,LN,LV),red,play], PossibleMoves) :-
	((bagof(NS,trovamangiadama(sc(LB,LN,LV),LB,NS),ListaMangia),!);
	(bagof(NS,trovamangiaped(sc(LB,LN,LV),LB,NS),ListaMangia),!);
	(bagof(NS,trovamuovi(sc(LB,LN,LV),LB,NS),ListaMangia))),
	getLongest(ListaMangia,L),
	leggimossered(L,PossibleMoves).

trovamosse([sc(LB,LN,LV),blue,play], PossibleMoves) :- 	
	((bagof(NS,trovamangiadama(sc(LB,LN,LV),LN,NS),ListaMangia),!);
	(bagof(NS,trovamangiaped(sc(LB,LN,LV),LN,NS),ListaMangia),!);
	(bagof(NS,trovamuovi(sc(LB,LN,LV),LN,NS),ListaMangia))),
	getLongest(ListaMangia,L),
	leggimosseblue(L,PossibleMoves).



%trovamangia(SC,L,NS):-
%	member(dama(A,B),L),
%	mangia(SC,dama(A,B),_,NS).
%trovamangia(SC,L,NS):-
%	member(ped(A,B),L),
%	mangia(SC,ped(A,B),_,NS).
%
trovamangiadama(SC,L,NS):-
	member(dama(A,B),L),
	mangia(SC,dama(A,B),_,NS).
trovamangiaped(SC,L,NS):-
	member(ped(A,B),L),
	mangia(SC,ped(A,B),_,NS).


trovamuovi(SC,L,NS):-
	member(X,L),
	muovi(SC,X,_,NS).

%getLongest(L,R):-
%	bagof(LV,member(sc(_,_,LV),L),Tmp),
%	get_lengths(Tmp,Lengths),
%	max_list(Lengths,Max),
%	get_proper_length(L,Max,R).

getLongest(L,R):-
	assert(lunghezza(0)),
	forall(member(sc(_,_,LV),L),
		(length(LV,N),lunghezza(Max),(
			Max < N, retract(lunghezza(_)), assert(lunghezza(N));
			true
		)
	)),
	lunghezza(X),
	retract(lunghezza(_)),
	get_proper_length(L,X,R).

get_proper_length([sc(A,B,LV)|C],N,[sc(A,B,LV)|L]):-
	length(LV,N),!,
	get_proper_length(C,N,L).
get_proper_length([_|C],N,L):-
	get_proper_length(C,N,L).

get_proper_length([sc(A,B,LV)],N,[sc(A,B,LV)]):-
	length(LV,N),!.
get_proper_length(_,_,[]).


get_lengths([L|C],[N|NL]) :-
	length(L,N),
	get_lengths(C,NL).
get_lengths([L],[N]) :-
	length(L,N).
get_lengths([],0).


leggimossered([SC|C],[[SC,blue,STATE]|L]):-
	((fine(SC,red),!,STATE = 'win');STATE = 'play'),
	leggimossered(C,L).
leggimossered([],[]).
leggimosseblue([SC|C],[[SC,red,STATE]|L]):-
	((fine(SC,blue),!,STATE = 'win');STATE = 'play'),
	leggimosseblue(C,L).
leggimosseblue([],[]).


utility([_,blue,win],-1000).

utility([_,red,win],1000).

utility([sc(LR,LB,LV),_,_],Score) :-
	length(LR,Uno),
	length(LB,Due),
	length(LV,Tre),
	Score is (Due - Uno) * Tre.





bestMove(Pos,[S,red,ST],_) :-
	trovamosse(Pos,[[S,_,ST]]),!.

bestMove(Pos,NextPos,D) :-
	minmax(max, Pos, NextPos, _,D, -100000, 100000).

bestMovetrace(Pos,[S,red,ST],_) :-
	trovamosse(Pos,[[S,_,ST]]),!.

bestMovetrace(Pos,NextPos,D) :-
	minmaxtrace(max, Pos, NextPos, _,D, -100000, 100000).


