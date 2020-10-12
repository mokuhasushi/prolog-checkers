



%type([0,1,2,3,4,5,6,7]:cord).
type([(0,1),(0,3),(0,5),(0,7),(1,0),(1,2),(1,4),(1,6),(2,1),(2,3),(2,5),(2,7),(3,0),(3,2),(3,4),(3,6),(4,1),(4,3),(4,5),(4,7),(5,0),(5,2),(5,4),(5,6),(6,1),(6,3),(6,5),(6,7),(7,0),(7,2),(7,4),(7,6)]:cord).
type([ped(cord),dama(cord)]:pezzi).
type([p(cord,cord)]:pos).
type([rosso,blu]:giocatore).
type([sc(list(pezzi),list(pezzi),list(cord))]:scacchiera).


%Definizione dei predicati

pred(linkfw(cord,cord)).
pred(linkbw(cord,cord)).
pred(link(cord,cord)).
pred(link2fw(cord,cord)).
pred(link2bw(cord,cord)).
pred(link2(cord,cord)).
%Predicati per il collegamento di caselle.
%modo: (++,++)

pred(muovi(scacchiera,pezzi,cord,scacchiera)).
% muove un pezzo dalla sua posizione a cord. 
% modo(++,++,?,--)
% muovi(SC, ped/dama(X,Y),(A,B), NS)
% SC è la scacchiera iniziale, ped/dama(X,Y) è il pezzo da muovere, in posizione X,Y,
% (A,B) è la posizione in cui si vuole muovere e NS la scacchiera risultante
% Il metodo controlla che sia presente il pezzo da muovere e la posizione di arrivo sia valida

pred(mangia(scacchiera,pezzi,cord,scacchiera)).
% può essere usato sia per effettuare una cattura, sia per cercare le catture possibili.
% modo (++, ++, ?, --)
% mangia(SC, ped/dama(X,Y),(A,B), NS)
% SC è la scacchiera iniziale, ped/dama(X,Y) è il pezzo da muovere, in posizione X,Y,
% (A,B) è la posizione in cui si vuole muovere e NS la scacchiera risultante
% Il metodo controlla che il pezzo sia presente, che la posizione di arrivo sia vuota, e che sia
% presente un pezzo avversario tra le due caselle (inoltre impedisce alle pedine di mangiare dame)


%si connette in avanti. modo (++,--)
linkfw((X,Y),(A,B)) :-
	A is X + 1,
	(B is Y + 1;
	B is Y - 1).

%idem, indietro
linkbw((X,Y),(A,B)) :-
	A is X - 1,
	(B is Y + 1;
	B is Y - 1).

%idem,avanti o indietro
link((X,Y),(A,B)) :-
	linkfw((X,Y),(A,B));
	linkbw((X,Y),(A,B)).

%controlla che a,b sia a un salto di distanza da x,y, in avanti
link2fw((X,Y),(A,B)) :-
	S is A - X,
	D is B - Y,
	S = 2,
	AD is abs(D),
	AD = 2.

%idem, indietro
link2bw((X,Y),(A,B)) :-
	S is X - A,
	D is B - Y,
	S = 2,
	AD is abs(D),
	AD = 2.

%idem,avanti o indietro
link2((X,Y),(A,B)) :-
	link2fw((X,Y),(A,B));
	link2bw((X,Y),(A,B)).



%pedina bianca
muovi(sc(LB,LN,LV),ped(X,Y),(A,B),sc(NLB,LN,NLV)):-
	member(ped(X,Y),LB),
	member((A,B),LV),
	linkfw((X,Y),(A,B)),
	((A = 7,
	select(ped(X,Y),LB,dama(A,B),NLB),
	select((A,B),LV,(X,Y),NLV));
	(select(ped(X,Y),LB,ped(A,B),NLB),
	select((A,B),LV,(X,Y),NLV))).
%pedina nera
muovi(sc(LB,LN,LV),ped(X,Y),(A,B),sc(LB,NLN,NLV)):-
	member(ped(X,Y),LN),
	member((A,B),LV),
	linkbw((X,Y),(A,B)),
	((A = 0,
	select(ped(X,Y),LN,dama(A,B),NLN),
	select((A,B),LV,(X,Y),NLV));
	(select(ped(X,Y),LN,ped(A,B),NLN),
	select((A,B),LV,(X,Y),NLV))).
%dama bianca
muovi(sc(LB,LN,LV),dama(X,Y),(A,B),sc(NLB,LN,NLV)):-
	member(dama(X,Y),LB),
	member((A,B),LV),
	link((X,Y),(A,B)),
	select(dama(X,Y),LB,dama(A,B),NLB),
	select((A,B),LV,(X,Y),NLV).
%dama nera
muovi(sc(LB,LN,LV),dama(X,Y),(A,B),sc(LB,NLN,NLV)):-
	member(dama(X,Y),LN),
	member((A,B),LV),
	link((X,Y),(A,B)),
	select(dama(X,Y),LN,dama(A,B),NLN),
	select((A,B),LV,(X,Y),NLV).

identita(C,C).
%mangia! ped bianca
mangia(sc(LB,LN,LV),ped(X,Y),(A,B),NS):-
	member(ped(X,Y),LB),
	member((A,B),LV),
	link2fw((X,Y),(A,B)),
	X1 is (A + X) / 2,
	Y1 is (B + Y) / 2,
	(member(ped(X1,Y1),LN),
	select(ped(X1,Y1),LN, NLN)),
	((A = 7,
	select(ped(X,Y),LB,dama(A,B),NLB),
	select((A,B),LV,(X,Y),NLV));
	(select(ped(X,Y),LB,ped(A,B),NLB),
	select((A,B),LV,(X,Y),NLV))),
	((mangia(sc(NLB,NLN,[(X1,Y1)|NLV]),ped(A,B),(_,_),NS),!);
	identita(sc(NLB,NLN,[(X1,Y1)|NLV]),NS)).
% mangia ped nera
mangia(sc(LB,LN,LV),ped(X,Y),(A,B),NS):-
	member(ped(X,Y),LN),
	member((A,B),LV),
	link2bw((X,Y),(A,B)),
	X1 is (A + X) / 2,
	Y1 is (B + Y) / 2,
	(member(ped(X1,Y1),LB),
	select(ped(X1,Y1),LB, NLB)),
	((A = 0,
	select(ped(X,Y),LN,dama(A,B),NLN),
	select((A,B),LV,(X,Y),NLV));
	(select(ped(X,Y),LN,ped(A,B),NLN),
	select((A,B),LV,(X,Y),NLV))),
	((mangia(sc(NLB,NLN,[(X1,Y1)|NLV]),ped(A,B),(_,_),NS),!);
	identita(sc(NLB,NLN,[(X1,Y1)|NLV]),NS)).
%mangia dama bianca
mangia(sc(LB,LN,LV),dama(X,Y),(A,B),NS):-
	member(dama(X,Y),LB),
	member((A,B),LV),
	link2((X,Y),(A,B)),
	X1 is (A + X) / 2,
	Y1 is (B + Y) / 2,
	((member(ped(X1,Y1),LN),
	select(ped(X1,Y1),LN, NLN));
	(member(dama(X1,Y1),LN),
	select(dama(X1,Y1),LN,NLN))),
	select(dama(X,Y),LB,dama(A,B),NLB),
	select((A,B),LV,(X,Y),NLV),
	((mangia(sc(NLB,NLN,[(X1,Y1)|NLV]),dama(A,B),(_,_),NS),!);
	identita(sc(NLB,NLN,[(X1,Y1)|NLV]),NS)).
%mangia dama nera
mangia(sc(LB,LN,LV),dama(X,Y),(A,B),NS):-
	member(dama(X,Y),LN),
	member((A,B),LV),
	link2((X,Y),(A,B)),
	X1 is (A + X) / 2,
	Y1 is (B + Y) / 2,
	((member(ped(X1,Y1),LB),
	select(ped(X1,Y1),LB, NLB));
	(member(dama(X1,Y1),LB),
	select(dama(X1,Y1),LB,NLB))),
	select(dama(X,Y),LN,dama(A,B),NLN),
	select((A,B),LV,(X,Y),NLV),
	((mangia(sc(NLB,NLN,[(X1,Y1)|NLV]),dama(A,B),(_,_),NS),!);
	identita(sc(NLB,NLN,[(X1,Y1)|NLV]),NS)).
% PER IL GIOCATORE BISOGNA LASCIARE LA SCELTA DI COSA MANGIARE DOPO
mangiap(sc(LB,LN,LV),ped(X,Y),(A,B),NS):-
	member(ped(X,Y),LB),
	member((A,B),LV),
	link2fw((X,Y),(A,B)),
	X1 is (A + X) / 2,
	Y1 is (B + Y) / 2,
	(member(ped(X1,Y1),LN),
	select(ped(X1,Y1),LN, NLN)),
	((A = 7,
	select(ped(X,Y),LB,dama(A,B),NLB),
	select((A,B),LV,(X,Y),NLV));
	(select(ped(X,Y),LB,ped(A,B),NLB),
	select((A,B),LV,(X,Y),NLV))),
	identita(sc(NLB,NLN,[(X1,Y1)|NLV]),NS).
% mangia ped nera
mangiap(sc(LB,LN,LV),ped(X,Y),(A,B),NS):-
	member(ped(X,Y),LN),
	member((A,B),LV),
	link2bw((X,Y),(A,B)),
	X1 is (A + X) / 2,
	Y1 is (B + Y) / 2,
	(member(ped(X1,Y1),LB),
	select(ped(X1,Y1),LB, NLB)),
	((A = 0,
	select(ped(X,Y),LN,dama(A,B),NLN),
	select((A,B),LV,(X,Y),NLV));
	(select(ped(X,Y),LN,ped(A,B),NLN),
	select((A,B),LV,(X,Y),NLV))),
	identita(sc(NLB,NLN,[(X1,Y1)|NLV]),NS).
%mangia dama bianca
mangiap(sc(LB,LN,LV),dama(X,Y),(A,B),NS):-
	member(dama(X,Y),LB),
	member((A,B),LV),
	link2((X,Y),(A,B)),
	X1 is (A + X) / 2,
	Y1 is (B + Y) / 2,
	((member(ped(X1,Y1),LN),
	select(ped(X1,Y1),LN, NLN));
	(member(dama(X1,Y1),LN),
	select(dama(X1,Y1),LN,NLN))),
	select(dama(X,Y),LB,dama(A,B),NLB),
	select((A,B),LV,(X,Y),NLV),
	identita(sc(NLB,NLN,[(X1,Y1)|NLV]),NS).
%mangia dama nera
mangiap(sc(LB,LN,LV),dama(X,Y),(A,B),NS):-
	member(dama(X,Y),LN),
	member((A,B),LV),
	link2((X,Y),(A,B)),
	X1 is (A + X) / 2,
	Y1 is (B + Y) / 2,
	((member(ped(X1,Y1),LB),
	select(ped(X1,Y1),LB, NLB));
	(member(dama(X1,Y1),LB),
	select(dama(X1,Y1),LB,NLB))),
	select(dama(X,Y),LN,dama(A,B),NLN),
	select((A,B),LV,(X,Y),NLV),
	identita(sc(NLB,NLN,[(X1,Y1)|NLV]),NS).





	

