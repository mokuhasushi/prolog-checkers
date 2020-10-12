 - - - - - - - DAMA ITALIANA - - - - - - -

Il progetto è costituito da 5 file: 
 - dama.pl : qui è definita la rappresentazione fondamentale del mondo, comprese le mosse eseguibili
 - visualdama.pl : qui è definita la parte grafica del progetto 
 - read_board.pl : qui è definita la traduzione dalla rappresentazione scelta alla grafica
 - minimaxab.pl : qui sono definiti i metodi principali per l'agente minimax, con alfabeta pruning
 - interface.pl : Molto corposo, contiene le procedure per giocare effettivamente, comprese la definizione di alcune regole (obbligo di cattura, fine del gioco)
 - scacchiere.pl : qui si trovano i vari problemi (e test) usati nel progetto

Per poter giocare si può: eseguire il comando "start(D)" che inizia una partita oppure caricare una tavola con "startgame(Nometavola,D)" dove in entrambi i casi D è la profondità cui si vuole cercare, fino a 7 termina, 8 richiede molto tempo. 

Attualmente l'euristica è molto banale, si limita a cercare posizioni in cui il numero di pezzi va a favore dell'agente, valutando di più una posizione con pochi pezzi (es: 3pz vs 1pz è una posizione con un valore più grande di 7pz vs 5pz) 

Durante il turno dell'utente (rosso) sarà data la possibilità di uscire (a), quindi saranno richiesti 4 numeri, rappresentanti linea - colonna del pezzo da muovere e linea - colonna di dove ci si vuole muovere. In caso sia possibile mangiare ma si provi a non farlo viene richiesto l'input. ATTENZIONE: NON SONO PRESENTI ULTERIORI CONTROLLI SULLE CATTURE PER QUANTO RIGUARDA L'UTENTE.

Sto attualmente cercando di debuggare un grosso problema in corrispondenza della settima linea, in quanto pare che l'agente non riconosca le pedine posizionate su questa, dando origine a problemi (si veda revss2 dopo le mosse (2-3)->(3-4), (3-4)->(5-2), (5-6)->(4-7)).

