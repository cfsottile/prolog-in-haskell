human(cristian).
human(totoy).
human(maxi).
human(ruben).
humans(X,Y) :- human(X), human(Y).
humans4(X,Y,W,Z) :- humans(X,Y), humans(W,Z).
humans8(A,B,C,D,E,F,G,H) :- humans4(A,B,C,D), humans4(E,F,G,H).
humans16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- humans8(A,B,C,D,E,F,G,H), humans8(I,J,K,L,M,N,O,P).