negro(cristian).
negro(totoy).
negro(maxi).
negro(ruben).
negros(X,Y) :- negro(X), negro(Y).
negros4(X,Y,W,Z) :- negros(X,Y), negros(W,Z).
negros8(A,B,C,D,E,F,G,H) :- negros4(A,B,C,D), negros4(E,F,G,H).
negros16(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :- negros8(A,B,C,D,E,F,G,H), negros8(I,J,K,L,M,N,O,P).