append(nil,Ys1,Ys1).

append(cons(X2,Xs2),Ys2,Zs2) :- 
    append(Xs2,Ys2,Ws2),
    equal(cons(X2,Ws2),Zs2).

equal(nil,nil).

equal(cons(X3,Xs3),cons(X3,Ys3)) :-
    equal(Xs3,Ys3).


% append(cons(a,cons(b,nil)), cons(c,cons(d,nil)), Zs)
%    Zs:= cons(a,cons(b,cons(c,cons(d,nil))))

% append(cons(a,cons(b,nil)), Ys, cons(a,cons(b,cons(c,nil))))
%    Ys:= cons(c,nil)

   
