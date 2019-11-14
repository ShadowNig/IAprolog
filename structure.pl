%some util relations
position(X,[X|_], 0).
position(X,[_|L],Z) :- position(X,L,Y), Z is Y+1.

substitute(_,[],_,[]) :- !.
substitute(0,[_|L], V, [V|L]):- !.
substitute(I,[X|L], V, [X|K]) :- J is I-1, substitute(J,L,V,K).

swap(I,J,L1, L3) :- position(V1,L1,J),position(V2,L1,I),substitute(I,L1,V1,L2),substitute(J,L2,V2,L3).


%Now start the problem-specific relations
%A struct will be a list of 9 positions, with the numbers 1 to 8 and blank

neighboard(0,1).
neighboard(1,0).

neighboard(0,3).
neighboard(3,0).

neighboard(1,2).
neighboard(2,1).

neighboard(1,4).
neighboard(4,1).

neighboard(2,5).
neighboard(5,2).

neighboard(3,4).
neighboard(4,3).

neighboard(3,6).
neighboard(6,3).

neighboard(4,5).
neighboard(5,4).

neighboard(4,7).
neighboard(7,4).

neighboard(5,8).
neighboard(8,5).

neighboard(6,7).
neighboard(7,6).

neighboard(7,8).
neighboard(8,7).

next(L1,L2) :- position(blank,L1,I), neighboard(I,J), swap(I,J,L1,L2).

objective([1,2,3,4,5,6,7,8,blank]).

myWrite(blank) :- write(" "),!.
myWrite(X) :- write(X),!.

prettyPrint([]) :- write("\n").
prettyPrint([X,Y,Z|XS]) :- myWrite(X), write(" "), myWrite(Y),write(" "), myWrite(Z),write("\n"),prettyPrint(XS).

printSolution([]).
printSolution([T|TS]) :- prettyPrint(T), printSolution(TS).
correctDoublePosition(blank,[2,2]) :- !.
correctDoublePosition(X,[P1,P2]) :- number(X),W is X-1,P1 is div(W,3), P2 is mod(W,3).


%resolution algorithms

depthFirst(I,S,Sol) :-
		objective(I),
		reverse([I|S],Sol).
depthFirst(I,S,S1) :-
		not(objective(I)),
		next(I,N),
		not(member(N,S)),
		depthFirst(N,[I|S],S1).

isNotMember([],_,[]).
isNotMember([X|XS],L,[X|YS]) :-
    not(member(X,L)),
    isNotMember(XS,L,YS).
isNotMember([X|XS],L,L2) :-
    member(X,L),
    isNotMember(XS,L,L2).

putFront([],_,[]).
putFront([X|XS],L,[[X|L]|YS]) :-
         putFront(XS,L,YS).

breadthFirst([[X|XS]|_],Sol) :-
    objective(X),
    reverse([X|XS],Sol).
breadthFirst([[X|XS]|LS], Sol) :-
    not(objective(X)),
    findall(K,next(X,K),L),
    isNotMember(L,[X|XS],L2),
    putFront(L2,[X|XS],L3),
    append(LS,L3,L4),
    breadthFirst(L4,Sol).


manhatham([X|_],MX) :- 
			
			

findBetterManhatham([Unique],Unique,[]).
findBetterManhatham([X,Y|L],Bet,[Y|R]) :-
					length(X,LX),
					length(Y,LY),
					manhatham(X,MX),
					manhatham(Y,MY),
					RX is LX + MX,
					RY is LY + MY,
					RX > RY,
					findBetterManhatham([X|L],Bet,R).
findBetterManhatham([X,Y|L],Bet,[X|R]) :-
					length(X,LX),
					length(Y,LY),
					manhatham(X,MX),
					manhatham(Y,MY),
					RX is LX + MX,
					RY is LY + MY,
					RX =< RY,
					findBetterManhatham([Y|L],Bet,R).


aStarManhatham(Paths,Sol) :- 
						findBetterManhatham(Paths,Better,Rest),
						not(objective(Better)),
						expand(Better,Betters),
						append(Betters,Rest,L3),
						aStarManhatham(L3,Sol).
aStarManhatham(Paths,Sol) :-
						findBetterManhatham(Paths,Better,_),
						objective(Better),
						Sol is Better.




sol(I) :- depthFirst(I,[],S), printSolution(S),!.
sol2(I) :- breadthFirst([[I]], Sol),printSolution(Sol),!.
sol3(I) :- aStarManhatham([[[I],[]]],Sol),printSolution(Sol),!.
sol4(I) :- aStarMislead([[I]],Sol),printSolution(Sol),!.



