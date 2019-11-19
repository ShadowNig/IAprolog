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


manhatham([X|_],MX) :- objective(G), manhathamAux(X,G,MX).
manhathamAux([],[],0).
manhathamAux([X|L],[G1|GS],MX) :-
				manhathamAux(L,GS,MXR),
				correctDoublePosition(X,[A1,A2]),
				correctDoublePosition(G1,[B1,B2]),
				Diffx is A1-B1,
				Diffy is A2-B2,
				Absx is abs(Diffx),
				Absy is abs(Diffy),
				MX is MXR+Absx+Absy.
			
			

findBetterManhatham([Unique],Unique,[]) :- !.
findBetterManhatham([X,Y|L],Bet,[Y|R]) :-
					length(X,LX),
					length(Y,LY),
					manhatham(X,MX),
					manhatham(Y,MY),
					RX is -LX - MX,
					RY is -LY - MY,
					RX > RY,
					findBetterManhatham([X|L],Bet,R).
findBetterManhatham([X,Y|L],Bet,[X|R]) :-
					length(X,LX),
					length(Y,LY),
					manhatham(X,MX),
					manhatham(Y,MY),
					RX is -LX - MX,
					RY is -LY - MY,
					RX =< RY,
					findBetterManhatham([Y|L],Bet,R).

notMembers([],_,[]).
notMembers([X|XS],L,YS) :- member(X,L),notMembers(XS,L,YS).
notMembers([X|XS],L,[X|YS]) :- not(member(X,L)), notMembers(XS,L,YS).

appends([],_,[]).
appends([X|XS],L,[[X|L]|K]) :-
				appends(XS,L,K).

expand([Atual|BetterS],Betters) :-
					findall(X,next(Atual,X),L),
					notMembers(L,[Atual|BetterS],N),
					appends(N,[Atual|BetterS],Betters).
aStarManhatham(Paths,Sol) :-
						solutionIsHere(Paths,Sol).
aStarManhatham(Paths,Sol) :- 
						not(solutionIsHere(Paths,_)),
						findBetterManhatham(Paths,Better,Rest),
						expand(Better,Betters),
						append(Betters,Rest,L3),
						aStarManhatham(L3,Sol).

solutionIsHere([[X|XS]|_], L) :- objective(X),reverse([X|XS],L).
solutionIsHere([[X|_]|R], K) :-
				not(objective(X)),
				solutionIsHere(R,K).



aStarMislead(Paths,Sol) :-
						solutionIsHere(Paths,Sol).
aStarMislead(Paths,Sol) :- 
						not(solutionIsHere(Paths,_)),
						findBetterMislead(Paths,Better,Rest),
						expand(Better,Betters),
						append(Betters,Rest,L3),
						aStarMislead(L3,Sol).
mislead([X|_],MX) :- objective(G),misleadAux(X,G,MX).

misleadAux([],[],0).
misleadAux([X|Y],[X|Z],K) :- misleadAux(Y,Z,K).
misleadAux([X|Y],[Z|W],R) :- X \= Z, misleadAux(Y,W,K), R is K+1.
findBetterMislead([Unique],Unique,[]) :- !.
findBetterMislead([X,Y|L],Bet,[Y|R]) :-
					length(X,LX),
					length(Y,LY),
					mislead(X,MX),
					mislead(Y,MY),
					RX is -LX - MX,
					RY is -LY - MY,
					RX > RY,
					findBetterMislead([X|L],Bet,R).
findBetterMislead([X,Y|L],Bet,[X|R]) :-
					length(X,LX),
					length(Y,LY),
					mislead(X,MX),
					mislead(Y,MY),
					RX is -LX - MX,
					RY is -LY - MY,
					RX =< RY,
					findBetterMislead([Y|L],Bet,R).

expand([Init_Point,Node|Path],New_Paths):-
    findall([Aux,NewNode,Node|Path]),(sc(CInt,Node,NewNode),isNotMember(NewNode,[Node,Path]),Aux is Init + CInt),New_paths).

branch_bound(No,Path,Sol):-
    objective(No),
    reverse([No|Path],Sol).
branch_bound(No,Path2,Sol):-   
    %putFront(I,Path2,Sol2),
    %append(Path2,Sol2,New_Path),
    expand(Path2,New_Path),
    append(Path2,New_Path,Full_Path),
    depthFirst(Full_Path,[],Old_Path),
    branch_bound(Old_Path,Rec_Sol,Sol).


sol1(I) :- depthFirst(I,[],S), printSolution(S),!.
sol2(I) :- breadthFirst([[I]], Sol),printSolution(Sol),!.
sol3(I) :- aStarManhatham([[I]],Sol),printSolution(Sol),!.
sol4(I) :- aStarMislead([[I]],Sol),printSolution(Sol),!.
sol5(I) :- branch_bound(I,[],Sol),printSolution(Sol),!.


/*

treino : [1,2,3,4,5,6,7,8,blank]
	 [1,2,3,4,5,6,7,blank,8] <-- a busca em profundidade demora horrores e retorna uma
	                             uma solução muito diferente da ótima.
	 [1,8,2,blank,4,3,7,6,5] <-- a busca em profundidade nem volta.

Todos passam no 1 teste, mas a busca em profundidade falha miseravelmente
no segundo, exatamente por ser sensivel a ordem especifica com que as relações são escritas
e pelo alto tamanho potencial de uma descida a arvore. (algoritmo teimoso, demora
demais para perceber que esta errado)

A busca em largura e a A* com manhatham se comportam bem nesses casos, com um pequeno
ganho de velocidade na A*, embora tenham tido casos de teste que explodiram em consumo
de memória na busca em largura, exatamente pela necessidade desta de expandir todos os
niveis da arvore antes de continuar.

*/

