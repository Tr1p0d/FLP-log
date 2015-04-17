use_module(library(arithmetics, random, lists)).

% fact database

sqrSigma(1).
mean(0).
maxTime(10).
maxIter(5).


% helper functions
squareList([], []).
squareList([X|XS], [Y|Z]) :- Y is X*X,squareList(XS,Z).

sumList([N1],N1).
sumList([], N1) :- N1 = 0.
sumList([X,Y|XS],Z) :- sumList([XS],Z), Z is Z + X + Y.

% sphere function
sumSquareList(X,Y) :- squareList(X,Z), sumList(Z, Y).

% fitness function
fitnesse(X,Y) :- sumSquareList(X,Y).

normal(Mu, SigmaSqr, NormalRandom) :-
    % random(0, 2147483647, R1),
    % random(0, 2147483647, R2),
    NormalRandom is Mu + SigmaSqr * sqrt(-2 * log((random(2147483647) / 2147483647))) * 
        sin(6.28319 * (random(2147483647) / 2147483647)), !.

gaussian(Mu, SigmaSqr, NormalRandom) :-
    normal(Mu, SigmaSqr, NormalRandom), !.


gaussianVector(_, _, [], _).
gaussianVector(Mu, SigmaSqr, [_|XS], [RN|OUT1] ) :- gaussian(Mu, SigmaSqr, RN), gaussianVector(Mu, SigmaSqr, XS, OUT1).

addVectors([], [], _).
addVectors([X|XS], [Y|YS], [S|SS]) :- addVectors(XS, YS, SS), S is Y + X.


muteVector(X, Sigma, Y) :- gaussianVector(0,Sigma,X,R), addVectors(X,R,Y).

avarageCrossing([],[],_).
avarageCrossing([X|XS], [Y|YS], [S|SS]) :- avarageCrossing(XS,YS,SS), S is (X + Y) / 2.

discreteCrossing(_,[],[],_).
discreteCrossing(C, [X|XS], [_|YS], [S|SS]) :- C > 0, random(-1.0, 1.0, NC), discreteCrossing(NC,XS,YS,SS),!, S is X.
discreteCrossing(_, [_|XS], [Y|YS], [S|SS]) :- random(-1.0, 1.0, NC), discreteCrossing(NC,XS,YS,SS),!, S is Y.


startEvolutionStrategy :- 
  sqrSigma(Sigma),
  mean(Mean),
  gaussianVector(Mean,Sigma,[1,2,3,4,5,6,7,8,9,0], X), 
  evolutionStrategy(X, Sigma, 0, 0).

test1(X) :- X =:= [1.234242432].

% one + one evolution strategy innerloop
evolutionStrategy(X, Sigma, Time, SuccessOffs) :- 
  maxTime(MaxTime),
  Time < MaxTime,
  muteVector(X,Sigma,Xp),
  fitnesse(Xp, FXp),
  fitnesse(X, FX),
  NewTime is Time + 1,
  ((FXp < FX, % successful offspring
	 NewSuccessOffs is SuccessOffs + 1,
	 NewX is Xp)
  ;
     (NewSuccessOffs is SuccessOffs,
	 NewX is X)),
  maxIter(IterMax),
  (
  ((Time mod IterMax) == 0,
  ((NewSuccessOffs/IterMax) < 0.2,
  NewSigma is Sigma*0.82;
  NewSigma is Sigma*1.22));
  NewSigma is Sigma
  ),
  evolutionStragegy(NewX, NewSigma, NewTime, NewSuccessOffs).

evolutionStrategy(X,_,_,_) :- write(X).
  

  
  



