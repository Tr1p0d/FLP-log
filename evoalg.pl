use_module(library(arithmetics, random, lists)).

% fact database
sqrSigma(1).
mean(0).
maxTime(200000).
maxIter(10).


% math functions
squareList([],[]).
squareList([X|XS], [X2|Y]) :- X2 is X*X, squareList(XS,Y).

sumList([N1],N1).
sumList([X,Y|XS],Z) :- F is X + Y, sumList([F|XS],Z),! .

addVectors([], [], []).
addVectors([X|XS], [Y|YS], [S|SS]) :- addVectors(XS, YS, SS), S is Y + X.

multVectors([], [], []).
multVectors([X|XS], [Y|YS], [S|SS]) :- addVectors(XS, YS, SS), S is Y * X.

rastrigin([],[]).
rastrigin([X|XS],[Y|YS]) :- 
  A is X*X,
  cos(2*pi*X,B1),
  B is 10*B1,
  Y is A - B + 10,
  rastrigin(XS, YS).
  

% sphere function
sphereFunction(X,Y) :- squareList(X,Z), sumList(Z, Y).

rastriginFunction(X,Y) :- rastrigin(X,Z), sumList(Z,Y).

% fitness function
fitnesse(X,Y) :- rastriginFunction(X,Y).
%fitnesse(X,Y) :- sphereFunction(X,Y).

normal(Mu, SigmaSqr, NormalRandom) :-
    % random(0, 2147483647, R1),
    % random(0, 2147483647, R2),
    NormalRandom is Mu + SigmaSqr * sqrt(-2 * log((random(2147483647) / 2147483647))) * 
        sin(6.28319 * (random(2147483647) / 2147483647)), !.

gaussian(Mu, SigmaSqr, NormalRandom) :-
    normal(Mu, SigmaSqr, NormalRandom), !.

% generates random vector of the same length as the input one
gaussianVector(_, _, [], []).
gaussianVector(Mu, SigmaSqr, [_|XS], [RN|OUT1] ) :- gaussian(Mu, SigmaSqr, RN), gaussianVector(Mu, SigmaSqr, XS, OUT1),!.

% here the sigma isnt just nuber but whole vector
gaussian2Vector(_, [], [], []).
gaussian2Vector(Mu, [S|SS], [_|XS], [R|RR]) :- gaussian(Mu, S, R), gaussian2Vector(Mu,SS,XS, RR).


muteTwoOneVector([],_,[]).
muteTwoOneVector([_|SS], Sigma0, [Y|YS]) :- gaussian(0,Sigma0, Z), pow(e, Z, Y), muteTwoOneVector(SS, Sigma0, YS).

% Sigma is vector as benefit it returns also the muted sigma
mute2Vector(X, Sigma, Z, Y) :- 
  sqrSigma(Sigma0), % the sigma0 is the constant sigma 
  mean(Mean0), % Mean0 is the constant Mu
  muteTwoOneVector(Sigma, Sigma0, Y), % gives me vector of e^(N(0,1))
  multVectors(Y, Sigma, Zp), % gives me the Sigmai*e^(N(0,1))
  gaussian2Vector(Mean0, Zp, X, SigmaMute), % gives random vector made with Sigmai from last step
  addVectors(X, SigmaMute, Z). % gives xi + N(0,Sigmai)

muteVector(X, Sigma, Y) :- gaussianVector(0,Sigma,X,R), addVectors(X,R,Y).

avarageCrossing([],[],_).
avarageCrossing([X|XS], [Y|YS], [S|SS]) :- avarageCrossing(XS,YS,SS), S is (X + Y) / 2.

discreteCrossing(_,[],[],[]).
discreteCrossing(C, [X|XS], [_|YS], [S|SS]) :- C > 0, random(-1.0, 1.0, NC), discreteCrossing(NC,XS,YS,SS),!, S is X.
discreteCrossing(_, [_|XS], [Y|YS], [S|SS]) :- random(-1.0, 1.0, NC), discreteCrossing(NC,XS,YS,SS),!, S is Y.

startTwoOneEvolutionStrategy(X) :-
  sqrSigma(Sigma),
  mean(Mean),
  gaussianVector(Mean,Sigma,X, SigmaV),
  fitnesse(X,FX), display(FX),nl,
  twooneEvolutionStrategy(X, SigmaV, 1).

startOneOneEvolutionStrategy(X) :- 
  sqrSigma(Sigma),
  fitnesse(X,FX), display(FX),nl,
  oneoneEvolutionStrategy(X, Sigma, 1, 0).

test1(X) :- pow(2,3,X).

twooneEvolutionStrategy(X,Sigma,Time) :- 
  maxTime(MaxTime),
  Time < MaxTime,
  mute2Vector(X,Sigma,Xp, NewSigma),
  fitnesse(Xp, FXp),
  fitnesse(X, FX),
  %display('generation : '), display(Time), display(' parent : '), display(FX), 
  %display(' offs : '), display(FXp), display(' sigma : '), display(Sigma),nl,
  NewTime is Time + 1,
  ((FXp < FX, % successful offspring
 %   display('got successfull offspring'),nl,
    %NewSuccessOffs is SuccessOffs + 1,
    NewX = Xp)
  ;
    %(NewSuccessOffs is SuccessOffs,
    NewX = X
  ),
  twooneEvolutionStrategy(NewX, NewSigma, NewTime).

twooneEvolutionStrategy(X,_,_) :- fitnesse(X, Y), display(Y),nl.

% one + one evolution strategy
% alter the sqrSigma changing period be setting the maxIter fact
oneoneEvolutionStrategy(X, Sigma, Time, SuccessOffs) :- 
  maxTime(MaxTime),
  Time < MaxTime,
  maxIter(IterMax),
  0 is (Time mod IterMax), 
  (((SuccessOffs/IterMax) < 0.2,
    NewSigma is Sigma*0.82)
    ;
    NewSigma is Sigma*1.22),
%  display('NewSigma : '), display(NewSigma),nl,
  NewSuccessOffs is 0,
  NewTime is Time + 1,
  oneoneEvolutionStrategy(X, NewSigma, NewTime, NewSuccessOffs),!.

oneoneEvolutionStrategy(X, Sigma, Time, SuccessOffs) :- 
  maxTime(MaxTime),
  Time < MaxTime,
  muteVector(X,Sigma,Xp),
  fitnesse(Xp, FXp),
  fitnesse(X, FX),
 % display('generation : '), display(Time), display(' parent : '), display(FX), 
 % display(' offs : '), display(FXp), display(' sigma : '), display(Sigma),nl,
  NewTime is Time + 1,
  ((FXp < FX, % successful offspring
 %   display('got successfull offspring'),nl,
    NewSuccessOffs is SuccessOffs + 1,
    NewX = Xp)
  ;
    (NewSuccessOffs is SuccessOffs,
    NewX = X)
  ),
  oneoneEvolutionStrategy(NewX, Sigma, NewTime, NewSuccessOffs).

oneoneEvolutionStrategy(X,_,_,_) :- fitnesse(X, Y), display(Y),nl.
  

  
  



