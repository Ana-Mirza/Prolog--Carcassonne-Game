:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').


%% TODO
% tile/2
% tile(Index, Tile)
%
% Fiecare soluție a predicatului tile este o corespondență între index
% (numărul piesei în lista din enunț) și reprezentarea internă a piesei
% respective.
%
% Puteți alege orice reprezentare doriți, în așa fel încât să puteți
% reprezenta toate piesele din enunț.
%
% Orice muchie a unei piese este o cetate, un drum, sau o pajiște.
% Pot exista cel mult 1 drum și cel mult 2 castele pe aceeași piesă.
%
% Reprezentarea trebuie să poată fi rotită (vezi predicatul ccw/3 mai
% jos) pentru a produce reprezentarea piesei rotite cu 90 de grade.
%
% Trebuie să definiți reprezentări pentru fiecare dintre cele 16 piese
% din enunțul temei.
%
% Exemplu: apelul tile(1, T1). trebuie să lege T1 la reprezentarea pe
% care o faceți pentru piesa 1. Această reprezentare poate fi transmisă
% celorlalte predicate din temă, pentru a întreba, de exemplu, ce se
% află pe muchia de nord a piesei 1, sau dacă piesa 1 se potrivește cu o
% altă piesă.
tile(1, (c, c, p, c, 1)).
tile(2, (c, c, d, c, 1)).
tile(3, (c, c, p, p, 1)).
tile(4, (c, c, p, p, 2)).
tile(5, (c, p, c, p, 2)).
tile(6, (c, p, c, p, 1)).
tile(7, (c, p, p, p, 1)).
tile(8, (c, c, d, d, 1)).
tile(9, (c, p, d, d, 1)).
tile(10, (c, d, d, p, 1)).
tile(11, (c, d, p, d, 1)).
tile(12, (c, d, d, d, 1)).
tile(13, (p, p, d, d, 1)).
tile(14, (p, d, p, d, 0)).
tile(15, (p, d, d, d, 0)).
tile(16, (d, d, d, d, 0)).



%% TODO
% at/3
% at(+Tile, +Direction, ?What)
%
% Predicatul este adevărat dacă pe piesa Tile are pe muchia de pe
% direcția Direction o entitate de tipul What.
%
% Directions este una dintre n, e, s, w (vezi predicatul directions/1
% din utils.pl).
%
% Entitatea (What) este una dintre c, d, sau p. reprezentând cetate,
% drum, sau pajiște.
%
% De exemplu, piesa 4 are cetate în nord și în este, și pajiște în sud
% și vest. Iar piesa 10 are cetate în nord, drum în este și sud, și
% pajiște în vest.
%
% Dacă What nu este legat, trebuie legat la entitatea care se află pe
% muchia din direcția Dir.
at((N, E, S, V, X), n, N).
at((N, E, S, V, X), e, E).
at((N, E, S, V, X), s, S).
at((N, E, S, V, X), w, V).



%% TODO
% atL/3
% atL(+Tile, +Directions, +What)
%
% Predicatul este adevărat dacă piesa Tile are entitatea what pe toate
% direcțiile din lista Directions, cu aceleași valori pentru entități și
% direcții ca și la predicatul at/3.
%
% De exemplu, predicatul este adevărat pentru reprezentarea piesei 1,
% pentru lista [w,n,e], și pentru entitatea c. Este adevărat de asemenea
% pentru reprezentarea piesei 14, pentru lista [e,w], și pentru
% entitatea d.
%
% Atenție! Pentru ca predicatul să fie adevărat, nu este nevoie ca în
% Directions să fie *toate* direcțiile pe care se află entitatea
% respectivă, pot fi doar o submulțime a acestora.
% De exemplu, la piesa 14, predicatul este adevărat pentru entitatea d
% și pentru oricare dintre listele [w], [e] sau [e,w].
atL(T, [], W).
atL(T, [D|R], W) :- at(T, D, W), atL(T, R, W).



%% TODO
% hasTwoCitadels/1
% hasTwoCitadels(+Tile)
%
% Predicatul întoarce adevărat dacă pe piesă există două cetăți diferite
% (ca în piesele 4 și 5).
hasTwoCitadels((N, E, S, V, X)) :- X > 1.



%% TODO
% ccw/3
% ccw(+Tile, +Rotation, -RotatedTile)
% Predicatul este adevărat dacă RotatedTile este reprezentarea piesei cu
% reprezentarea Tile, dar rotită de Rotation ori, în sens trigonometric.
%
% De exemplu, dacă T4 este reprezentarea piesei 4, atunci ccw(4, 1, R)
% va lega R la reprezentarea unei piese care are pajiște la nord și
% vest, și cetate la est și sud.
%
% Pentru piesele cu simetrie, reprezentarea unora dintre rotații este
% identică cu piesa inițială.
% De exemplu, la piesele 5, 6 și 14, rotirea cu Rotation=2 va duce la o
% reprezentare identică cu piesa inițială, și la fel rezultatele pentru
% Rotation=1 și Rotation=3 vor fi identice.
% La piesa 16, orice rotire trebuie să aibă aceeași reprezentare cu
% reprezentarea inițială.
ccw(T, 0, T).
ccw((N, E, S, V, X), Y, R) :- Y > 0, Y1 is Y - 1, ccw((E, S, V, N, X), Y1, R).



%% TODO
% rotations/2
% rotations(+Tile, -RotationPairs)
%
% Predicatul leagă RotationPairs la o listă de perechi
% (Rotation, RotatedTile)
% în care Rotation este un număr de rotații între 0 și 3 inclusiv și
% RotatedTile este reprezentarea piesei Tile rotită cu numărul respectiv
% de rotații.
%
% Rezultatul trebuie întotdeauna să conțină perechea (0, Tile).
%
% IMPORTANT:
% Rezultatul nu trebuie să conțină rotații duplicate. De exemplu, pentru
% piesele 5,6 și 14 rezultatul va conține doar 2 perechi, iar pentru
% piesa 16 rezultatul va conține o singură pereche.
%
% Folosiți recursivitate (nu meta-predicate).
rotationsHelper(Tile, N, []) :- ccw(Tile, N, Tile).
rotationsHelper(Tile, N, [H|T]) :- 
    N < 4, ccw(Tile, N, H1), 
    H = (N, H1), N1 is N + 1, 
    rotationsHelper(Tile, N1, T).

rotations(Tile, [H|T]) :- H = (0, Tile), rotationsHelper(Tile, 1, T), !.



%% TODO
% match/3
% match(+Tile, +NeighborTile, +NeighborDirection)
%
% Predicatul întoarce adevărat dacă NeighborTile poate fi pusă în
% direcția NeighborDirection față de Tile și se potrivește, adică muchia
% comună este de același fel.
%
% De exemplu, dacă T2 este reprezentarea piesei 2, iar T16 este
% reprezentarea piesei 16, atunci match(T2, T16, s) este adevărat.
%
% Similar, pentru piesele 8 și 10, este adevărat
% ccw(T8, 3, T8R), match(T8R, T10, w).
%
% Puteți folosi predicatul opposite/2 din utils.pl.
match(Tile, NeighborTile, NeighborDirection) :- 
    at(Tile, NeighborDirection, X), 
    opposite(NeighborDirection, N1),
    at(NeighborTile, N1, X).



%% TODO
% findRotation/3
% findRotation(+Tile, +Neighbors, -Rotation)
%
% Predicatul leagă Rotation la rotația (între 0 și 3 inclusiv) pentru
% piesa cu reprezentarea Tile, astfel încât piesa să se potrivească cu
% vecinii din Neighbors.
%
% Neighbors este o listă de perechi (NeighborTile, NeighborDirection) și
% specifică că pe direcția NeighborDirection se află piesa cu
% reprezentarea NeighborTile. Este posibil ca Neighbors să conțină mai
% puțin de 4 elemente.
%
% Se vor da toate soluțiile care duc la potrivire.
%
% De exemplu, pentru piesa 11, dacă la nord se află piesa 14 rotită o
% dată (drumul este vertical), iar la sud se află piesa 2 rotită de 2
% ori (drumul este spre nord), atunci posibilele rotații pentru piesa 11
% sunt 1 sau 3, deci findRotation trebuie să aibă 2 soluții, în care
% leagă R la 1, și la 3.
% În același exemplu, dacă am avea și piesa 1 ca vecin spre est, atunci
% soluția de mai sus s-ar reduce doar la rotația 3.
%
% Hint: Prolog face backtracking automat. Folosiți match/3.
findRotation(Tile, [], Rotation).
findRotation(Tile, [(Neighbor, Direction)|T], Rotation) :- 
    member(Rotation, [0, 1, 2, 3]), ccw(Tile, Rotation, T1),
    match(T1, Neighbor, Direction), findRotation(Tile, T, Rotation).



%%%%%%%%%%%%%%%%%%%%%%%%%% Etapa 2


%% TODO
% emptyBoard/1
% emptyBoard(-Board)
%
% Leagă Board la reprezentarea unei table goale de joc (nu a fost
% plasată încă nicio piesă).
emptyBoard([]).



%% TODO
% boardSet/4
% boardSet(+BoardIn, +Pos, +Tile, -BoardOut)
%
% Predicatul întoarce false dacă se încearcă plasarea unei piese pe o
% poziție pe care este deja o piesă, pe o poziție fără muchie comună
% cu o piesă existentă, sau într-un loc unde piesa nu se potrivește cu
% vecinii săi.
%
% Pentru o tablă goală, predicatul reușește întotdeauna, și poziția Pos
% devine singura de pe tablă.
%
% Poziția este dată ca un tuplu (X, Y).
boardSet([], Pos, Tile, [(Tile, Pos)]) :- !.
boardSet(BoardIn, Pos, Tile, BoardOut) :- canPlaceTile(BoardIn, Pos, Tile), BoardOut = [(Tile, Pos)|BoardIn].



%% TODO
% boardGet/3
% boardGet(+Board, +Pos, -Tile)
%
% Predicatul leagă Tile la reprezentarea piesei de pe tabla Board, de la
% poziția Pos. Poziția este dată ca un tuplu (X, Y).
%
% Dacă la poziția Pos nu este nicio piesă, predicatul eșuează.
boardGet([], Pos, Tile) :- !, false.
boardGet([(Tile, Pos)|Rest], Pos, Tile) :- !.
boardGet([(Dif_tile, Dif_pos)|Rest], Pos, Tile) :- boardGet(Rest, Pos, Tile).



%% TODO
% boardGetLimits/5
% boardGetLimits(+Board, -XMin, -Ymin, -XMax, -YMax)
%
% Predicatul leagă cele 4 argumente la coordonatele x-y extreme la
% care există piese pe tablă.
%
% Pentru o tablă goală, predicatul eșuează.
%
% Hint: max_list/2 și min_list/2
boardGetLimits([], _, _, _, _) :- !, false.
boardGetLimits(Board, XMin, YMin, XMax, YMax) :- 
    maplist([(_, _, Y), Y]>>true, Board, ListOfY), max_list(ListOfY, YMax),
    maplist([(_, _, Y2), Y2]>>true, Board, ListOfY2), min_list(ListOfY2, YMin),
    maplist([(_, X, _), X]>>true, Board, ListOfX), max_list(ListOfX, XMax),
    maplist([(_, X2, _), X2]>>true, Board, ListOfX2), min_list(ListOfX2, XMin).



%% TODO
% canPlaceTile/3
% canPlaceTile(+Board, +Pos, +Tile)
%
% Întoarce adevărat dacă este o mișcare validă plasarea piese Tile la
% poziția Pos pe tabla Board. Poziția este dată ca un tuplu (X, Y).
%
% O mișcare este validă dacă tabla este goală sau dacă:
% - poziția este liberă;
% - poziția este adiacentă (are o muchie comună) cu o piesă deja
% existentă pe tablă;
% - piesa se potrivește cu toți vecinii deja existenți pe tablă.
%
% Hint: neighbor/3 și directions/1 , ambele din utils.pl
canPlaceTile([], Pos, Tile) :- !.
canPlaceTile(Board, Pos, Tile) :-
    findall(I, (member((T0, I), Board), Pos == I), L), L == [],
    findall((Index, Dir), (member(Dir, [n, s, e, w]), neighbor(Pos, Dir, Index)), Neighbors), 
    findall((T, NeighborDir), (member((NeighborIndex, NeighborDir), Neighbors), member((T, NeighborIndex), Board)), List), List \== [], 
    forall(member((NeighborTile, NeighborDirection), List), match(Tile, NeighborTile, NeighborDirection)).



%% TODO
% getAvailablePositions/2
% getAvailablePositions(+Board, -Positions)
%
% Predicatul leagă Positions la o listă de perechi (X, Y)
% a tuturor pozițiilor de pe tabla Board unde se pot pune piese (poziții
% libere vecine pe o muchie cu piese existente pe tablă).
%
% Pentru o tablă goală, predicatul eșuează.
%
% Hint: between/3 (predefinit) și neighbor/3 din utils.pl
%
% Atenție! Și în afara limitelor curente există poziții disponibile.
getAvailablePositions([], Positions) :- !, false.
getAvailablePositions(Board, Positions) :-
    findall(NIndex, (member((Tile, Index), Board), member(Dir, [n, s, e, w]), neighbor(Index, Dir, NIndex), \+ member((_, NIndex), Board)), List),
    List \== [], sort(List, Positions).



% findPositionForTile/4
% findPositionForTile(+Board, +Tile, -Position, -Rotation)
%
% Predicatul are ca soluții toate posibilele plasări pe tabla Board ale
% piesei Tile, legând Position la o pereche (X, Y) care reprezintă
% poziția și Rotation la un număr între 0 și 3 inclusiv, reprezentând de
% câte ori trebuie rotită piesa ca să se potrivească.
%
% Unele piese se pot potrivi cu mai multe rotații pe aceeași poziție și
% acestea reprezintă soluții diferite ale predicatului, dar numai dacă
% rotațiile duc la rezultate diferite.
%
% Dacă tabla este goală, predicatul leagă Position la (0, 0) și Rotation
% la 0.
%
% De exemplu, dacă pe tablă se află doar piesa 11, la vest de ea piesa 9
% se potrivește cu rotația 1 sau 2 - două soluții diferite. Pentru
% plasarea la vest de piesa 11 a piesei 16 însă există o singură soluție
% - rotație 0.
%
% În ieșirea de la teste, rezultatele vor fi asamblate ca
% (X,Y):Rotation.
findPositionForTile([], Tile, (0, 0), 0).
findPositionForTile(Board, Tile, Position, Rotation) :- 
    getAvailablePositions(Board, Positions),
    member(Position, Positions), rotations(Tile, RotationList), member((Rotation, T), RotationList), canPlaceTile(Board, Position, T).
