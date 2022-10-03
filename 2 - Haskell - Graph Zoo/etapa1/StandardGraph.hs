{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO1 ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO2 ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
-- Filtreaza un set de noduri/muchii dintr-un graf dupa o functie data.
filterGraphSet f setType graph = S.filter f $ setType graph

-- Filtreaza vecinii unui nod, in functie de pozitia acestuia in muchie (fst/snd).
filterNeighbors filterFunc node graph = filterGraphSet (\x -> filterFunc x == node) edges graph

-- Extrage din lista filtrata de vecini (cu filterNeighbors), nodurile de pe o anumita pozitie (fst/snd).
-- Daca vrem sa aflam nodurile de **out** -> selectam din fiecare muchie nodul **snd**
-- Daca vrem sa aflam nodurile de **in**  -> selectam din fiecare muchie nodul **fst**
findNeighbors mapFunc filterFunc node graph = S.map mapFunc filteredNeigh
    where filteredNeigh = filterNeighbors filterFunc node graph

outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = findNeighbors snd fst node graph
{-
    *** TODO3 ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = findNeighbors fst snd node graph

{-
    *** TODO4 ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
-- Daca nodul nu se gaseste in graf, se returneaza graful initial.
-- Altfel, se contruieste un nou graf, pentru care se filtreaza lista de noduri/muchii a grafului initial.
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph
    | not $ node `S.member` nodes graph = graph
    | otherwise = (filteredNodesSet, filteredEdgesSet)
        where
            filteredNodesSet = filterGraphSet (\x -> x /= node) nodes graph
            filteredEdgesSet = filterGraphSet (\x -> fst x /= node && snd x /= node) edges graph

{-
    *** TODO5 ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
-- Adauga elemente noi intr-un set existent de noduri/muchii
addInExistingSet setType graph newElems = S.union existingSet newElems
    where existingSet = setType graph

-- Imi creeaza un set de muchii, pe baza unei liste de noduri si vecinii de in/out a acestora.
createEdgesSet graph nodes ins outs = S.union newIns newOuts
    where
        newIns = S.cartesianProduct ins nodes
        newOuts = S.cartesianProduct nodes outs

-- newsSet          - set cu noile noduri
-- inNeigh/outNeigh - setul de vecini in/out a vechiului nod
-- newGraph         - graf fara vechiul nod
-- newNodes         - se adauga noile noduri in set-ul existent de noduri
-- newEdges         - se adauga niule muchii in set-ul existent de muchii
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = (newNodes, newEdges)
    where
        newsSet = S.fromList news
        inNeigh = inNeighbors old graph
        outNeigh = outNeighbors old graph
        newGraph = removeNode old graph
        newNodes = addInExistingSet nodes newGraph newsSet
        newEdges = addInExistingSet edges newGraph newEdgesSet
        newEdgesSet = createEdgesSet newGraph newsSet inNeigh outNeigh

        

{-
    *** TODO6 ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
-- Creeaza un set de vecini de un anumit tip dat (in/out) pentru un set de noduri dat.
getNeighbors neighborsType junkNodes graph = S.foldl (\acc x -> S.union x acc) S.empty neighborSets
    where neighborSets = S.map (\x -> neighborsType x graph) junkNodes

-- Elimina mai multe elemente dintr-o lista de moduri/muchii.
removeMultipleElems setType pos junkNodes graph = S.foldl (\acc x -> S.intersection x acc) (setType graph) newGraphs
    where newGraphs = S.map pos $ S.map (\x -> removeNode x graph) junkNodes

-- Elimina mai multe noduri si arcele acestora dintr-un graf.
removeMultipleNodes junkNodes graph = (newNodes, newEdges)
    where
        newNodes = removeMultipleElems nodes fst junkNodes graph
        newEdges = removeMultipleElems edges snd junkNodes graph

-- Inlocuieste un junk node cu un new node in cadrul unei muchii.
transformEdge (x, y) junkNodes newNode
    | x `S.member` junkNodes = (newNode, y)
    | y `S.member` junkNodes = (x, newNode)
    | otherwise = (x, y)

-- junkNodes        - set cu nodurile care respecta proprietatea
-- newNode          - un set care contine noul nod
-- inNeigh/outNeigh - toti vecinii de in/out ai nodurilor excluse
-- newGraph         - s-au exclus din graul initial junk nodes
-- newNodes         - se adauga noile noduri in setul existent
-- newEdges         - se adauga noile muchii in setul existent
-- newEdgesSet      - set format din noile muchii
-- filteredEdges    - se filtreaza muchiile care mai contin vre-un nod exclus
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph
    | S.difference (nodes graph) (nodes newGraph) == S.empty = graph
    | otherwise = (newNodes, newEdges)
    where
        junkNodes = S.filter prop (nodes graph)
        newNode = S.fromList [node]
        inNeigh = getNeighbors inNeighbors junkNodes graph
        outNeigh = getNeighbors outNeighbors junkNodes graph
        newGraph = removeMultipleNodes junkNodes graph
        newNodes = addInExistingSet nodes newGraph newNode
        newEdges = addInExistingSet edges newGraph filteredEdges
        newEdgesSet = createEdgesSet newGraph newNode inNeigh outNeigh
        filteredEdges = S.map (\edge -> transformEdge edge junkNodes node) newEdgesSet