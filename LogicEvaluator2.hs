import Data.Maybe
import Data.Either
import FPPrac.Trees
import Data.List
import Data.Function
-- Data types
data Pred = A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D | P | Q |R
            deriving (Eq, Show)

data Term = Var String
          | Const String
            deriving (Eq, Show)

data Atom = Predicate Pred Term
            deriving (Eq)

instance Ord Term where
  compare (Var x) (Var y)                                           = compare x y

instance Show Atom where
  show (Predicate p t )                                             = show(p) ++ " " ++  show(t)

-- Type declarations
type Op = String
type Clause = (Atom, [ Atom ])
type Program = [Clause]
type Query = [Atom]
type Substitution = (Term,Term)

-- Type class Substitute and substitute functions
class Substitute a where
  (<==) :: Substitution -> a -> a

instance Substitute Term where
  (<==) (Var y, Const a) (Var x)
                  | x == y                                          = (Const a)
                  | otherwise                                       = (Var x)
  (<==) (Var x,Const b) (Const a)                                   = (Const a)

instance Substitute Atom where
  (<==) sub (Predicate p t)                                         = (Predicate p ((<==) sub t))

substituteRHS :: Substitution -> [Atom] -> [Atom]
substituteRHS sub xs                                                = u
      where
        u = map ((<==) sub) xs

substituteClause :: Substitution -> Clause -> Clause
substituteClause sub (atom,atoms)                                   = (u,v)
    where
      u = (<==) sub atom
      v = map ((<==) sub) atoms


-- Rename functions
renameClause :: Clause -> [Term] -> Clause
renameClause ((Predicate a (Var t)),xs) ys
        | elem (Var t) ys == True                                   = ((Predicate a (Var (t ++ "1"))), (renameRHS xs ys))
        | otherwise                                                 = ((Predicate a (Var t) ), (renameRHS xs ys))

renameRHS :: [Atom] -> [Term] -> [Atom]
renameRHS [] ys = []
renameRHS ((Predicate a (Var t)):xs) ys
        | elem (Var t) ys == True                                   = [(Predicate a (Var (t ++ "1")))] ++ (renameRHS xs ys)
        | otherwise                                                 = [(Predicate a (Var t))] ++ (renameRHS xs ys)

-- Unifying functions
unify::  Program -> Atom -> [Substitution]
unify program (Predicate p (Var x))
        | u /= []         = zip (repeat (Var x)) u
        | otherwise       = []
  where
    u = [ (Const y) | ((Predicate q (Const y)), atoms) <- program, p == q ]

evalOne :: Program -> Query -> Either Bool [Substitution]
evalOne program query
    | checkforVar query == True         = Right (nub u)
    | otherwise                         = Left v
    where
      u = evalSub program query (expand program query)
      withVars = (replaceConst query 1)
      w = evalSub program withVars (expand program withVars)
      v = evalBool program (getSubs query 1) (w)

getSubs :: Query -> Int -> [Substitution]
getSubs [] number = []
getSubs ((Predicate p (Const a)):query) number = (Var ("X" ++ show(number)), Const a) : (getSubs query (number+1))

replaceConst :: Query -> Int -> Query
replaceConst [] number = []
replaceConst ((Predicate p (Const a)):query) number = (Predicate p (Var ("X" ++ show(number) ))) : (replaceConst query (number+1))

evalBool :: Program -> [Substitution] -> [Substitution] -> Bool
evalBool program [] w = True
evalBool program (s:sub) w
    | elem s w == True             = evalBool program sub w
    | otherwise                    = False

evalSub :: Program -> Query -> [Query] -> [Substitution]
evalSub program originalquery [] = []
evalSub program originalquery (q:queries)
    | w /= []                         = w ++ (evalSub program originalquery queries)
    | otherwise                       = evalSub program  originalquery queries
    where
      u = getRestVars originalquery (sortQueryVars $ filterVars q)
      v = intersectSubstitutions program q
      w = removeOtherVars (u) v

removeOtherVars :: [Term] -> [Substitution] ->  [Substitution]
removeOtherVars terms []                = []
removeOtherVars terms (s:subs)
    | elem (fst(s)) terms == True       = removeOtherVars terms subs
    | otherwise                         = s : (removeOtherVars terms (subs))

checkOtherVars :: [Substitution] -> [Term] -> Bool
checkOtherVars subs [] = True
checkOtherVars subs (v:vars) = case tv of
    Just x      -> (checkOtherVars subs vars)
    Nothing     -> False
  where
    tv = lookup v subs

getRestVars :: Query -> Query -> [Term]
getRestVars query expandedquery
    | u == []                                                             = []
    | otherwise                                                           = getRestVarsH query u
      where
        u = filter (notcontainedinQuery query) expandedquery
getRestVarsH :: Query -> Query -> [Term]
getRestVarsH query [] = []
getRestVarsH query expandedquery = [z] ++ (getRestVarsH w x)
  where
    w = [getVarTerm expandedquery] ++ query
    z = getTermofAtom $ getVarTerm expandedquery
    x = filter (  notcontainedinQuery w) expandedquery


getTermofAtom :: Atom -> Term
getTermofAtom (Predicate p t) = t

getVarTerm :: Query -> Atom
getVarTerm [] = error "Should not happen, getVar Term "
getVarTerm (q:query) = case q of
  (Predicate p (Var x)) -> (Predicate p (Var x))
  (Predicate q (Const a)) -> getVarTerm query

notcontainedinQuery :: Query -> Atom -> Bool
notcontainedinQuery [] (Predicate p t) = True
notcontainedinQuery ((Predicate p1 t1):query) (Predicate p t)
    | t == t1           = False
    | otherwise         = notcontainedinQuery query (Predicate p t)

getVarName :: Query -> String
getVarName [] = error "Should not happen, getVar Term"
getVarName (q:query) = case q of
    (Predicate p (Var x)) -> x
    (Predicate q (Const a)) -> getVarName query

intersectSubstitutions :: Program -> Query -> [Substitution]
intersectSubstitutions program query = v
  where
    u = unifyQueries (program) $ separate $ sortQueryVars $ filterVars query
    v = concat ( map (intersectVars) u)


sortQueryVars :: Query -> Query
sortQueryVars query = sortBy (compare `on` (\(Predicate p (Var x)) -> x)) query

separate :: Query -> [Query]
separate [] = []
separate ((Predicate p (Var x)):query) = [u] ++ (separate v)
  where
    u = (Predicate p (Var x)) : (takeWhile (equalAtomVar x) query)
    v = dropWhile (equalAtomVar x) query

equalAtomVar :: String -> Atom -> Bool
equalAtomVar x (Predicate _ (Var y)) = x == y

unifyQueries :: Program -> [Query] -> [[[Substitution]]]
unifyQueries program queries = map (map (unify program)) queries

intersectVars :: [[Substitution]]  -> [Substitution]
intersectVars (x:subs) = foldl (intersectBy (equalOrDifferentVar)) x subs

equalOrDifferentVar :: Substitution -> Substitution -> Bool
equalOrDifferentVar (Var x, Const a) (Var y, Const b)
    | x == y && a == b  = True
    | x /= y            = True
    | otherwise         = False

filterVars :: Query -> Query
filterVars [] = []
filterVars (q:query) = case q of
    (Predicate p (Var x)) -> q : (filterVars query)
    (Predicate q (Const a)) -> filterVars query

checkforVar :: Query -> Bool
checkforVar [] = False
checkforVar (x:xs) = case x of
    (Predicate p (Var y)) -> True
    (Predicate q (Const a)) -> checkforVar xs

paste :: Query -> [Query] -> [Query]
paste a [] = []
paste a (x:b) = [a ++ x] ++ (paste a b)

pasteAll :: [Query] -> [Query] -> [Query]
pasteAll [] b = []
pasteAll (x:a) b = (paste x b) ++ (pasteAll a b)

expand :: Program -> Query -> [Query]
expand program query = expandComplete program $ expandQuery program query

expandComplete :: Program -> [Query] -> [Query]
expandComplete program queries
    | u == v = u
    | otherwise  = expandComplete program u
     where
       u = concat $ map (expandQuery program) queries
       v = concat $ map (expandQuery program) u


expandQuery :: Program -> Query -> [[Atom]]
expandQuery program (x:query) = foldl (pasteAll) (expandAtom program x) (map (expandAtom program) query)

expandAtom :: Program -> Atom -> [[Atom]]
expandAtom program (Predicate p t)
    | v == []           = [[(Predicate p t)]]
    | otherwise         = v
      where
        u = [((Predicate q (Var x)),atoms) | ((Predicate q (Var x)),atoms) <- program, atoms /= [], p == q]
        w = map (refactorClause (Predicate p t)) u
        v = map (snd ) w

refactorClause :: Atom -> Clause -> Clause
refactorClause (Predicate p t ) (Predicate p2 t2, xs) = (Predicate p2 t, u)
  where
    u = map (refactorAtom (Predicate p t) (Predicate p2 t2)) xs

refactorAtom :: Atom -> Atom -> Atom -> Atom
refactorAtom (Predicate p t) (Predicate p1 t1) (Predicate p2 t2)
    | t1 == t2          = (Predicate p2 t)
    | otherwise         = (Predicate p2 t2)



-- Tests
testRestVars = (sortQueryVars $ filterVars [(Predicate A1 (Var "X")), (Predicate A1 (Var "Y"))])

substituteTest = (<==) (Var "X", Const "a") (Predicate A0 (Var "X"))

testUnify = unify testProgram (Predicate A0 (Var "X"))

renameTest = renameClause ((Predicate A0 (Var "Y")), [(Predicate B0 (Var "X")),(Predicate B1 (Var "Y"))]) [(Var "X")]

testProgram = [((Predicate A0 (Const "b")),[]),
              ((Predicate A0 (Const "a")),[]),
              ((Predicate A0 (Const "c")),[]),
              ((Predicate A1 (Const "a")),[]),
              ((Predicate A1 (Const "b")),[]),
              ((Predicate A2 (Var "X")),[(Predicate A0 (Var "Y")),(Predicate A0 (Var "X")),(Predicate B1 (Var "Y"))]),
              ((Predicate A2 (Var "X")),[(Predicate A0 (Var "X"))]),
              ((Predicate B0 (Var "X")),[(Predicate A1 (Var "X")), (Predicate A0 (Const "a"))]),
              ((Predicate B0 (Var "X")),[(Predicate A1 (Var "X"))]),
              ((Predicate B2 (Var "X")),[(Predicate B1 (Var "X")), (Predicate B0 (Var "X"))]),
              ((Predicate B1 (Const "a")),[])
              ]

sampleProgram = [((Predicate P (Const "a")),[]),
                ((Predicate P (Const "b")),[]),
                ((Predicate P (Const "c")),[]),
                ((Predicate Q (Const "a")),[]),
                ((Predicate Q (Const "b")),[]),
                ((Predicate R (Var "X")),[(Predicate P (Var "X")), (Predicate Q (Var "X"))])
                ]


testRHS = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
query1 = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
query2 = [(Predicate B2 (Const "a"))]
query3 = [(Predicate B2 (Const "c"))]
query4 = [(Predicate B1 (Const "b"))]
query5 = [(Predicate B2 (Var "X"))]
query8 = [(Predicate B0 (Var "Y"))]
query6 = [(Predicate B1 (Var "X")), (Predicate B2 (Var "X"))]
query7 = [(Predicate B0 (Var "X")), (Predicate B0 (Var "X"))]
testSub = (Var "X", Const "a")

sampleQuery1 = [(Predicate R (Const "a"))]
sampleQuery2 = [(Predicate R (Const "b"))]
sampleQuery3 = [(Predicate R (Const "c"))]
sampleQuery4 = [(Predicate R (Var "X"))]
