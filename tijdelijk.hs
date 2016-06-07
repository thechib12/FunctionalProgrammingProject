import Data.Maybe
import Data.Either
import FPPrac.Trees
import Data.List
import Data.Function

-- Data types
data Pred = A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D | Begin
            deriving (Eq, Show)

data Term = Var String
          | Const String
            deriving (Eq, Show)

data Atom = Predicate Pred Term
            deriving (Eq)

data AtomTree = AtomNode OpType Atom [AtomTree]
            deriving (Show)

data OpType = Union
            | Inter

instance Ord Term where
  compare (Var x) (Var y) = compare x y

instance Show Atom where
  show (Predicate p t ) = show(p) ++ " " ++  show(t)

instance Show OpType where
  show (Union) = " u"
  show (Inter) = " n"

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
                  | x == y            = (Const a)
                  | otherwise         = (Var x)
  (<==) (Var x,Const b) (Const a)     = (Const a)

instance Substitute Atom where
  (<==) sub (Predicate p t)           = (Predicate p ((<==) sub t))

substituteQuery :: Substitution -> Query -> Query
substituteQuery sub xs                  = u
      where
        u = map ((<==) sub) xs

substituteClause :: Substitution -> Clause -> Clause
substituteClause sub (atom,atoms)     = (u,v)
    where
      u = (<==) sub atom
      v = map ((<==) sub) atoms


-- Rename functions
renameClause :: Clause -> [Term] -> Clause
renameClause ((Predicate a (Var t)),xs) ys
        | elem (Var t) ys == True       = ((Predicate a (Var (t ++ "1"))), (renameRHS xs ys))
        | otherwise                     = ((Predicate a (Var t) ), (renameRHS xs ys))

renameRHS :: [Atom] -> [Term] -> [Atom]
renameRHS [] ys = []
renameRHS ((Predicate a (Var t)):xs) ys
        | elem (Var t) ys == True       = [(Predicate a (Var (t ++ "1")))] ++ (renameRHS xs ys)
        | otherwise                     = [(Predicate a (Var t))] ++ (renameRHS xs ys)

-- Unifying functions
unify::  Program -> Atom -> [Substitution]
unify program (Predicate p (Var x))
        | u /= []         = zip (repeat (Var x)) u
        | otherwise       = []
  where
    u = [ (Const y) | ((Predicate q (Const y)), atoms) <- program, p == q ]


evaluator :: Program -> Query -> [Atom]
evaluator program [] = []
evaluator program (q:query)
    | u /= []               = []++(evaluator program query)
    | otherwise             = [q] ++ (evaluator program query)
      where
        u = evaluatorAtom program q

evaluatorAtom :: Program -> Atom -> [Atom]
evaluatorAtom program atom = [ btom | (btom,btoms)<- program, btom == atom , btoms ==[]]

--  voeg externe variabelen unification toe
evalBool :: Program -> Substitution -> Query -> [Query] -> Bool
evalBool program sub originalquery [] = False
evalBool program sub originalquery (q:querys)
  | elem sub w == True             = True
  | otherwise                       = evalBool program sub originalquery querys
    where
      u = getRestVars originalquery (sortQueryVars q)
      v = intersectSubstitutions program q
      w = removeOtherVars (u) v


evalSub :: Program -> Query -> [Query] -> [Substitution]
evalSub program originalquery [] = []
evalSub program originalquery (q:queries)
  | w /= []                         = w ++ (evalSub program originalquery queries)
  | otherwise                       = evalSub program  originalquery queries
  where
    u = getRestVars originalquery (sortQueryVars q)
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

getRestVars query expandedquery
    | dropWhile (equalAtomVar (getVarName query)) expandedquery == [] = []
    | otherwise   = getRestVarsH query expandedquery

getRestVarsH :: Query -> Query -> [Term]
getRestVarsH query [] = []
getRestVarsH query expandedquery = [w] ++ (getRestVarsH query v)
  where
    v = dropWhile (equalAtomVar (getVarName query)) expandedquery
    w = getVarTerm expandedquery

getVarTerm :: Query -> Term
getVarTerm [] = error "Should not happen"
getVarTerm (q:query) = case q of
  (Predicate p (Var x)) -> (Var x)
  (Predicate q (Const a)) -> getVarTerm query

getVarName :: Query -> String
getVarName [] = error "Should not happen"
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

notequalAtomVar :: String -> Atom -> Bool
notequalAtomVar x (Predicate _ (Var y)) = x /= y

unifyQueries :: Program -> [Query] -> [[[Substitution]]]
unifyQueries program queries = map (map (unify program)) queries

intersectVars :: [[Substitution]]  -> [Substitution]
intersectVars (x:subs) = foldl (intersectBy (equalOrDifferentVar)) x subs

equalOrDifferentVar :: Substitution -> Substitution -> Bool
equalOrDifferentVar (Var x, Const a) (Var y, Const b)
    | x == y && a == b  = True
    | x /= y            = True
    | otherwise         = False

-- separateVars :: Query -> [Query]


filterVars :: Query -> Query
filterVars [] = []
filterVars (q:query) = case q of
  (Predicate p (Var x)) -> q : (filterVars query)
  (Predicate q (Const a)) -> filterVars query



-- Tests

substituteTest = (<==) (Var "X", Const "a") (Predicate A0 (Var "X"))

testUnify = unify testProgram (Predicate A0 (Var "X"))

renameTest = renameClause ((Predicate A0 (Var "Y")), [(Predicate B0 (Var "X")),(Predicate B1 (Var "Y"))]) [(Var "X")]

testProgram = [((Predicate A0 (Const "b")),[]),
              ((Predicate A0 (Const "a")),[]),
              ((Predicate A0 (Const "c")),[]),
              ((Predicate A1 (Const "a")),[]),
              ((Predicate A1 (Const "b")),[]),
              ((Predicate A2 (Var "X")),[(Predicate A0 (Var "X")), (Predicate A1 (Var "Y")), (Predicate A1 (Var "Y"))]),
              ((Predicate B0 (Var "X")),[(Predicate A2 (Var "X")), (Predicate A0 (Const "a"))]),
              ((Predicate B0 (Var "X")),[(Predicate A1 (Var "X"))]),
              ((Predicate B2 (Var "X")),[(Predicate B1 (Var "X")), (Predicate B0 (Var "X"))]),
              ((Predicate B1 (Const "a")),[])

              ]
testRHS = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
query1 = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
query2 = [(Predicate B2 (Const "a"))]
query3 = [(Predicate B2 (Const "c"))]
query4 = [(Predicate B1 (Const "b"))]
query5 = [(Predicate B2 (Var "X"))]
query6 = [(Predicate B1 (Var "X")), (Predicate B2 (Var "X"))]
testSub = (Var "X", Const "a")
