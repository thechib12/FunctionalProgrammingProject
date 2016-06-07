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

substituteRHS :: Substitution -> [Atom] -> [Atom]
substituteRHS sub xs                  = u
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

evalOne :: Program -> Query -> Either Bool [Substitution]
evalOne program query
    | checkforVar query == True         = Right (nub u)
    | otherwise                         = Left v
    where
      u = evalSub program query (expand program query)
      replace = (replaceConst query 1)
      w = evalSub program replace (expand program replace)
      v = evalBool program (getSubs query 1) (w)

getSubs :: Query -> Int -> [Substitution]
getSubs [] number = []
getSubs ((Predicate p (Const a)):query) number = (Var ("X" ++ show(number)), Const a) : (getSubs query (number+1))

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
getVarTerm [] = error "Should not happen, getVar Term "
getVarTerm (q:query) = case q of
  (Predicate p (Var x)) -> (Var x)
  (Predicate q (Const a)) -> getVarTerm query

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

-- separateVars :: Query -> [Query]


filterVars :: Query -> Query
filterVars [] = []
filterVars (q:query) = case q of
  (Predicate p (Var x)) -> q : (filterVars query)
  (Predicate q (Const a)) -> filterVars query







-- getRHS :: Atom -> Program -> Maybe [Atom]
-- getRHS n []             = Nothing
-- getRHS n ((x,ys):xs)
--         | n == x        = Just ys
--         | otherwise     = getRHS n xs
--
-- hasEmptyList:: Program -> Bool
-- hasEmptyList [] = False
-- hasEmptyList (((Predicate p x), rules):xs)
--         | rules == [] = True
--         | otherwise = False
--
-- getRuleOfClause :: Clause -> [Atom]
-- getRuleOfClause (x,xs) = xs
--
-- getRulesOfProgram :: Program -> [[Atom]]
-- getRulesOfProgram xs = map (getRuleOfClause) xs
--
--
-- getTerminalConstants :: Program -> [[Atom]]
-- getTerminalConstants [] = []
-- getTerminalConstants (((Predicate p x), rules):xs)
--         | rules == [] = [(Predicate p x)] : getTerminalConstants xs
--         | otherwise   = getTerminalConstants xs
--
--
--
-- findRule:: Program -> Atom -> Program
-- findRule [] _ = []
-- findRule (((Predicate p x), rules):xs) (Predicate q z) = case z of
--       Var u
--         | q == p              -> [((Predicate p x), rules)] ++ ( findRule xs (Predicate q z))
--         | otherwise           -> findRule xs (Predicate q z)
--
--       Const a
--         | q == p && x == z    -> [((Predicate p x), rules)] ++ (findRule xs (Predicate q z))
--         | otherwise           -> findRule xs (Predicate q z)
--
-- evaluateAtom :: Program -> Atom -> Either [[Atom]] Bool
-- evaluateAtom prog atom = case atom of
--       (Predicate p (Const a))
--         | terminationrule == True   -> Right True
--         | u == []                   -> Right False
--         | otherwise                 -> Left v
--           where
--             v = getRulesOfProgram u
--
--       (Predicate q (Var x))
--         | terminationrule == True   -> Left w
--         | u == []                   -> Right False
--         | otherwise                 -> Left v
--           where
--             v = getRulesOfProgram u
--             w = getTerminalConstants u
--     where
--       u = findRule prog atom
--       terminationrule = hasEmptyList u
--
--
-- makeTree prog query atom = AtomNode Inter atom (map (makeTreeH prog) query)
--
-- makeTreeH prog atom = AtomNode Union atom v
--   where
--     u = findRule prog atom
--     v = findRuleTree prog u
--
-- class PPTree a where
--   ppTree :: a -> RoseTree
--
-- instance PPTree AtomTree where
--   ppTree (AtomNode op atom xs) = RoseNode (show(atom) ++ show(op)) u
--     where
--       u = map (ppTree) xs
--
--
-- findRuleTree :: Program -> Program -> [AtomTree]
-- findRuleTree prog [] = []
-- findRuleTree prog ((x,xs):ys)  = [makeTree prog xs x] ++ (findRuleTree prog ys)
--
-- deriveConstants :: AtomTree -> [Term]
-- deriveConstants (AtomNode Union atom xs) = case atom of
--       (Predicate p (Var x)) -> u
--          where
--            u = concat (map deriveConstants xs)
--       (Predicate q (Const a)) -> [(Const a)]
--
-- deriveConstants (AtomNode Inter atom (xs)) = case atom of
--       (Predicate p (Var y)) -> u
--          where
--            x = head xs
--            z = tail xs
--
--            u = foldl (intersect) (deriveConstants x) (map deriveConstants z)
--       (Predicate q (Const a)) -> [(Const a)]



-- expands a query, which returns for every atom in the query a list of expanded atoms.
-- expandQuery:: Program -> Program -> Query -> [[[Atom]]]
-- expandQuery prog prog2 query = map (expandAtom prog prog2) query
--
-- -- expands the atom. If the right LHS is found, then get the right RHS by expanding every atom.
-- -- expandAtom:: Program -> Program -> Atom -> [[Atom]]
-- expandAtom [] _ (Predicate q y) = []
-- expandAtom ((Predicate p x, xs):xxs) prog2 (Predicate q y) = case (Predicate p x) of
--   (Predicate p' (Const x'))
--     | p == q  -> [[(Predicate p' (Const x'))]] ++ (expandAtom xxs prog2 (Predicate q y))
--     | otherwise ->  (expandAtom xxs prog2 (Predicate q y))
--   (Predicate p1 (Var x1))
--     | p == q             ->   [(filterNonEmptyLists prog2 xs )] ++  (concat (expandQuery prog2 prog2 (filterEmptyLists prog2 xs))) ++ (expandAtom xxs prog2 (Predicate q y))
--     | otherwise          ->   expandAtom xxs prog2 (Predicate q y)
--
-- -- -- [(Predicate p (Const (show(p)++ show (q))))]++
-- filterEmptyLists:: Program -> [Atom] -> [Atom]
-- filterEmptyLists _ [] = []
-- filterEmptyLists prog (x:xs)
--     | expandedHasEmptyList  prog x = filterEmptyLists prog  xs
--     | otherwise = [x] ++ filterEmptyLists prog  xs
-- filterNonEmptyLists:: Program -> [Atom] -> [Atom]
-- filterNonEmptyLists _ [] = []
-- filterNonEmptyLists prog (x:xs)
--     | not (expandedHasEmptyList prog x) = filterNonEmptyLists prog  xs
--     | otherwise = [x] ++ filterNonEmptyLists prog  xs
--
-- expandedHasEmptyList:: Program -> Atom -> Bool
-- expandedHasEmptyList [] _ = False
-- expandedHasEmptyList ((Predicate p x, xs):xxs) (Predicate q y)
--     | p == q && xs == [] = True
--     | otherwise = expandedHasEmptyList xxs (Predicate q y)



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
expand program query = go program $ expandQueryv2 program query

go :: Program -> [Query] -> [Query]
go program queries
  | u == v = u
  | otherwise  = go program u
   where
     u = concat $ map (expandQueryv2 program) queries
     v = concat $ map (expandQueryv2 program) u



expandQueryv2 :: Program -> Query -> [[Atom]]
expandQueryv2 program []        = []
expandQueryv2 program (x:query) = foldl (pasteAll) (expandAtomv2 program x) (map (expandAtomv2 program) query)
-- expandQueryv2 program (x:query) = [(expandAtomv2 program x)] ++ (expandQueryv2 program query)

expandAtomv2 :: Program -> Atom -> [[Atom]]
expandAtomv2 program (Predicate p t)
    | u == []           = [[(Predicate p t)]]
    | otherwise         = u
      where
        u = [atoms | ((Predicate q (Var x)),atoms) <- program, atoms /= [], p == q]

refactorVarlist :: Term -> [[Atom]] -> [[Atom]]
refactorVarlist term atoms  = map (refactorVar term) atoms

refactorVar ::  Term -> [Atom] -> [Atom]
refactorVar term [] = []
refactorVar term (atom:atoms) = case atom of
      (Predicate p (Var x)) -> (Predicate p term) : (refactorVar term atoms)
      (Predicate p (Const a)) -> atom : (refactorVar term atoms)

-- Tests

substituteTest = (<==) (Var "X", Const "a") (Predicate A0 (Var "X"))

testUnify = unify testProgram (Predicate A0 (Var "X"))

renameTest = renameClause ((Predicate A0 (Var "Y")), [(Predicate B0 (Var "X")),(Predicate B1 (Var "Y"))]) [(Var "X")]

testProgram = [((Predicate A0 (Const "b")),[]),
              ((Predicate A0 (Const "a")),[]),
              ((Predicate A0 (Const "c")),[]),
              ((Predicate A1 (Const "a")),[]),
              ((Predicate A1 (Const "b")),[]),
              ((Predicate A2 (Var "X")),[(Predicate A1 (Var "X")) ]),
              ((Predicate A2 (Var "X")),[(Predicate A0 (Var "X")), (Predicate A1 (Var "Y")), (Predicate A1 (Var "Y"))]),
              ((Predicate B0 (Var "X")),[(Predicate A1 (Var "X")), (Predicate A0 (Const "a"))]),
              ((Predicate B0 (Var "X")),[(Predicate A1 (Var "X"))]),
              ((Predicate B2 (Var "X")),[(Predicate B1 (Var "X")), (Predicate B0 (Var "X"))]),
              ((Predicate B1 (Const "a")),[])

              ]

-- b2 x -> [b1x, b0x] -> [b1a, a2x, a0a, a]
testRHS = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
query1 = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
query2 = [(Predicate B2 (Const "a"))]
query3 = [(Predicate B2 (Const "c"))]
query4 = [(Predicate B1 (Const "b"))]
query5 = [(Predicate B2 (Var "X"))]
query8 = [(Predicate B0 (Var "X"))]
query6 = [(Predicate B1 (Var "X")), (Predicate B2 (Var "X"))]
query7 = [(Predicate B0 (Var "X")), (Predicate B0 (Var "X"))]
testSub = (Var "X", Const "a")
