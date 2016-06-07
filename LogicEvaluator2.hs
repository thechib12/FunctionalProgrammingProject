import Data.Maybe
import Data.Either
import FPPrac.Trees
import Data.List

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
unify:: Atom -> Program -> [Substitution]
unify (Predicate p (Var x)) program
        | u /= []         = zip (repeat (Var x)) u
        | otherwise       = []
  where
    u = [ (Const y) | ((Predicate q (Const y)), atoms) <- program, p == q ]









getRHS :: Atom -> Program -> Maybe [Atom]
getRHS n []             = Nothing
getRHS n ((x,ys):xs)
        | n == x        = Just ys
        | otherwise     = getRHS n xs

hasEmptyList:: Program -> Bool
hasEmptyList [] = False
hasEmptyList (((Predicate p x), rules):xs)
        | rules == [] = True
        | otherwise = False

getRuleOfClause :: Clause -> [Atom]
getRuleOfClause (x,xs) = xs

getRulesOfProgram :: Program -> [[Atom]]
getRulesOfProgram xs = map (getRuleOfClause) xs


getTerminalConstants :: Program -> [[Atom]]
getTerminalConstants [] = []
getTerminalConstants (((Predicate p x), rules):xs)
        | rules == [] = [(Predicate p x)] : getTerminalConstants xs
        | otherwise   = getTerminalConstants xs



findRule:: Program -> Atom -> Program
findRule [] _ = []
findRule (((Predicate p x), rules):xs) (Predicate q z) = case z of
      Var u
        | q == p              -> [((Predicate p x), rules)] ++ ( findRule xs (Predicate q z))
        | otherwise           -> findRule xs (Predicate q z)

      Const a
        | q == p && x == z    -> [((Predicate p x), rules)] ++ (findRule xs (Predicate q z))
        | otherwise           -> findRule xs (Predicate q z)

evaluateAtom :: Program -> Atom -> Either [[Atom]] Bool
evaluateAtom prog atom = case atom of
      (Predicate p (Const a))
        | terminationrule == True   -> Right True
        | u == []                   -> Right False
        | otherwise                 -> Left v
          where
            v = getRulesOfProgram u

      (Predicate q (Var x))
        | terminationrule == True   -> Left w
        | u == []                   -> Right False
        | otherwise                 -> Left v
          where
            v = getRulesOfProgram u
            w = getTerminalConstants u
    where
      u = findRule prog atom
      terminationrule = hasEmptyList u


makeTree prog query atom = AtomNode Inter atom (map (makeTreeH prog) query)

makeTreeH prog atom = AtomNode Union atom v
  where
    u = findRule prog atom
    v = findRuleTree prog u

class PPTree a where
  ppTree :: a -> RoseTree

instance PPTree AtomTree where
  ppTree (AtomNode op atom xs) = RoseNode (show(atom) ++ show(op)) u
    where
      u = map (ppTree) xs


findRuleTree :: Program -> Program -> [AtomTree]
findRuleTree prog [] = []
findRuleTree prog ((x,xs):ys)  = [makeTree prog xs x] ++ (findRuleTree prog ys)

deriveConstants :: AtomTree -> [Term]
deriveConstants (AtomNode Union atom xs) = case atom of
      (Predicate p (Var x)) -> u
         where
           u = concat (map deriveConstants xs)
      (Predicate q (Const a)) -> [(Const a)]

deriveConstants (AtomNode Inter atom (xs)) = case atom of
      (Predicate p (Var y)) -> u
         where
           x = head xs
           z = tail xs

           u = foldl (intersect) (deriveConstants x) (map deriveConstants z)
      (Predicate q (Const a)) -> [(Const a)]


--
-- evalOne :: Program -> Query -> Either Bool [Substitution]
-- evalOne prog query
--       | checkforVar query == True   = Right (evalOneSub prog query)
--       | otherwise                   = Left (evalOneBool prog query)
--
-- evalOneBool prog (x:query)
--       | u /= []     =
--
--
--       | otherwise   = False
--     where
--       u = findRule prog x
--
-- boolHelper prog clause atom = case atom of
--   (Predicate p (Var x))
--     |
--
--   Predicate q (Const a)
--
--
--
-- evalOneSub prog query = [(Const "a",Var "X")]

-- expands a query, which returns for every atom in the query a list of expanded atoms.
expandQuery:: Program -> Program -> Query -> [[Atom]]
expandQuery prog prog2 query = map (expandAtom prog prog2) query

-- expands the atom. If the right LHS is found, then get the right RHS by expanding every atom.
expandAtom:: Program -> Program -> Atom -> [Atom]
expandAtom [] _ (Predicate q y) = []
expandAtom ((Predicate p x, xs):xxs) prog2 (Predicate q y) = case (Predicate p x) of
  (Predicate p' (Const x')) -> expandAtom xxs prog2 (Predicate q y)
  (Predicate p1 (Var x1))
    | p == q                -> ( (concat (expandQuery prog2 prog2 xs))) ++ (expandAtom xxs prog2   (Predicate q y))
    | otherwise             ->  expandAtom xxs prog2 (Predicate q y)


filterEmptyLists:: Program -> [Atom] -> [Atom]
filterEmptyLists _ [] = []
filterEmptyLists prog (x:xs)
    | expandedHasEmptyList  prog x = filterEmptyLists prog  xs
    | otherwise = [x] ++ filterEmptyLists prog  xs

expandedHasEmptyList:: Program -> Atom -> Bool
expandedHasEmptyList [] _ = False
expandedHasEmptyList ((Predicate p x, xs):xxs) (Predicate q y)
    | p == q && xs == [] = True
    | otherwise = expandedHasEmptyList xxs (Predicate q y)

-- getVarRHS:: [Atom] -> Program-> [Atom]
-- getVarRHS [] prog = []
-- getVarRHS (x:xs) prog = case x of
--   (Predicate p (Var x')) -> [x] ++ (getVarRHS (expandAtom prog prog xs) prog)
--   _                       -> [x] ++ (getVarRHS xs prog)

checkforVar :: Query -> Bool
checkforVar [] = False
checkforVar (x:xs) = case x of
      (Predicate p (Var y)) -> True
      (Predicate q (Const a)) -> checkforVar xs

-- Tests

substituteTest = (<==) (Var "X", Const "a") (Predicate A0 (Var "X"))

-- testUnify = unify (Predicate A0 (Var "X")) (Predicate A0 (Var "X"))

renameTest = renameClause ((Predicate A0 (Var "Y")), [(Predicate B0 (Var "X")),(Predicate B1 (Var "Y"))]) [(Var "X")]
--
-- testProgram = [((Predicate A0 (Const "b")),[]),
--               ((Predicate A0 (Const "a")),[]),
--               ((Predicate A0 (Const "c")),[]),
--               ((Predicate A1 (Const 0"a")),[]),
--               ((Predicate A1 (Const "b")),[]),
--               ((Predicate A2 (Var "X")),[(Predicate A0 (Var "X")),(Predicate A1 (Var "Y")), (Predicate A1 (Var "Y"))])]
-- testRHS = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
-- testQuery = [(Predicate A2 (Var "X")) ]
-- testSub = (Var "X", Const "a")
-- showQueryTree = showRoseTree (ppTree ( makeTree testProgram testQuery ( Predicate Begin ( Var "X"))))



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

-- b2 x -> [b1x, b0x] -> [b1a, a2x, a0a, a]
testRHS = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
query1 = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
query2 = [(Predicate B2 (Const "a"))]
query3 = [(Predicate B2 (Const "c"))]
query4 = [(Predicate B1 (Const "b"))]
query5 = [(Predicate B2 (Var "X"))]
query6 = [(Predicate B1 (Var "X")), (Predicate B2 (Var "X"))]
query7 = [(Predicate B0 (Var "X"))]
testSub = (Var "X", Const "a")
