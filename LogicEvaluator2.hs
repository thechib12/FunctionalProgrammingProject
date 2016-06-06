import Data.Maybe
import Data.Either
import FPPrac.Trees

-- Data types
data Pred = A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D | Begin
            deriving (Eq, Show)

data Term = Var String
          | Const String
            deriving (Eq, Show)

data Atom = Predicate Pred Term
            deriving (Eq)

data AtomTree = AtomNode Op Atom [AtomTree]
            deriving (Show)

instance Show Atom where
  show (Predicate p t ) = show(p) ++ " " ++  show(t)

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
unify:: Atom -> Atom -> [Substitution]
unify (Predicate p (Const a)) (Predicate q (Const b)) = []
unify (Predicate p (Const a)) (Predicate q (Var x))
            | p == q                                  = [(Var x, Const a)]
            | otherwise                               = []
unify (Predicate p (Var x)) (Predicate q (Var y))
            | x == y && p == q                        = [(Var x, Var y)]
            | p == q                                  = [(Var x, Var y), (Var y, Var x)]
            | otherwise                               = []
unify (Predicate p (Var x)) (Predicate q (Const a))
            | p == q                                  = [(Var x, Const a)]
            | otherwise                               = []


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


makeTree prog query atom = AtomNode "&&" atom (map (makeTreeH prog) query)

makeTreeH prog atom = AtomNode "||" atom v
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


checkforVar :: Query -> Bool
checkforVar [] = False
checkforVar (x:xs) = case x of
      (Predicate p (Var y)) -> True
      (Predicate q (Const a)) -> checkforVar xs

-- Tests

substituteTest = (<==) (Var "X", Const "a") (Predicate A0 (Var "X"))

testUnify = unify (Predicate A0 (Var "X")) (Predicate A0 (Var "X"))

renameTest = renameClause ((Predicate A0 (Var "Y")), [(Predicate B0 (Var "X")),(Predicate B1 (Var "Y"))]) [(Var "X")]

testProgram = [((Predicate A0 (Const "b")),[]),
              ((Predicate A0 (Const "a")),[]),
              ((Predicate A0 (Const "c")),[]),
              ((Predicate A1 (Const "a")),[]),
              ((Predicate A1 (Const "b")),[]),
              ((Predicate A2 (Var "X")),[(Predicate A0 (Var "X")),(Predicate A1 (Var "X"))])]
testRHS = [(Predicate A1 (Var "X")), (Predicate A2 (Var "Y"))]
testQuery = [(Predicate A2 (Var "X")) ]
testSub = (Var "X", Const "a")
