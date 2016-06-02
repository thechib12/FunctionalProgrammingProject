import Data.Maybe

data Pred = A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D
            deriving (Eq, Show)

data Term = Var String
          | Const String
            deriving (Eq, Show)

data Atom = Predicate Pred Term
            deriving (Eq, Show)

type Clause = (Atom, [ Atom ])
type Program = [Clause]
type Query = [Atom]
type Substitution = (Term,Term)

predList = [(Var )]
class Substitute a where
  (<==) :: a -> Substitution -> a

instance Substitute Term where
  (<==) (Var x) (Var y, Const a)
                  | x == y          = (Const a)
                  | otherwise       = (Var x)
  (<==) (Const a) (Var x,Const b)   = (Const a)

instance Substitute Atom where
  (<==) (Predicate p t) (Var y, Const a) = (Predicate p ((<==) t (Var y, Const a)))

substituteTest = (<==) (Predicate A0 (Var "X")) (Var "X", Const "a")

rename :: Clause -> [Term] -> Clause
rename ((Predicate a (Var t)),xs) ys
        | elem (Var t) ys == True       = ((Predicate a (Var (t ++ "1"))), (renameRHS xs ys))
        | otherwise                     = ((Predicate a (Var t) ), (renameRHS xs ys))

renameRHS :: [Atom] -> [Term] -> [Atom]
renameRHS [] ys = []
renameRHS ((Predicate a (Var t)):xs) ys
        | elem (Var t) ys == True       = [(Predicate a (Var (t ++ "1")))] ++ (renameRHS xs ys)
        | otherwise                     = [(Predicate a (Var t))] ++ (renameRHS xs ys)

renameTest = rename ((Predicate A0 (Var "Y")), [(Predicate B0 (Var "X")),(Predicate B1 (Var "Y"))]) [(Var "X")]

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

testUnify = unify (Predicate A0 (Var "X")) (Predicate A0 (Var "X"))

evalOne :: Program -> Query -> Bool
evalOne prog [] = True
evalOne prog (a:query)
        | evalOneSingle prog a == False  = False
        | otherwise                      = evalOne prog query


getRHS :: Atom -> Program -> Maybe [Atom]
getRHS n []             = Nothing
getRHS n ((x,ys):xs)
        | n == x        = Just ys
        | otherwise     = getRHS n xs

evalOneSingle :: Program -> Atom -> Bool
evalOneSingle prog a = case ta of
      Nothing       -> False

      Just []       -> True

      Just xs       -> evalOne prog xs
    where
      ta = getRHS a prog

testProgram = [((Predicate A0 (Const "a")),[]),
              ((Predicate A0 (Const "b")),[]),
              ((Predicate A0 (Const "c")),[]),
              ((Predicate A1 (Const "a")),[]),
              ((Predicate A1 (Const "b")),[]),
              ((Predicate A2 (Const "a")),[(Predicate A0 (Const "a")),(Predicate A1 (Const "a"))])]
