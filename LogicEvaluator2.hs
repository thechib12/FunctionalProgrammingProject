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
