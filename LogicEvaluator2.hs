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

class Substitute a where
  (<==) :: a -> Substitution -> a

instance Substitute Term where
  (<==) (Var x) (Var y, Const a)
                  | x == y          = (Const a)
                  | otherwise       = (Var x)
  (<==) (Const a) (Var x,Const b)   = (Const a)

instance Substitute Atom where
  (<==) (Single p t) (Var y, Const a) = (Single p ((<==) t (Var y, Const a)))

-- rename :: Clause -> Clause
-- rename () =
