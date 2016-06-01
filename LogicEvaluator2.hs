import Data.Maybe


data Atom = A0 | A1 | A2 | B0 | B1 | B2 | C0 | C1 | D | Predicate Term
            deriving (Eq, Show)
type Clause = (Atom, [ Atom ])
type Program = [Clause]
type Query = [Atom]

type Substitution = (Term,Term)

data Term = Var String
          | Const String

getRHS :: Atom -> Program -> Maybe [Atom]
getRHS n []             = Nothing
getRHS n ((x,ys):xs)
        | n == x        = Just ys
        | otherwise     = getRHS n xs

evalProp :: Program -> Query -> Bool
evalProp prog [] = True
evalProp prog (a:query)
        | evalPropSingle prog a == False = False
        | otherwise                      = evalProp prog query


evalPropSingle :: Program -> Atom -> Bool
evalPropSingle prog a = case ta of
        Nothing             -> False

        Just []             -> True

        Just xs             -> evalProp prog xs
        where
          ta = getRHS a prog


class Substitute a where
  (<-) :: a -> Substitution -> a

instance Substitute Term where
  (Var x)   (<-) (x,a)  = (Const a)
  (Const a) (<-) (x,a)  = (Const a)

instance Substitute
