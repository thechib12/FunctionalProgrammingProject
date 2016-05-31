import Data.Maybe


data Atom = A0 |A1 |A2 |B0 |B1 |B2 |C0 |C1 |D deriving (Eq, Show)
type Clause = (Atom, [ Atom ])
type Program = [Clause]
type Query = [Atom]
type Predicate = Atom Term
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


(<-) :: Substitution ->  
e (<-) (x,a) =







testProgram = [ (A0, []),
                (A1, []),
                (A2, []),
                (B0, [A0, A1]),
                (B1, [A1, A2]),
                (B2, [A1, A2, D]),
                (C0, [B0, B1]),
                (C1, [B0, B1, B2])
              ]
