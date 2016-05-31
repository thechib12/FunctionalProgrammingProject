data Atom = A0 |A1 |A2 |B0 |B1 |B2 |C0 |C1 |D deriving (Eq, Show)
type Clause = (Atom, [ Atom ])
type Program = [Clause]
type Query = [Atom]

evalProp :: Program -> Query -> Bool
evalProp prog quer = True
\\\adahsd