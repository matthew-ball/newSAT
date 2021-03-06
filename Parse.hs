{-- Parser for newSAT --}

module Parse where

import Expr

-- this does not work (well would work as grabbing a list of variables I guess)
parse :: [Expression] -> [Expression]
parse [] = []
parse (Disjunction l r:xs) = parse (l:[]) ++ parse (r:[]) ++ parse xs
parse (Conjunction l r:xs) = parse (l:[]) ++ parse (r:[]) ++ parse xs
parse (Negation a:xs) = parse (a:[]) ++ parse xs
parse (Variable v:xs) = [Variable v] ++ parse xs
parse (x:xs) = [x] ++ parse xs