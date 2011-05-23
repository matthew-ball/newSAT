{-- Define a data structure for an expression --}

module Expr (Expression(..), assignment, evaluate) where

-- data structure to represent (prefix) grammar of a formula (expression)
data Expression = Variable String
                | Top
                | Bottom
                | Conjunction Expression Expression
                | Disjunction Expression Expression
                | Implication Expression Expression
                | Bimplication Expression Expression
                | Negation Expression
                  deriving (Eq, Show, Read)

-- environment type mapping variables to values (expressions)
type Environment = [(String, Expression)]

-- an environment based evaluation function
assignment :: Environment -> Expression
-- assignment [] = error "Empty variable environment."
assignment [] = Bottom
assignment (x:xs) = snd x

-- evaluate an expression with respect to an environment
evaluate :: Expression -> Environment -> Expression
evaluate expr env = (assignment env)