{-- A formal data structure and functions for expressions --}

module Expr (Expression(..), assignment, evaluate, interpretation, verify, models, variables) where

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

-- an environment maps variables to expressions
type Environment = [(String, Expression)]

-- return an environment which satisfies the expression
models :: Expression -> Environment
models expr = ("", expr) : []

-- verify an expression
verify :: Expression -> Bool
verify Top               = True
verify Bottom            = False
verify (Negation e)      = not(verify e)
verify (Conjunction l r) = (verify l) && (verify r)
verify (Disjunction l r) = (verify l) || (verify r)
verify _ = False

-- assign a variable a value under an environment
assignment :: Expression -> Expression -> Environment -> Environment
assignment (Variable var) expr env = (var, expr) : env
assignment _ expr env              = env

-- evaluate an expression with respect to an environment
evaluate :: Expression -> Environment -> Expression
evaluate Top env               = Top
evaluate Bottom env            = Bottom
evaluate _ []                  = Bottom
evaluate (Variable v) env      = interpretation (Variable v) env
evaluate (Negation e) env      = (Negation (evaluate e env))
evaluate (Conjunction l r) env = (Conjunction (evaluate l env) (evaluate r env))
evaluate (Disjunction l r) env = (Disjunction (evaluate l env) (evaluate r env))

-- return a list of variables in a single expression
variables :: Expression -> [Expression]
variables (Variable v)      = (Variable v) : []
variables (Conjunction l r) = variables l ++ variables r ++ []
variables (Disjunction l r) = variables l ++ variables r ++ []
variables (Negation e)      = variables e ++ []

-- return an interpretation value for a given variable
interpretation :: Expression -> Environment -> Expression
interpretation (Variable v) env =
    case env of
      []              -> Bottom
      (x:xs)
        | (fst x) == v -> snd x
        | otherwise   -> (interpretation (Variable v) xs)

