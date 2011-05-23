{-- Transform an expression to a respective conjunctive normal form --}

module CNF where

import Expr

-- assume expression is already in negation normal form?
cnf ::  Expression -> Expression
-- use de morgan's laws to push negations inwards onto atoms (not formulae)
cnf (Negation (Disjunction l r)) = (Conjunction (cnf (Negation l)) (cnf (Negation r)))
cnf (Negation (Conjunction l r)) = (Disjunction (cnf (Negation l)) (cnf (Negation r)))
cnf (Negation (Negation e)) = (cnf e)
-- distribute conjunctions over disjunctions
cnf (Disjunction (Conjunction l r) c) = (Conjunction (cnf (Disjunction l c)) (cnf (Disjunction r c)))
cnf (Disjunction c (Conjunction l r)) = (Conjunction (cnf (Disjunction l c)) (cnf (Disjunction r c)))
-- recurse inside disjunctions
cnf (Disjunction l r) = (Disjunction (cnf l) (cnf r))
-- recurse inside conjunctions
cnf (Conjunction l r) = (Conjunction (cnf l) (cnf r))
-- base cases
cnf Top = Top
cnf Bottom = Bottom
cnf e = e
