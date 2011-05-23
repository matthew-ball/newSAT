{-- Transform an expression to a respective negation normal form --}

module NNF where

import Expr

nnf :: Expression -> Expression
-- remove equivalence
nnf (Bimplication l r) = (nnf (Conjunction (Implication l r) (Implication r l)))
-- remove implications
nnf (Implication l r) = (nnf (Disjunction (Negation l) r))
-- demorgan's transformations
nnf (Negation (Bimplication l r)) = (nnf (Negation (Conjunction (Implication l r) (Implication r l))))
nnf (Negation (Implication l r)) = (nnf (Negation (Disjunction (Negation l) r)))
nnf (Negation (Disjunction l r)) = (nnf (Conjunction (Negation l) (Negation r)))
nnf (Negation (Conjunction l r)) = (nnf (Disjunction (Negation l) (Negation r)))
-- remove double negations
nnf (Negation (Negation e)) = (nnf e)
-- recurse inside conjunctions and disjunctions
nnf (Conjunction l r) = (Conjunction (nnf l) (nnf r))
nnf (Disjunction l r) = (Disjunction (nnf l) (nnf r))
-- base cases
nnf Top = Top
nnf Bottom = Bottom
nnf e = e
