{-- Transform an expression to a respective disjunctive normal form --}

module DNF where

import Expr

-- assume Expression is already in negation normal form?
dnf ::  Expression -> Expression
-- push conjunctions inside
dnf (Conjunction (Disjunction l r) c) = (Disjunction (Conjunction (dnf l) (dnf c)) (Conjunction (dnf r) (dnf c)))
dnf (Conjunction c (Disjunction l r)) = (Disjunction (Conjunction (dnf c) (dnf l)) (Conjunction (dnf c) (dnf r)))
-- recurse inside conjunctions
dnf (Conjunction l r) = (Conjunction (dnf l) (dnf r))
-- recurse inside disjunctions
dnf (Disjunction l r) = (Disjunction (dnf l) (dnf r))
-- remove double negations
dnf (Negation (Negation e)) = (dnf e)
-- push negations inside disjunctions and conjunctions
dnf (Negation (Conjunction l r)) = (Disjunction (Negation (dnf l)) (Negation (dnf r)))
dnf (Negation (Disjunction l r)) = (Conjunction (Negation (dnf l)) (Negation (dnf r)))
-- base cases
dnf Top = Top
dnf Bottom = Bottom
dnf e = e