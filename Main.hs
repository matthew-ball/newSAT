{-- Main file for newSAT --}

module Main where

import Expr
import NNF
import CNF
import DNF
import Parse
-- import DPLL

-- TODO: implement DPLL in Haskell

main :: IO ()
main = do { putStr "Enter an expression of propositional logic: ";
	    proposition <- getLine;
	    putStrLn (("The expression '") ++ (read (show proposition)) ++ ("' has the following models: ") ++ "")
            -- (read (show (cnf (nnf proposition))))
	    -- putStrLn ("Proposition evaluates to: " ++ (show (nnf (read proposition))) ++ "")
          }
