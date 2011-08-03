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
main = do { putStr "Enter a Proposition: ";
	    proposition <- getLine;
	    putStrLn (("Proposition '") ++ (read (show proposition)) ++ ("' evaluates to: ") ++ "")}
            -- (read (show (cnf (nnf proposition))))
	    -- putStrLn ("Proposition evaluates to: " ++ (show (nnf (read proposition))))}
