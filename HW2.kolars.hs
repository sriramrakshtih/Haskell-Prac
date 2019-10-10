module HW2 where

import HW1


-- | A string rendering of the expression in Reverse Polish Notation (RPN).
--
--   >>> toRPN (Lit 3)
--   "3"
--
--   >>> toRPN e1
--   "2 3 4 * +"
--
--   >>> toRPN e2
--   "7 6 + 5 *"
--
--   >>> toRPN e3
--   "3 2 * 5 4 * +"
--
--   >>> elem (toRPN e4) ["8 7 9 * + 6 +", "8 7 9 * 6 + +"]
--   True
--   
toRPN :: Expr -> String
toRPN (Lit x)   = show x
toRPN (Add x y) = toRPN x ++ " " ++ toRPN y ++ " +"
toRPN (Mul x y) = toRPN x ++ " " ++ toRPN y ++ " *"




-- | Convert a string rendering of an expression in RPN into an expression
--   represented as an abstract syntax tree. You can assume that your function
--   will only be given valid strings, i.e. it need not fail gracefully if it
--   encounters an error.
--
--   >>> fromRPN "3"
--   Lit 3
--
--   >>> fromRPN "2 3 +"
--   Add (Lit 2) (Lit 3)
--
--   >>> fromRPN "2 3 4 + +"
--   Add (Lit 2) (Add (Lit 3) (Lit 4))
--
--   >>> all (\e -> e == fromRPN (toRPN e)) [e1,e2,e3,e4]
--   True
--
fromRPN :: String -> Expr
fromRPN s = head (go [] (words s)) 
	where 
		go :: [Expr] -> [String] -> [Expr]
		go es 				[] = es
		go (e1 : e2 : e3) ("+" : ws) = go (Add e2 e1 : es) ws
		go (e1 : e2 : e3) ("*" : ws) = go (Mul e2 e1 : es) ws


