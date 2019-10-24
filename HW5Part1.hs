


--int	::=	(any integer)	integers
 			
-- bool	::=	true   |   false	booleans
 			
-- reg	::=	A   |   B	register names
 			
--expr	::=	int	integer literal
-- |	bool	boolean literal
-- |	reg	load from register
-- |	expr + expr	integer addition
-- |	expr <= expr	less than or equal to
-- |	not expr	boolean negation
-- |	expr & expr	boolean conjunction
 			
--stmt	::=	reg := expr	store to register
-- |	if expr	conditional statement
--then prog	
--else prog	
-- end	
 			
-- prog	::=	ε  |  stmt ; prog	sequence of statements




-- First 

type Prog = [Stmt]

data Reg = A 
		 | B

data Expr =  Int Int 
          |  Bool 
          |  Reg 	
          |  Add Expr Expr  
		  |  LessThanEq Expr Expr 
          |  Not Expr 
          |  And Expr Expr
          deriving (Eq,Show)
          
   

data Stmt = Copy Reg Expr	
          | IfElse Prog Prog






-- second

Copy  AReg Int
Copy  BAdd AReg Int
[IfElse (LessThanEq AReg BReg )
		(Copy AReg AReg)
		(Copy BReg BReg)]

Copy Breg Add AReg BReg





-- Third

sumFiveOrLess :: [Int] -> Prog
sumFiveOrLess [] = Null 


 




 
 

