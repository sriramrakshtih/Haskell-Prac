--A := not 3;             { not can only be applied to booleans }
--B := 4 + true;          { can't add an integer and a boolean }
--A := true;              { registers may only contain integers }
--if 2+3 then else end;   { condition must be a boolean }

-- Fourth 


data Exp1 = Int
          | Reg
          | Exp1 + Exp1 

data Exp2 = Bool 
          | Exp2 <= Exp2 
          | not Exp2 
          | Exp2 & Exp2      


-- Five

--type Prog = [Expr]

data Expr = Int Int
          | Reg 
          | Add Expr Expr

data Expr = Bool 
          | LessThanEq Expr Expr 
          | Not Expr
          | And Expr Expr 
          deriving (Eq,Show)


