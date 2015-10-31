module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

listBinOp :: [(BinOp, Double -> Double -> Double)]
listBinOp = [(Add,(+)),(Mul,(*)),(Div,(/))]


listUnop = [(Neg, (*(-1))),(Sin, sin),(Cos, cos),(Log,log)]

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k env
  = head $ [value | (key,value)<-env,k==key]

eval :: Exp -> Env -> Double
eval (Val x) _
  = x
eval (Id x) env
  = lookUp x env
eval (UnApp uni x) env
  = (lookUp uni listUnop) $ eval x env
eval (BinApp bin x y) env
  = (lookUp bin listBinOp) (eval x env) (eval y env)



diff :: Exp -> String -> Exp
diff (Val _) _
  = Val 0
diff (Id x) sym
  | x == sym  = Val 1
  | otherwise = Val 0
diff (BinApp Add e1 e2) sym
  = (BinApp Add (diff e1 sym) (diff e2 sym))
diff (BinApp Mul e1 e2) sym
  = (BinApp Add (BinApp Mul e1 (diff e2 sym)) (BinApp Mul (diff e1 sym) e2))
diff (BinApp Div e1 e2) sym
  = (BinApp Div (BinApp Add (BinApp Mul (diff e1 sym) (e2)) (BinApp Mul e1 (diff e2 sym))) (BinApp Mul e2 e2))
diff (UnApp Sin u) sym
  = (BinApp Mul (UnApp Cos u) (diff u sym))
diff (UnApp Cos u) sym
  = (UnApp Neg (BinApp Mul (UnApp Sin u) (diff u sym)))
diff (UnApp Log u) sym
  = (BinApp Div (diff u sym) u)
diff (UnApp Neg u) sym
  = (UnApp Neg (diff u sym))

maclaurin :: Exp -> Double -> Int -> Double
maclaurin func val order
  = sum (zipWith3 f a b c)
    where
      a = [val^n | n <- [0..(order-1)]]
      b = iterate (flip diff "x") func
      c = map fromIntegral $ scanl (*) 1 [1..(order-1)]
      f value exp factor
        = eval (BinApp Div (BinApp Mul exp (Val value)) (Val factor)) [("x",0)]




bOp = [(Add,"+"),(Mul,"*"),(Div,"/")]

aOp = [(Neg, "-"),(Sin, "sin"),(Cos, "cos"),(Log,"log")]


showExp :: Exp -> String
showExp (Id x)
  = x
showExp (Val x)
  = show x
showExp (BinApp op e1 e2)
  = ('(':showExp e1)++(lookUp op bOp)++(showExp e2)++")"
showExp (UnApp op e1)
  = (lookUp op aOp)++('(':(showExp e1)++")")



---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))


-- > log(3*x^2+2)::Exp
--e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x"))))
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x"))) (Val 2.0))





----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

-- instance Num Exp where

-- instance Fractional Exp where

-- instance Floating Exp where


-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
