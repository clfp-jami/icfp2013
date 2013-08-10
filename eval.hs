module Eval where

  import Data.Bits
  import Data.Word
  import Ast

  eval :: Prog -> Word64 -> Word64
  eval (Lam x e) n =
    evalExpr [(x, n)] e
    
  evalExpr env Zero = 0
  evalExpr env One  = 1
  evalExpr env (Var x) = let out (Just x) = x in out $ lookup x env
  evalExpr env (IfZ e1 e2 e3) =
    case evalExpr env e1 of
      0 -> evalExpr env e2
      _ -> evalExpr env e3
  evalExpr env (UOp uop e) = evUOp uop $ evalExpr env e
  evalExpr env (BOp bop e1 e2) = evBOp bop (evalExpr env e1) (evalExpr env e2)
  evalExpr env (Fold e1 e2 x y e3) = -- didn't actually test this guy!
    foldr (\vx vy -> evalExpr ((x, vx) : (y, vy) : env) e3) (evalExpr env e2) (splitBytes $ evalExpr env e1)
    where splitBytes n = map (\k -> (shiftR n (8 * k)) .&. 255) [7, 6 .. 0]
  
  evUOp Not   = complement
  evUOp Shl1  = \x -> shiftL x 1
  evUOp Shr1  = \x -> shiftR x 1
  evUOp Shr4  = \x -> shiftR x 4
  evUOp Shr16 = \x -> shiftR x 16
  
  evBOp Plus = (+)
  evBOp And  = (.&.)
  evBOp Or   = (.|.)
  evBOp Xor  = xor
