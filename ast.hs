module Ast where

type Var   = String
data UnOp  = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving Eq
data BinOp = And | Or   | Xor  | Plus deriving Eq
data Exp   = Zero | One | Var Var | IfZ Exp Exp Exp | UOp UnOp Exp | BOp BinOp Exp Exp
           | Fold Exp Exp Var Var Exp
data Prog  = Lam Var Exp

size (Zero) = 1
size (One) = 1
size (Var _) = 1
size (IfZ e1 e2 e3) = 1 + size e1 + size e2 + size e3
size (UOp _ e1) = 1 + size e1
size (BOp _ e1 e2) = 1 + size e1 + size e2
size (Fold e1 e2 v1 v2 e3) = 2 + size e1 + size e2 + size e3

pprint :: Prog -> String
pprint (Lam x e) = "(lambda (" ++ x ++ ") " ++ ppexp e ++ ")"

ppexp Zero = "0"
ppexp One  = "1"
ppexp (Var x) = x
ppexp (IfZ e1 e2 e3) = "(if0 " ++ ppexp e1 ++ " " ++ ppexp e2 ++ " " ++ ppexp e3 ++ ")"
ppexp (UOp o e) = "(" ++ ppuop o ++ " " ++ ppexp e ++ ")"
ppexp (BOp o e1 e2) = "(" ++ ppbop o ++ " " ++ ppexp e1 ++ " " ++ ppexp e2 ++ ")"
ppexp (Fold e1 e2 v1 v2 e3) = "(fold " ++ ppexp e1 ++ " " ++ ppexp e2 ++ " (lambda (" ++ v1 ++ " " ++ v2 ++ ") " ++ ppexp e3 ++ "))"


ppuop Not   = "not"
ppuop Shl1  = "shl1"
ppuop Shr1  = "shr1"
ppuop Shr4  = "shr4"
ppuop Shr16 = "shr16"

ppbop Plus = "plus"
ppbop And  = "and"
ppbop Or   = "or"
ppbop Xor  = "xor"

instance Show UnOp  where show = ppuop
instance Show BinOp  where show = ppbop
instance Show Exp  where show = ppexp
instance Show Prog where 
  show = pprint
  showList progs = \s -> concatMap (\prog -> pprint prog ++ "\n") progs ++ s

-- oszukane ale nie umiem lepiej
-- niektóre elementy wyniku sa tylko zeby sie typowalo
optopr "not"  = (Not, Or, 1)
optopr "shl1" = (Shl1, Or,  1)
optopr "shr1" = (Shr1, Or, 1) 
optopr "shr4" = (Shr4, Or, 1)
optopr "shr16"= (Shr16, Or, 1)
optopr "plus" = (Not,Plus, 2) 
optopr "and"  = (Not,And, 2)
optopr "or"   = (Not,Or, 2) 
optopr "xor"  = (Not,Xor, 2)
optopr "if0"   = (Not,Or, 3)
optopr "fold"   = (Not,Or, 4)
optopr "tfold"  = (Not,Or, 5)
