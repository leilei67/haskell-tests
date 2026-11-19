module Parser where

import Data.List
import Data.Maybe
import Data.Char
import Data.Map as Map
import Data.Set as Set

import Types
import Examples

-------------------------------------------------------------------

-- The following are given...

ops :: [Operator]
ops = ['+', '-', '*', '/', '^', '(', ')', '$']

opTable :: [(Operator, (Precedence, Associativity))]
opTable = [('$',(0,N)), ('(',(1,N)), (')',(1,N)), ('+',(6,L)),
           ('-',(6,L)), ('*',(7,L)), ('/',(7,L)), ('^',(8,R))]

stringToInt :: String -> Int
stringToInt = read

prettyExpr :: Expr -> String
prettyExpr (ENum n) = show n
prettyExpr (EVar s) = s
prettyExpr (EApp op e e') = "(" ++ prettyExpr e ++ [op] ++ prettyExpr e' ++ ")"

-------------------------------------------------------------------

precedence :: Operator -> Precedence
-- Pre: the operator has a binding in opTable
precedence op  = fst (opMap Map.! op)
    where
        opMap = Map.fromList opTable

associativity :: Operator -> Associativity
-- Pre: the operator has a binding in opTable
associativity op = snd (opMap Map.! op)
    where
        opMap = Map.fromList opTable

supersedes :: Operator -> Operator -> Bool
supersedes '(' _ = True
supersedes _ '(' = True
supersedes o1 o2 = (p1 > p2) || ((p1 == p2) && (associativity o1 == R))
    where
        p1 = precedence o1
        p2 = precedence o2

tokenise :: String -> [Token]
-- Pre: The input string is a well-formed expression
tokenise [] = []
tokenise xss@(x:xs) 
    | isSpace x = tokenise  xs
    | x `elem` ops = (TOp x) : tokenise xs
    | isDigit x = 
        let (h, t) = span isDigit xss
        in (TNum (read h)) : tokenise t
    | isAlpha x = 
        let (h, t) = span (\x -> (isDigit x) || (isAlpha x)) xss
        in (TVar h) : tokenise t


allVars :: String -> [String]
allVars xs = Set.toList $ Set.fromList [g y | y <- tks, f y]
    where
        tks = tokenise xs
        f (TVar _) = True
        f _ = False
        g (TVar z) = z
        g _ = ""

--
-- This function is given
--
parseExpr :: String -> Expr
-- Pre: The input string is a well-formed expression
parseExpr s = parse (tokenise s) [] ['$']

parse :: [Token] -> ExprStack -> OpStack -> Expr
-- Pre: The tokens and stacks collectively represent a
--      valid parser state.
parse [] (e:[]) _ = e
parse [] (e1:e2:es) (o:os) = parse [] ((EApp o e2 e1) : es) os 
parse ((TNum x):ts) ess oss@(o:os) = parse ts ((ENum x):ess) oss
parse ((TVar x):ts) ess oss@(o:os) = parse ts ((EVar x):ess) oss

parse ((TOp ')'):ts) ess ('(':os)
    =  parse ts ess os
parse tss@((TOp ')'):ts) ess@(e1:e2:es) (o:os) 
    = parse tss ((EApp o e2 e1):es) os 

parse ((TOp x):ts) ess oss@(o:os) =
                      if (supersedes x o) then 
                        parse ts ess (x:oss) 
                      else
                        -- evaluate an expression and
                        -- pop an operator off the opStac
                        let (e1:e2:es') = ess
                        in parse ts ((EApp o e2 e1):es') (x:os)
parse _ _ _ = ENum 0
