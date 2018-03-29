import Prelude hiding (exp)
import Parse
import Data.Char (isAlpha)

data Prog = Prog [Eqn]
data Eqn = Eqn Name [Pat] Exp
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp
data Pat = PNil | PVar Name | PCons Name Name
type Name = String

name :: Parser Name
name = many1 (sat isAlpha)

--arg :: Parser arg
--arg = string "[]" .*. name .*. '(' exp ')'

prog :: Parser Prog
prog = many1 eqn

eqn :: Parser Eqn
eqn = name exp

exp :: Parser Exp
exp = name
