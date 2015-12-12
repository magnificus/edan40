module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement | While Expr.T Statement | Read Expr.T | Write Expr.T
	| Begin [Statement] | Skip
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skipStmt = accept "skip" # require ";" >-> buildSkip
--ifstatement = accept "if" -# Expr.T #-
buildSkip r = Skip

ifStmt = accept "if" -# Expr.parse #- require "then" # Expr.parse #- require "else" # Expr.parse >-> buildIf
buildIf((a,b),c) = If a b c

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
