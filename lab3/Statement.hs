module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement | While Expr.T Statement | Read String | Write Expr.T
	| Compound [Statement] | Skip
    deriving Show

statement = assignment ! ifStmt ! skipStmt ! compoundStmt ! whileStmt ! readStmt ! writeStmt

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e
skipStmt = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

ifStmt = accept "if" -# Expr.parse #- require "then" # statement #- require "else" # statement >-> buildIf
buildIf((a,b),c) = If a b c

compoundStmt = accept "begin" -# iter statement #- require "end" >-> Compound

whileStmt = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
buildWhile (a,b) = While a b

readStmt = accept "read" -# word #- require ";" >-> Read

writeStmt = accept "write" -# Expr.parse #- require ";" >-> Write
	
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

exec [] _ _ = []

exec (Assignment a e:s) d i = exec s (Dictionary.insert (a, Expr.value e d) d) i

exec (If a thenStmts elseStmts: stmts) dict input = 
    if (Expr.value a dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
	
exec (While a e:s) d i = 
	if (Expr.value a d) > 0
	then exec ([e] ++ [While a e] ++ s) d i
	else exec s d i
	
exec (Read a:s) d (i:r) = exec s (Dictionary.insert (a, i) d) r

exec (Write a:s) d i = [Expr.value a d] ++ (exec s d i)	
	
exec (Compound a:s) d i = exec (a++s) d i
	
exec (Skip:s) d i = exec s d i

toString' :: Statement -> String
toString' Skip = "skip;\n"
toString' (Assignment a b)  = a ++ ":=" ++ Expr.toString b ++ ";\n"
toString' (While a b) = "while " ++ Expr.toString a ++ " do\n" ++ toString' b
toString' (Read a) = "read " ++ a ++ ";\n"
toString' (Write a) = "write " ++ Expr.toString a ++ ";\n"
toString' (Compound a) = "begin\n" ++ (foldr1 (++) $ map toString' a) ++ "end\n"
toString' (If a b c) = "if " ++ Expr.toString a ++ " then \n"  ++ toString' b ++ "else\n" ++ toString' c


instance Parse Statement where
  parse = statement
  toString = toString' 
