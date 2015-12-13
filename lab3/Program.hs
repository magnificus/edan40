module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) =  foldr1 (++) (map Statement.toString stmts)
             
exec (Program s) i = Statement.exec s Dictionary.empty i
