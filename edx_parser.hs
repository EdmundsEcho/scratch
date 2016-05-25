import Data.Char
import Control.Monad
--infixr 5 +++

newtype Parser a = P (String -> [(a,String)])

-- Basic parsers
failure :: Parser a
failure = \inp -> []

return :: a -> Parser a
return v = \inp -> [(v,inp)]

item :: Parser Char
item = \inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)]