import Data.Char (isDigit, isLower)
import Control.Monad
import Control.Applicative

-- parser from scratch

data Parser a = P (String -> [(a,String)])
-- a could be Tree

instance Functor Parser where
    fmap = liftM
    
instance Applicative Parser where
    pure v = P(\inp -> [(v,inp)])
    (<*>) = ap
    
instance Monad Parser where
    return = pure
    p >>= f = P(\inp -> case parse p inp of
                             [] -> []
                             [(v,out)] -> parse (f v) out) 
   
return' :: a -> Parser a
return' v = P(\inp -> [(v,inp)])

failure :: Parser a
failure = P(\inp -> [])

item :: Parser Char
item = P(\inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)])
    
parse :: Parser a -> String -> [(a,String)]
parse (p inp) = p inp

(+++) :: Parser a -> Parser a -> Parser a
p+++q = \inp -> case parse p inp of
    [] -> parse q inp   -- if the first fails, try the second
    [(v,out)] -> [(v,out)] -- else, return value of first

{-
p :: Parser' (Char, Char)
p = do x <- item
       item
       y <- item
       return (x,y)
-}

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item 
           if p x then return x else failure
          
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

char :: Char -> Parser Char
char = \x -> sat (==x)