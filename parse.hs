import Data.Char (isDigit, isLower)
import Control.Monad

-- parser from scratch

type Parser a = String -> [(a,String)]
-- a could be Tree
{-
instance Functor Parser where
    fmap = liftM
    
instance Applicative Parser where
    pure v = (\inp -> [(v,inp)])
    (<*>) = ap
    
instance Monad Parser where
    return = pure
    p >>= f = (\s -> let (v,s') = p s in f v s') 
    fail _ = []
 -}   
return' :: a -> Parser a
return' v = \inp -> [(v,inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)]
    
parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

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
--sat p = do x <- item
-- x <- action
-- what is returned by item? [(x,xs)]
sat p = let (\x -> [(v,inp')]) = (\inp -> item inp) in
            if p v then return' v else failure
--[(v,inp)]
--type Parser a = String -> [(a,String)]
 {-         
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

char :: Char -> Parser Char
char = \x -> sat (==x)
-}