import Data.Char
import Control.Monad
--import qualified Control.Applicative as CA 
--import Prelude hiding ((>>=), return)
-- Edmund's parser
-- Monad = Parser
-- parse unwraps P and is the run function
-- deconstruct of Parser: parse p inp
-- Recall: Parser a -> [(data structure, String to parse)]
-- we use [] to enable failure/flexible return size
-- [how does [] as a monad work here?]

-- Unlike monad in 15 minutes where we were using [] we have
-- to do more work with Parser because it is not already a 
-- monad (like list is)

newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
    fmap = liftM
  {-  
    lifM f m = m >>= return . f
    liftM f m = do
        val <- m       -- extract the value from monad
        return (f val) -- return the value now with f applied
  -}

-- defines hoe to process Just (2*) <*> Just 2
-- aka fmpa applied to a binary function
-- For pure (2*) = Just (2*)
-- In this case the inside value is the data structure in
-- parser (data,String); v in (v,inp)
-- the context is P (\inp ->...) 
instance Applicative Parser where 
     pure  = return     -- is this a dixie chaining? no.
     (<*>) = ap         -- ap is used to reference what is
                        -- defined by "higher" Monad
  {-  
    ap mf mx = do
        f <- mf        -- extract the function e.g., (2*)
        x <- mx        -- extract the value
        return (f x)   -- return the value now with f applied
  -}

-- m = Parser; a = data structure
instance Monad Parser where
    return v = P (\inp -> [(v,inp)])
    p >>= f = P (\inp -> case parse p inp of
                      [(v,out)] -> parse (f v) out 
                      [] -> [])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- needs to work within parse
-- needs access to String
-- this is deconstruction
item :: Parser Char
item = P (\inp -> case inp of
                   []     -> []
                   (x:xs) -> [(x,xs)])
                       
failure :: Parser a
failure = P (\inp -> [])
 
-- test passed: parse item "hello"

-- This is join another way to think about Monad
-- TODO: compare to (>>=)
-- choice of parsers p (+++) q
-- first fails return second
-- will use item to parse
-- parse (item +++ return '1') "hello" = [('h',"ello")]
-- parse (failure +++ return '1') "hello" = [('1',"hello")]
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                      [] -> parse q inp
                      [(v,out)] -> [(v,out)])

-- tests a predicate before returning a Parser                      
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure
           
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many p
             return (v:vs)

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
                   
digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

space :: Parser ()
space = do many (sat isSpace)
           return ()

mplus :: Parser a -> Parser a -> Parser a
p `mplus` q = P (\inp -> case parse p inp of
                      [] -> parse q inp
                      [(v,out)] -> [(v,out)])

mzero :: Parser a
mzero = P (\inp -> [])