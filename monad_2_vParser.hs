import Data.Char (isAlpha, isDigit, ord, intToDigit)

-- Monad is a trifecta
-- M as Parser = list, State = String
type M a = State -> [(a,State)]
type State = String

unit :: a -> M a
unit a = \s -> [(a,s)]

bind :: M a -> (a -> M b) -> M b
(m `bind` k) s = [(b,s'')|(a,s') <- m s, (b,s'') <- k a s']
{-    \s -> let (a,s')  = m s
                       (b,s'') = k a s' 
                   in ( b,s'')
-}
                   
--tick :: M ()
--tick = \s -> ((), s + 1)

-- Parser
data Term = Con Int | Div Term Term deriving (Show)
{-A parser is unambiguous if for every input x
    the list of possible parses m x is either
    empty or has exactly one itme.  An ambiguous
    parser may return a list with two or more atlernative
    parsings.  -}
-- the parser
term :: M Term
term = (number `bind` \a ->
        unit (Con a))
       `merge`
       (lit '(' `bind` \_ ->
        term    `bind` \t ->
        lit '/' `bind` \_ ->
        term    `bind` \u ->
        lit ')' `bind` \_ ->
        unit (Div t u))
        
item :: M Char
item [] = []
item (a:s) = [(a,s)]

-- failure = zero
zero :: M a
zero = \s -> []


number :: M Int
number = 
          digit `bind` \a -> 
          iterate' digit `bind` \x ->
          unit (asNumber (a:x))


merge :: M a -> M a -> M a
(m `merge` n) x = m x ++ n x

-- returns the same as m, unless m fails
-- then n.  Enables us to rewrite iterate
-- as reiterate.
bias :: M a -> M a -> M a
(m `bias` n) x = case m x of
    [] -> n x 
    _  -> m x

iterate' :: M a -> M [a]
iterate' m = (m `bind` \a ->
              iterate' m `bind` \x ->
              unit(a:x)) `merge` unit []
{-many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many p
             return (v:vs) -}
reiterate :: M a -> M [a]
reiterate m = (m `bind` \a ->
              reiterate m `bind` \x -> 
              unit(a:x)) `bias` unit []
              
oneOrTwoItems :: M String
oneOrTwoItems = (item `bind` \a -> unit[a])
        `merge` (item `bind` \a -> 
                 item `bind` \b ->
                 unit [a,b])

-- Parser predicate
is :: M a -> (a -> Bool) -> M a
m `is` p = m `bind` \a ->
           if p a then unit a else zero
-- Parsers
letter :: M Char
letter = item `is` isAlpha

lit :: Char -> M Char
lit c = item `is` (\a -> (a==c))

digit :: M Int
digit = (item `is` isDigit) `bind` \a ->
        unit(ord a - ord '0')
        
asNumber = foldl addDigit 0
   where addDigit num d = 10*num + d

{- m * \a.n
   let a = m in n
   perform the computation m, bind it to a
   THEN perform computation n and 
   THEN return the result.
-}

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = eval t `bind` \a -> 
                 eval u `bind` \b ->
                 unit (a `div` b)


--lit 'm' "monad"  =>  [('m',"onad")]
--number "23 and more"  =>  [(23," and more"),(2,"3 and more")]
----- ambiguous
--reiterate digit "23 and more" => [([2,3]," and more")]

--oneOrTwoItems "monad" => [("m","onad"),("mo","nad")]

-- reiterate onOrTwoItems "mon" => [(["m","o","n"],""),(["m","on"],""),(["mo","n"],"")]

-- term "(4/2)"  =>  [(Div (Con 4) (Con 2),"")]