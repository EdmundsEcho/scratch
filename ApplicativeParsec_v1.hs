import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- -- Hide Parsec's definitions of some Applicative functions.
import Text.ParserCombinators.Parsec 
--import Data.Numeric

-- Every monad is an Applicative
-- instance Applicative (GenParser s a) where
--   pure = return
--   (<*>) = ap
--
-- -- Every MonadPlus is an Alternative
-- instance Alternative (GenParser s a) where
--   empty = mzero
--   (<|>) = mplus


-- this is the focus
-- p_query :: CharParser () [(String, Maybe String)]
-- p_query = pair `sepBy` char '&'
--   where pair = liftA2 (,) (many1 safe)
--                           (optional (char '=' *> many safe))
--         safe = oneOf urlBaseChars
--            <|> char '%' *> liftA2 diddle hexDigit hexDigit
--            <|> ' ' <$ char '+'
--         diddle a b = toEnum . fst . head . readHex $ [a,b]
--
-- urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"
--
-- p_headers :: CharParser st [(String, String)]
-- p_headers = header `manyTill` crlf
--   where header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
--         fieldName = liftA2 (:) letter (many fieldChar)
--         fieldChar = letter <|> digit <|> oneOf "-_"
--         contents = liftA2 (++) (many1 notEOL <* crlf)
--                                (continuation <|> pure [])
--         continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents
--
-- crlf :: CharParser st ()
-- crlf = (() <$ string "\r\n") <|> (() <$ newline)
--
-- notEOL :: CharParser st Char
-- notEOL = noneOf "\r\n"
--
--
-- --The sepBy combinator takes two parsers as its arguments. It applies the left parser, then the right, then
-- -- the first, accumulating the results of each left-hand parse in a list.
