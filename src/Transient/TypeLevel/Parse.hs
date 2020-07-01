{-# LANGUAGE RebindableSyntax,OverloadedStrings,DataKinds, TypeOperators, FlexibleContexts, TypeFamilies  #-}

module Transient.TypeLevel.Parse(
-- * Setting the stream
setParseStream, setParseString, withParseString, withParseStream,
-- * parsing
string, tDropUntilToken, tTakeUntilToken, integer, hex, int, double, tChar,anyChar
manyTill, chainManyTill,between, symbol,parens, braces,angles,brackets,
semi, comma, dot,colon, sepBy, sepBy1, chainSepBy, chainSepBy1,chainMany,
commaSep, semiSep, commaSep1, dropSpaces,dropTillEndOfLine,
parseString, tTakeWhile,tTakeUntil, tTakeWhile', tTake, tDrop, tDropUntil, tPutStr,
isDone,dropUntilDone, 
-- * giving the parse string
withGetParseString, giveParseString,
-- * debug
notParsed, getParseBuffer,clearParseBuffer, showNext,
-- Composing parsing processes
(|-)
) where
import  Prelude hiding (return,mempty,(<*>),(>>),(>>=))
import Transient.TypeLevel.Effects
import Transient.Base(StreamData(..))
import qualified Data.ByteString.Lazy.Char8 as BS 
import qualified Transient.Parse as P
import Data.String(fromString)


data Parse  -- the Parse effect


-- | set a stream of strings to be parsed
setParseStream ::  T '[] '[] (StreamData BS.ByteString) -> T '[] '[Parse] ()
setParseStream (TR s)= TR $ P.setParseStream s

-- | set a string to be parsed
setParseString ::  BS.ByteString -> T '[] '[Parse] ()
setParseString = TR . P.setParseString

withParseString ::  BS.ByteString -> T req eff a -> T req (Parse :> eff) a
withParseString s (TR p) = TR $ P.withParseString s p

withParseStream :: T '[] '[] (StreamData BS.ByteString) -> T req eff a -> T req (Parse :> eff) a
withParseStream (TR st) (TR p)= TR $ P.withParseStream st p
    

-- | The parse context contains either the string to be parsed or a computation that gives an stream of
-- strings or both. First, the string is parsed. If it is empty, the stream is pulled for more.
-- data ParseContext str = IsString str => ParseContext (IO  (StreamData str)) str deriving Typeable


-- | succeed if read the string given as parameter
string :: BS.ByteString -> T '[Parse] '[Parse] BS.ByteString
string= TR . P.string

-- | fast search for a token.
-- If the token is not found, the parse is left in the original state.
tDropUntilToken :: BS.ByteString -> T '[Parse] '[Parse] ()
tDropUntilToken = TR . P.tDropUntilToken

tTakeUntilToken :: BS.ByteString -> T '[Parse]'[Parse] BS.ByteString
tTakeUntilToken= TR . P.tTakeUntilToken

-- | read an Integer
integer :: T '[Parse] '[Parse] Integer
integer = TR P.integer

-- | parse an hexadecimal number
hex ::  T '[Parse] '[Parse] Int
hex = TR P.hex

-- | read an Int
int :: T '[Parse] '[Parse] Int
int = TR P.int

-- | read a double in floating point/scientific notation
double :: T '[Parse] '[Parse] Double
double = TR  P.double

tChar :: T '[Parse] '[Parse] Char
tChar= TR  P.tChar

-- | read many results with a parser (at least one) until a `end` parser succeed.
manyTill :: T req eff a -> T req' eff' b -> T (Parse :> req :++ req') (Parse :> eff' :++ eff) [a]
manyTill (TR p) (TR end)= TR $ P.manyTill p end



chainManyTill   :: Monoid a 
                =>  (m -> a -> a) 
                -> T req eff m 
                -> T req' eff' t 
                -> T (Parse :> req :++ req') (Parse :> eff' :++ eff) a
chainManyTill op (TR p) (TR end)= TR $ P.chainManyTill op p end   

between :: T req   eff a1 
        -> T req'  eff' a2 
        -> T req'' eff'' b 
        -> T (Parse :> req :++ req' :++ req'') (Parse :> eff'' :++ eff' :++ eff) b
between (TR open) (TR close) (TR p) = TR $ P.between open close p

symbol :: BS.ByteString -> T '[Parse]'[Parse] BS.ByteString
symbol = TR . P.symbol 
         
parens p        = between (symbol "(") (symbol ")") p  
braces p        = between (symbol "{") (symbol "}") p  
angles p        = between (symbol "<") (symbol ">") p  
brackets p      = between (symbol "[") (symbol "]") p  

semi            = symbol ";" 
comma           = symbol "," 
dot             = symbol "." 
colon           = symbol ":" 



sepBy p sep     = sepBy1 p sep <|> return []


sepBy1 = chainSepBy1 (:) 


chainSepBy chain p sep= chainSepBy1 chain p sep <|> return mempty

-- take a byteString of elements separated by a separator and  apply the desired operator to the parsed results
chainSepBy1
  :: (Monoid b) 
  => (a -> b -> b) 
  -> T req eff a 
  -> T req' eff' x 
  -> T (Parse :> req :++ req' ) (Parse :> eff' :++ eff) b
chainSepBy1 chain (TR p) (TR sep)= TR $ P.chainSepBy1 chain p sep

-- chainMany :: Monoid a1 =>
--    (a2 -> a1 -> a1) -> T req eff a2 -> T (Parse :> req)(Parse :> eff) a1
chainMany chain v= (chain <$> v <*> chainMany chain v) <|> (return mempty)
      
-- >>> :t  (,) <$>

commaSep p      = sepBy p comma
semiSep p       = sepBy p semi

commaSep1 p     = sepBy1 p comma
semiSep1 p      = sepBy1 p semi

dropSpaces:: T '[Parse] '[Parse]()
dropSpaces=  TR P.dropSpaces

dropTillEndOfLine= withGetParseString $ \str -> return ((),BS.dropWhile ( /= '\n') str) 


parseString :: T '[Parse]'[Parse] BS.ByteString
parseString= TR P.parseString


-- | take characters while they meet the condition. if no char matches, it returns empty
tTakeWhile :: (Char -> Bool) -> T '[Parse] '[Parse] BS.ByteString
tTakeWhile = TR . P.tTakeWhile
   
-- | take characters while they meet the condition and drop the next character
tTakeWhile' :: (Char -> Bool) -> T '[Parse] '[Parse] BS.ByteString
tTakeWhile' = TR . P.tTakeWhile'
 

-- | take n characters 
tTake n= withGetParseString $ \s ->  return $ BS.splitAt n s  

-- | drop n characters
tDrop n= withGetParseString $ \s ->  return $ ((),BS.drop n s)

-- | read a char. If there is no input left it fails with empty
anyChar ::  T '[Parse] '[Parse] Char
anyChar= TR  P.anyChar

-- | verify that the next character is the one expected
tChar :: Char -> T '[Parse] '[Parse] Char
tChar = TR . P.tChar

withGetParseString ::   (BS.ByteString -> T req eff (a,BS.ByteString)) -> T (Parse :> req) (Parse :> eff) a
withGetParseString  p= TR $ P.withGetParseString $ \s->let TR r= p s in r
-- | bring the data of the parse context as a lazy byteString
giveParseString :: T '[Parse] '[Parse] BS.ByteString
giveParseString = TR  P.giveParseString

tDropUntil ::  (BS.ByteString -> Bool) -> T '[Parse] '[Parse] ()
tDropUntil = TR . P.tDropUntil

tTakeUntil ::  (BS.ByteString -> Bool) -> T '[Parse] '[Parse] BS.ByteString
tTakeUntil = TR . P.tTakeUntil

tPutStr :: String -> T '[Parse] '[Parse] ()
tPutStr= TR . P.TPut

-- | True if the stream has finished
isDone :: T '[Parse]'[Parse] Bool
isDone= TR P.isDone

dropUntilDone :: T '[Parse] '[] Bool
dropUntilDone= TR P.isDone
               

-- | return the portion of the string not parsed
-- it is useful for testing purposes:
--
-- >  result <- myParser  <|>  (do rest <- notParsed ; liftIO (print "not parsed this:"++ rest))
--
--  would print where myParser stopped working. 
-- This does not work with (infinite) streams. Use `getParseBuffer` instead
notParsed:: T '[Parse] '[] BS.ByteString
notParsed = TR P.notParsed

-- | get the current buffer already read but not yet parsed
getParseBuffer :: T '[Parse] '[Parse] BS.ByteString
getParseBuffer = TR  P.getParseBuffer

-- | empty the buffer
clearParseBuffer :: T '[Parse]'[Parse] ()
clearParseBuffer = TR  P.clearParseBuffer

-- | Used for debugging. It shows the next N characters to parse. If there are not enough octets
-- availbable it will try to read from the input
showNext :: Show a => a -> Int -> T '[Parse]'[Parse] ()
showNext msg n= TR $ P.showNext msg $ fromIntegral  n 
        

(|-) :: T '[] '[] (StreamData BS.ByteString) -> T req eff b -> T (Parse :> req) (Parse :> eff) b
(|-) (TR p) (TR q)= TR $ (P.|-) p q