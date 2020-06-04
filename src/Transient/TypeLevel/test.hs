{-# LANGUAGE RebindableSyntax, TypeFamilies, TypeOperators,DataKinds,RankNTypes, ScopedTypeVariables #-}
import  Prelude  hiding ((>>=),(>>),return,concat)
import qualified Prelude as P 
import Transient.TypeLevel.Effects
import Transient.TypeLevel.Base 
import Transient.TypeLevel.Move
import Transient.TypeLevel.Indeterminism
import Control.Exception hiding (onException)

test1=  atRemote $ local $ do
        set  (0:: Int)
        x <- get
        liftIO $ putStrLn x 
        --guard $ x=="hello"
        onException $ \(SomeException e) -> liftIO $ print e
        x  <-  getState   <|> return 0
        r <- ( liftIO (print "hello") >> async (P.return "hello"))  <|> waitEvents (P.return "world")
        liftIO $ print (r,x :: Int)
        empty
        return()
{-

expresiones con guard -> Maybe Terminates
expresionescon empty solo, o getSData etc ->Terminates
expresiones  con alternativo: eliminar terminates

async anula termination en alternativo   alternativo directamente anula termination
empty es  termination
guard  es   Maybe termination
-}       

test8= do
    x <- choose [1..10:: Int]
    guard $ x `rem` 2 == 0 
    liftIO $ print x

test9= do
   test8
   empty

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y


-- >>> :t async
-- async :: IO a -> T '[Async, MThread] a
--

-- >>> :t waitEvents
-- waitEvents :: IO a -> T '[Streaming, MThread] a
--

test7=   do guard False; liftIO $ print "hello" 



test =  do
        x <- get
        liftIO $ putStrLn x 
        async (P.return "hello")  <|> (liftIO (print "HELLO") >> waitEvents (P.return "world") )



data HasPaid
data SentStuff

pay  ::Int->  T '[] '[HasPaid]()
pay i=  undefined

sendStuff ::   T '[HasPaid] '[SentStuff] ()
sendStuff =undefined


test5= liftIO (print "hello") >> (return "hello" >> sendStuff)

test6=  pay 10 >> test5






--  vlue-level effect combination
cons x [] = [x]

cons y (x : xs) 
 | x==y = (x : xs)
 | otherwise = ( x : (y `cons` xs))


rem1 x [] = []
rem1 x (y:ys)
   | x==y = ys
   | otherwise= y: rem1 x ys

xs `minus` []= xs
xs `minus` (y:ys)= rem1 y (xs `minus` ys)



concat   [] xs= xs

concat  (x : xs) ys= concat xs ( x `cons` ys)


mnub :: X -> [X] -> [X]
mnub x [] = [M $ Just x]

mnub (M (Just x)) (M(Just y):xs)
  | x==y= (x : xs)
  | otherwise=  (M(Just x) : mnub (M $ Just y)  xs)

mnub y (M(Just x) : xs)
  | x == y = (x : xs)
  | otherwise=  ( M(Just x) : mnub y  xs)

mnub y (x : xs)
  | x==y = (x : xs)
  | x == Async1 = (x : mnub y xs)
  | otherwise= ( M(Just x) : mnub y  xs)



data X = Async1 | MThread1 |IOEff1 | Streaming1 | M (Maybe X) deriving (Eq,Show)

alter  [] xs= xs
alter  (Async1 : xs) ys= alter1 xs ( mnub Async1 ys)

alter  (x : xs) (ys@(y:ys'))
  | x==y=   (x: (xs `alter` ys'))

  | otherwise=   (x: (xs `alter` ys))


alter1   [] xs= xs
alter1  (x : xs) ys= alter1 xs (mnub x  ys)
---------------------------


test3= [IOEff1,Async1,MThread1] `alter` [IOEff1,Async1,Streaming1, MThread1]