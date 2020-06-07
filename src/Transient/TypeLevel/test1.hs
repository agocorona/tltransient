

mnub :: X -> [X] -> [X]
 
mnub x [] = [Maybe1  x]

mnub (Maybe1 x) (Maybe1 y:xs)
  | x==y = (x : xs)
  | otherwise=  (Maybe1 x : mnub (Maybe1  y)  xs)

mnub x (Maybe1 y : xs)
  | x == y = (y : xs)
  | otherwise=  ( Maybe1 y : mnub x  xs)

mnub y (x : xs)
  | x==y = (x : xs)
  | x == Async1 = (x : mnub y xs)
  | otherwise= ( Maybe1 x : mnub y  xs)


xs `minus` []= xs
xs `minus` (y:ys)= rem1 y (xs `minus` ys)


rem1 x [] = []
rem1 x (y:ys)
   | x==y = ys
   | otherwise= y: rem1 x ys


data X =  Terminates1 |  Terminal1 | Async1 | MThread1 |IOEff1 | Streaming1 | Maybe1 ( X) deriving (Eq,Show)

alter _ []= []

alter [] _=[]

alter (Terminates1:_) xs=  filterTerminate xs --filtrar terminates

alter xs (Terminates1:_)=  filterTerminate xs

alter  (Maybe1 Terminates1 : xs) ys= (alter1 xs ( mnub (Maybe1 Terminates1) ys)) `minus` [Maybe1 (Maybe1 Terminates1)]

alter  (Async1 : xs) ys= alter1 xs ( mnub Async1 ys)

alter  (x : xs) (ys@(y:ys'))
  | x==y=   (x: (xs `alter` ys'))

  | otherwise=   (x: (xs `alter` ys))

alter1 [] []= []
alter1 []xs= xs
alter1  (x : xs) ys= alter1 xs (mnub x  ys)

alter2 [] []= []
alter2 [] xs= putMaybes xs
--alter2  [] (x@(Maybe1 _):xs)= x:alter2 [] xs
--alter2  [] (x:xs)= Maybe1 x:alter2 [] xs
alter2  (x : xs) ys= alter2 xs (mnub x  ys)

putMaybes []=[]
putMaybes (x@(Maybe1 _):xs)= x:putMaybes xs
putMaybes (x:xs)= Maybe1 x:putMaybes xs


filterTerminate []=[]
filterTerminate (Terminates1:xs)= []
filterTerminate (x:xs)= x:filterTerminate  xs
---------------------------

test20 = [IOEff1] `alter` [MThread1]

test3 = [IOEff1,Async1,MThread1] `alter` [IOEff1,Async1,Streaming1, MThread1]

test13= [Maybe1 $  Terminates1 ] `alter` [Terminal1,IOEff1]

test132= []  `alter1`  [Terminal1,IOEff1]

test14= [Async1,MThread1 ] `alter` [IOEff1,Async1,Terminal1,IOEff1,MThread1]   

test16= [Terminal1] `alter` [] 

test17= [Terminal1,Terminates1,IOEff1] `alter` [MThread1,Terminates1,MThread1]

test18= [Maybe1 Terminates1]`alter`[]

test11= [Maybe1 Terminates1,IOEff1]`alter`[Terminal1,IOEff1]

test111= [IOEff1]`alter2`[Terminal1,IOEff1]

test1111= alter2 [] (mnub IOEff1 [Terminates1,IOEff1])