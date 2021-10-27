module Main where



f :: a -> b
f x= undefined


f' :: IO a -> IO b
f' ioa = do
        a <- ioa
        return $ f a

trans :: (a ->b) -> (IO a -> IO b)
trans f= \iox -> do
    x <- iox
    return $ f x

g= return (2 :: Int) 

h= (+) <$> g <$> g <$> g


main= print $ h  2 4 

-- >>> main
-- 6
--





