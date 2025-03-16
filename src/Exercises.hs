module Exercises (zip', zip'', zipWith', mapM', mapM_') where

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' _ [] = return []
mapM' f (x : xs) = do
  b <- f x
  ys <- mapM' f xs
  return (b : ys)

mapM_' :: (a -> IO b) -> [a] -> IO ()
mapM_' _ [] = return ()
mapM_' f (x : xs) = do
  _ <- f x
  mapM_' f xs
  return ()
