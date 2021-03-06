-- NOTE: This program does not support multiple characters and
-- is best used to demonstrate the fetching of user input.
-- CONFIG CRAP
boadSize :: [Integer];
boadSize = [20,10];



-- GRAPHICAL CRAP
printBoad :: [Integer] -> [Char];
printBoad g = wrap $ init $ foldr (++) [] $ prettify lines
  where 
    lines = reverse $ map (printLine . d) [0..boadSize !! 1]
    prettify = map ((++ "|\n") . ("|"++))
    d j
      | j == g !! 1 = g !! 0
      | otherwise = -1
    wrap k = bvx ++ "\n" ++ k ++ "\n" ++ bvx
    bvx = take (fromInteger $ boadSize !! 0 + 3:: Int) (repeat '-')

printLine :: Integer -> [Char];
printLine n
  | n == (-1) = take k bk
  | otherwise = take (toInt n) bk ++ "@" ++ (take n1 bk)
  where
    bk = repeat '.'
    toInt = \a -> fromIntegral a :: Int
    k = toInt $ boadSize !! 0 + 1
    n1 = toInt $ (boadSize !! 0) - n



-- GENERAL CRAP
getNewCoords :: [Integer] -> IO [Integer];
getNewCoords a = getChar >>= \b -> return $ getWithinBounds $ sumCoords b;
  where
  sumCoords k = zipWith (+) a (viKey k)
  getWithinBounds = getBelowMax . getAboveMin
  getBelowMax = zipWith min (map (boadSize !!) [0,1])
  getAboveMin = zipWith max [0,0]

viKey :: Char -> [Integer];
viKey 'h' = [-1,0];
viKey 'j' = [0,-1];
viKey 'k' = [0, 1];
viKey 'l' = [1, 0];
viKey n   = [0, 0];

mane :: [Integer] -> IO ();
mane = \a -> getNewCoords a >>= \newCoords ->
  putStrLn (printBoad newCoords ++ "\n") >> mane newCoords;

main :: IO ();
main = mane [0,0];
