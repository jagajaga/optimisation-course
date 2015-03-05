import           Control.Monad.Writer.Lazy
import           Data.List
import           Data.Ord

fib :: Int -> Double
fib n = fromIntegral $ round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

myFunc :: Double -> Double
myFunc x = abs $ x ** 3 - 30

tellLog :: Double -> Double -> Writer [String] ()
tellLog l r =
    tell ([show l ++ "," ++ show r])

robinMethod :: (Double -> Double)
            -> Double -> Double
            -> Double
            -> Int
            -> Writer [String] Double
robinMethod f l r eps n = do
    return $ minimumBy (comparing f) xiths
    where
        xiths = map ((l +) . (/ (fromIntegral n + 1)) . ((r - l) *) . fromIntegral) [1..n]

dichotomy :: (Double -> Double) -- f
          -> Double -> Double    -- interval
          -> Double              -- eps
          -> Int                 -- dummy
          -> Writer [String] Double
dichotomy f l r eps _ = dichotomy' f l r eps (eps / 2)

dichotomy' :: (Double -> Double) -- f
          -> Double -> Double    -- interval
          -> Double              -- eps
          -> Double              -- beta
          -> Writer [String] Double
dichotomy' f l r eps beta = do
    tellLog l r
    if r - l < eps then return $ (l + r) / 2
                   else
                       if f1 < f2 then dichotomy' f l x2 eps beta else dichotomy' f x1 r eps beta
                       where x1 = (l + r - beta) / 2
                             x2 = (l + r + beta) / 2
                             f1 = f x1
                             f2 = f x2

goldenRatio :: (Double -> Double)   -- f
         -> Double -> Double    -- interval
         -> Double             -- eps
         -> Int
         -> Writer [String] Double
goldenRatio f l r eps n = do
    tellLog l r
    if r - l < eps then return $ (l + r) / 2
                   else
                       if f1 < f2 then goldenRatio f l x2 eps n else goldenRatio f x1 r eps n
                       where x1 = l + (3 - sqrt 5) * (r - l) / 2
                             x2 = l + (sqrt 5 - 1) * (r - l) / 2
                             f1 = f x1
                             f2 = f x2

fibonacci :: (Double -> Double)   -- f
         -> Double -> Double    -- interval
         -> Double
         -> Int             -- steps
         -> Writer [String] Double
fibonacci f l r eps n = do
    fibonacci' f l r x1 x2 y1 y2 (n - 1) eps 0
    where
        x1 = l + (r - l) * fib (n - 2) / fib n
        x2 = l + (r - l) * fib (n - 1) / fib n
        y1 = f x1
        y2 = f x2

fibonacci' :: (Double -> Double)
           -> Double -> Double
           -> Double
           -> Double
           -> Double
           -> Double
           -> Int
           -> Double
           -> Int
           -> Writer [String] Double
fibonacci' f l r x1 x2 y1 y2 n eps k = do
    tellLog l r
    if (n == 1 ||  (r - l) < eps) then return $ min x1 x2
             else
                if y1 > y2 then let l' = x1
                                    x1' = x2
                                    x2' = l' + fib (n - k -1) / fib (n - k) * (r - l')
                                    y1' = y2
                                    y2' = f x2'
                                in fibonacci' f l' r x1' x2' y1' y2' (n - 1) eps (k + 1)
                           else let r' = x2
                                    x2' = x1
                                    x1' = l + fib (n - k - 2) / fib (n - k) * (r' - l)
                                    y2' = y1
                                    y1' = f x1'
                                in fibonacci' f l r' x1' x2' y1' y2' (n - 1) eps (k + 1)

runMethod :: ((String -- function name
            , (Double -> Double) -> Double -> Double -> Double -> Int -> Writer [String] Double) -- function
            , (Double, Double, Double, Int)) -- l, r, eps, n
            -> IO ()
runMethod ((name, f), (l, r, eps, n)) = do
    putStrLn name
    let (x_min, log) = runWriter $ f myFunc l r eps n
    putStrLn $ "x_min = " ++ show x_min
    if not $ null log then mapM_ (putStrLn . \(i, msg) -> show i ++ "," ++ msg) $ zip [1..] log
                    else return ()

main = do
    putStrLn "Enter minimal x:"
    l <- getLine >>= return . read
    putStrLn "Enter maximal x:"
    r <- getLine >>= return . read
    putStrLn "Enter iterations count for robin method and fibonacci method:"
    n <- getLine >>= return . read
    putStrLn "Enter eps for dichotomy and golden ratio:"
    eps <- getLine >>= return . read
    mapM_ runMethod $ zip [("robin", robinMethod), ("dichotomy", dichotomy), ("golden ratio", goldenRatio), ("fibonacci", fibonacci)] (cycle [(l, r, eps, n)])

