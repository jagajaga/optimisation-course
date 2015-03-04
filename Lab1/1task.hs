import Control.Monad.Writer.Lazy
import Data.List
import Data.Ord

fib :: Int -> Double
fib n = fromIntegral $ round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

myFunc :: Double -> Double
myFunc x = abs $ x ** 3 - 30

robinMethod :: (Double -> Double)
            -> Double -> Double
            -> Int
            -> Writer [String] Double 
robinMethod f l r n = do
    mapM (\i -> tell [show i]) xiths
    return result
    where
        xiths = map ((l +) . (/ (fromIntegral n + 1)) . ((r - l) *) . fromIntegral) [1..n]
        result = minimumBy (comparing f) xiths

dichotomy :: (Double -> Double)   -- f
         -> Double -> Double    -- interval
         -> Double              -- eps
         -> Writer [String] Double
dichotomy f l r eps = do
    tell (["l = " ++ show l ++ "; r = " ++ show r])
    if r - l < eps then return $ (l + r) / 2
                   else
                       if f1 < f2 then dichotomy f l x2 eps else dichotomy f x1 r eps
                       where beta = (r - l) / 10
                             x1 = (l + r - beta) / 2
                             x2 = (l + r + beta) / 2
                             f1 = f x1
                             f2 = f x2

goldenRatio :: (Double -> Double)   -- f
         -> Double -> Double    -- interval
         -> Double             -- eps
         -> Writer [String] Double
goldenRatio f l r eps = do
    tell (["l = " ++ show l ++ "; r = " ++ show r])
    if r - l < eps then return $ (l + r) / 2
                   else
                       if f1 < f2 then goldenRatio f l x2 eps else goldenRatio f x1 r eps
                       where x1 = l + (3 - sqrt 5) * (r - l) / 2 
                             x2 = l + (sqrt 5 - 1) * (r - l) / 2 
                             f1 = f x1
                             f2 = f x2

fibonacci :: (Double -> Double)   -- f
         -> Double -> Double    -- interval
         -> Int             -- steps
         -> Double
         -> Writer [String] Double
fibonacci f l r n eps = do
    fibonacci' f l r x1 x2 y1 y2 (n - 1) eps 1
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
    tell (["l = " ++ show l ++ "; r = " ++ show r])
    if (n == 1 ||  (r - l) < eps) then return $ min x1 x2
             else 
                if y1 > y2 then let l' = x1
                                    x1' = x2
                                    x2' = l' + fib (n - k -1) / fib (n - k) * (r - l')
                                    y1' = y2
                                    y2' = f x2'
                                in fibonacci' f l' r x1' x2' y1' y2' (n - 1) eps k
                           else let r' = x2
                                    x2' = x1
                                    x1' = l + fib (n - k - 2) / fib (n - k) * (r' - l)
                                    y2' = y1
                                    y1' = f x1'
                                in fibonacci' f l r' x1' x2' y1' y2' (n - 1) eps k
 
main = do
    putStrLn "Enter minimal x:"
    l <- getLine
    putStrLn "Enter maximal x:"
    r <- getLine
    putStrLn "Enter iterations count for robin method and fibonacci method:"
    n <- getLine
    putStrLn "Enter eps for dichotomy and golden ratio:"
    eps <- getLine
    let (r_x_min, r_log) = runWriter $ robinMethod myFunc (read l) (read r) (read n)
    let (d_x_min, d_log) = runWriter $ dichotomy myFunc (read l) (read r) (read eps)
    let (g_x_min, g_log) = runWriter $ goldenRatio myFunc (read l) (read r) (read eps)
    let (f_x_min, f_log) = runWriter $ fibonacci myFunc (read l) (read r) (read n) (read eps)
    putStrLn "robin"
    putStrLn $ "r_x_min = "  ++ show r_x_min
    mapM_ (putStrLn . show) $ zip [1..] r_log
    putStrLn "dichotomy"
    putStrLn $ "d_x_min = "  ++ show d_x_min
    mapM_ (putStrLn . show) $ zip [1..] d_log
    putStrLn "\ngolden ratio"
    putStrLn $ "g_x_min = "  ++ show g_x_min
    mapM_ (putStrLn . show) $ zip [1..] g_log
    putStrLn "\nfibonacci"
    putStrLn $ "f_x_min = "  ++ show f_x_min
    mapM_ (putStrLn . show) $ zip [1..] f_log

