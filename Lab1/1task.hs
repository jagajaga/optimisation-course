import Control.Monad.Writer.Lazy

fib n = fromIntegral $ round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

myFunc :: Double -> Double
myFunc x = x * x

dichotomy :: (Double -> Double)   -- f
         -> Double -> Double    -- interval
         -> Double              -- eps
         -> Double              -- beta
         -> Writer [String] Double
dichotomy f l r eps beta = do
    tell (["l = " ++ show l ++ "; r = " ++ show r])
    if r - l < eps then return $ (l + r) / 2
                   else
                       if f1 < f2 then dichotomy f l x2 eps beta else dichotomy f x1 r eps beta
                       where x1 = (l + r - beta) / 2
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
         -> Writer [String] Double
fibonacci f l r n = do
                if y1 < y2 then fibonacci' f l x2 (l + (x2 - x1)) x1 (n - 1)
                           else fibonacci' f x1 r x2 (r - (x2 - x1)) (n - 1)
    where
        x1 = l + (r - l) * fib (n - 2) / fib n
        x2 = l + (r - l) * fib (n - 1) / fib n
        y1 = f x1
        y2 = f x2

fibonacci' :: (Double -> Double)
           -> Double -> Double
           -> Double
           -> Double 
           -> Int
           -> Writer [String] Double
fibonacci' f l r x1 x2 n = do
    tell (["l = " ++ show l ++ "; r = " ++ show r])
    if n == 1 then return x1
             else 
                if y1 < y2 then fibonacci' f l x2 (l + (x2 - x1)) x1 (n - 1)
                           else fibonacci' f x1 r x2 (r - (x2 - x1)) (n - 1)
    where
        y1 = f x1
        y2 = f x2
main = do
    putStrLn "Enter minimal x:"
    l <- getLine
    putStrLn "Enter maximal x:"
    r <- getLine
    let (d_x_min, d_log) = runWriter $ dichotomy myFunc (read l) (read r) 0.1 0.01
    let (g_x_min, g_log) = runWriter $ goldenRatio myFunc (read l) (read r) 0.1
    let (f_x_min, f_log) = runWriter $ fibonacci myFunc (read l) (read r) 20
    putStrLn "dichotomy"
    putStrLn $ "d_x_min = "  ++ show d_x_min
    {-mapM_ (putStrLn . show) $ zip [1..] d_log-}
    putStrLn "\ngolden ratio"
    putStrLn $ "g_x_min = "  ++ show g_x_min
    {-mapM_ (putStrLn . show) $ zip [1..] g_log-}
    putStrLn "\nfibonacci"
    putStrLn $ "f_x_min = "  ++ show f_x_min
    {-mapM_ (putStrLn . show) $ zip [1..] f_log-}

