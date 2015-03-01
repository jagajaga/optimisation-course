myFunc :: Double -> Double
myFunc x = x * x

findMin :: (Double -> Double)   -- f
         -> Double -> Double    -- interval
         -> Double              -- eps
         -> Double              -- beta
         -> Double
findMin f l r eps beta = if r - l < eps then (l + r) / 2
                                        else
                         let x1 = (l + r - beta) / 2
                             x2 = (l + r + beta) / 2
                             f1 = f x1
                             f2 = f x2
                         in if f1 < f2 then findMin f l x2 eps beta else findMin f x1 r eps beta

main = do
    putStrLn "Enter minimal x:"
    l <- getLine
    putStrLn "Enter maximal x:"
    r <- getLine
    x_min <- return $ findMin myFunc (read l) (read r) 0.1 0.01
    putStrLn $ "x_min = "  ++ show x_min

