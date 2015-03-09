import Control.Monad.Writer.Lazy

type Point2 = (Double, Double)

f :: Point2 -> Double
f (x, y) = x ** 4 + y ** 4 - 5 * (x * y - x ** 2 * y **2)

gradF :: Point2 -> Point2
gradF (x, y) = (4 * x ** 3 - 5 * y + 10 * x * y ** 2, 4 * y ** 3 - 5 * x + 10 * x ** 2 * y)

infixl 5 +.
(+.) :: Point2 -> Point2 -> Point2
(x, y) +. (x1, y1) = (x + x1, y + y1)

infixl 5 -.
(-.) :: Point2 -> Point2 -> Point2
(x, y) -. (x1, y1) = (x - x1, y - y1)

infixl 6 *.
(*.) :: Point2 -> Double -> Point2
(x, y) *. n = (n * x, n * y)

norm :: Point2 -> Double
norm (x, y) = sqrt $ x ** 2 + y ** 2

normalized :: Point2 -> Point2
normalized p = p *. (1 / norm p)

gradientMethodConstantStep :: (Point2 -> Double) ->     -- function
                              (Point2 -> Point2) ->     -- gradient
                              Point2 ->                 -- start point
                              Double ->                 -- epsilon
                              Double ->                 -- step
                              Writer [String] Point2    -- result (min point)
gradientMethodConstantStep f g p0 eps step = do
    tell ["p0 = " ++ show p0, 
          "g p0 = " ++ show (g p0),
          "dir = " ++ show dir]
    if abs (f p0 - f p) < eps then return p else gradientMethodConstantStep f g p eps step
        where dir = normalized (g p0 *. (-1))
              p = p0 +. dir *. step

gradientMethodFastest :: (Point2 -> Double) ->     -- function
                              (Point2 -> Point2) ->     -- gradient
                              Point2 ->                 -- start point
                              Double ->                 -- epsilon
                              Writer [String] Point2    -- result (min point)
gradientMethodFastest = undefined

main = do
    putStrLn "Enter epsilon:"
    eps <- getLine
    putStrLn "Enter value of the step for the first method:"
    step <- getLine
    putStrLn "Running gradient method with constant step"
    let (minP1, log1) = runWriter $ gradientMethodConstantStep f gradF (0.2, 0.8) (read eps) (read step)
    mapM_ putStrLn log1
    putStrLn $ "Result of gradient method with constant step: " ++ show minP1
    putStrLn $ "Running gradient method with the fastest descent"
    let (minP2, log2) = runWriter $ gradientMethodFastest f gradF (0.2, 0.8) (read eps)
    mapM_ putStrLn log2
    putStrLn $ "Result of gradient method with constant step: " ++ show minP2

