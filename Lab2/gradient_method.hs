import Control.Monad.Writer.Lazy

type Point2 = (Double, Double)

f :: Point2 -> Double
f (x, y) = x ** 4 + y ** 4 - 5 * (x * y - x ** 2 * y **2)

gradF :: Point2 -> Point2
gradF (x, y) = (4 * x ** 3 - 5 * y + 10 * x * y ** 2, 4 * y ** 3 - 5 * x + 10 * x ** 2 * y)

minPoint :: Point2
minPoint = (0, 0)

maxPoint :: Point2
maxPoint = (1, 1)

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

goldenRatio :: (Double -> Double)   -- f
         -> Double -> Double        -- interval
         -> Double                  -- eps
         -> Int
         -> Writer [String] Double
goldenRatio f l r eps n = do
    if r - l < eps then return $ (l + r) / 2
                   else
                       if f1 < f2 then goldenRatio f l x2 eps n else goldenRatio f x1 r eps n
                       where x1 = l + (3 - sqrt 5) * (r - l) / 2
                             x2 = l + (sqrt 5 - 1) * (r - l) / 2
                             f1 = f x1
                             f2 = f x2


gradientMethodConstantStep :: (Point2 -> Double) ->     -- function
                              (Point2 -> Point2) ->     -- gradient
                              Point2 ->                 -- min bound
                              Point2 ->                 -- max bound
                              Point2 ->                 -- start point
                              Double ->                 -- epsilon
                              Double ->                 -- step
                              Writer [String] Point2    -- result (min point)
gradientMethodConstantStep f g pMin@(xMin, yMin) pMax@(xMax, yMax) p0 eps step = do
    let dir = normalized (g p0 *. (-1))
    let p@(x, y) = p0 +. dir *. step
    tell ["p0 = " ++ show p0, 
          "dir = " ++ show dir]
    if x < xMin || x > xMax || y < yMin || y > yMax then return p0 else
        if abs (f p0 - f p) < eps then return p else
            gradientMethodConstantStep f g pMin pMax p eps step
          
findMaxStep :: Point2 -> Point2 ->  -- bounds
               Point2 ->            -- start
               Point2 ->            -- direction
               Double
findMaxStep (xMin, yMin) (xMax, yMax) (x, y) (xDir, yDir) = 
    case xDir `compare` 0 of
         LT -> let maxByX = (xMin - x) / xDir in case yDir `compare` 0 of
                                                      LT -> let maxByY = (yMin - y) / yDir in min maxByX maxByY
                                                      EQ -> maxByX
                                                      GT -> let maxByY = (yMax - y) / yDir in min maxByX maxByY
         EQ -> case yDir `compare` 0 of
                    LT -> let maxByY = (yMin - y) / yDir in maxByY
                    EQ -> error "dir == 0, wtf?"
                    GT -> let maxByY = (yMax - y) / yDir in maxByY
         GT -> let maxByX = (xMax - x) / xDir in case yDir `compare` 0 of
                                                      LT -> let maxByY = (yMin - y) / yDir in min maxByX maxByY
                                                      EQ -> maxByX
                                                      GT -> let maxByY = (yMax - y) / yDir in min maxByX maxByY

gradientMethodFastest :: (Point2 -> Double) ->     -- function
                         (Point2 -> Point2) ->     -- gradient
                         Point2 ->                 -- min bound
                         Point2 ->                 -- max bound
                         Point2 ->                 -- start point
                         Double ->                 -- epsilon
                         Writer [String] Point2    -- result (min point)
gradientMethodFastest f g pMin@(xMin, yMin) pMax@(xMax, yMax) p0 eps = do
    let dir = normalized (g p0 *. (-1))
    let maxStep = findMaxStep pMin pMax p0 dir
    let (step, findStepLog) = runWriter $ goldenRatio (\step -> f $ p0 +. dir *. step) 0 maxStep eps undefined
    let p@(x, y) = p0 +. dir *. step
    tell ["p0 = " ++ show p0, 
          "dir = " ++ show dir]
    if step == 0 then return p0 else
        if abs (f p0 - f p) < eps then return p else
            gradientMethodFastest f g pMin pMax p eps

main = do
    putStrLn "Enter epsilon:"
    eps <- getLine
    putStrLn "Enter value of the step for the first method:"
    step <- getLine
    putStrLn "Running gradient method with constant step"
    let (minP1, log1) = runWriter $ gradientMethodConstantStep f gradF minPoint maxPoint (0.2, 0.8) (read eps) (read step)
    mapM_ putStrLn log1
    putStrLn $ "Result of gradient method with constant step: " ++ show minP1
    putStrLn $ "Running gradient method with the fastest descent"
    let (minP2, log2) = runWriter $ gradientMethodFastest f gradF minPoint maxPoint (0.2, 0.8) (read eps)
    mapM_ putStrLn log2
    putStrLn $ "Result of gradient method with the fastest descent: " ++ show minP2

