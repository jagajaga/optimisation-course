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

startPoint :: Point2
startPoint = (0.2, 0.8)

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

findMaxStep :: Point2 -> Point2 ->  -- bounds
               Point2 ->            -- start
               Point2 ->            -- direction
               Double
findMaxStep (xMin, yMin) (xMax, yMax) (x, y) (xDir, yDir) =
    if xDir == 0 then maxByY else if yDir == 0 then maxByX else min maxByX maxByY
    where maxByX = if xDir > 0 then (xMax - x) / xDir else (xMin - x) / xDir
          maxByY = if yDir > 0 then (yMax - y) / yDir else (yMin - y) / yDir

gradientMethodConstantStep :: (Point2 -> Double) ->     -- function
                              (Point2 -> Point2) ->     -- gradient
                              Point2 ->                 -- min bound
                              Point2 ->                 -- max bound
                              Point2 ->                 -- start point
                              Double ->                 -- epsilon
                              Double ->                 -- step
                              Writer [String] Point2    -- result (min point)
gradientMethodConstantStep f g pMin pMax p0 eps step = do
    let dir = normalized (g p0 *. (-1))
    let p@(x, y) = p0 +. dir *. step
    let maxStep = findMaxStep pMin pMax p0 dir
    tell ["p0 = " ++ show p0, 
          "dir = " ++ show dir]
    if step > maxStep then return p0 else
        if abs (f p0 - f p) < eps then return p else
            gradientMethodConstantStep f g pMin pMax p eps step
          
gradientMethodFastest :: (Point2 -> Double) ->      -- function
                         (Point2 -> Point2) ->      -- gradient
                         Point2 ->                  -- min bound
                         Point2 ->                  -- max bound
                         Point2 ->                  -- start point
                         Double ->                  -- epsilon
                         Double ->                  -- dummy argument
                         Writer [String] Point2     -- result (min point)
gradientMethodFastest f g pMin pMax p0 eps _ = do
    let dir = normalized (g p0 *. (-1))
    let maxStep = findMaxStep pMin pMax p0 dir
    let (step, findStepLog) = runWriter $ goldenRatio (\step -> f $ p0 +. dir *. step) 0 maxStep eps undefined
    let p@(x, y) = p0 +. dir *. step
    tell ["p0 = " ++ show p0, 
          "dir = " ++ show dir]
    if step == 0 then return p0 else
        if abs (f p0 - f p) < eps then return p else
            gradientMethodFastest f g pMin pMax p eps undefined

type MethodType = (Point2 -> Double) -> (Point2 -> Point2) -> Point2 -> Point2 -> Point2 -> Double -> Double -> Writer [String] Point2
runMethod :: String ->      -- method name
             MethodType ->  -- method
             String ->      -- eps
             String ->      -- step (where needed)
             IO ()
runMethod name method eps step = do
    putStrLn $ "Running " ++ name
    let (minP, messages) = runWriter $ method f gradF minPoint maxPoint startPoint (read eps) (read step)
    mapM_ putStrLn messages
    putStrLn $ "Result of " ++ name ++ ": " ++ show minP

main = do
    putStrLn "Enter epsilon:"
    eps <- getLine
    putStrLn "Enter value of the step for the first method:"
    step <- getLine
    runMethod "gradient method with constant step" gradientMethodConstantStep eps step
    runMethod "gradient method with the fastest descent" gradientMethodFastest eps step

