import           Control.Monad.Writer.Lazy
import           Data.List
import           Lab1.TaskOne              (goldenRatio)

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Plot.Contour

import Control.Lens
import Control.Applicative
import Data.Default.Class

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

findMaxStep :: Point2 -> Point2 ->  -- bounds
               Point2 ->            -- start
               Point2 ->            -- direction
               Double
findMaxStep (xMin, yMin) (xMax, yMax) (x, y) (xDir, yDir) =
    if xDir == 0 then maxByY else if yDir == 0 then maxByX else min maxByX maxByY
    where maxByX = if xDir > 0 then (xMax - x) / xDir else (xMin - x) / xDir
          maxByY = if yDir > 0 then (yMax - y) / yDir else (yMin - y) / yDir

-- TODO: write generic gradient method
gradientMethodConstantStep :: (Point2 -> Double) ->     -- function
                              (Point2 -> Point2) ->     -- gradient
                              Point2 ->                 -- min bound
                              Point2 ->                 -- max bound
                              Point2 ->                 -- start point
                              Double ->                 -- epsilon
                              Double ->                 -- step
                              Writer [String] Point2    -- result (min point)
gradientMethodConstantStep f g pMin pMax p0 eps step = doMethod p0 (f p0) 1 where
    doMethod p prevValue fCalcCount = do
        let dir = normalized (g p *. (-1))
        let maxStep = findMaxStep pMin pMax p dir
        let pNext@(x, y) = p +. dir *. step
        let nextValue = f pNext
        let newFCalcCount = fCalcCount + 1
        let finish res = do {tell ["f calcilations count: " ++ show newFCalcCount]; return res}
        tell ["p = " ++ show p,
              "dir = " ++ show dir]
        if step > maxStep then finish p0 else
            if abs (nextValue - prevValue) < eps then finish pNext else
                doMethod pNext nextValue newFCalcCount

gradientMethodFastest :: (Point2 -> Double) ->      -- function
                         (Point2 -> Point2) ->      -- gradient
                         Point2 ->                  -- min bound
                         Point2 ->                  -- max bound
                         Point2 ->                  -- start point
                         Double ->                  -- epsilon
                         Double ->                  -- dummy argument
                         Writer [String] Point2     -- result (min point)
gradientMethodFastest f g pMin pMax p0 eps _ = doMethod p0 (f p0) 1 where
    doMethod p prevValue fCalcCount = do
        let dir = normalized (g p *. (-1))
        let maxStep = findMaxStep pMin pMax p0 dir
        let (step, findStepLog) = runWriter $ goldenRatio (\step -> f $ p +. dir *. step) 0 maxStep eps undefined
        let pNext@(x, y) = p +. dir *. step
        let nextValue = f pNext
        let newFCalcCount = fCalcCount + length findStepLog + 1
        let finish res = do {tell ["f calcilations count: " ++ show newFCalcCount]; return res}
        tell ["p = " ++ show p,
              "dir = " ++ show dir]
        if step == 0 then finish p else
            if abs (nextValue - prevValue) < eps then finish pNext else
                doMethod pNext nextValue newFCalcCount

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
    putStrLn $ "gradient calculations count: " ++ show (length $ filter (isPrefixOf "dir") messages)
    putStrLn $ "Result of " ++ name ++ ": " ++ show minP

{-main = do-}
    {-putStrLn "Enter epsilon:"-}
    {-eps <- getLine-}
    {-putStrLn "Enter value of the step for the first method:"-}
    {-step <- getLine-}
    {-runMethod "gradient method with constant step" gradientMethodConstantStep eps step-}
    {-runMethod "gradient method with the fastest descent" gradientMethodFastest eps step-}

{---- Test ----}


am :: Double -> Double
am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

sinusoid2 = plot_points_values .~ [ (x,(am x)) | x <- [0,7..20]]
            $ plot_points_title .~ "am points"
            $ def

main :: IO ()
main = do
    let f' x y = f(x,y) 
        sz = 7.5
        stp = 1000
        rng = (-sz,sz)
        stps = (stp,stp)
        n = 20
        plts = contourPlot rng rng stp stp n f'
        stls = solidLine 3 <$> rgbaGradient (0,0,1,1) (1,0,0,1) n
        plts' = zipWith (plot_lines_style .~) stls plts
        lyt = toRenderable
            $ layout_title .~ "Contours of a 2D Sin Curve"
            $ layout_plots .~ (map toPlot plts') ++ [toPlot sinusoid2]
            $ def
    renderableToFile (FileOptions (1000,700) PNG) "sind2d.png" lyt
    return ()

