import           Control.Monad.Writer.Lazy
import           Data.List
import           Lab1.TaskOne                           (goldenRatio)

import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Plot.Contour

import           Control.Applicative
import           Control.Lens
import           Data.Default.Class

import           Data.Colour
import           Data.Colour.Names

type Point2 = (Double, Double)

data Result = P Point2
            | PNext Point2
            | Dir Point2
            | FCount Int

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
                              Writer [Result] Point2    -- result (min point)
gradientMethodConstantStep f g pMin pMax p0 eps step = doMethod p0 (f p0) 1 where
    doMethod p prevValue fCalcCount = do
        let dir = normalized (g p *. (-1))
        let maxStep = findMaxStep pMin pMax p dir
        let pNext@(x, y) = p +. dir *. step
        let nextValue = f pNext
        let newFCalcCount = fCalcCount + 1
        let finish res = do {tell [FCount newFCalcCount]; return res}
        tell [P p, Dir dir, PNext pNext]
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
                         Writer [Result] Point2     -- result (min point)
gradientMethodFastest f g pMin pMax p0 eps _ = doMethod p0 (f p0) 1 where
    doMethod p prevValue fCalcCount = do
        let dir = normalized (g p *. (-1))
        let maxStep = findMaxStep pMin pMax p0 dir
        let (step, findStepLog) = runWriter $ goldenRatio (\step -> f $ p +. dir *. step) 0 maxStep eps undefined
        let pNext@(x, y) = p +. dir *. step
        let nextValue = f pNext
        let newFCalcCount = fCalcCount + length findStepLog + 1
        let finish res = do {tell [FCount newFCalcCount]; return res}
        tell [P p, Dir dir, PNext pNext]
        if step == 0 then finish p else
            if abs (nextValue - prevValue) < eps then finish pNext else
                doMethod pNext nextValue newFCalcCount

type MethodType = (Point2 -> Double) -> (Point2 -> Point2) -> Point2 -> Point2 -> Point2 -> Double -> Double -> Writer [Result] Point2
runMethod :: String ->      -- method name
             MethodType ->  -- method
             String ->      -- eps
             String ->      -- step (where needed)
             IO ()
runMethod name method eps step = do
    putStrLn $ "Running " ++ name
    let (minP, messages) = runWriter $ method f gradF minPoint maxPoint startPoint (read eps) (read step)
        ps = [a | P a <- messages]
        dirs = [b | Dir b <- messages]
        pnexts = [c | PNext c <- messages]
        lines = zipWith (\a b -> [a,b]) ps pnexts
        minimisationLines = plot_lines_values .~ lines --TODO better name?
                    $ plot_lines_style  . line_color .~ opaque blue
                    $ def
    mapM_ (\(a,b) -> putStrLn $ "p = " ++ show a ++ "\ndir = " ++ show b) $ zip ps dirs
    putStrLn $ head $ ["f calcilations count: " ++ show x | FCount x <- messages]
    putStrLn $ "gradient calculations count: " ++ (show $ length dirs)
    putStrLn $ "Result of " ++ name ++ ": " ++ show minP
    let sz = fst maxPoint
        stp = 1000
        rng = (-sz, sz)
        n = 40 -- TODO What to put here? That's isolines number
        plts = contourPlot rng rng stp stp n (curry f)
        stls = solidLine 3 <$> rgbaGradient (0,0,1,1) (1,0,0,1) n
        plts' = zipWith (plot_lines_style .~) stls plts
        lyt = toRenderable
            $ layout_title .~ "Contours of a " ++ name
            $ layout_plots .~ (map toPlot plts') ++ [toPlot minimisationLines]
            $ def
    renderableToFile (FileOptions (stp,stp) PNG) (name ++ ".png") lyt
    return ()

main = do
    putStrLn "Enter epsilon:"
    eps <- getLine
    putStrLn "Enter value of the step for the first method:"
    step <- getLine
    runMethod "gradient method with constant step" gradientMethodConstantStep eps step
    runMethod "gradient method with the fastest descent" gradientMethodFastest eps step
