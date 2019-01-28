module Main where

import Data.Foldable (foldl')
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

import Debug.Trace

type World = [Body]

data Body = Body Float Point Point deriving (Eq, Show, Ord)

g :: Float
g = 3000

start :: World
start = laplaceResonance 100000 8 400
-- start = [ Body 1000 (0,    0) (  0, 0)
--         , Body 30   (0, -100) (-90, 0)
--         , Body 30   (0,  300) (90 , 0)
--         , Body 30   (0,  300) (90 , 0)
--         ]

normalizeMomentum :: World -> World
normalizeMomentum bodies = newBody <$> bodies
  where
    momentum :: Body -> Point
    momentum (Body m _ v) = m `mult` v

    totalMomentum :: Point
    totalMomentum = foldl' add (0, 0) $ map momentum bodies
    totalMass :: Float
    totalMass = foldl' (+) 0 $ map (\(Body m _ _) -> m) bodies
    velocity :: Point
    velocity = mult (1/totalMass) totalMomentum

    newBody :: Body -> Body
    newBody (Body m pos vel) = Body m pos (sub vel velocity)


-- 2 pi r / T = v
-- T = 2 pi sqrt( r^3 / G M )
laplaceResonance :: Float -> Float -> Float -> World
laplaceResonance mL mS r = normalizeMomentum [ Body mL (0, 0) (0, 0)
                                             , Body mS (0,  r1) v1
                                             , Body mS (0, -r2) v2
                                             , Body mS (0, -r3) v3
                                             ]
  where
    t3 = 2 * pi * sqrt (r^3 / g / mL)
    t2 = t3 / 2
    t1 = t2 / 2

    r3 = r
    r2 = ((t2 / 2 / pi)^2 * g * mL) ** (1/3)
    r1 = ((t1 / 2 / pi)^2 * g * mL) ** (1/3)

    v3 = (-2 * pi * r3 / t3, 0)
    v2 = (-2 * pi * r2 / t2, 0)
    v1 = ( 2 * pi * r1 / t1, 0)


render :: World -> Picture
render bodies = pictures $ renderBody <$> bodies
  where
    renderBody (Body m (x, y) _) = translate x y . color (dim red) $ circleSolid 10


step :: ViewPort -> Float -> World -> World
step _ t = applyN steps (fourthOrder (t / fromIntegral steps))
  where
    steps = 500
    applyN :: Int -> (a -> a) -> a -> a
    applyN n f x
      | n <= 0    = x
      | otherwise = applyN (n-1) f (f x)


firstOrder, secondOrder, thirdOrder, fourthOrder :: Float -> World -> World
firstOrder  = kOrder [(1, 1)]
secondOrder = kOrder [(0, 0.5), (1, 0.5)]
thirdOrder  = kOrder [(1, -1/24), (-2/3, 3/4), (2/3, 7/24)]
fourthOrder = kOrder [(c14, d13), (c23, d2), (c23, d13), (c14, d4)]
  where
    c14 = 1 / (2*(2 - 2**(1/3)))
    c23 = (1 - 2**(1/3)) / (2*(2 - 2**(1/3)))
    d13 = 1 / (2 - 2**(1/3))
    d2  = -(2**(1/3)) / (2 - 2**(1/3))
    d4  = 0


kOrder :: [(Float, Float)] -> Float -> World -> World
kOrder [] t bodies = bodies
kOrder ((c, d) : coeffs) t bodies = kOrder coeffs t $ map newBody bodies
  where
    newBody :: Body -> Body
    newBody b@(Body m pos vel) = Body m pos' vel'
      where
        vel' = vel `add` ((d*t/m) `mult` forceOn b bodies)
        pos' = pos `add` mult (c*t) vel'


forceOn :: Body -> World -> Point
forceOn b@(Body m1 pos1 _) = foldl' add (0, 0) . map forceFor . filter (/= b)
  where
    forceFor :: Body -> Point
    forceFor (Body m2 pos2 _) = (g * m1 * m2 / norm towards ** 3) `mult` towards
      where towards = pos2 `sub` pos1


mult :: Float -> Point -> Point
mult a (x, y) = (a*x, a*y)

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1-x2, y1-y2)

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) = (x1+x2, y1+y2)

norm :: Point -> Float
norm (x, y) = sqrt $ x*x + y*y


main :: IO ()
main = simulate window bg fps start render step
  where
    window :: Display
    -- window = InWindow "N Body" (1280, 720) (0, 0)
    window = FullScreen

    bg :: Color
    bg = greyN 0.25

    fps :: Int
    fps = 60
