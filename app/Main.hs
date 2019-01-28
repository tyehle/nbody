module Main where

import Graphics.Gloss
import Physics

start :: World
start = laplaceResonance 100000 8 400
-- start = [ Body 1000 (0,    0) (  0, 0)
--         , Body 30   (0, -100) (-90, 0)
--         , Body 30   (0,  300) (90 , 0)
--         , Body 30   (0,  300) (90 , 0)
--         ]


render :: World -> Picture
render bodies = pictures $ renderBody <$> bodies
  where
    renderBody (Body m (x, y) _) = translate x y . color (dim red) $ circleSolid 10


main :: IO ()
main = simulate window bg fps start render (const step)
  where
    window :: Display
    -- window = InWindow "N Body" (1280, 720) (0, 0)
    window = FullScreen

    bg :: Color
    bg = greyN 0.25

    fps :: Int
    fps = 60
