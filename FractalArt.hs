{-
The MIT License (MIT)

Copyright (c) 2015 Tom Smeets

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
module FractalArt (generateArt) where

import System.Environment          (getArgs)
import System.Random               (randomIO)
import System.Random.MWC           (Gen, initialize, uniformR)
import System.Directory            (getHomeDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath             ((</>))

import Control.Monad               (liftM, unless)
import Control.Monad.Primitive

import Data.Word                   (Word32)

import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MV

import Data.Vector.Unboxed.Mutable (MVector, write)
import Data.Vector.Unboxed         (Vector, (!), freeze)

import Codec.Picture

type Position = (Int, Int)
type Size     = (Int, Int)
type Color    = (Float, Float, Float)

type Grid m   = MVector m (Bool, Color)

generateArt :: FilePath -> IO ()
generateArt imageFile = do
    --home <- getHomeDirectory
    pwd <- getCurrentDirectory

    --let workingDir = pwd
    --let imageFile  = (workingDir </> "blogImage.png")

    let size@(width,height) = (640,360)

    seed  <- randomIO :: IO Word32
    gen   <- initialize (V.singleton seed)

    startPosition <- do
        x <- uniformR (0, width  - 1) gen
        y <- uniformR (0, height - 1) gen
        return (x, y)

    -- Generate and force evaluation
    grid <- runBG startPosition size gen

    -- Create an immutable copy of the grid
    frozen <- freeze grid
    let bitmap = UV.map snd frozen

    writePng imageFile $ createImage size bitmap

-- | Create an image from a vector of colors
createImage :: Size -> Vector Color -> Image PixelRGB8
createImage (width, height) grid = generateImage colorAt width height
  where
    colorAt x y       = toPixel $ grid ! toIndex (width, height) (x, y)
    toPixel (r, g, b) = PixelRGB8 (toByte r) (toByte g) (toByte b)
    toByte f          = floor(f * 255)

-- | Generate the wallpaper
runBG :: PrimMonad m => Position -> Size -> Gen (PrimState m) -> m (Grid (PrimState m))
runBG pos@(x, y) size@(w, h) gen = do
    grid <- startGrid
    mapM_ (iter pos size gen grid) [1..iterations]
    return grid
  where
    startGrid  = do
        grid <- MV.replicate (w * h)  (False, (0, 0, 0))
        col  <- startColor
        write grid (toIndex size pos) (True, col)
        return grid

    startColor = do
      hue <- uniformR (0, 360) gen
      --sat <- uniform           gen
      return $ hsv hue 0.6 1

    iterations = maximum [x, w - 1 - x, y, h - 1 - y]

iter :: PrimMonad m => Position -> Size -> Gen (PrimState m) -> Grid (PrimState m) -> Int -> m ()
iter pos size gen grid n = mapM_ f next
  where
    f p = do
      col <- nextColor grid size gen p
      write grid (toIndex size p) (True, col)

    next = filter (isInside size) $ ringAt pos n

nextColor :: PrimMonad m => Grid (PrimState m) -> Size -> Gen (PrimState m) -> Position -> m Color
nextColor grid size gen pos = do
    c <- colors
    i <- uniformR (0, length c - 1) gen
    m <- modColor (mkRange $ length c) (c !! i)
    return . clampColor $ m
  where
    colors = liftM (map snd . filter isValid) . mapM (MV.read grid . toIndex size) . filter (isInside size) $ ringAt pos 1

    mkRange l = 0.006 * 4 / fromIntegral l

    modColor range (r, g, b)  = do
      mr <- uniformR (-range, range) gen
      mg <- uniformR (-range, range) gen
      mb <- uniformR (-range, range) gen
      return (r + mr, g + mg, b + mb)

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

hsv :: Float -> Float -> Float -> Color
hsv h s v = case hId of
    0 -> (v, t, p)
    1 -> (q, v, p)
    2 -> (p, v, t)
    3 -> (p, q, v)
    4 -> (t, p, v)
    5 -> (v, p, q)
    _ -> error $ "invalid hue: " ++ show h
  where
    hId :: Int
    hId = floor (h / 60) `mod` 6
    f    = h / 60 - fromIntegral hId
    p    = v * (1 - s)
    q    = v * (1 - s * f)
    t    = v * (1 - s * (1 - f))

clampColor :: Color -> Color
clampColor (r, g, b) = (f r, f g, f b)
  where
    f = min 1 . max 0

ringAt :: Position -> Int -> [Position]
ringAt (x, y) l = sides ++ top ++ bottom
  where
    top    = [(n + x, l + y) | n <- [-l .. l]]
    bottom = [(n + x, -l + y) | n <- [-l .. l]]
    sides  = concat [[(l + x, n + y), (-l + x, n + y)] | n <- [1 - l .. l - 1]]

toIndex :: Size -> Position -> Int
toIndex (w, _) (x, y) = y * w + x

isValid :: (Bool, a) -> Bool
isValid = fst

isInside :: Size -> Position -> Bool
isInside (w, h) (x, y) = inside' x w && inside' y h
  where
    inside' n s = (n < s) && (n >= 0)
