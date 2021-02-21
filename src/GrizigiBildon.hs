
{-|
    la 'GrizigiBildon' modulo esta bazitas sur la kodo de la artikolo
    "https://codereview.stackexchange.com/questions/128335/converting-images-to-greyscale-using-juicypixel"
-}

module GrizigiBildon 
(
    grizigi
) where

import           Codec.Picture
import           Graphics.Color.Algebra


grizigi :: DynamicImage -> Image Pixel8
grizigi (ImageY8 b) = b
grizigi (ImageY16 b) = pixelMap pixelAvgY16 b
grizigi (ImageY32 b) = pixelMap pixelAvgY32 b
grizigi (ImageYF b) = pixelMap pixelAvgYF b
grizigi (ImageYA8 b) = pixelMap pixelAvgYA8 b
grizigi (ImageYA16 b) = pixelMap pixelAvgYA16 b 
grizigi (ImageRGB8 b) = pixelMap pixelAvg8 b
grizigi (ImageRGB16 b) = pixelMap pixelAvg16 b
grizigi (ImageRGBA8 b) = pixelMap pixelAvg8a b
grizigi (ImageRGBA16 b) = pixelMap pixelAvg16a b
grizigi (ImageYCbCr8 b) = pixelMap pixelAvgYCbCr8 b
grizigi (ImageRGBF b) = pixelMap pixelAvgRGBF b
grizigi (ImageCMYK8 b) = pixelMap pixelAvgCMYK8 b
grizigi (ImageCMYK16 b) = pixelMap pixelAvgCMYK16 b

pixelAvgY16 :: Pixel16 -> Pixel8
pixelAvgY16 p = toWord8 p

pixelAvgY32 :: Pixel32 -> Pixel8
pixelAvgY32 p = toWord8 p

pixelAvgYF :: PixelF -> Pixel8
pixelAvgYF p = toWord8 p

pixelAvgYA8 :: PixelYA8 -> Pixel8
pixelAvgYA8 (PixelYA8 l a) = l

pixelAvgYA16 :: PixelYA16 -> Pixel8
pixelAvgYA16 (PixelYA16 l a) = toWord8 l

pixelAvg8 :: PixelRGB8 -> Pixel8
pixelAvg8 (PixelRGB8 r g b) = fromIntegral $ ((fromIntegral r + fromIntegral g + fromIntegral b) :: Int) `quot` 3

pixelAvg8a :: PixelRGBA8 -> Pixel8
--pixelAvg8a (PixelRGBA8 r g b a) = toWord8 $ ((((fromIntegral r)*0.299 + (fromIntegral g)*0.587 + (fromIntegral b)*0.114)) :: Float)
pixelAvg8a (PixelRGBA8 r g b a) = fromIntegral $ ((fromIntegral r + fromIntegral g + fromIntegral b) :: Int) `quot` 3

pixelAvg16a :: PixelRGBA16 -> Pixel8
--pixelAvg16a (PixelRGBA16 r g b a) = toWord8 $ (((fromIntegral r)*0.299 + (fromIntegral g)*0.587 + (fromIntegral b)*0.114))
pixelAvg16a (PixelRGBA16 r g b a) = fromIntegral $ ((fromIntegral r + fromIntegral g + fromIntegral b) :: Int) `quot` 3

pixelAvg16 :: PixelRGB16 -> Pixel8
pixelAvg16 (PixelRGB16 r g b) = toWord8 $ ((fromIntegral r + fromIntegral g + fromIntegral b) :: Int) `quot` 3

pixelAvgYCbCr8 :: PixelYCbCr8 -> Pixel8
pixelAvgYCbCr8 (PixelYCbCr8 y cb cr) = y

pixelAvgRGBF :: PixelRGBF -> Pixel8
pixelAvgRGBF (PixelRGBF r g b) = toWord8 (r*0.299 + g*0.587 + b*0.114)

pixelAvgCMYK8 :: PixelCMYK8 -> Pixel8
pixelAvgCMYK8 (PixelCMYK8 c m y k) = 
    let 
        lk = fromIntegral k :: Int
        r = toDouble $ 1 - ((fromIntegral c) * (1 - lk) + lk) 
        g = toDouble $ 1 - ((fromIntegral m) * (1 - lk) + lk)
        b = toDouble $ 1 - ((fromIntegral y) * (1 - lk) + lk)
    in
       toWord8 (0.299 * r + 0.587 * g + 0.114 * b)

pixelAvgCMYK16 :: PixelCMYK16 -> Pixel8
pixelAvgCMYK16 (PixelCMYK16 c m y k) = 
    let 
        lk = fromIntegral k :: Int
        r = toDouble $ 1 - ((fromIntegral c) * (1 - lk) + lk) 
        g = toDouble $ 1 - ((fromIntegral m) * (1 - lk) + lk)
        b = toDouble $ 1 - ((fromIntegral y) * (1 - lk) + lk)
    in
       toWord8 (0.299 * r + 0.587 * g + 0.114 * b)
