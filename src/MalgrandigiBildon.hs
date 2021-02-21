{-# LANGUAGE RecordWildCards #-}

{-|
    la 'MalgrandigiBildon' modulo esta bazitas sur la kodo de "https://github.com/afcondon/juicy-pixel-processor" (MIT-a licenso) 
-}

module MalgrandigiBildon 
(
    malgrandi
) where

import           Codec.Picture               
import qualified Codec.Picture.Extra    as E  (scaleBilinear)
import           Codec.Picture.Types    as T  (newMutableImage, unsafeFreezeImage, convertImage, dropTransparency, promotePixel) 
import           Control.Monad.ST             (runST)


malgrandi :: DynamicImage -> Int -> DynamicImage
malgrandi bildo novLargxo = 
    let
      (largxo, alto) = getWidthHeight bildo 
    in 
      if (largxo > novLargxo) then
        let
          (nLargxo, nAlto) = chooseNewDimensions novLargxo (largxo, alto)
        in resize (nLargxo, nAlto) bildo
      else 
        bildo

getWidthHeight :: DynamicImage -> (Int, Int)
getWidthHeight bildon = (dynamicMap imageWidth bildon, dynamicMap imageHeight bildon)

{-|
    Tie estas la kodo de Afcondon
-}

-- calc needs to be done as Rationals and then converted back to Ints
chooseNewDimensions :: Int -> (Int, Int) -> (Int, Int)
chooseNewDimensions le (w,h) =
  toInts $ scaleTo (toRational le) (toRational w, toRational  h)
  where
    toInts (w,h) = (floor w, floor h)
    scaleTo le (w,h) =
      let ratio = le / w  -- tie estas la sola kodlinio, kiun mi sxangxis.
      in (w * ratio, h * ratio)

resize :: (Int,Int) -> DynamicImage -> DynamicImage
resize (w,h) (ImageRGB8 image)  = ImageRGB8 $ E.scaleBilinear w h image
resize (w,h) (ImageRGB16 image) = ImageRGB16 $ scaleBilinear16 w h image
resize (w,h) (ImageYCbCr8 image) = ImageRGB8 $ E.scaleBilinear w h (T.convertImage image)
resize (w,h) (ImageCMYK8 image)  = ImageRGB8 $ E.scaleBilinear w h (T.convertImage image)
resize (w,h) (ImageCMYK16 image) = ImageRGB16 $ scaleBilinear16 w h (T.convertImage image)

resize (w,h) (ImageRGBA16 image) = ImageRGB16 $ scaleBilinear16 w h (pixelMap T.dropTransparency image)
resize (w,h) (ImageYA16 image) = ImageRGB16 $ scaleBilinear16 w h (pixelMap T.promotePixel image) -- ?
resize (w,h) (ImageRGBA8 image)  = ImageRGB8 $ E.scaleBilinear w h (pixelMap T.dropTransparency image) 
resize (w,h) (ImageY8 image)  = ImageRGB8 $ E.scaleBilinear w h (pixelMap T.promotePixel image)
resize (w,h) (ImageY16 image)  = ImageRGB16 $ E.scaleBilinear w h (pixelMap T.promotePixel image)
resize (w,h) (ImageYA8 image)  = ImageRGB8 $ E.scaleBilinear w h (pixelMap T.promotePixel image)

resize _ other = other -- file is unchanged if it's a format we can't resize TODO


-- straight copy from JuicyPixels-extras adapted for 16 bit images
scaleBilinear16
  :: Int               -- ^ Desired width
  -> Int               -- ^ Desired height
  -> Image PixelRGB16   -- ^ Original image
  -> Image PixelRGB16   -- ^ Scaled image
scaleBilinear16 width height img@Image {..} = runST $ do
  mimg <- T.newMutableImage width height
  let sx, sy :: Float
      sx = fromIntegral imageWidth  / fromIntegral width
      sy = fromIntegral imageHeight / fromIntegral height
      go x' y'
        | x' >= width = go 0 (y' + 1)
        | y' >= height = T.unsafeFreezeImage mimg
        | otherwise = do
            let xf = fromIntegral x' * sx
                yf = fromIntegral y' * sy
                x, y :: Int
                x  = floor xf
                y  = floor yf
                δx = xf - fromIntegral x
                δy = yf - fromIntegral y
                pixelAt' i j =
                  if i >= imageWidth || j >= imageHeight
                    then PixelRGB16 0 0 0
                    else pixelAt img i j
            writePixel mimg x' y' $
              mulp16 (pixelAt' x y) ((1 - δx) * (1 - δy)) `addp16`
              mulp16 (pixelAt' (x + 1) y) (δx * (1 - δy)) `addp16`
              mulp16 (pixelAt' x (y + 1)) ((1 - δx) * δy) `addp16`
              mulp16 (pixelAt' (x + 1) (y + 1)) (δx * δy)
            go (x' + 1) y'
  go 0 0

mulp16 :: PixelRGB16 -> Float -> PixelRGB16
mulp16 pixel x = colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp16 #-}

addp16 :: PixelRGB16 -> PixelRGB16 -> PixelRGB16
addp16 = mixWith (const f)
  where
    f x y = fromIntegral $
      (0xff :: Pixel16) `min` (fromIntegral x + fromIntegral y)
{-# INLINE addp16 #-}