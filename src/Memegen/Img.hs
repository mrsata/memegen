module Memegen.Img
    ( createMeme
    ) where

import qualified Graphics.GD as GD
import qualified Data.ByteString as B

borderColor :: GD.Color
borderColor = GD.rgb 0 0 0

textColor :: GD.Color
textColor = GD.rgb 255 255 255

textSize :: Double
textSize = 32.0

createMeme :: B.ByteString -> String -> String -> IO B.ByteString
createMeme imgBs upperText lowerText = do
  img <- GD.loadJpegByteString imgBs
  (imgW, imgH) <- GD.imageSize img

  _ <- GD.useFontConfig True

  -- Draw upper text
  (_, (lrx, lry), _, (ulx, uly))
      <- GD.measureString "sans" textSize 0.0 (0, 0) upperText textColor
  let (textW, textH) = (1 + lrx - ulx, 1 + lry - uly)
  let (upperPosX, upperPosY) = (imgW `div` 2 - textW `div` 2, textH + 10)
  let upperPos = (upperPosX, upperPosY)
  let upperBorder = [(i, j)
                    | i <- [upperPosX - 2 .. upperPosX + 2]
                    , j <- [upperPosY - 2 .. upperPosY + 2]
                    ]
  mapM_ (\x -> GD.drawString "sans" textSize 0.0 x upperText borderColor img)
        upperBorder
  _ <- GD.drawString "sans" textSize 0.0 upperPos upperText textColor img

  -- Draw lower text
  (_, (lrx, lry), _, (ulx, uly))
      <- GD.measureString "sans" textSize 0.0 (0, 0) lowerText textColor
  let (textW, textH) = (1 + lrx - ulx, 1 + lry - uly)
  let (lowerPosX, lowerPosY) = (imgW `div` 2 - textW `div` 2, imgH - 20)
  let lowerPos = (lowerPosX, lowerPosY)
  let lowerBorder = [(i, j)
                    | i <- [lowerPosX - 2 .. lowerPosX + 2]
                    , j <- [lowerPosY - 2 .. lowerPosY + 2]
                    ]
  mapM_ (\x -> GD.drawString "sans" textSize 0.0 x lowerText borderColor img)
        lowerBorder
  _ <- GD.drawString "sans" textSize 0.0 lowerPos lowerText textColor img

  GD.saveJpegByteString 100 img
