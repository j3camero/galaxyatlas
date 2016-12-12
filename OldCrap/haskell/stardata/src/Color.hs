-- Code to deal with star colors

module Color where

data Color = RGB
             { colorR :: Double
             , colorG :: Double
             , colorB :: Double
             }
           deriving (Show,Eq)

addColors :: Color -> Color -> Color
addColors a b =
  RGB { colorR = ((colorR a) + (colorR b))
      , colorG = ((colorG a) + (colorG b))
      , colorB = ((colorB a) + (colorB b))
      }

scaleColor :: Color -> Double -> Color
scaleColor clr sf =
  RGB { colorR = sf * (colorR clr)
      , colorG = sf * (colorG clr)
      , colorB = sf * (colorB clr)
      }

