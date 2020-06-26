module ADT (adt) where

-- sum type
data Color = Red | Blue | Green deriving (Show, Eq)

prettyPrint :: Color -> IO ()
prettyPrint color =
  case color of
    Red   -> print $ show Red
    Blue  -> print $ show Blue
    Green -> print $ show Green

-- product type
-- data RGB = RGB Int Int Int deriving Show
data RGB =
  RGB {
    red :: Int,
    green :: Int,
    blue :: Int
  } deriving Eq

readRed :: RGB -> Int
readRed (RGB r _ _) = r -- Pattern Matching

data PrettyRGB = PrettyRGB { rgb :: RGB }
--newtype PrettyRGB = PrettyRGB { rgb :: RGB }

-- instance of TypeClass Show(PrettyRGB)
instance Show PrettyRGB where
  show pRgb = let pRgb' = rgb pRgb in
    "RGB(red = " ++ show (red pRgb') ++
    ", green = " ++ show (green pRgb') ++ ", " ++
    ", blue = " ++ show (blue pRgb') ++
    ")"

  showsPrec = undefined

instance Eq PrettyRGB where
  (==) pRgb1 pRgb2 = rgb pRgb1 == rgb pRgb2

adt :: IO ()
adt = do
  prettyPrint Red
  let rgb = RGB 1 2 3
  let pRgb = PrettyRGB rgb
  print $ show pRgb
  print $ show $ readRed rgb
  let pRgb2 = PrettyRGB $ RGB 1 2 2
  print $ pRgb == pRgb2