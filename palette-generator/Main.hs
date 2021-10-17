import Codec.Picture ( DynamicImage, Image(imageWidth, imageHeight), PixelRGB8(PixelRGB8), convertRGB8, pixelAt, readImage )
import Data.Bifunctor ( second )
import Data.List ( sortOn )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Word ( Word8 )
import RGBHSV ( HSV(HSV), RGB(RGB), hsvToRgb, rgbToHsv )
import System.Environment ( getArgs )
import System.Exit ( die )
import Text.JSON ( JSObject, encode, toJSObject )
import Text.Printf ( printf )

type OutputTable = JSObject String

makeOutputTable :: [(String, RGB Float)] -> OutputTable
makeOutputTable = toJSObject . concatMap makeOutputs

  where makeOutputs :: (String, RGB Float) -> [(String, String)]
        makeOutputs (name, RGB r g b) =
          [ (name ++ "-dec-r", show $ r / 255)
          , (name ++ "-dec-g", show $ g / 255)
          , (name ++ "-dec-b", show $ b / 255)
          , (name ++ "-rgb-r", show r')
          , (name ++ "-rgb-g", show g')
          , (name ++ "-rgb-b", show b')
          , (name ++ "-hex-r", printf "%02x" r')
          , (name ++ "-hex-g", printf "%02x" g')
          , (name ++ "-hex-b", printf "%02x" b')
          , (name ++ "-hex", printf "%02x%02x%02x" r' g' b')
          , (name ++ "-hash", printf "#%02x%02x%02x" r' g' b')
          ]
            where r' :: Word8
                  r' = round r
                  g' :: Word8
                  g' = round g
                  b' :: Word8
                  b' = round b

data Bin a = Bin Int a a a
type Bins a = Map Int (Bin a)

selectColours :: [HSV Float] -> [(String, HSV Float)]
selectColours image = zip names palette

  where names :: [String]
        names = map (printf "base%02X") ([0..15] :: [Int])

        emptyBin :: (Num a) => Bin a
        emptyBin = Bin 0 0 0 0

        emptyBins :: (Num a) => Bins a
        emptyBins = Map.fromList [(x, emptyBin) | x <- [0..8]]

        insertToBin :: (Num a) => HSV a -> Bin a -> Bin a
        insertToBin (HSV h s v) (Bin count h' s' v')
          = Bin (count + 1) (h' + h) (s' + s) (v' + v)

        allocateToBin :: (Fractional a, Num a, RealFrac a) => HSV a -> Bins a -> Bins a
        allocateToBin colour = Map.adjust (insertToBin colour) bin
          where (HSV h _ _) = colour
                bin = floor $ (h / 6) * 9

        bins :: Bins Float
        bins = foldr allocateToBin emptyBins image

        binSize :: Bin a -> Int
        binSize (Bin size _ _ _) = size

        averageColour :: (Fractional a) => Bin a -> HSV a
        averageColour (Bin size h' s' v')
          = HSV (h' / size') (s' / size') (v' / size')
          where size' = fromIntegral size

        shortlist :: [HSV Float]
        shortlist = map averageColour $ sortOn binSize $ Map.elems bins

        primaryScale :: [HSV Float]
        primaryScale = [HSV h s (v / 8) | v <- [1..8]]
          where (HSV h s _) = head shortlist

        secondaryScale :: [HSV Float]
        secondaryScale = sortOn (\(HSV h _ _) -> h) $ tail shortlist

        palette :: [HSV Float]
        palette = primaryScale ++ secondaryScale

unpackImage :: DynamicImage -> [RGB Float]
unpackImage image = do
  let image' = convertRGB8 image
  x <- [0 .. imageWidth image' - 1]
  y <- [0 .. imageHeight image' - 1]
  let (PixelRGB8 r g b) = pixelAt image' x y
  return (RGB (fromIntegral r) (fromIntegral g) (fromIntegral b))

loadImage :: String -> IO DynamicImage
loadImage input = either error id <$> readImage input

main :: IO ()
main = either die mainProcess . parseArguments =<< getArgs

  where parseArguments :: [String] -> Either String (String, String)
        parseArguments [input, output] = Right (input, output)
        parseArguments [_] = Left "Please specify an output file"
        parseArguments [] = Left "Please specify an image"
        parseArguments _ = Left "Too many arguments"

        mainProcess :: (String, String) -> IO ()
        mainProcess (input, output) = do
          putStrLn $ "Processing " ++ input
          image <- loadImage input
          let outputTable = makeOutputTable
                          $ map (second hsvToRgb)
                          $ selectColours
                          $ map rgbToHsv
                          $ unpackImage image
          writeFile output $ encode outputTable
          putStrLn $ "Saved to " ++ output
