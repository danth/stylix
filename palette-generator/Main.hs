import Codec.Picture ( DynamicImage, Image(imageWidth, imageHeight), PixelRGB8(PixelRGB8), convertRGB8, pixelAt, readImage )
import Data.Bifunctor ( second )
import Data.List ( sortOn )
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

selectColours :: [HSV Float] -> [(String, HSV Float)]
selectColours image = zip names palette

  where names :: [String]
        names = map (printf "base%02X") ([0..15] :: [Int])

        hueInRange :: (Ord a) => a -> a -> HSV a -> Bool
        hueInRange low high (HSV hue _ _) = hue >= low && hue < high

        binThresholds :: [Float]
        binThresholds = [i * (6/9) | i <- [1..9]]

        average :: (Fractional a) => [a] -> a
        average xs = sum xs / fromIntegral (length xs)

        averageColour :: (Fractional a) => [HSV a] -> HSV a
        averageColour colours = HSV (average $ map (\(HSV h _ _) -> h) colours)
                                    (average $ map (\(HSV _ s _) -> s) colours)
                                    (average $ map (\(HSV _ _ v) -> v) colours)

        bins :: [HSV Float]
        bins = map averageColour
             $ sortOn length
             $ map (\bin -> filter (hueInRange (bin - (6/9)) bin) image)
               binThresholds

        primaryScale :: [HSV Float]
        primaryScale = [HSV h s (v / 8) | v <- [1..8]]
          where (HSV h s _) = head bins

        palette :: [HSV Float]
        palette = primaryScale ++ tail bins

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
