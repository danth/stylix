import Ai.Evolutionary ( EvolutionConfig(EvolutionConfig), evolve )
import Codec.Picture ( DynamicImage, Image(imageWidth, imageHeight), PixelRGB8(PixelRGB8), convertRGB8, pixelAt, readImage )
import Data.Colour ( LAB, RGB(RGB), lab2rgb, rgb2lab )
import qualified Data.Vector as V
import Stylix.Output ( makeOutputTable )
import Stylix.Palette ( )
import System.Environment ( getArgs )
import System.Exit ( die )
import System.Random ( mkStdGen )
import Text.JSON ( encode )

-- | Run the genetic algorithm to generate a palette from the given image.
selectColours :: (Floating a, Real a)
              => String -- ^ Scheme type: "either", "light" or "dark"
              -> V.Vector (LAB a) -- ^ Colours of the source image
              -> V.Vector (LAB a) -- ^ Generated palette
selectColours polarity image
  = snd $ evolve (polarity, image) (EvolutionConfig 1000 100 0.5 0.01) (mkStdGen 0)

-- | Convert a 'DynamicImage' to a simple 'V.Vector' of colours.
unpackImage :: (Num a) => DynamicImage -> V.Vector (RGB a)
unpackImage image = do
  let image' = convertRGB8 image
  x <- V.enumFromN 0 (imageWidth image')
  y <- V.enumFromN 0 (imageHeight image')
  let (PixelRGB8 r g b) = pixelAt image' x y
  return $ RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | Load an image file.
loadImage :: String -- ^ Path to the file
          -> IO DynamicImage
loadImage input = either error id <$> readImage input

mainProcess :: (String, String, String) -> IO ()
mainProcess (polarity, input, output) = do
  putStrLn $ "Processing " ++ input
  image <- loadImage input
  let outputTable = makeOutputTable
                  $ V.map lab2rgb
                  $ selectColours polarity
                  $ V.map rgb2lab
                  $ unpackImage image
  writeFile output $ encode outputTable
  putStrLn $ "Saved to " ++ output

parseArguments :: [String] -> Either String (String, String, String)
parseArguments [polarity, input, output] = Right (polarity, input, output)
parseArguments [_, _] = Left "Please specify an output file"
parseArguments [_] = Left "Please specify an image"
parseArguments [] = Left "Please specify a polarity: either, light or dark"
parseArguments _ = Left "Too many arguments"

main :: IO ()
main = either die mainProcess . parseArguments =<< getArgs
