import Ai.Evolutionary ( EvolutionConfig(EvolutionConfig), evolve )
import Codec.Picture ( DynamicImage, Image, PixelRGB8, convertRGB8, readImage )
import Data.Colour ( LAB, RGB(RGB), lab2rgb )
import qualified Data.Vector as V
import Stylix.Output ( makeOutputTable )
import Stylix.Palette ( )
import System.Environment ( getArgs )
import System.Exit ( die )
import Text.JSON ( encode )

-- | Run the genetic algorithm to generate a palette from the given image.
selectColours :: (Floating a, Real a)
              => String -- ^ Scheme type: "either", "light" or "dark"
              -> Image PixelRGB8 -- ^ Source image
              -> IO (V.Vector (LAB a)) -- ^ Generated palette
selectColours polarity image
  = evolve (polarity, image) (EvolutionConfig 1000 100 0.5 0.01)

-- | Load an image file.
loadImage :: String -- ^ Path to the file
          -> IO DynamicImage
loadImage input = either error id <$> readImage input

mainProcess :: (String, String, String) -> IO ()
mainProcess (polarity, input, output) = do
  putStrLn $ "Processing " ++ input

  image <- loadImage input
  palette <- selectColours polarity (convertRGB8 image)
  let outputTable = makeOutputTable $ V.map lab2rgb palette

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
