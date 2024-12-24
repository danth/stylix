import Ai.Evolutionary ( evolve )
import Codec.Picture ( DynamicImage, convertRGB8, readImage )
import Data.Colour ( lab2rgb )
import qualified Data.Vector as V
import Stylix.Output ( makeOutputTable )
import Stylix.Palette ( )
import System.Environment ( getArgs )
import System.Exit ( die )
import System.Random ( setStdGen, mkStdGen )
import Text.JSON ( encode )
import Text.Read (readMaybe)

-- String to float
stringToDouble :: String -> Double
stringToDouble s = case readMaybe s of
   Just x  -> x
   Nothing -> error "Invalid float value"

-- | Load an image file.
loadImage :: String -- ^ Path to the file
          -> IO DynamicImage
loadImage input = either error id <$> readImage input

mainProcess :: (String,String,String, String, String) -> IO ()
mainProcess (polarity, primaryScaleDark, primaryScaleLight, input, output) = do
  putStrLn $ "Processing " ++ input

  -- Random numbers must be deterministic when running inside Nix.
  setStdGen $ mkStdGen 0

  image <- loadImage input
  palette <- evolve (polarity,stringToDouble primaryScaleDark,stringToDouble primaryScaleLight,convertRGB8 image)
  let outputTable = makeOutputTable $ V.map lab2rgb palette

  writeFile output $ encode outputTable
  putStrLn $ "Saved to " ++ output

parseArguments :: [String] -> Either String (String, String,String,String, String)
parseArguments [polarity, primaryScaleDark, primaryScaleLight,input, output] = Right (polarity,primaryScaleDark, primaryScaleLight, input, output)
parseArguments [_, _,_,_] = Left "Please specify an output file"
parseArguments [_, _,_] = Left "Please specify an image"
parseArguments [_, _] = Left "Please specify a primary scaling factor for the light polarity"
parseArguments [_] = Left "Please specify a primary scaling factor for the dark polarity"
parseArguments [] = Left "Please specify a polarity: either, light or dark"
parseArguments _ = Left "Too many arguments"

main :: IO ()
main = either die mainProcess . parseArguments =<< getArgs
