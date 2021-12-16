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

selectColours :: (Floating a, Real a) => V.Vector (LAB a) -> V.Vector (LAB a)
selectColours image
  = snd $ evolve image (EvolutionConfig 1000 100 0.5 150) (mkStdGen 0)

unpackImage :: (Num a) => DynamicImage -> V.Vector (RGB a)
unpackImage image = do
  let image' = convertRGB8 image
  x <- V.enumFromN 0 (imageWidth image')
  y <- V.enumFromN 0 (imageHeight image')
  let (PixelRGB8 r g b) = pixelAt image' x y
  return $ RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

loadImage :: String -> IO DynamicImage
loadImage input = either error id <$> readImage input

mainProcess :: (String, String) -> IO ()
mainProcess (input, output) = do
  putStrLn $ "Processing " ++ input
  image <- loadImage input
  let outputTable = makeOutputTable
                  $ V.map lab2rgb
                  $ selectColours
                  $ V.map rgb2lab
                  $ unpackImage image
  writeFile output $ encode outputTable
  putStrLn $ "Saved to " ++ output

parseArguments :: [String] -> Either String (String, String)
parseArguments [input, output] = Right (input, output)
parseArguments [_] = Left "Please specify an output file"
parseArguments [] = Left "Please specify an image"
parseArguments _ = Left "Too many arguments"

main :: IO ()
main = either die mainProcess . parseArguments =<< getArgs
