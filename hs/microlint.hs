-- | Read a Microscope JSON file and verify that it's correct.
module Main where
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Microscope.JSON
import Microscope.Types

main = do
  as <- getArgs
  mapM_ verifyFile as

verifyFile :: FilePath -> IO ()
verifyFile f = do
  s <- BL.readFile f
  case eitherDecode s of
    Right world -> verifyWorld world
    Left err    -> putStrLn $ "File doesn't parse: " ++ err

verifyWorld :: World -> IO ()
verifyWorld w = return ()
