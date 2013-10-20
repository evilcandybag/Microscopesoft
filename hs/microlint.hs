-- | Read a Microscope JSON file and verify that it's correct.
module Main where
import Control.Monad
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Maybe
import Data.List
import Microscope.JSON
import Microscope.Types
import Microscope.References
import Microscope.Query

main = do
  as <- getArgs
  mapM_ verifyFile as

-- | Verify that a file is OK, including parsing and contents.
verifyFile :: FilePath -> IO ()
verifyFile f = do
  s <- BL.readFile f
  case eitherDecode s of
    Right world -> verifyWorld world
    Left err    -> putStrLn $ "File doesn't parse: " ++ err

-- | Verify that a World structure is OK.
verifyWorld :: World -> IO ()
verifyWorld w = do
    when (not $ null unresolved) $ do
      putStrLn "*WARNING* unresolved references:"
      forM_ unresolved $ \ref -> do
        putStrLn $ "  " ++ show ref
  where
    unresolved = nubSort [r | r <- refsIn w, isNothing (named w r)]

nubSort :: (Eq a, Ord a) => [a] -> [a]
nubSort = map head . group . sort
