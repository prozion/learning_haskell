module MainGlitch where

import qualified Data.ByteString.Char8 as BC
import Glitch

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  imageFile <- BC.readFile path
  glitched <- foldM
                (\bytes func -> func bytes)
                [ randomReplaceByte, randomSortSection, randomReplaceByte, randomSortSection, randomReplaceByte]
  let splittedPath = splitOn "/" path
  let fileName = last splittedPath
  let directories = init splittedPath
  let directoriesPath = (L.intercalate "/" directories) <> if (null directories) then "" else "/"
  let glitchedFileName = directoriesPath <> "glitched_" <> fileName
  BC.writeFile glitchedFileName glitched
