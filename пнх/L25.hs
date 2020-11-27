import System.Environment
import System.IO
import System.Random
import Control.Monad
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

splitOn :: String -> String -> [String]
splitOn delimeter astr = map T.unpack (T.splitOn (T.pack delimeter) (T.pack astr))

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes =
    mconcat [before, changed, after]
  where (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed = BC.reverse (BC.sort target)

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes =
    mconcat [before, changed, after]
  where (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed = BC.reverse target

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  chV <- randomRIO (0, 255)
  return (replaceByte location chV bytes)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
  let bytesLength = BC.length bytes
  sectionSize  <- randomRIO (2, bytesLength)
  start <- randomRIO (0, bytesLength - sectionSize)
  return (reverseSection start sectionSize bytes)

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

compareSymbolsVsSize :: FilePath -> IO ()
compareSymbolsVsSize filename = do
  input <- BC.readFile filename
  putStrLn ("Number of symbols: " <> show (T.length (E.decodeUtf8 input)) <> ", size of file: " <> show (B.length input))
