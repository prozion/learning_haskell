import System.Environment
import System.IO
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main1 :: IO ()
main1 = do
  args <- getArgs
  let file1 = head args
  let file2 = head . tail $ args
  content <- readFile file1
  writeFile file2 content

main2 :: IO ()
main2 = do
  args <- getArgs
  let fileName = head args
  hFile <- openFile fileName ReadMode
  content <- hGetContents hFile
  let newContent = map toUpper content
  putStrLn newContent
  hClose hFile
  hFile <- openFile fileName WriteMode
  hPutStr hFile newContent
  hClose hFile

main3 :: IO ()
main3 = do
  args <- getArgs
  let fileName = head args
  input <- TI.readFile fileName
  TI.writeFile fileName (T.toUpper input)
