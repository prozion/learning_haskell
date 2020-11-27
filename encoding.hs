import GHC.IO.Encoding (getLocaleEncoding)

main :: IO ()
main = getLocaleEncoding >>= print
