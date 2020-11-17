type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          deriving (Show)
data Author = Author Name deriving (Show)
data Artist = Person Name | Band String deriving (Show)

data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)

data Book = Book {
  author :: Creator,
  isbn :: String,
  bookTitle :: String,
  bookYear :: Int,
  bookPrice :: Double }

data VinylRecord = VinylRecord {
  artist :: Creator,
  recordTitle :: String,
  recordYear :: Int,
  recordPrice :: Double }

data CollectibleToy = CollectibleToy {
  name :: String,
  description :: String,
  toyPrice :: Double }

data Pamphlet = Pamphlet {
  pamphletTitle :: String,
  pamphletDescription :: String,
  contactInfo :: String }

data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"

data Shape = Circle {
              x :: Double,
              y :: Double,
              radius :: Double }
           | Square {
              x0 :: Double,
              y0 :: Double,
              side :: Double }
           | Rectangle {
              x1 :: Double,
              y1 :: Double,
              x2 :: Double,
              y2 :: Double }
           deriving (Show)

perimeter :: Shape -> Double
perimeter (Circle _ _ r) = 2 * pi * r
perimeter (Square _ _ side) = 4 * side
perimeter (Rectangle x1 y1 x2 y2) = 2 * (abs (x1 - x2) + abs (y1 - y2))

square :: Shape -> Double
square (Circle _ _ r) = pi * r^2
square (Square _ _ side) = side^2
square (Rectangle x1 y1 x2 y2) = abs (x1 - x2) * abs (y1 - y2)

testBook = (BookItem
              (Book
                (AuthorCreator
                  (Author
                    (TwoInitialsWithLast 'H' 'P' "Lovecraft")))
                "000-00-11"
                "The Lurker at the Threshold"
                1965
                2.95))
