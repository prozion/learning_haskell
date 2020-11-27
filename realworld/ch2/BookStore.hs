data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID ReviewBody
type CustomerID = Int
type ReviewBody = String

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                  | CashOnDelivery
                  | Invoice CustomerID
                  deriving (Show)

type Vector = (Double, Double)
data Shape = Circle Vector Double
            | Poly [Vector]

bookId (Book id _ _) = id

data Customer = Customer {
      customerID :: CustomerID
    , customerName :: String
    , customerAddress :: Address
    } deriving (Show, Eq)

customer1 = Customer 271828 "J.R.Hacker" ["255 Syntax Ct", "Milpitas, CA 95134", "USA"]

customer2 = Customer {
              customerID = 271828,
              customerAddress = ["1048576 Disk Drive", "Milpitas, CA 95134", "USA"],
              customerName = "Jane Q. Citizen"
            }
