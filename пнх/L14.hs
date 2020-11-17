data FooType = Foo | Bar | Baz deriving (Show, Enum)

instance Eq FooType where
  (==) a b = (fromEnum a) == (fromEnum b)

instance Ord FooType where
  (<=) a b = (fromEnum a) <= (fromEnum b)

class Die a where
  roll :: Int -> a
  foobar :: a -> a
  -- foobar a = roll 1

data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show)

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)

t = S2 == S3
