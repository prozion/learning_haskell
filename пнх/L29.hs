module L29 where
  
import Data.List

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func val1 = pure func <*> val1

example :: Maybe Int
example = (pure (*)) <*> ((pure (+)) <*> (pure 2) <*> (pure 4)) <*> (pure 6)

type Bottle = Int

yesterdayBeerIn, lastNightBeerOut, todayFriends, bottlesPerPersonPerNight :: [Bottle]

localDwellers = 2 :: Int

yesterdayBeerIn = [6, 12]
lastNightBeerOut = [4]
todayFriends = [2, 3]
bottlesPerPersonPerNight = [3, 4]


beerToBeConsumed, beerToBuyVariants :: [Bottle]
beerToBuy :: Bottle
beerToBeConsumed = pure (*) <*> (map (+ localDwellers) todayFriends) <*> bottlesPerPersonPerNight
beerToBuyVariants = pure (-) <*> beerToBeConsumed <*> (pure (-) <*> yesterdayBeerIn <*> lastNightBeerOut)
beerToBuy = maximum beerToBuyVariants

startingBeer :: [Int]
startingBeer = [6, 12]

remainingBeer :: [Int]
remainingBeer = (\count -> count - 4) <$> startingBeer

guests :: [Int]
guests = [2, 3]

totalPeople :: [Int]
totalPeople = (+ 2) <$> guests

beersPerGuest :: [Int]
beersPerGuest = [3, 4]

totalBeersNeeded :: [Int]
totalBeersNeeded = (pure (*)) <*> beersPerGuest <*> totalPeople

beersToPurchase :: [Int]
beersToPurchase = (pure (-)) <*> totalBeersNeeded <*> remainingBeer

main :: IO ()
main = do
  putStrLn $ show $ (sort beersToPurchase) == (sort beerToBuyVariants)
