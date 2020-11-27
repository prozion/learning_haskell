import qualified Data.Map as Map
import L29

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [ (1, "nYarlathoTep"),
                            (2, "KINGinYELLOW"),
                            (3, "dagon1997"),
                            (4, "rcarter1919"),
                            (5, "xCTHULHUx"),
                            (6, "yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [ ("nYarlathoTep", 2000),
                            ("KINGinYELLOW", 15000),
                            ("dagon1997", 300),
                            ("rcarter1919", 12),
                            ("xCTHULHUx", 50000),
                            ("yogSOThoth", 150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

-- 30.1
allFmapM :: Monad m => (a -> b) -> m a -> m b
--  (a -> b) to (a -> m b)
allFmapM func val = val >>= (\ x -> return (func x))

-- 30.2
allMap :: Monad m => m (a -> b) -> m a -> m b
-- m (a -> b) to (a -> m b)
allMap func val = func >>= (\ f -> val >>= (\ x -> return (f x)))

-- 30.1
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just val) f = f val
