module Life where
{- |
File defining how the Life data works as well as it's random instance
-}
import System.Random

data Life = Alive | Dead deriving (Eq, Enum,Bounded)

instance Show Life where
    show Alive = "⬛"
    show Dead = "⬜"

instance Random Life where
    randomR (a,b) g =
        case randomR (fromEnum a, fromEnum b) g of
          (x, g') -> (toEnum x, g')
    random g = randomR (minBound,maxBound) g
