module Auto.Auto where
import Data.Aeson
import Data.Text --(pack, unpack) 
import Data.Aeson.Key (fromString)

data Auto = Auto { year         :: Integer
               , makeandmodel   :: String
               , mileage        :: String
               , color          :: String
               , engine         :: String
               , price          :: Double
               } deriving Show

instance ToJSON Auto where
  toJSON (Auto year makeandmodel mileage color engine price) =
    object [(fromString "year"          ) .= year,
            (fromString "makeandmodel"  ) .= makeandmodel,
            (fromString "mileage"       ) .= mileage,
            (fromString "color"         ) .= color,
            (fromString "engine"        ) .= engine,
            (fromString "price"         ) .= price]