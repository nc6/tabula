module Tabula.Destination.File where
  import Data.Aeson (encode)
  import qualified Data.ByteString.Lazy.Char8 as B
  
  import Tabula.Destination

  fileDestination :: FilePath -> Destination
  fileDestination fp = Destination {
    makeEntry = B.appendFile fp . encode
  }