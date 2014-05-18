module NLP.Grabber.Download where
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Tree.NTree.TypeDefs
import           Network.HTTP
import           Network.URI
import           Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative

openUrl :: Text -> MaybeT IO Text
openUrl url = let surl = T.unpack url in
  case parseURI surl of
    Nothing -> fail ""
    Just u  -> T.pack <$> liftIO ( getResponseBody =<< simpleHTTP (mkRequest GET u))

download :: Bool -> Text -> IO (IOSArrow XmlTree (NTree XNode))
download asHTML url = let surl=T.unpack url in
  return $ readDocument config surl
    where
        config= if asHTML
            then withParseHTML yes : baseConfig
            else baseConfig
        baseConfig = [
            withWarnings no
            , withHTTP []
            , withRedirect True
            , withEncodingErrors no]


