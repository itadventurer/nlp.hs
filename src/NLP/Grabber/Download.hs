module NLP.Grabber.Download where
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Data.Tree.NTree.TypeDefs
import           Network.HTTP
import           Network.URI
import           Text.XML.HXT.Core
import Text.XML.HXT.HTTP

openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

getXML :: String -> IO (IOSArrow XmlTree (NTree XNode))
getXML url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withWarnings no,
    withEncodingErrors yes] (fromMaybe "" contents)

get :: Bool -> String -> IO (IOSArrow XmlTree (NTree XNode))
get asHTML url = return $ readDocument config url
    where
        config= if asHTML
            then (withParseHTML yes) : baseConfig
            else baseConfig
        baseConfig = [
            withWarnings no
            , withHTTP []
            , withRedirect True
            , withEncodingErrors no]


